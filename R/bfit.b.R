

bfitClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "bfitClass",
    inherit = bfitBase,
    private = list(
      .htmlwidget = NULL,
      
      .init = function() {
        
        if (self$options$mode == 'simple') {
          private$.htmlwidget <- HTMLWidget$new()
          
          if (is.null(self$data) | is.null(self$options$vars)) {
            self$results$instructions$setVisible(visible = TRUE)
          }
          
          self$results$instructions$setContent(
            private$.htmlwidget$generate_accordion(
              title = "Instructions",
              content = paste(
                '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
                '<div style="text-align:justify;">',
                '<ul>',
                '<li>The traditional Rasch model is performed by the mixRasch R package using Joint Maximum Likelihood (JML).</li>',
                '<li>Specify <b>Step(number of category-1) and Bootstrap N</b> in the Analysis options.</li>',
                '<li>For methodological details, see Seol, H. (2016). Using the Bootstrap Method to Evaluate the Critical Range of Misfit for Polytomous Rasch Fit Statistics. Psychological Reports, 118, 937–956.</li>',
                '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
                '</ul></div></div>'
              )
            )
          )
          
          self$results$text1$setContent(
            paste0(
              '<div style="margin: 18px 0 22px 0; padding: 14px 16px; ',
              'border-left: 4px solid #2f65a7; background-color: #f7fbff; ',
              'font-family: Arial, sans-serif; color: #333;">',
              
              '<div style="font-size: 15px; font-weight: 700; color: #2f65a7; margin-bottom: 10px;">',
              'Common guideline for item fit',
              '</div>',
              
              '<div style="font-size: 13px; line-height: 1.7;">',
              '<b>Acceptable:</b> 0.5 &le; Infit and Outfit &le; 1.5<br>',
              '<b>Possible overfit:</b> Infit or Outfit &lt; 0.5<br>',
              '<b>Possible misfit:</b> Infit or Outfit &gt; 1.5<br><br>',
              'If Infit and Outfit show different signals, values above 1.5 are usually treated as more important for detecting misfit.<br>',
              'These guidelines are screening aids and should not be used as strict decision rules.',
              '</div>',
              
              '</div>'
            )
          )
        }
        
        if (self$options$mode == "complex") {
          private$.htmlwidget <- HTMLWidget$new()
          
          if (is.null(self$data) | is.null(self$options$vars1)) {
            self$results$instructions1$setVisible(visible = TRUE)
          }
          
          self$results$instructions$setContent(
            private$.htmlwidget$generate_accordion(
              title = "Instructions",
              content = paste(
                '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
                '<div style="text-align:justify;">',
                '<ul>',
                '<li>Specify <b>Type and Bootstrap N</b> in the Analysis options.</li>',
                '<li>To use the <b>correction methods</b>, uncheck the <b>Run</b> checkbox.</li>',
                '<li>A fitted Rasch model or Partial Credit Model in R package <b>eRm</b> is used to compute bootstrap fit statistics.</li>',
                '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
                '</ul></div></div>'
              )
            )
          )
          
          self$results$outfit$setNote(
            "Note",
            "Adj.p = Adjusted p-values for Multiple Comparisons. Diagnosis is based on adjusted p-values: * p < .05, ** p < .01."
          )
          
          self$results$infit$setNote(
            "Note",
            "Adj.p = Adjusted p-values for Multiple Comparisons. Diagnosis is based on adjusted p-values: * p < .05, ** p < .01."
          )
          
          self$results$noutfit$setNote(
            "Note",
            "Diagnosis: * p < .05, ** p < .01."
          )
          
          self$results$ninfit$setNote(
            "Note",
            "Diagnosis: * p < .05, ** p < .01."
          )
        }
        
        self$results$progressBarHTML$setVisible(FALSE)
      },
      
      .run = function() {
        
        if (self$options$mode == 'simple' && !isTRUE(self$options$run))
          return()
        
        if (self$options$mode == 'complex' && !isTRUE(self$options$run1))
          return()
        
        if (self$options$mode == 'simple') {
          if (is.null(self$options$vars) || length(self$options$vars) < 2)
            return()
        }
        
        if (self$options$mode == 'complex') {
          if (is.null(self$options$vars1) || length(self$options$vars1) < 2)
            return()
        }
        
        self$results$progressBarHTML$setVisible(TRUE)
        self$results$progressBarHTML$setContent(
          appleSpinnerH('Performing bootstrap item fit analysis...')
        )
        
        on.exit({
          self$results$progressBarHTML$setVisible(FALSE)
        }, add = TRUE)
        
        private$.checkpoint()
        
        if (self$options$mode == 'complex') {
          
          data <- self$data
          data <- na.omit(data)
          vars1 <- self$options$vars1
          bn1 <- self$options$bn1
          type <- self$options$type
          adj <- self$options$adj
          nco <- self$options$nco
          
          if (type == 'bi') {
            obj <- eRm::RM(data)
          } else if (type == 'ra') {
            obj <- eRm::PCM(data)
          }
          
          private$.checkpoint()
          
          if (nco == TRUE) {
            set.seed(1234)
            fit <- iarm::boot_fit(obj, B = bn1, p.adj = 'none')
            
            table <- self$results$noutfit
            
            outfit <- fit[[1]][, 1]
            outfit <- as.vector(outfit)
            
            pvalue <- fit[[1]][, 2]
            pvalue <- as.vector(pvalue)
            
            for (i in seq_along(vars1)) {
              row <- list()
              row[["fit"]] <- outfit[i]
              row[["p"]] <- pvalue[i]
              row[["diagnosis"]] <- if (is.na(pvalue[i])) "" else if (pvalue[i] < .01) "**" else if (pvalue[i] < .05) "*" else ""
              table$setRow(rowKey = vars1[i], values = row)
            }
            
            table <- self$results$ninfit
            
            infit <- fit[[1]][, 3]
            infit <- as.vector(infit)
            
            pvalue <- fit[[1]][, 4]
            pvalue <- as.vector(pvalue)
            
            for (i in seq_along(vars1)) {
              row <- list()
              row[["fit"]] <- infit[i]
              row[["p"]] <- pvalue[i]
              row[["diagnosis"]] <- if (is.na(pvalue[i])) "" else if (pvalue[i] < .01) "**" else if (pvalue[i] < .05) "*" else ""
              table$setRow(rowKey = vars1[i], values = row)
            }
            
          } else {
            set.seed(1234)
            fit <- iarm::boot_fit(obj, B = bn1, p.adj = adj)
            
            table <- self$results$outfit
            
            outfit <- fit[[1]][, 1]
            outfit <- as.vector(outfit)
            
            pvalue <- fit[[1]][, 2]
            pvalue <- as.vector(pvalue)
            
            padj <- fit[[1]][, 3]
            padj <- as.vector(padj)
            
            for (i in seq_along(vars1)) {
              row <- list()
              row[["fit"]] <- outfit[i]
              row[["p"]] <- pvalue[i]
              row[["adp"]] <- padj[i]
              row[["diagnosis"]] <- if (is.na(padj[i])) "" else if (padj[i] < .01) "**" else if (padj[i] < .05) "*" else ""
              table$setRow(rowKey = vars1[i], values = row)
            }
            
            table <- self$results$infit
            
            infit <- fit[[1]][, 4]
            infit <- as.vector(infit)
            
            pvalue <- fit[[1]][, 5]
            pvalue <- as.vector(pvalue)
            
            padj <- fit[[1]][, 6]
            padj <- as.vector(padj)
            
            for (i in seq_along(vars1)) {
              row <- list()
              row[["fit"]] <- infit[i]
              row[["p"]] <- pvalue[i]
              row[["adp"]] <- padj[i]
              row[["diagnosis"]] <- if (is.na(padj[i])) "" else if (padj[i] < .01) "**" else if (padj[i] < .05) "*" else ""
              table$setRow(rowKey = vars1[i], values = row)
            }
          }
          
          return()
        }
        
        ### Simple mode analysis ###
        data <- private$.cleanData()
        
        private$.checkpoint()
        
        results <- private$.compute(data)
        
        private$.populateInTable(results)
        private$.populateOutTable(results)
        private$.populateFitSummary(results)
        
        private$.prepareInPlot(results)
        private$.prepareOutPlot(results)
      },
      
      .compute = function(data) {
        vars <- self$options$vars
        step <- self$options$step
        bn   <- self$options$bn
        
        set.seed(1234)
        
        boot_stat <- function(data, indices) {
          d <- data[indices, , drop = FALSE]
          
          res1 <- mixRasch::mixRasch(
            data  = d,
            steps = step,
            model = "RSM",
            n.c   = 1
          )
          
          io <- res1$item.par$in.out
          
          c(
            io[, 1],
            io[, 3]
          )
        }
        
        private$.checkpoint()
        
        boot.res <- boot::boot(
          data = data,
          statistic = boot_stat,
          R = bn
        )
        
        nItems <- length(vars)
        
        all.raw  <- boot.res$t0
        all.boot <- boot.res$t
        
        infit.raw  <- all.raw[1:nItems]
        outfit.raw <- all.raw[(nItems + 1):(2 * nItems)]
        
        infit.mat  <- all.boot[, 1:nItems, drop = FALSE]
        outfit.mat <- all.boot[, (nItems + 1):(2 * nItems), drop = FALSE]
        
        infitlow   <- apply(infit.mat, 2, quantile, prob = 0.025, na.rm = TRUE)
        infithigh  <- apply(infit.mat, 2, quantile, prob = 0.975, na.rm = TRUE)
        
        outfitlow  <- apply(outfit.mat, 2, quantile, prob = 0.025, na.rm = TRUE)
        outfithigh <- apply(outfit.mat, 2, quantile, prob = 0.975, na.rm = TRUE)
        
        results <- list(
          infit      = infit.raw,
          outfit     = outfit.raw,
          infitlow   = infitlow,
          infithigh  = infithigh,
          outfitlow  = outfitlow,
          outfithigh = outfithigh
        )
        
        return(results)
      },
      
      .populateInTable = function(results) {
        table <- self$results$item$binfit
        vars <- self$options$vars
        
        infit <- results$infit
        infitlow <- results$infitlow
        infithigh <- results$infithigh
        
        for (i in seq_along(vars)) {
          row <- list()
          
          row[["infit"]] <- infit[i]
          row[["infitlow"]] <- infitlow[i]
          row[["infithigh"]] <- infithigh[i]
          
          table$setRow(rowKey = vars[i], values = row)
        }
      },
      
      .populateOutTable = function(results) {
        table <- self$results$item$boutfit
        vars <- self$options$vars
        
        outfit <- results$outfit
        outfitlow <- results$outfitlow
        outfithigh <- results$outfithigh
        
        for (i in seq_along(vars)) {
          row <- list()
          
          row[["outfit"]] <- outfit[i]
          row[['outfitlow']] <- outfitlow[i]
          row[['outfithigh']] <- outfithigh[i]
          
          table$setRow(rowKey = vars[i], values = row)
        }
      },
      
      .populateFitSummary = function(results) {
        
        if (!isTRUE(self$options$fitSummary))
          return()
        
        table <- self$results$item$fitSummary
        vars <- self$options$vars
        
        infit <- results$infit
        outfit <- results$outfit
        
        hasHigh <- (!is.na(infit) & infit > 1.5) |
          (!is.na(outfit) & outfit > 1.5)
        
        hasLow <- (!is.na(infit) & infit < 0.5) |
          (!is.na(outfit) & outfit < 0.5)
        
        acceptable <- !is.na(infit) & !is.na(outfit) &
          infit >= 0.5 & infit <= 1.5 &
          outfit >= 0.5 & outfit <= 1.5
        
        possibleMisfit <- hasHigh
        possibleOverfit <- hasLow & !hasHigh
        
        review <- possibleOverfit | possibleMisfit
        
        itemList <- function(x) {
          if (sum(x, na.rm = TRUE) == 0)
            return("None")
          
          paste(vars[which(x)], collapse = ", ")
        }
        
        table$setRow(
          rowNo = 1,
          values = list(
            criterion = "Acceptable",
            count = sum(acceptable, na.rm = TRUE),
            items = itemList(acceptable)
          )
        )
        
        table$setRow(
          rowNo = 2,
          values = list(
            criterion = "Possible overfit",
            count = sum(possibleOverfit, na.rm = TRUE),
            items = itemList(possibleOverfit)
          )
        )
        
        table$setRow(
          rowNo = 3,
          values = list(
            criterion = "Possible misfit",
            count = sum(possibleMisfit, na.rm = TRUE),
            items = itemList(possibleMisfit)
          )
        )
        
        table$setRow(
          rowNo = 4,
          values = list(
            criterion = "Items requiring review",
            count = sum(review, na.rm = TRUE),
            items = itemList(review)
          )
        )
      },
      
      .prepareInPlot = function(results) {
        item <- self$options$vars
        
        infit <- results$infit
        infitlow <- results$infitlow
        infithigh <- results$infithigh
        
        infit1 <- data.frame(item, infit, infitlow, infithigh)
        
        image <- self$results$inplot
        image$setState(infit1)
      },
      
      .inPlot = function(image, ggtheme, theme, ...) {
        
        if (self$options$mode != 'simple')
          return(FALSE)
        
        if (!isTRUE(self$options$run))
          return(FALSE)
        
        if (is.null(self$options$vars) || length(self$options$vars) < 2)
          return(FALSE)
        
        infit1 <- image$state
        
        if (is.null(infit1) || !is.data.frame(infit1))
          return(FALSE)
        
        needed <- c("item", "infit", "infitlow", "infithigh")
        if (!all(needed %in% names(infit1)))
          return(FALSE)
        
        infit1$item <- factor(infit1$item, levels = self$options$vars)
        
        plot <- ggplot2::ggplot(
          infit1,
          ggplot2::aes(x = item, y = infit, color = item)
        ) +
          ggplot2::geom_point(size = 3) +
          ggplot2::geom_errorbar(
            ggplot2::aes(ymin = infitlow, ymax = infithigh),
            width = 0.3
          ) +
          ggplot2::geom_hline(
            yintercept = 1,
            linetype = "dashed",
            color = "red"
          ) +
          ggplot2::labs(
            title = "",
            x = "Item",
            y = "Infit"
          ) +
          ggplot2::scale_color_brewer(palette = "Set1") +
          ggplot2::theme_minimal(base_size = 15) +
          ggplot2::guides(color = "none")
        
        if (!is.null(ggtheme))
          plot <- plot + ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot +
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(
                angle = self$options$angle,
                hjust = 1
              )
            )
        }
        
        print(plot)
        TRUE
      },
      
      .prepareOutPlot = function(results) {
        item <- self$options$vars
        
        outfit <- results$outfit
        outfitlow <- results$outfitlow
        outfithigh <- results$outfithigh
        
        outfit1 <- data.frame(item, outfit, outfitlow, outfithigh)
        
        image <- self$results$outplot
        image$setState(outfit1)
      },
      
      .outPlot = function(image, ggtheme, theme, ...) {
        
        if (self$options$mode != 'simple')
          return(FALSE)
        
        if (!isTRUE(self$options$run))
          return(FALSE)
        
        if (is.null(self$options$vars) || length(self$options$vars) < 2)
          return(FALSE)
        
        outfit1 <- image$state
        
        if (is.null(outfit1) || !is.data.frame(outfit1))
          return(FALSE)
        
        needed <- c("item", "outfit", "outfitlow", "outfithigh")
        if (!all(needed %in% names(outfit1)))
          return(FALSE)
        
        outfit1$item <- factor(outfit1$item, levels = self$options$vars)
        
        plot <- ggplot2::ggplot(
          outfit1,
          ggplot2::aes(x = item, y = outfit, color = item)
        ) +
          ggplot2::geom_point(size = 3) +
          ggplot2::geom_errorbar(
            ggplot2::aes(ymin = outfitlow, ymax = outfithigh),
            width = 0.3
          ) +
          ggplot2::geom_hline(
            yintercept = 1,
            linetype = "dashed",
            color = "red"
          ) +
          ggplot2::labs(
            title = "",
            x = "Item",
            y = "Outfit"
          ) +
          ggplot2::scale_color_brewer(palette = "Set1") +
          ggplot2::theme_minimal(base_size = 15) +
          ggplot2::guides(color = "none")
        
        if (!is.null(ggtheme))
          plot <- plot + ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot +
            ggplot2::theme(
              axis.text.x = ggplot2::element_text(
                angle = self$options$angle,
                hjust = 1
              )
            )
        }
        
        print(plot)
        TRUE
      },
      
      .cleanData = function() {
        items <- self$options$vars
        
        data <- list()
        
        for (item in items)
          data[[item]] <- jmvcore::toNumeric(self$data[[item]])
        
        attr(data, 'row.names') <- seq_len(length(data[[1]]))
        attr(data, 'class') <- 'data.frame'
        data <- jmvcore::naOmit(data)
        
        return(data)
      }
    )
  )

appleSpinnerH <- function(message = '') {
  paste0(
    '<div style="text-align:center;padding:24px;">',
    
    '<style>',
    '@keyframes snowsoftAppleDotPulse {',
    '0%, 80%, 100% { transform: scale(0.72); opacity: 0.55; }',
    '40% { transform: scale(1.20); opacity: 1; }',
    '}',
    '</style>',
    
    '<div style="margin-bottom:10px;">',
    
    '<span style="',
    'display:inline-block;',
    'width:12px;',
    'height:12px;',
    'margin:0 5px;',
    'border-radius:50%;',
    'background:#007AFF;',
    'animation:snowsoftAppleDotPulse 1.2s infinite ease-in-out;',
    'vertical-align:middle;',
    '"></span>',
    
    '<span style="',
    'display:inline-block;',
    'width:12px;',
    'height:12px;',
    'margin:0 5px;',
    'border-radius:50%;',
    'background:#34C759;',
    'animation:snowsoftAppleDotPulse 1.2s infinite ease-in-out;',
    'animation-delay:0.15s;',
    'vertical-align:middle;',
    '"></span>',
    
    '<span style="',
    'display:inline-block;',
    'width:12px;',
    'height:12px;',
    'margin:0 5px;',
    'border-radius:50%;',
    'background:#FF9500;',
    'animation:snowsoftAppleDotPulse 1.2s infinite ease-in-out;',
    'animation-delay:0.30s;',
    'vertical-align:middle;',
    '"></span>',
    
    '</div>',
    
    '<div style="font-size:12px;color:#666;">',
    message,
    '</div>',
    
    '</div>'
  )
}