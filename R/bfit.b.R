# This file is a generated template, your changes will not be overwritten

bfitClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "bfitClass",
    inherit = bfitBase,
    private = list(
      .htmlwidget = NULL,
      
      ###### .init function--------
      
      .init = function() {
        
        # Progress bar--- 
        self$results$progressBarHTML$setVisible(TRUE)
        html <- progressBarH(0, 100, 'Initializing analysis...')
        self$results$progressBarHTML$setContent(html)
        
        if (self$options$mode == 'simple') {
          private$.htmlwidget <- HTMLWidget$new()
          
          if (is.null(self$data) | is.null(self$options$vars)) {
            self$results$instructions$setVisible(visible = TRUE)
          }
          
          # Progress bar 5%
          html <- progressBarH(5, 100, 'Setting up UI...')
          self$results$progressBarHTML$setContent(html)
          
          self$results$instructions$setContent(
            private$.htmlwidget$generate_accordion(
              title = "Instructions",
              content = paste(
                '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
                '<div style="text-align:justify;">',
                '<ul>',
                '<li>The traditional Rasch model is performed by <b>mixRasch</b> R package using Jonint Maximum Liklihood(JML).</li>',
                '<li>Specify <b>Step(number of category-1) and Bootstrap N</b> in the Analysis options.</li>',
                '<li>Please, be patient. The bootstrapped confidence interval is quite time-consuming.</li>',
                '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
                '</ul></div></div>'
              )
            )
          )
          
          if (isTRUE(self$options$inplot)) {
            width <- self$options$width
            height <- self$options$height
            self$results$inplot$setSize(width, height)
          }
          
          if (isTRUE(self$options$outplot)) {
            width <- self$options$width
            height <- self$options$height
            self$results$outplot$setSize(width, height)
          }
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
          
          if (self$options$outfit)
            self$results$outfit$setNote("Note", "Adj.p= Adjusted p-values for Multiple Comparisons.")
          
          if (self$options$infit)
            self$results$infit$setNote("Note", "Adj.p= Adjusted p-values for Multiple Comparisons.")
        }
        
        # Progress bar 10%
        html <- progressBarH(10, 100, 'Ready to start analysis...')
        self$results$progressBarHTML$setContent(html)
      },
      
      .run = function() {
        # Progress bar 15%
        html <- progressBarH(15, 100, 'Starting analysis...')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
        
        if (self$options$mode == 'complex') {
          if (is.null(self$options$vars1) |
              length(self$options$vars1) < 2)
            return()
          
          # Progress bar 20%
          html <- progressBarH(20, 100, 'Preparing complex mode data...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          
          data <- self$data
          data <- na.omit(data)
          vars1 <- self$options$vars1
          bn1 <- self$options$bn1
          type <- self$options$type
          adj <- self$options$adj
          nco <- self$options$nco
          
          # Progress bar 30%
          html <- progressBarH(30, 100, 'Building model...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          
          if (type == 'bi') {
            obj <- eRm::RM(data)
          } else if (type == 'ra') {
            obj <- eRm::PCM(data)
          }
          
          # Progress bar 50%
          html <- progressBarH(50, 100, 'Running bootstrap fit...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          
          if (nco == TRUE) {
            set.seed(1234)
            fit <- iarm::boot_fit(obj, B = bn1, p.adj = 'none')
            
            # Progress bar 70%
            html <- progressBarH(70, 100, 'Processing outfit statistics...')
            self$results$progressBarHTML$setContent(html)
            private$.checkpoint()
            
            #Outfit-----------------
            table <- self$results$noutfit
            
            outfit <- fit[[1]][, 1]
            outfit <- as.vector(outfit)
            
            pvalue <- fit[[1]][, 2]
            pvalue <- as.vector(pvalue)
            
            for (i in seq_along(vars1)) {
              row <- list()
              row[["fit"]] <- outfit[i]
              row[["p"]] <- pvalue[i]
              table$setRow(rowKey = vars1[i], values = row)
            }
            
            # Progress bar 80%
            html <- progressBarH(80, 100, 'Processing infit statistics...')
            self$results$progressBarHTML$setContent(html)
            private$.checkpoint()
            
            # Infit-------------
            table <- self$results$ninfit
            
            infit <- fit[[1]][, 3]
            infit <- as.vector(infit)
            
            pvalue <- fit[[1]][, 4]
            pvalue <- as.vector(pvalue)
            
            for (i in seq_along(vars1)) {
              row <- list()
              row[["fit"]] <- infit[i]
              row[["p"]] <- pvalue[i]
              table$setRow(rowKey = vars1[i], values = row)
            }
            
          } else {
            set.seed(1234)
            fit <- iarm::boot_fit(obj, B = bn1, p.adj = adj)
            
            # Progress bar 70%
            html <- progressBarH(70, 100, 'Processing outfit with corrections...')
            self$results$progressBarHTML$setContent(html)
            private$.checkpoint()
            
            # Outfit------------
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
              table$setRow(rowKey = vars1[i], values = row)
            }
            
            # Progress bar 80%
            html <- progressBarH(80, 100, 'Processing infit with corrections...')
            self$results$progressBarHTML$setContent(html)
            private$.checkpoint()
            
            # Infit table-------------
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
              table$setRow(rowKey = vars1[i], values = row)
            }
          }
        }
        
        # Progress bar 25%
        html <- progressBarH(25, 100, 'Cleaning data...')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
        
        ### Simple mode analysis ###
        data <- self$data
        vars <- self$options$vars
        
        # Ready--------
        ready <- TRUE
        
        if (is.null(self$options$vars) |
            length(self$options$vars) < 2)
          ready <- FALSE
        
        if (ready) {
          data <- private$.cleanData()
          
          # Progress bar 40%
          html <- progressBarH(40, 100, 'Computing bootstrap statistics...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          
          results <- private$.compute(data)
          
          # Progress bar 70%
          html <- progressBarH(70, 100, 'Populating result tables...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          
          # populate Boot Fit table-------------
          private$.populateInTable(results)
          private$.populateOutTable(results)
          
          # Progress bar 85%
          html <- progressBarH(85, 100, 'Preparing plots...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          
          # plot------
          private$.prepareInPlot(results)
          private$.prepareOutPlot(results)
        }
        
        # Progress bar 완료 후 숨기기
        self$results$progressBarHTML$setVisible(FALSE)
      },
      
      .compute = function(data) {
        # get variables--------
        vars <- self$options$vars
        step <- self$options$step
        bn <- self$options$bn
        
        set.seed(1234)
        
        # Using memoise package---
        boot_stat_memoised <- memoise::memoise(function(data, indices, stat) {
          d <- data[indices, ]
          res1 <- mixRasch::mixRasch(
            data = d,
            steps = step,
            model = "RSM",
            n.c = 1
          )
          stat.raw <- res1$item.par$in.out[, stat]
          return(stat.raw)
        })
        
        # Progress update within bootstrap
        html <- progressBarH(45, 100, 'Running infit bootstrap...')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
        
        # Perform bootstrapping for infit and outfit
        boot.in <- boot::boot(
          data = data,
          statistic = boot_stat_memoised,
          stat = 1,
          R = bn
        )
        
        html <- progressBarH(55, 100, 'Running outfit bootstrap...')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
        
        boot.out <- boot::boot(
          data = data,
          statistic = boot_stat_memoised,
          stat = 3,
          R = bn
        )
        
        html <- progressBarH(65, 100, 'Computing confidence intervals...')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
        
        # Extract original infit and outfit statistics and confidence intervals
        infit.raw <- boot.in$t0
        infit <- boot.in$t
        infitlow <- apply(infit, 2, quantile, prob = 0.025)
        infithigh <- apply(infit, 2, quantile, prob = 0.975)
        
        outfit.raw <- boot.out$t0
        outfit <- boot.out$t
        outfitlow <- apply(outfit, 2, quantile, prob = 0.025)
        outfithigh <- apply(outfit, 2, quantile, prob = 0.975)
        
        results <-
          list(
            'infit' = infit.raw,
            'outfit' = outfit.raw,
            'infitlow' = infitlow,
            'infithigh' = infithigh,
            'outfitlow' = outfitlow,
            'outfithigh' = outfithigh
          )
      },
      
      
      .populateInTable = function(results) {
        table <- self$results$item$binfit
        vars <- self$options$vars
        bn <- self$options$bn
        
        # results------
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
      
      .prepareInPlot = function(results) {
        inplot <- self$results$inplot
        item <- self$options$vars
        
        infit <- results$infit
        infitlow <- results$infitlow
        infithigh <- results$infithigh
        
        infit1 <- data.frame(item, infit, infitlow, infithigh)
        
        image <- self$results$inplot
        image$setState(infit1)
      },
      
      .inPlot = function(image, ggtheme, theme, ...) {
        inplot <- self$options$inplot
        
        if (is.null(self$options$vars))
          return()
        
        infit1 <- image$state
        
        plot <- ggplot(infit1, aes(x = item, y = infit, color = item)) +
          geom_point(size = 3) +
          geom_errorbar(aes(ymin = infitlow, ymax = infithigh), width = 0.3) +
          geom_hline(yintercept = 1,
                     linetype = "dashed",
                     color = "red") +
          labs(title = "", x = "Item", y = "Infit") +
          scale_color_brewer(palette = "Set1") +
          theme_minimal(base_size = 15) +
          guides(color = "none")
        
        plot <- plot + ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        
        print(plot)
        TRUE
      },
      
      .prepareOutPlot = function(results) {
        outplot <- self$results$outplot
        
        item <- self$options$vars
        
        outfit <- results$outfit
        outfitlow <- results$outfitlow
        outfithigh <- results$outfithigh
        
        outfit1 <- data.frame(item, outfit, outfitlow, outfithigh)
        
        image <- self$results$outplot
        image$setState(outfit1)
      },
      
      .outPlot = function(image, ggtheme, theme, ...) {
        outplot <- self$options$outplot
        
        if (is.null(self$options$vars))
          return()
        
        outfit1 <- image$state
        
        plot <- ggplot(outfit1, aes(x = item, y = outfit, color = item)) +
          geom_point(size = 3) +
          geom_errorbar(aes(ymin = outfitlow, ymax = outfithigh), width = 0.3) +
          geom_hline(yintercept = 1,
                     linetype = "dashed",
                     color = "red") +
          labs(title = "", x = "Item", y = "Outfit") +
          scale_color_brewer(palette = "Set1") +
          theme_minimal(base_size = 15) +
          guides(color = "none")
        
        plot <- plot + ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        
        print(plot)
        TRUE
      },
      
      #### Helper functions =================================
      
      .cleanData = function() {
        items <- self$options$vars
        
        data <- list()
        
        for (item in items)
          data[[item]] <-
          jmvcore::toNumeric(self$data[[item]])
        
        attr(data, 'row.names') <- seq_len(length(data[[1]]))
        attr(data, 'class') <- 'data.frame'
        data <- jmvcore::naOmit(data)
        
        return(data)
      }
    )
  )

# Added Progress Bar HTML---
progressBarH <- function(progress = 0, total = 100, message = '') {
  percentage <- round(progress / total * 100)
  width <- 400 * percentage / 100
  
  html <- paste0(
    '<div style="text-align: center; padding: 20px;">',
    '<div style="width: 400px; height: 20px; border: 1px solid #ccc; ',
    'background-color: #f8f9fa; margin: 0 auto; border-radius: 4px;">',
    '<div style="width: ', width, 'px; height: 18px; ',
    'background-color: #999999; border-radius: 3px; ',
    'transition: width 0.3s ease;"></div>',
    '</div>',
    '<div style="margin-top: 8px; font-size: 12px; color: #666;">',
    message, ' (', percentage, '%)</div>',
    '</div>'
  )
  
  return(html)
}
