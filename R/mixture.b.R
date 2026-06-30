
#' Mixture Rasch Analysis

mixtureClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "mixtureClass",
    inherit = mixtureBase,
    
    active = list(
      res = function() {
        if (is.null(private$.allCache) || is.null(private$.allCache$res)) {
          data <- private$.cleanData()
          if (is.null(private$.allCache)) private$.allCache <- list()
          private$.allCache$res <- private$.computeRES(data)
        }
        return(private$.allCache$res)
      }
    ),
    
    
    private = list(
      .allCache = NULL,
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        private$.allCache <- NULL
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Specify the number of <b>Class, Step, and Type</b> in the Analysis option.</li>',
            '<li>When <b>Step=1</b>, the partial credit model is not analyzed and an error occurs.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
          
        ))
        
        if (self$options$catStats)
          self$results$categoryStats$setNote(
            "Note",
            "Mean Measure is the average ability estimate of respondents selecting each category within a class."
          )
        
        if (self$options$thresholdOrder)
          self$results$thresholdOrder$setNote(
            "Note",
            "Ordered thresholds increase across categories; disordered thresholds suggest that adjacent categories may not be functioning distinctly."
          )
        
        
      },
      
      .run = function() {
        
        if (!isTRUE(self$options$run))
          return()
        
        ready <- TRUE
        
        if (is.null(self$options$vars) |
            length(self$options$vars) < 2)
          
          ready <- FALSE
        
        if (ready) {
          if (is.null(private$.allCache)) {
            data <- private$.cleanData()
            private$.allCache <- private$.compute(data)
          }
          results <- private$.allCache
          
          # populate Model information table-----
          if (!is.null(results$out))
            private$.populateModelTable(results)
          
          # populate Item Statistics table-----
          private$.populateImeasureTable(results)
          private$.populateIseTable(results)
          private$.populateImeanTable(results)
          private$.populateInfitTable(results)
          private$.populateOutfitTable(results)
          private$.populatePbisTable(results)
          private$.populateCategoryStatsTable(results)
          private$.populateThresholdOrderTable(results)
          
          # populate Average theta table----
          private$.populateAverageTable(results)
        }
      },
      
      # compute results---
      
      .compute = function(data) {
        vars <- self$options$vars
        nc <- self$options$nc
        step <- self$options$step
        type <- self$options$type
        out <- NULL
        
        res <- self$res
        
        # ---- Model information (Preformatted) ----
        seed_used <- 1234
        starts_used <- 1
        private$.populateModelInfoText(res, seed_used, starts_used)
        
        
        # item statistics--------
        
        imeasure <- sapply(res$LatentClass, function(x)
          x$item.par$delta.i)
        imeasure <- as.data.frame(imeasure)
        
        ise <- sapply(res$LatentClass, function(x)
          x$item.par$SE.delta.i)
        ise <- as.data.frame(ise)
        
        infit <- sapply(res$LatentClass, function(x)
          x$item.par$in.out[, 1])
        infit <- as.data.frame(infit)
        
        outfit <- sapply(res$LatentClass, function(x)
          x$item.par$in.out[, 3])
        outfit <- as.data.frame(outfit)
        
        imean <- sapply(res$LatentClass, function(x)
          x$item.par$itemDescriptives[, 1])
        imean <- as.data.frame(imean)
        
        pbis <- sapply(res$LatentClass, function(x)
          x$item.par$itemDescriptives[, 2])
        pbis <- as.data.frame(pbis)
        
        ######### Person analysis####################
        
        # person measure--------------
        pmeasure <- sapply(res$LatentClass, function(x)
          x$person.par$theta)
        pmeasure <- as.data.frame(pmeasure)
        
        # person error-------------------
        pse <- sapply(res$LatentClass, function(x)
          x$person.par$SE.theta)
        pse <- as.data.frame(pse)
        
        # person fit--------------
        pinfit <- sapply(res$LatentClass, function(x)
          x$person.par$infit)
        pinfit <- as.data.frame(pinfit)
        
        poutfit <- sapply(res$LatentClass, function(x)
          x$person.par$outfit)
        poutfit <- as.data.frame(poutfit)
        
        # 재매핑용 인덱스는 한 번만 계산
        n_row <- nrow(self$data)
        not_na_idx <- which(stats::complete.cases(self$data[, self$options$vars, drop = FALSE]))
        
        # Person Outputs-------------------------
        # pmeasure--------------------------------
        if (isTRUE(self$options$pmeasure)) {
          keys <- 1:self$options$nc
          measureTypes <- rep("continuous", self$options$nc)
          titles <- paste("Measure_Class", keys)
          descriptions <- paste("Measure_Class", keys)
          self$results$pmeasure$set(
            keys = keys,
            titles = titles,
            descriptions = descriptions,
            measureTypes = measureTypes
          )
          self$results$pmeasure$setRowNums(rownames(self$data))
          for (i in 1:self$options$nc) {
            scores <- rep(NA, n_row)
            scores[not_na_idx] <- as.numeric(pmeasure[, i])
            self$results$pmeasure$setValues(index = i, scores)
          }
        }
        
        # pse---------------------------------------
        if (isTRUE(self$options$pse)) {
          keys <- 1:self$options$nc
          measureTypes <- rep("continuous", self$options$nc)
          titles <- paste("SE_Class", keys)
          descriptions <- paste("SE_Class", keys)
          self$results$pse$set(
            keys = keys,
            titles = titles,
            descriptions = descriptions,
            measureTypes = measureTypes
          )
          self$results$pse$setRowNums(rownames(self$data))
          for (i in 1:self$options$nc) {
            scores <- rep(NA, n_row)
            scores[not_na_idx] <- as.numeric(pse[, i])
            self$results$pse$setValues(index = i, scores)
          }
        }
        
        # pinfit-------------------------------
        if (isTRUE(self$options$pinfit)) {
          keys <- 1:self$options$nc
          measureTypes <- rep("continuous", self$options$nc)
          titles <- paste("Infit_Class", keys)
          descriptions <- paste("Infit_Class", keys)
          self$results$pinfit$set(
            keys = keys,
            titles = titles,
            descriptions = descriptions,
            measureTypes = measureTypes
          )
          self$results$pinfit$setRowNums(rownames(self$data))
          for (i in 1:self$options$nc) {
            scores <- rep(NA, n_row)
            scores[not_na_idx] <- as.numeric(pinfit[, i])
            self$results$pinfit$setValues(index = i, scores)
          }
        }
        
        # poutfit----------------------------------------
        if (isTRUE(self$options$poutfit)) {
          keys <- 1:self$options$nc
          measureTypes <- rep("continuous", self$options$nc)
          titles <- paste("Outfit_Class", keys)
          descriptions <- paste("Outfit_Class", keys)
          self$results$poutfit$set(
            keys = keys,
            titles = titles,
            descriptions = descriptions,
            measureTypes = measureTypes
          )
          self$results$poutfit$setRowNums(rownames(self$data))
          for (i in 1:self$options$nc) {
            scores <- rep(NA, n_row)
            scores[not_na_idx] <- as.numeric(poutfit[, i])
            self$results$poutfit$setValues(index = i, scores)
          }
        }
        
        # model fit information------
        if (isTRUE(self$options$fit) || isTRUE(self$options$plot2)) {
          
          for (i in 1:nc) {
            set.seed(1234)
            res_k <- mixRasch::mixRasch(
              data = data,
              steps = step,
              model = type,
              n.c = i
            )
            
            model <- res_k$info.fit
            df <- data.frame(model)
            if (is.null(out)) {
              out <- df
            } else {
              out <- rbind(out, df)
            }
          }
          
          # elbow plot----------
          out1 <- out[, c(1:3)]
          colnames(out1) <- c('AIC', 'BIC', 'CAIC')
          
          df <- as.data.frame(out1)
          df$Class <- seq.int(nrow(df))
          elbow <- reshape2::melt(
            df,
            id.vars = 'Class',
            variable.name = "Fit",
            value.name = 'Value'
          )
          
          image <- self$results$plot2
          image$setState(elbow)
        }
        
        # number of class
        set.seed(1234)
        res0 <- mixRasch::getEstDetails(res)
        class <- res0$nC
        
        # Average Theta Values
        set.seed(1234)
        average <- mixRaschTools::avg.theta(res)
        
        # class membership-------------
        pclass <- res$class
        mem <- as.numeric(apply(pclass, 1, which.max))
        
        if (self$options$pmember == TRUE) {
          mem_vec <- rep(NA, n_row)
          mem_vec[not_na_idx] <- as.factor(mem)
          
          self$results$pmember$setRowNums(rownames(self$data))
          self$results$pmember$setValues(mem_vec)
        }
        
        # Category statistics by modal class assignment-------------
        categoryStats <- NULL
        if (isTRUE(self$options$catStats)) {
          categoryStats <- private$.computeCategoryStats(
            data = data,
            mem = mem,
            measure = pmeasure
          )
        }
        
        # PCM threshold ordering by class-------------
        thresholdOrder <- NULL
        if (isTRUE(self$options$thresholdOrder)) {
          thresholdOrder <- private$.computeThresholdOrder(
            res = res,
            n_items = length(vars),
            n_steps = step,
            model_type = type
          )
        }
        
        # Person density across class-------------
        colnames(pmeasure) <- c(1:self$options$nc)
        dat <- reshape2::melt(pmeasure, variable.name = "Class", value.name = 'Measure')
        image <- self$results$plot3
        image$setState(dat)
        
        results <-
          list(
            'out' = out,
            'imeasure' = imeasure,
            'ise' = ise,
            'infit' = infit,
            'outfit' = outfit,
            'imean' = imean,
            'pbis' = pbis,
            'class' = class,
            'average' = average,
            'mem' = mem,
            'pmeasure' = pmeasure,
            'pse' = pse,
            'pinfit' = pinfit,
            'poutfit' = poutfit,
            'categoryStats' = categoryStats,
            'thresholdOrder' = thresholdOrder
          )
      },
      
      # populate Model information table-----
      .populateModelTable = function(results) {
        if (is.null(results$out))
          return()
        
        table <- self$results$item$fit
        fit <- as.data.frame(results$out)
        
        if (nrow(fit) == 0)
          return()
        
        colnames(fit) <- c("aic", "bic", "caic", "loglik", "parm", "person")
        
        lapply(rownames(fit), function(name) {
          row <- as.list(fit[name, ])
          table$addRow(rowKey = name, values = row)
        })
      },
      
      # ---- Model information (Preformatted) helpers ----
      .safeLogLik = function(res) {
        ll <- tryCatch(as.numeric(res$info.fit$loglik), error = function(e) NA_real_)
        if (is.na(ll)) {
          ll <- tryCatch(as.numeric(stats::logLik(res)), error = function(e) NA_real_)
        }
        ll
      },
      
      .safeConverged = function(res) {
        
        get1 <- function(x) {
          if (is.null(x)) return(NA)
          if (length(x) < 1) return(NA)
          as.logical(x)[1]
        }
        
        conv <- tryCatch(get1(res$converged), error = function(e) NA)
        
        if (is.na(conv)) {
          conv <- tryCatch(get1(res$conv), error = function(e) NA)
        }
        
        conv
      },
      
      .populateModelInfoText = function(res, seed_used, starts_used) {
        
        conv <- private$.safeConverged(res)
        conv_txt <- if (isTRUE(conv)) "Yes" else "Not reported by model"
        
        txt <- paste0(
          "Converged: ", conv_txt
        )
        
        self$results$text$setContent(txt)
        self$results$text$setVisible(TRUE)
      },
      
      # populate Item Statistics table-----
      .populateImeasureTable = function(results) {
        table <- self$results$item$imeasure
        nc <- self$options$nc
        vars <- self$options$vars
        imeasure <- results$imeasure
        nclass <- results$class
        
        if (nclass > 1) {
          for (i in 2:nclass)
            table$addColumn(
              name = paste0("pc", i),
              title = as.character(i),
              type = 'number',
              superTitle = 'Class'
            )
        }
        for (i in seq_along(vars)) {
          row <- list()
          for (j in 1:nclass) {
            row[[paste0("pc", j)]] <- imeasure[i, j]
          }
          table$addRow(rowKey = vars[i], values = row)
        }
        
        # Prepare Data For Item Plot -------
        image <- self$results$iplot
        #imeasure$item <- seq.int(nrow(imeasure))
        imeasure$item <- vars
        n <- paste(1:self$options$nc, sep = '')
        colnames(imeasure) <- c(n, 'item')
        data <- tidyr::gather(data = imeasure, class, measure, -item)
        image$setState(data)
      },
      
      .populateIseTable = function(results) {
        table <- self$results$item$ise
        nc <- self$options$nc
        vars <- self$options$vars
        ise <- results$ise
        nclass <- results$class
        
        if (nclass > 1) {
          for (i in 2:nclass)
            table$addColumn(
              name = paste0("pc", i),
              title = as.character(i),
              type = 'number',
              superTitle = 'Class'
            )
        }
        for (i in seq_along(vars)) {
          row <- list()
          for (j in 1:nclass) {
            row[[paste0("pc", j)]] <- ise[i, j]
          }
          table$addRow(rowKey = vars[i], values = row)
        }
      },
      
      .populateImeanTable = function(results) {
        table <- self$results$item$imean
        nc <- self$options$nc
        vars <- self$options$vars
        imean <- results$imean
        nclass <- results$class
        
        if (nclass > 1) {
          for (i in 2:nclass)
            table$addColumn(
              name = paste0("pc", i),
              title = as.character(i),
              type = 'number',
              superTitle = 'Class'
            )
        }
        for (i in seq_along(vars)) {
          row <- list()
          for (j in 1:nclass) {
            row[[paste0("pc", j)]] <- imean[i, j]
          }
          table$addRow(rowKey = vars[i], values = row)
        }
      },
      
      .populateInfitTable = function(results) {
        table <- self$results$item$infit
        nc <- self$options$nc
        vars <- self$options$vars
        infit <- results$infit
        nclass <- results$class
        
        if (nclass > 1) {
          for (i in 2:nclass)
            table$addColumn(
              name = paste0("pc", i),
              title = as.character(i),
              type = 'number',
              superTitle = 'Class'
            )
        }
        for (i in seq_along(vars)) {
          row <- list()
          for (j in 1:nclass) {
            row[[paste0("pc", j)]] <- infit[i, j]
          }
          table$addRow(rowKey = vars[i], values = row)
        }
      },
      
      .populateOutfitTable = function(results) {
        table <- self$results$item$outfit
        nc <- self$options$nc
        vars <- self$options$vars
        outfit <- results$outfit
        nclass <- results$class
        
        if (nclass > 1) {
          for (i in 2:nclass)
            table$addColumn(
              name = paste0("pc", i),
              title = as.character(i),
              type = 'number',
              superTitle = 'Class'
            )
        }
        for (i in seq_along(vars)) {
          row <- list()
          for (j in 1:nclass) {
            row[[paste0("pc", j)]] <- outfit[i, j]
          }
          table$addRow(rowKey = vars[i], values = row)
        }
      },
      
      .populatePbisTable = function(results) {
        table <- self$results$item$pbis
        nc <- self$options$nc
        vars <- self$options$vars
        pbis <- results$pbis
        nclass <- results$class
        
        if (nclass > 1) {
          for (i in 2:nclass)
            table$addColumn(
              name = paste0("pc", i),
              title = as.character(i),
              type = 'number',
              superTitle = 'Class'
            )
        }
        for (i in seq_along(vars)) {
          row <- list()
          for (j in 1:nclass) {
            row[[paste0("pc", j)]] <- pbis[i, j]
          }
          table$addRow(rowKey = vars[i], values = row)
        }
      },
      
      
      # Category statistics by class-----
      .computeCategoryStats = function(data, mem, measure) {
        
        vars <- self$options$vars
        nc <- self$options$nc
        
        rows <- list()
        row_no <- 0L
        
        # measure가 벡터로 단순화되는 경우에도 행렬 형태 유지
        measure <- as.matrix(measure)
        
        for (item_no in seq_along(vars)) {
          
          item_values <- data[[item_no]]
          
          categories <- sort(unique(
            item_values[!is.na(item_values)]
          ))
          
          for (class_no in seq_len(nc)) {
            
            # NA가 논리 인덱스에 남지 않도록 명시적으로 제거
            class_rows <- !is.na(mem) & mem == class_no
            class_total <- sum(class_rows, na.rm = TRUE)
            
            for (category_value in categories) {
              
              selected <- class_rows &
                !is.na(item_values) &
                item_values == category_value
              
              frequency <- sum(selected, na.rm = TRUE)
              
              percent <- if (class_total > 0L) {
                frequency / class_total * 100
              } else {
                NA_real_
              }
              
              mean_measure <- NA_real_
              
              if (frequency > 0L &&
                  ncol(measure) >= class_no) {
                
                measure_values <- measure[selected, class_no]
                
                if (any(is.finite(measure_values))) {
                  mean_measure <- mean(
                    measure_values[is.finite(measure_values)]
                  )
                }
              }
              
              row_no <- row_no + 1L
              
              rows[[row_no]] <- data.frame(
                item = vars[item_no],
                class = class_no,
                category = category_value,
                frequency = as.integer(frequency),
                percent = percent,
                meanMeasure = mean_measure,
                stringsAsFactors = FALSE
              )
            }
          }
        }
        
        if (length(rows) == 0L)
          return(NULL)
        
        values <- do.call(rbind, rows)
        
        values <- values[
          order(
            values$item,
            values$class,
            values$category
          ),
          ,
          drop = FALSE
        ]
        
        rownames(values) <- NULL
        
        return(values)
      },
      
      .populateCategoryStatsTable = function(results) {
        if (!isTRUE(self$options$catStats))
          return()
        
        values <- results$categoryStats
        if (is.null(values) || nrow(values) == 0)
          return()
        
        table <- self$results$categoryStats
        
        for (i in seq_len(nrow(values))) {
          table$addRow(
            rowKey = paste0(
              values$item[i], "_",
              values$class[i], "_",
              values$category[i], "_",
              i
            ),
            values = as.list(values[i, , drop = FALSE])
          )
        }
      },
      
      # Extract PCM threshold matrix from a latent-class result-----
      .extractThresholdMatrix = function(item_par, n_items) {
        if (is.null(item_par))
          return(NULL)
        
        item_names <- names(item_par)
        if (is.null(item_names))
          return(NULL)
        
        exact_candidates <- c(
          "threshold", "thresholds", "thresh",
          "tau", "taus",
          "step", "steps", "step.par", "step.pars",
          "delta.k", "delta.ik", "delta.step"
        )
        
        candidate_names <- unique(c(
          intersect(exact_candidates, item_names),
          grep(
            "threshold|thresh|tau|step",
            item_names,
            ignore.case = TRUE,
            value = TRUE
          )
        ))
        
        if (length(candidate_names) == 0)
          return(NULL)
        
        for (candidate in candidate_names) {
          object <- item_par[[candidate]]
          
          if (is.null(object))
            next
          
          if (is.data.frame(object)) {
            numeric_columns <- vapply(object, is.numeric, logical(1))
            object <- object[, numeric_columns, drop = FALSE]
          }
          
          if (is.matrix(object) || is.data.frame(object)) {
            matrix_object <- as.matrix(object)
            storage.mode(matrix_object) <- "numeric"
            
            if (nrow(matrix_object) == n_items && ncol(matrix_object) >= 1)
              return(matrix_object)
            
            if (ncol(matrix_object) == n_items && nrow(matrix_object) >= 1)
              return(t(matrix_object))
          }
          
          if (is.numeric(object)) {
            numeric_object <- as.numeric(object)
            
            if (length(numeric_object) > n_items &&
                length(numeric_object) %% n_items == 0) {
              threshold_count <- length(numeric_object) / n_items
              return(matrix(
                numeric_object,
                nrow = n_items,
                ncol = threshold_count,
                byrow = TRUE
              ))
            }
          }
        }
        
        NULL
      },
      
      .computeThresholdOrder = function(res, n_items, n_steps, model_type) {
        is_pcm <- grepl(
          "partial|pcm",
          tolower(as.character(model_type)[1])
        )
        
        if (!is_pcm || is.na(n_steps) || n_steps <= 1)
          return(NULL)
        
        vars <- self$options$vars
        nclass <- min(self$options$nc, length(res$LatentClass))
        rows <- list()
        row_no <- 0L
        
        for (class_no in seq_len(nclass)) {
          item_par <- res$LatentClass[[class_no]]$item.par
          thresholds <- private$.extractThresholdMatrix(
            item_par = item_par,
            n_items = n_items
          )
          
          if (is.null(thresholds))
            next
          
          threshold_count <- ncol(thresholds)
          
          for (item_no in seq_len(min(n_items, nrow(thresholds)))) {
            item_thresholds <- as.numeric(thresholds[item_no, ])
            valid_thresholds <- item_thresholds[is.finite(item_thresholds)]
            
            item_ordered <- if (length(valid_thresholds) <= 1)
              NA
            else
              all(diff(valid_thresholds) > 0)
            
            ordered_text <- if (is.na(item_ordered))
              "Not available"
            else if (item_ordered)
              "Yes"
            else
              "No"
            
            for (step_no in seq_along(item_thresholds)) {
              
              threshold_value <- item_thresholds[step_no]
              
              if (!is.finite(threshold_value))
                threshold_value <- NA_real_
              
              row_no <- row_no + 1L
              
              rows[[row_no]] <- data.frame(
                item = vars[item_no],
                class = class_no,
                ordered = if (step_no == 1L) ordered_text else "",
                step = step_no,
                threshold = threshold_value,
                stringsAsFactors = FALSE
              )
            }
          }
        }
        
        if (length(rows) == 0)
          return(NULL)
        
        values <- do.call(rbind, rows)
        values <- values[
          order(values$item, values$class, values$step),
          ,
          drop = FALSE
        ]
        rownames(values) <- NULL
        values
      },
      
      .populateThresholdOrderTable = function(results) {
        if (!isTRUE(self$options$thresholdOrder))
          return()
        
        values <- results$thresholdOrder
        if (is.null(values) || nrow(values) == 0)
          return()
        
        table <- self$results$thresholdOrder
        
        for (i in seq_len(nrow(values))) {
          table$addRow(
            rowKey = paste0(
              values$item[i], "_",
              values$class[i], "_",
              values$step[i], "_",
              i
            ),
            values = as.list(values[i, , drop = FALSE])
          )
        }
      },
      
      # populate Average theta table-----
      .populateAverageTable = function(results) {
        nc <- self$options$nc
        table <- self$results$person$average
        value <- results$average
        value <- as.matrix(value)
        for (i in seq_len(nc)) {
          row <- list()
          row[["value"]] <- value[i, 1]
          table$setRow(rowNo = i, values = row)
        }
      },
      
      # plot--------------
      .itemPlot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        plotData <- image$state
        
        plot <- ggplot2::ggplot(
          plotData,
          ggplot2::aes(
            x = as.factor(item),
            y = measure,
            group = class
          )
        ) +
          ggplot2::geom_line(ggplot2::aes(color = class), size = 1.1) +
          ggplot2::geom_point(ggplot2::aes(color = class), size = 3) +
          ggplot2::labs(
            title = "Item parameters by class",
            x = "Item number",
            y = "Measure",
            color = "Class"
          ) +
          ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = self$options$angle,
              hjust = 1
            )
          )
        }
        
        print(plot)
        TRUE
      },
      
      .plot3 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        dat <- image$state
        
        plot3 <- ggplot2::ggplot(
          dat,
          ggplot2::aes(x = Measure, color = Class)
        ) +
          ggplot2::geom_density() +
          ggplot2::coord_cartesian(xlim = c(-5, 5)) +
          ggtheme
        
        print(plot3)
        TRUE
      },
      
      .plot2 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        elbow <- image$state
        plot2 <- ggplot2::ggplot(elbow, ggplot2::aes(x = Class, y = Value, color = Fit)) +
          ggplot2::geom_line(size = 1.1) +
          ggplot2::geom_point(size = 3) +
          ggplot2::scale_x_continuous(breaks = seq(1, length(elbow$Class), by = 1))
        plot2 <- plot2 + ggtheme
        print(plot2)
        TRUE
      },
      
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
      },
      
      .computeRES = function(data = NULL) {
        if (is.null(data)) data <- private$.cleanData()
        set.seed(1234)
        res <-
          mixRasch::mixRasch(
            data = data,
            steps = self$options$step,
            model = self$options$type,
            n.c = self$options$nc
          )
        return(res)
      }
    )
  )