

#' @importFrom utils capture.output

lcaordClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "lcaordClass",
    inherit = lcaordBase,
    
    active = list(
      res = function() {
        if (is.null(private$.res_cache)) {
          
          ind_names <- self$options$vars
          
          if (is.null(ind_names) || length(ind_names) < 3)
            stop("Select at least 3 ordinal indicators in 'Variables'.")
          
          data0 <- as.data.frame(self$data)
          
          if (!all(ind_names %in% names(data0))) {
            missing_ind <- setdiff(ind_names, names(data0))
            stop(sprintf(
              "Indicators not found in data: %s",
              paste(missing_ind, collapse = ", ")
            ))
          }
          
          # LCA model uses only indicator variables
          data_ind <- data0[, ind_names, drop = FALSE]
          
          # Missing-value handling for the LCA model
          # FIML: keep rows as they are
          # Listwise: remove rows with missing values in indicator variables only
          if (self$options$miss == "listwise") {
            keep <- stats::complete.cases(data_ind)
            data_ind <- data_ind[keep, , drop = FALSE]
          }
          
          if (nrow(data_ind) < 2)
            stop("Too few complete cases for the selected ordinal indicators.")
          
          # mx_lca requirement: all indicators must be binary or ordered factors
          data_ind[] <- lapply(data_ind, function(x) {
            if (is.ordered(x))
              return(x)
            
            if (is.factor(x))
              return(ordered(x))
            
            stop("All indicators must be ordinal (binary or ordered factors).")
          })
          
          private$.res_cache <- tidySEM::mx_lca(
            data    = data_ind,
            classes = self$options$nc
          )
        }
        
        private$.res_cache
      },
      
      desc = function() {
        if (is.null(private$.desc_cache)) {
          
          ind_names <- self$options$vars
          
          if (is.null(ind_names) || length(ind_names) < 3)
            return(NULL)
          
          data0 <- as.data.frame(self$data)
          
          if (!all(ind_names %in% names(data0))) {
            missing_ind <- setdiff(ind_names, names(data0))
            stop(sprintf(
              "Indicators not found in data: %s",
              paste(missing_ind, collapse = ", ")
            ))
          }
          
          data_ind <- data0[, ind_names, drop = FALSE]
          
          if (self$options$miss == "listwise") {
            keep <- stats::complete.cases(data_ind)
            data_ind <- data_ind[keep, , drop = FALSE]
          }
          
          rows <- lapply(ind_names, function(v) {
            
            x <- data_ind[[v]]
            
            if (is.factor(x) || is.ordered(x))
              x <- droplevels(x)
            
            x_nonmiss <- x[!is.na(x)]
            
            tab <- table(x_nonmiss, useNA = "no")
            
            mode_value <- if (length(tab) > 0) {
              names(tab)[which.max(tab)]
            } else {
              NA_character_
            }
            
            mode_prop <- if (length(tab) > 0 && length(x_nonmiss) > 0) {
              as.numeric(max(tab)) / length(x_nonmiss)
            } else {
              NA_real_
            }
            
            data.frame(
              name = v,
              type = if (is.ordered(x)) {
                "ordered factor"
              } else if (is.factor(x)) {
                "factor"
              } else {
                class(x)[1]
              },
              n = sum(!is.na(x)),
              missing = sum(is.na(x)) / length(x),
              unique = length(tab),
              mode = mode_value,
              modeProp = mode_prop,
              stringsAsFactors = FALSE
            )
          })
          
          private$.desc_cache <- do.call(rbind, rows)
        }
        
        private$.desc_cache
      }
    
      ),
    
    private = list(
      .htmlwidget      = NULL,
      .results_cache   = NULL,
      .res_cache       = NULL,
      .desc_cache      = NULL,
      .barplot_cache = NULL,
 
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) || is.null(self$options$vars))
          self$results$instructions$setVisible(TRUE)
        
        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title = "Instructions",
            content = paste(
              '<div style="border:2px solid #e6f4fe;border-radius:15px;padding:15px;',
              'background-color:#e6f4fe;margin-top:10px;"><ul>',
              '<li>This analysis supports binary, nominal, and ordinal categorical indicators.</li>',
              '<li>3-step auxiliary results are provided as approximate posterior-probability-based comparisons and should be interpreted with caution for strict methodological applications.</li>',
              '<li>Latent class analysis for ordinal indicators is described in the ',              
              '<a href="https://cjvanlissa.github.io/tidySEM/articles/lca_ordinal.html" target="_blank">tidySEM article</a>.</li>',
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div>'
            )
          )
        )
        
        if (!is.null(self$results$localDep))
          self$results$localDep$setNote(
            "Note",
            "BVR = bivariate residual. Larger BVR values may indicate local dependence between item pairs. As a rough guide, BVR values above 10 may warrant closer inspection."
          )
        
        if (!is.null(self$results$reg))
          self$results$reg$setNote(
            "Note",
            "Class-wise regression is performed using the distal variable as outcome."
          )
        
        self$results$cp$setNote(
          "Note",
          "Count is based on summed posterior probabilities, so non-integer values can appear."
        )
        
        private$.registerCallbacks()
      },
      
      .registerCallbacks = function() {
        callbacks <- list(
          desc = private$.populateDescTable,
          fit  = private$.populateFitTable,
          cp   = private$.populateClassSizeTable,
          mem  = private$.populateClassMemberTable,
          post = private$.populatePosteriorOutput,
          localDep = private$.populateLocalDepTable,
          use3step_means    = private$.populateThreeStepMeansTable,
          use3step_omnibus  = private$.populateThreeStepOmnibusTable,
          use3step_pairwise = private$.populateThreeStepPairwiseTable,
          reg  = private$.populateRegressionTable
        )
        for (name in names(callbacks)) {
          if (name %in% names(self$results) &&
              !is.null(self$results[[name]]) &&
              "setCallback" %in% names(self$results[[name]])) {
            self$results[[name]]$setCallback(callbacks[[name]])
          }
        }
      },
      
      .run = function() {

        if (!isTRUE(self$options$run))
          return()
        
        if (is.null(self$options$vars) || length(self$options$vars) < 3)
          return()
        
        # Show progress spinner
        self$results$progressBarHTML$setVisible(TRUE)
        self$results$progressBarHTML$setContent(
          appleSpinnerH('Performing ordinal LCA...')
        )
        private$.checkpoint()
        
        # --- Separate full data and indicator data (with cached loading of needed variables) ---
        # (!) Force-load columns referenced by formula/auxVar as well
        ind_names <- self$options$vars
        needed <- ind_names
        
        aux <- self$options$auxVar
        if (!is.null(aux) && nzchar(aux))
          needed <- unique(c(needed, aux))
        
        fstr_needed <- self$options$auxFormula
        if (!is.null(fstr_needed) && nzchar(fstr_needed)) {
          f_need <- try(stats::as.formula(fstr_needed), silent = TRUE)
          if (!inherits(f_need, "try-error"))
            needed <- unique(c(needed, all.vars(f_need)))
        }
        
        # Keep only columns that actually exist; ignore missing names but record them
        all_cols <- try(colnames(self$data), silent = TRUE)
        if (inherits(all_cols, "try-error") || is.null(all_cols))
          all_cols <- names(as.data.frame(self$data))
        
        needed <- unique(needed)
        missing_cols <- setdiff(needed, all_cols)
        needed_present <- intersect(needed, all_cols)
        
        # Stop immediately if any indicator variable is missing
        miss_ind <- setdiff(ind_names, needed_present)
        if (length(miss_ind) > 0)
          stop(sprintf("Indicators not found in data: %s", paste(miss_ind, collapse = ", ")))
        
        # # Subset only existing columns so jamovi loads those columns
        # data_all <- self$data[needed_present]
        # 
        # # Missing-value handling / data.frame conversion
        # if (self$options$miss == 'listwise')
        #   data_all <- jmvcore::naOmit(data_all)
        # data_all <- as.data.frame(data_all)

        data0 <- as.data.frame(self$data[needed_present])
        
        data_ind0 <- data0[, ind_names, drop = FALSE]
        
        if (self$options$miss == "listwise") {
          analysis_rows <- which(stats::complete.cases(data_ind0))
        } else {
          analysis_rows <- seq_len(nrow(data_ind0))
        }
        
        data_all <- data0[analysis_rows, , drop = FALSE]
        data_ind <- data_ind0[analysis_rows, , drop = FALSE]
        
        data_ind[] <- lapply(data_ind, function(x) {
          if (is.ordered(x)) x else if (is.factor(x)) ordered(x) else x
        })
        
        
        # Compute results once
        if (is.null(private$.results_cache)) {
          
          set.seed(1234)
          private$.barplot_cache <- NULL


          res  <- self$res
          desc <- self$desc
          
        
          fit <- tidySEM::table_fit(res)

          cp1 <- tidySEM::class_prob(res)
          cp  <- data.frame(cp1$sum.posterior)
          

          private$.results_cache <- list(
            data_all = data_all,
            data_ind = data_ind,
            analysis_rows = analysis_rows,
            res  = res,
            desc = desc,
            fit  = fit,
            cp   = cp,
            cp1  = cp1,
            local_dep = private$.computeLocalDependence(data_ind, cp1),
            three_step_means    = private$.emptyThreeMeansRows(),
            three_step_omnibus  = private$.emptyThreeOmnibusRows(),
            three_step_pairwise = private$.emptyThreePairwiseRows(),
            reg_tests = private$.emptyInferentialRows(),
            missing_aux_cols = missing_cols
          )
                    
          # 45%: populate desc
          if (isTRUE(self$options$desc)) {
            
            private$.populateDescTable()
          }
          
          # 55%: populate fit
          if (isTRUE(self$options$fit)) {

            private$.populateFitTable()
          }
          
          # 65%: populate class size
          if (isTRUE(self$options$cp)) {
            private$.populateClassSizeTable()
          }
          
          # 70%: populate local dependence diagnostics
          if (isTRUE(self$options$localDep)) {

            private$.populateLocalDepTable()
          }
          
          
          # 75%: populate class member
          if (isTRUE(self$options$mem)) {
            private$.populateClassMemberTable()
          }
          
          # 77%: save posterior probabilities
          if (isTRUE(self$options$post)) {
            private$.populatePosteriorOutput()
          }
          
          # 80%: local dependence heatmap
          if (isTRUE(self$options$residualHeatmap)) {
            private$.setResidualHeatmap()
          }

          # 85%: plot response probabilities
          if (isTRUE(self$options$plot)) {
            private$.setResponseProbPlot()
          }
          
          # 90%: profile plot
          if (isTRUE(self$options$profilePlot)) {
            private$.setProfilePlot()
          }
          
          
          # 95%: plot bar
          if (isTRUE(self$options$plot1)) {
            private$.setBarPlot()
          }
          
          # ===================== 96%: 3-step result tables =====================
          if (isTRUE(self$options$use3step)) {
           
            dat <- private$.results_cache$data_all
            
            # (A) Distal BCH/DCAT tables
            vname <- self$options$auxVar
            if (!is.null(vname) && nzchar(vname)) {
              tres <- private$.computeThreeStep(dat, vname)
              private$.results_cache$three_step_means    <- tres$means
              private$.results_cache$three_step_omnibus  <- tres$omnibus
              private$.results_cache$three_step_pairwise <- tres$pairwise
            }
            private$.populateThreeStepMeansTable()
            private$.populateThreeStepOmnibusTable()
            private$.populateThreeStepPairwiseTable()
            
            # (B) Class-wise regression table
            if (isTRUE(self$options$reg)) {
              fstr <- self$options$auxFormula
              if (!is.null(fstr) && nzchar(fstr)) {
                rres <- private$.run3stepReg(self$res, dat, fstr)
                private$.results_cache$reg_tests <- rres$rows
              }
              private$.populateRegressionTable()
            }
            
          }
        }
        
        
        # 100%: complete and hide
        self$results$progressBarHTML$setVisible(FALSE)
        private$.registerCallbacks()
      },
      
      .populateDescTable = function() {
        vars  <- self$options$vars
        table <- self$results$desc
        d     <- data.frame(private$.results_cache$desc)
        
        lapply(seq_along(vars), function(i) {
          row <- list(
            type     = d$type[i],
            n        = d$n[i],
            missing  = d$missing[i],
            unique   = d$unique[i],
            mode     = d$mode[i],
            modeProp = d$modeProp[i]
          )
          
          table$addRow(rowKey = vars[i], values = row)
        })
      },
      
      .populateFitTable = function() {
        table <- self$results$fit
        df    <- as.data.frame(t(private$.results_cache$fit))
        lapply(rownames(df), function(name) {
          table$addRow(rowKey = name, values = list(value = df[name,1]))
        })
      },
      
      .populateClassSizeTable = function() {
        table <- self$results$cp
        d     <- private$.results_cache$cp
        
        lapply(seq_len(nrow(d)), function(i) {
          rk <- if (!is.null(rownames(d)) && nzchar(rownames(d)[i])) rownames(d)[i] else i
          table$addRow(
            rowKey = rk,
            values = list(
              name  = d[[1]][i],
              count = d[[2]][i],
              prop  = d[[3]][i]
            )
          )
        })
      },
      
      .populateLocalDepTable = function() {
        table <- self$results$localDep
        d     <- private$.results_cache$local_dep
        
        if (is.null(table) || is.null(d) || nrow(d) == 0)
          return()
        
        lapply(seq_len(nrow(d)), function(i) {
          table$addRow(
            rowKey = i,
            values = as.list(d[i, , drop = FALSE])
          )
        })
      },
      
      .populateClassMemberTable = function() {
        table <- self$results$mem
        mem   <- data.frame(private$.results_cache$cp1$individual)
        m     <- as.factor(mem$predicted)
       
        
                
        if (table$isNotFilled()) {
          # match displayed row numbers to the actually analyzed data after listwise deletion
          analyzed_n <- NROW(private$.results_cache$data_all)
          if (!is.null(rownames(private$.results_cache$data_all)) &&
              length(rownames(private$.results_cache$data_all)) == analyzed_n) {
            table$setRowNums(rownames(private$.results_cache$data_all))
          } else {
            table$setRowNums(seq_len(analyzed_n))
          }
          table$setValues(m)
        }
      },
      
      .populatePosteriorOutput = function() {
        
        if (!isTRUE(self$options$post))
          return()
        
        if (is.null(private$.results_cache) ||
            is.null(private$.results_cache$cp1))
          return()
        
        post <- private$.getPosteriorMatrix(private$.results_cache$cp1)
        
        if (is.null(post) || nrow(post) == 0)
          return()
        
        n_row <- NROW(private$.results_cache$data_all)
        nc <- ncol(post)
        
        if (nrow(post) != n_row)
          return()
        
        if (self$results$post$isNotFilled()) {
          
          keys <- seq_len(nc)
          
          self$results$post$set(
            keys = keys,
            titles = paste0("Pr(Class ", keys, ")"),
            descriptions = paste0("Posterior probability for Class ", keys),
            measureTypes = rep("continuous", nc)
          )
          
          if (!is.null(rownames(private$.results_cache$data_all)) &&
              length(rownames(private$.results_cache$data_all)) == n_row) {
            self$results$post$setRowNums(rownames(private$.results_cache$data_all))
          } else {
            self$results$post$setRowNums(seq_len(n_row))
          }
          
          for (i in seq_len(nc)) {
            scores <- as.numeric(post[, i])
            self$results$post$setValues(index = i, scores)
          }
        }
      },
      
      .populateThreeStepMeansTable = function() {
        table <- self$results$use3step_means
        d     <- private$.results_cache$three_step_means
        if (is.null(table) || is.null(d) || nrow(d) == 0)
          return()
        lapply(seq_len(nrow(d)), function(i) {
          table$addRow(rowKey = i, values = as.list(d[i, , drop = FALSE]))
        })
      },
      
      .populateThreeStepOmnibusTable = function() {
        table <- self$results$use3step_omnibus
        d     <- private$.results_cache$three_step_omnibus
        if (is.null(table) || is.null(d) || nrow(d) == 0)
          return()
        lapply(seq_len(nrow(d)), function(i) {
          table$addRow(rowKey = i, values = as.list(d[i, , drop = FALSE]))
        })
      },
      
      .populateThreeStepPairwiseTable = function() {
        table <- self$results$use3step_pairwise
        d     <- private$.results_cache$three_step_pairwise
        if (is.null(table) || is.null(d) || nrow(d) == 0)
          return()
        lapply(seq_len(nrow(d)), function(i) {
          table$addRow(rowKey = i, values = as.list(d[i, , drop = FALSE]))
        })
      },
      
      .populateRegressionTable = function() {
        table <- self$results$reg
        d     <- private$.results_cache$reg_tests
        if (is.null(table) || is.null(d) || nrow(d) == 0)
          return()
        lapply(seq_len(nrow(d)), function(i) {
          table$addRow(rowKey = i, values = as.list(d[i, , drop = FALSE]))
        })
      },
      
      .setResidualHeatmap = function() {
        if (is.null(private$.results_cache))
          return()
        
        d <- private$.results_cache$local_dep
        
        if (is.null(d) || nrow(d) == 0)
          return()
        
        self$results$residualHeatmap$setState(d)
      },
      
      .setResponseProbPlot = function() {
        image <- self$results$plot
        image$setState(private$.results_cache$res)
      },
      
      .setProfilePlot = function() {
        image <- self$results$profilePlot
        image$setState(private$.results_cache$res)
      },
      
      .setBarPlot = function() {
        if (is.null(private$.barplot_cache)) {
          df <- as.data.frame(private$.results_cache$data_ind)
          names(df) <- paste0("Value.", names(df))
          private$.barplot_cache <- reshape(df, varying = names(df), direction = "long")
        }
        
        self$results$plot1$setState(private$.barplot_cache)
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) return(FALSE)
        p <- tidySEM::plot_prob(image$state, bw=TRUE) + ggtheme
        if (self$options$angle>0)
          p <- p + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=self$options$angle, hjust=1))
        print(p); TRUE
      },
      
      .plotProfilePlot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        # Create the original response probability plot object
        p0 <- try(tidySEM::plot_prob(image$state, bw = FALSE), silent = TRUE)
        
        if (inherits(p0, "try-error"))
          return(FALSE)
        
        d <- p0$data
        
        # If the ggplot object does not carry raw data, stop safely
        if (is.null(d) || nrow(d) == 0)
          return(TRUE)
        
        # Standardize column names in a flexible way
        nms <- names(d)
        nml <- tolower(nms)
        
        find_col <- function(candidates) {
          hit <- which(nml %in% candidates)
          if (length(hit) == 0)
            return(NA_character_)
          nms[hit[1]]
        }
        
        vcol <- find_col(c("variable", "variables", "item", "name", "var"))
        ycol <- find_col(c("value", "prob", "probability", "estimate", "est"))
        ccol <- find_col(c("class", "classes", "group"))
        gcol <- find_col(c("category", "response", "level", "cat"))
        
        # If required columns are not found, print an empty plot instead of spinning forever
        if (any(is.na(c(vcol, ycol, ccol, gcol)))) {
          p <- ggplot2::ggplot() +
            ggplot2::theme_void() +
            ggplot2::annotate(
              "text",
              x = 0,
              y = 0,
              label = "Profile plot data could not be extracted from tidySEM::plot_prob().",
              size = 4
            )
          print(p)
          return(TRUE)
        }
        
        # Prepare data for the profile plot
        d2 <- data.frame(
          Variable = as.character(d[[vcol]]),
          Value    = suppressWarnings(as.numeric(d[[ycol]])),
          Class    = as.factor(d[[ccol]]),
          Category = as.factor(d[[gcol]]),
          stringsAsFactors = FALSE
        )
        
        d2 <- d2[is.finite(d2$Value), , drop = FALSE]
        
        if (nrow(d2) == 0) {
          p <- ggplot2::ggplot() +
            ggplot2::theme_void() +
            ggplot2::annotate(
              "text",
              x = 0,
              y = 0,
              label = "No valid probability values were available for the profile plot.",
              size = 4
            )
          print(p)
          return(TRUE)
        }
        
        # Preserve the original variable order
        d2$Variable <- factor(d2$Variable, levels = unique(d2$Variable))
        
        # Create facet labels such as Pr(1), Pr(2), ...
        d2$Facet <- paste0("Pr(", as.character(d2$Category), ")")
        
        p <- ggplot2::ggplot(
          d2,
          ggplot2::aes(
            x = Variable,
            y = Value,
            colour = Class,
            group = Class
          )
        ) +
          ggplot2::geom_line(size = 0.8) +
          ggplot2::geom_point(size = 2) +
          ggplot2::facet_wrap(~ Facet, nrow = 1) +
          ggplot2::scale_y_continuous(limits = c(0, 1)) +
          ggplot2::labs(
            x = "Variable",
            y = "Value",
            colour = "Class"
          ) +
          ggplot2::theme_bw() +
          ggtheme +
          ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank()
          )
        
        if (self$options$angle2 > 0)
          p <- p + ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = self$options$angle2,
              hjust = 1
            )
          )
        
        print(p)
        TRUE
      },
      
      .plotResidualHeatmap = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        d <- image$state
        
        if (is.null(d) || nrow(d) == 0)
          return(FALSE)
        
        vars <- unique(c(as.character(d$item1), as.character(d$item2)))
        
        plot_df <- data.frame(
          Item1 = as.character(d$item1),
          Item2 = as.character(d$item2),
          BVR   = suppressWarnings(as.numeric(d$bvr)),
          stringsAsFactors = FALSE
        )
        
        plot_df <- plot_df[!is.na(plot_df$BVR), , drop = FALSE]
        
        if (nrow(plot_df) == 0)
          return(FALSE)
        
        plot_df$Item1 <- factor(plot_df$Item1, levels = vars)
        plot_df$Item2 <- factor(plot_df$Item2, levels = rev(vars))
        
        p <- ggplot2::ggplot(
          plot_df,
          ggplot2::aes(x = Item1, y = Item2, fill = BVR)
        ) +
          ggplot2::geom_tile(color = "white") +
          ggplot2::geom_text(
            ggplot2::aes(label = sprintf("%.2f", BVR)),
            size = 3
          ) +
          ggplot2::theme_bw() +
          ggtheme +
          ggplot2::scale_fill_gradientn(
            colours = c("#F7FBFF", "#6BAED6", "#FD8D3C", "#CB181D"),
            na.value = "white"
          ) +
          ggplot2::labs(
            x = NULL,
            y = NULL,
            fill = "BVR"
          ) +
          ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank()
          )
        
        if (self$options$angle1 > 0)
          p <- p + ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = self$options$angle1, hjust = 1)
          )
        
        print(p)
        TRUE
      },
      
      
      .plot1 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        p <- ggplot2::ggplot(
          image$state,
          ggplot2::aes(x = Value)
        ) +
          ggplot2::geom_bar() +
          ggplot2::facet_wrap(~ time, scales = "free") +
          ggplot2::theme_bw() +
          ggtheme
        
        print(p)
        TRUE
      },
      
      .emptyLocalDepRows = function() {
        data.frame(
          item1 = character(0),
          item2 = character(0),
          bvr = numeric(0),
          p = numeric(0),
          stringsAsFactors = FALSE
        )
      },
      
      .getPosteriorMatrix = function(cp1) {
        if (is.null(cp1) || is.null(cp1$individual))
          return(NULL)
        
        ind <- data.frame(cp1$individual)
        
        pats <- c(
          "^CPROB[0-9]+$",
          "^Class[_\\.]?[0-9]+$",
          "^C[0-9]+$",
          "^p[0-9]+$",
          "^posterior[_\\.]?[0-9]+$"
        )
        
        hits <- unique(unlist(lapply(pats, function(p) grep(p, names(ind)))))
        
        if (length(hits) > 0) {
          pmat <- as.matrix(ind[, hits, drop = FALSE])
          storage.mode(pmat) <- "numeric"
          return(pmat)
        }
        
        num <- names(ind)[vapply(ind, is.numeric, TRUE)]
        cand <- setdiff(num, c("id", "ID", "predicted", "Predicted"))
        
        if (length(cand) > 1) {
          pmat <- as.matrix(ind[, cand, drop = FALSE])
          storage.mode(pmat) <- "numeric"
          rs <- rowSums(pmat, na.rm = TRUE)
          if (all(is.finite(rs)) && mean(abs(rs - 1)) < 1e-3)
            return(pmat)
        }
        
        NULL
      },
      
      .computeLocalDependence = function(data_ind, cp1) {
        out <- private$.emptyLocalDepRows()
        
        if (is.null(data_ind) || ncol(data_ind) < 2)
          return(out)
        
        pmat <- private$.getPosteriorMatrix(cp1)
        if (is.null(pmat) || nrow(pmat) != nrow(data_ind))
          return(out)
        
        K <- ncol(pmat)
        if (K < 2)
          return(out)
        
        vars <- names(data_ind)
        rows <- list()
        idx <- 1L
        
        for (a in 1:(length(vars) - 1)) {
          for (b in (a + 1):length(vars)) {
            
            x <- data_ind[[vars[a]]]
            y <- data_ind[[vars[b]]]
            
            ok <- !is.na(x) & !is.na(y)
            if (sum(ok) < 5)
              next
            
            x <- droplevels(as.factor(x[ok]))
            y <- droplevels(as.factor(y[ok]))
            w <- pmat[ok, , drop = FALSE]
            
            lx <- levels(x)
            ly <- levels(y)
            
            if (length(lx) < 2 || length(ly) < 2)
              next
            
            obs <- table(x, y)
            obs <- as.matrix(obs)
            
            exp_total <- matrix(
              0,
              nrow = length(lx),
              ncol = length(ly),
              dimnames = list(lx, ly)
            )
            
            for (k in seq_len(K)) {
              wk <- w[, k]
              nk <- sum(wk, na.rm = TRUE)
              
              if (!is.finite(nk) || nk <= 0)
                next
              
              px <- sapply(lx, function(v) sum(wk[x == v], na.rm = TRUE) / nk)
              py <- sapply(ly, function(v) sum(wk[y == v], na.rm = TRUE) / nk)
              
              exp_total <- exp_total + nk * outer(px, py)
            }
            
            common_x <- intersect(rownames(obs), rownames(exp_total))
            common_y <- intersect(colnames(obs), colnames(exp_total))
            
            obs2 <- obs[common_x, common_y, drop = FALSE]
            exp2 <- exp_total[common_x, common_y, drop = FALSE]
            
            bvr <- sum((obs2 - exp2)^2 / pmax(exp2, .Machine$double.eps), na.rm = TRUE)
            df <- max(1, (nrow(obs2) - 1) * (ncol(obs2) - 1))
            pval <- stats::pchisq(bvr, df = df, lower.tail = FALSE)
            
            # flag <- if (is.finite(bvr) && bvr >= 10) {
            #   "Possible local dependence"
            # } else {
            #   "No concern"
            # }
            
            # rows[[idx]] <- data.frame(
            #   item1 = vars[a],
            #   item2 = vars[b],
            #   bvr = as.numeric(bvr),
            #   p = as.numeric(pval),
            #   flag = flag,
            #   stringsAsFactors = FALSE
            # )
            rows[[idx]] <- data.frame(
              item1 = vars[a],
              item2 = vars[b],
              bvr = as.numeric(bvr),
              p = as.numeric(pval),
              stringsAsFactors = FALSE
            )            
            
            
            idx <- idx + 1L
          }
        }
        
        if (length(rows) == 0)
          return(out)
        
        do.call(rbind, rows)
      },
      
      
      .emptyInferentialRows = function() {
        data.frame(
          method = character(0),
          variable = character(0),
          wald = numeric(0),
          df = numeric(0),
          p = numeric(0),
          stringsAsFactors = FALSE
        )
      },
      
      .emptyThreeMeansRows = function() {
        data.frame(
          class = character(0),
          statistic = character(0),
          value = numeric(0),
          stringsAsFactors = FALSE
        )
      },
      
      .emptyThreeOmnibusRows = function() {
        data.frame(
          method = character(0),
          variable = character(0),
          statistic = numeric(0),
          df1 = numeric(0),
          df2 = numeric(0),
          p = numeric(0),
          stringsAsFactors = FALSE
        )
      },
      
      .emptyThreePairwiseRows = function() {
        data.frame(
          comparison = character(0),
          z = numeric(0),
          p = numeric(0),
          p_bh = numeric(0),
          p_bonf = numeric(0),
          d = numeric(0),
          stringsAsFactors = FALSE
        )
      },
      
      # ------------------------------------------------------------------
      # 3-step (BCH/DCAT): split output into means, omnibus, and pairwise
      # ------------------------------------------------------------------
      .computeThreeStep = function(dat_all, auxName) {
        means_rows    <- private$.emptyThreeMeansRows()
        omnibus_rows  <- private$.emptyThreeOmnibusRows()
        pairwise_rows <- private$.emptyThreePairwiseRows()
        
        ind <- NULL
        if (!is.null(private$.results_cache$cp1$individual)) {
          ind <- data.frame(private$.results_cache$cp1$individual)
        } else {
          cp <- try(tidySEM::class_prob(self$res), silent = TRUE)
          if (!inherits(cp, "try-error") && !is.null(cp$individual))
            ind <- data.frame(cp$individual)
        }
        
        if (is.null(ind)) {
          return(list(
            means = means_rows,
            omnibus = omnibus_rows,
            pairwise = pairwise_rows
          ))
        }
        
        .findPcols <- function(nms) {
          pats <- c("^CPROB[0-9]+$", "^Class[_\\.]?[0-9]+$", "^C[0-9]+$",
                    "^p[0-9]+$", "^posterior[_\\.]?[0-9]+$")
          hits <- unique(unlist(lapply(pats, function(p) grep(p, nms))))
          if (length(hits))
            return(nms[hits])
          
          num <- nms[vapply(ind, is.numeric, TRUE)]
          cand <- setdiff(num, c("id", "ID", "predicted", "Predicted"))
          if (length(cand) > 1) {
            S <- rowSums(ind[, cand, drop = FALSE], na.rm = TRUE)
            if (all(is.finite(S)) && mean(abs(S - 1)) < 1e-3)
              return(cand)
          }
          character(0)
        }
        
        pcols <- .findPcols(names(ind))
        if (length(pcols) == 0) {
          return(list(
            means = means_rows,
            omnibus = omnibus_rows,
            pairwise = pairwise_rows
          ))
        }
        
        K <- length(pcols)
        if (K < 2) {
          return(list(
            means = means_rows,
            omnibus = omnibus_rows,
            pairwise = pairwise_rows
          ))
        }
        
        
        if (is.null(dat_all[[auxName]])) {
          return(list(
            means = means_rows,
            omnibus = omnibus_rows,
            pairwise = pairwise_rows
          ))
        }
        
        yraw <- dat_all[[auxName]]
        ok <- if (is.numeric(yraw)) is.finite(yraw) else !is.na(yraw)
        for (pc in pcols)
          ind[[pc]][!ok] <- 0
        
        if (is.numeric(yraw) && !(is.factor(yraw) || is.ordered(yraw))) {
          y <- yraw
          
          eff <- sapply(pcols, function(pc) sum(ind[[pc]], na.rm = TRUE))
          m <- sapply(pcols, function(pc) {
            sum(ind[[pc]] * y, na.rm = TRUE) /
              pmax(sum(ind[[pc]], na.rm = TRUE), .Machine$double.eps)
          })
          v <- mapply(function(pc, mu) {
            w <- ind[[pc]]
            sum(w * (y - mu)^2, na.rm = TRUE) /
              pmax(sum(w, na.rm = TRUE), .Machine$double.eps)
          }, pcols, m)
          
          se <- sqrt(v / pmax(eff, 1))
          
          means_rows <- do.call(
            rbind,
            lapply(seq_len(K), function(k) {
              data.frame(
                class = paste0("Class ", k),
                statistic = c("Mean", "SD", "SE", "Effective N"),
                value = c(
                  as.numeric(m[k]),
                  as.numeric(sqrt(v[k])),
                  as.numeric(se[k]),
                  as.numeric(eff[k])
                ),
                stringsAsFactors = FALSE
              )
            })
          )
          
          weights <- rowSums(ind[, pcols, drop = FALSE], na.rm = TRUE)
          total_weight <- sum(weights, na.rm = TRUE)
          
          if (is.finite(total_weight) && total_weight > 0) {
            wbar <- sum(y * weights, na.rm = TRUE) / total_weight
            ssb <- sum(eff * (m - wbar)^2, na.rm = TRUE)
            
            ssw <- 0
            for (i in seq_along(pcols)) {
              w <- ind[[pcols[i]]]
              mu <- m[i]
              ssw <- ssw + sum(w * (y - mu)^2, na.rm = TRUE)
            }
            
            df1 <- K - 1
            df2 <- max(1, sum(eff, na.rm = TRUE) - K)
            Fst <- (ssb / df1) / (ssw / df2)
            pF <- stats::pf(Fst, df1, df2, lower.tail = FALSE)
            
            if (is.finite(Fst) && is.finite(pF)) {
              omnibus_rows <- rbind(
                omnibus_rows,
                data.frame(
                  method = "PP-weighted F omnibus (approx.)",
                  variable = auxName,
                  statistic = as.numeric(Fst),
                  df1 = as.numeric(df1),
                  df2 = as.numeric(df2),
                  p = as.numeric(pF),
                  stringsAsFactors = FALSE
                )
              )
            }
            
            if (K >= 2) {
              pw_list <- list()
              idx <- 1L
              
              for (a in 1:(K - 1)) {
                for (b in (a + 1):K) {
                  zval <- (m[a] - m[b]) / sqrt(se[a]^2 + se[b]^2)
                  pval <- 2 * stats::pnorm(-abs(zval))
                  
                  s_pooled <- sqrt(((eff[a] - 1) * v[a] + (eff[b] - 1) * v[b]) /
                                     pmax(eff[a] + eff[b] - 2, 1))
                  dval <- if (is.finite(s_pooled) && s_pooled > 0)
                    (m[a] - m[b]) / s_pooled else NA_real_
                  
                  pw_list[[idx]] <- data.frame(
                    comparison = sprintf("Class %d vs %d", a, b),
                    z = as.numeric(zval),
                    p = as.numeric(pval),
                    p_bh = NA_real_,
                    p_bonf = NA_real_,
                    d = as.numeric(dval),
                    stringsAsFactors = FALSE
                  )
                  idx <- idx + 1L
                }
              }
              
              if (length(pw_list) > 0) {
                pairwise_rows <- do.call(rbind, pw_list)
                pairwise_rows$p_bh <- stats::p.adjust(pairwise_rows$p, method = "BH")
                pairwise_rows$p_bonf <- stats::p.adjust(pairwise_rows$p, method = "bonferroni")
              }
            }
          }
          
          return(list(
            means = means_rows,
            omnibus = omnibus_rows,
            pairwise = pairwise_rows
          ))
        }
        
        f <- as.factor(yraw)
        J <- nlevels(f)
        if (J < 2) {
          return(list(
            means = means_rows,
            omnibus = omnibus_rows,
            pairwise = pairwise_rows
          ))
        }
        W <- matrix(0, nrow = K, ncol = J, dimnames = list(paste0("C", 1:K), levels(f)))
        
        for (k in 1:K) {
          for (j in 1:J) {
            W[k, j] <- sum((f == levels(f)[j]) * ind[[pcols[k]]], na.rm = TRUE)
          }
        }
        
        rs <- rowSums(W)
        cs <- colSums(W)
        N <- sum(rs)
        E <- outer(rs, cs) / ifelse(N > 0, N, NA_real_)
        chi <- sum((W - E)^2 / pmax(E, .Machine$double.eps), na.rm = TRUE)
        df <- (K - 1) * (J - 1)
        pchi <- stats::pchisq(chi, df = df, lower.tail = FALSE)
        
        if (is.finite(chi) && is.finite(pchi)) {
          omnibus_rows <- rbind(
            omnibus_rows,
            data.frame(
              method = "DCAT omnibus",
              variable = auxName,
              statistic = as.numeric(chi),
              df1 = as.numeric(df),
              df2 = NA_real_,
              p = as.numeric(pchi),
              stringsAsFactors = FALSE
            )
          )
        }
        
        if (K >= 2) {
          pw_list <- list()
          idx <- 1L
          
          for (a in 1:(K - 1)) {
            for (b in (a + 1):K) {
              sub <- rbind(W[a, ], W[b, ])
              rs2 <- rowSums(sub)
              cs2 <- colSums(sub)
              N2 <- sum(rs2)
              E2 <- outer(rs2, cs2) / ifelse(N2 > 0, N2, NA_real_)
              chi2 <- sum((sub - E2)^2 / pmax(E2, .Machine$double.eps), na.rm = TRUE)
              df2 <- J - 1
              p2 <- stats::pchisq(chi2, df = df2, lower.tail = FALSE)
              
              pw_list[[idx]] <- data.frame(
                comparison = sprintf("Class %d vs %d", a, b),
                z = as.numeric(sqrt(chi2)),
                p = as.numeric(p2),
                p_bh = NA_real_,
                p_bonf = NA_real_,
                d = NA_real_,
                stringsAsFactors = FALSE
              )
              idx <- idx + 1L
            }
          }
          
          if (length(pw_list) > 0) {
            pairwise_rows <- do.call(rbind, pw_list)
            pairwise_rows$p_bh <- stats::p.adjust(pairwise_rows$p, method = "BH")
            pairwise_rows$p_bonf <- stats::p.adjust(pairwise_rows$p, method = "bonferroni")
          }
        }
        
        list(
          means = means_rows,
          omnibus = omnibus_rows,
          pairwise = pairwise_rows
        )
      },
      
      # ---- 3-step regression (generalized to K classes) -------------------
      .run3stepReg = function(res_final, dat, formula_str) {
        if (is.null(formula_str) || !nzchar(formula_str))
          return(list(rows = private$.emptyInferentialRows()))
        
        f <- try(stats::as.formula(formula_str), silent = TRUE)
        if (inherits(f, "try-error"))
          return(list(rows = private$.emptyInferentialRows()))
        vars <- all.vars(f)
        if (length(vars) < 2)
          return(list(rows = private$.emptyInferentialRows()))
        
        y     <- vars[1]
        xvars <- vars[-1]
        
        use_x <- character(0)
        
        for (vx in xvars) {
          if (is.null(dat[[vx]]))
            return(list(rows = private$.emptyInferentialRows()))
          
          v <- dat[[vx]]
          if (is.character(v)) v <- factor(v)
          if (is.factor(v)) {
            v <- droplevels(v)
            nl <- nlevels(v)
            if (nl <= 1) {
              next
            } else if (nl == 2) {
              dat[[vx]] <- as.integer(v) - 1L
              use_x <- c(use_x, vx)
            } else {
              tab <- table(v)
              ref <- names(tab)[which.max(tab)]
              for (lev in levels(v)) {
                if (lev == ref) next
                new_name <- paste0(vx, "_", make.names(lev))
                dat[[new_name]] <- as.integer(v == lev)
                use_x <- c(use_x, new_name)
              }
            }
          } else {
            dat[[vx]] <- suppressWarnings(as.numeric(v))
            use_x <- c(use_x, vx)
          }
        }
        
        if (is.null(dat[[y]]))
          return(list(rows = private$.emptyInferentialRows()))
        if (is.character(dat[[y]]) || is.factor(dat[[y]]))
          dat[[y]] <- suppressWarnings(as.numeric(dat[[y]]))
        
        if (length(use_x) == 0)
          return(list(rows = private$.emptyInferentialRows()))
        
        needed_cols <- unique(c(y, use_x))
        cc <- stats::complete.cases(dat[, needed_cols, drop = FALSE])
        ncc <- sum(cc)
        if (ncc < 10)
          return(list(rows = private$.emptyInferentialRows()))
        dat_cc <- dat[cc, , drop = FALSE]
        
        new_formula <- sprintf("%s ~ %s", y, paste(use_x, collapse = " + "))
        
        fit2 <- try(tidySEM::BCH(res_final, model = new_formula, data = dat_cc), silent = TRUE)
        if (inherits(fit2, "try-error"))
          return(list(rows = private$.emptyInferentialRows()))
        
        lrA <- try(tidySEM::lr_test(fit2, compare = "A"), silent = TRUE)
        if (!inherits(lrA, "try-error") && is.data.frame(lrA) && nrow(lrA) > 0) {
          nml <- tolower(names(lrA))
          get <- function(cs) { i <- which(nml %in% cs)[1]; if (length(i) == 0 || is.na(i)) NA else lrA[[i]] }
          dff  <- suppressWarnings(as.integer(get(c("df","dof"))[1]))
          stat <- suppressWarnings(as.numeric(get(c("lr","chisq","x2","lrchisq"))[1]))
          pv   <- suppressWarnings(as.numeric(get(c("p","p.value"))[1]))
          rows <- data.frame(
            method = "LR (compare='A')",
            variable = new_formula,
            wald = stat,
            df = dff,
            p = pv,
            stringsAsFactors = FALSE
          )
          return(list(rows = rows))
        }
        
        # Generalized Wald equality constraints across all classes
        K <- NA_integer_
        cp_try <- try(tidySEM::class_prob(res_final), silent = TRUE)
        if (!inherits(cp_try, "try-error") && !is.null(cp_try$sum.posterior)) {
          K <- NROW(as.data.frame(cp_try$sum.posterior))
        }
        if (is.na(K) || !is.finite(K) || K < 2)
          K <- 2L
        
        p <- length(use_x)
        cons_list <- character(0)
        
        if (K >= 2 && p >= 1) {
          for (j in seq_len(p)) {
            idx <- paste0("[1,", j + 1L, "]")
            for (k in 2:K) {
              cons_list <- c(cons_list, paste0("class1.A", idx, "=class", k, ".A", idx))
            }
          }
        }
        
        if (length(cons_list) == 0)
          return(list(rows = private$.emptyInferentialRows()))
        
        cons <- paste(cons_list, collapse = "&")
        
        wd <- try(tidySEM::wald_test(fit2, cons), silent = TRUE)
        if (!inherits(wd, "try-error") && is.data.frame(wd) && nrow(wd) > 0) {
          nmt <- tolower(names(wd))
          get2 <- function(cs) { i <- which(nmt %in% cs)[1]; if (length(i) == 0 || is.na(i)) NA else wd[[i]] }
          dff  <- suppressWarnings(as.integer(get2(c("df","dof"))[1]))
          if (is.na(dff) || !is.finite(dff))
            dff <- (K - 1L) * p
          stat <- suppressWarnings(as.numeric(get2(c("wald","w","statistic"))[1]))
          pv   <- suppressWarnings(as.numeric(get2(c("p","p.value"))[1]))
          if (is.na(stat) && is.finite(pv))
            stat <- stats::qchisq(1 - pv, dff)
          rows <- data.frame(
            method = "Wald equality of slopes",
            variable = new_formula,
            wald = stat,
            df = dff,
            p = pv,
            stringsAsFactors = FALSE
          )
          return(list(rows = rows))
        }
        
        list(rows = private$.emptyInferentialRows())
      }
      
    )
  )

# Progress Bar HTML  (R/progressBarH.R)
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