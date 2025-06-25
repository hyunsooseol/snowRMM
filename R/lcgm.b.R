
# This file is a generated template, your changes will not be overwritten
#' @importFrom magrittr %>%

lcgmClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "lcgmClass",
    inherit = lcgmBase,
    
    # Active bindings ----
    active = list(
      res = function() {
        if (is.null(private$.res_cache)) {
          data <- self$data
          if (self$options$miss == 'listwise') {
            data <- jmvcore::naOmit(data)
          }
          private$.res_cache <- tidySEM::mx_growth_mixture(
            model = self$options$model,
            data = as.data.frame(data),
            classes = self$options$nc,
            thresholds = self$options$thr
          )
        }
        return(private$.res_cache)
      },
      
      desc = function() {
        if (is.null(private$.desc_cache)) {
          data <- self$data
          desc <- tidySEM::descriptives(data)
          private$.desc_cache <- desc[, c("name", "n", "missing", "mean", "median", "sd", "min", "max")]
        }
        return(private$.desc_cache)
      },
      
      fit = function() {
        if (is.null(private$.fit_cache)) {
          private$.fit_cache <- tidySEM::table_fit(self$res)
        }
        return(private$.fit_cache)
      },
      
      parameters = function() {
        if (is.null(private$.para_cache)) {
          para <- tidySEM::table_results(self$res, columns = NULL)
          private$.para_cache <- para[para$Category %in% c("Means", "Variances"), 
                                      c("Category", "lhs", "est", "se", "pval", "confint", "name")]
        }
        return(private$.para_cache)
      },
      
      classProbabilities = function() {
        if (is.null(private$.cp_cache)) {
          cp1 <- tidySEM::class_prob(self$res)
          private$.cp_cache <- list(
            summary = data.frame(cp1$sum.posterior),
            individual = data.frame(cp1$individual)
          )
        }
        return(private$.cp_cache)
      }
    ),
    
    # Private members ----
    private = list(
      # Cache variables
      .htmlwidget = NULL,
      .res_cache = NULL,
      .desc_cache = NULL,
      .fit_cache = NULL,
      .para_cache = NULL,
      .cp_cache = NULL,
      .mc_cache = NULL,
      
      # Init function ----
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) || is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
        }
        
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li><b>tidySEM</b> R package is described in the <a href="https://cjvanlissa.github.io/tidySEM/articles/lca_lcga.html" target = "_blank">page</a>.</li>',
            '<li>Please set <b>Thresholds=TRUE</b> when analyzing ordinal data.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        
        if (self$options$mc)
          self$results$mc$setNote("Note",
                                  "Entropy values are not displayed due to estimation instability in some models.")
        
        # Set plot sizes
        if (isTRUE(self$options$plot1)) {
          self$results$plot1$setSize(self$options$width1, self$options$height1)
        }
        
        if (isTRUE(self$options$plot)) {
          self$results$plot$setSize(self$options$width, self$options$height)
        }
      },
      
      # Run function ----
      .run = function() {
        # Check if we have enough variables
        if (is.null(self$options$vars) || length(self$options$vars) < 3)
          return()
        
        # Set random seed for reproducibility
        set.seed(1234)
        
        # Populate tables based on options
        if (isTRUE(self$options$desc))
          private$.populateDescTable()
        
        if (isTRUE(self$options$fit))
          private$.populateFitTable()
        
        if (isTRUE(self$options$est))
          private$.populateEST()
        
        if (isTRUE(self$options$cp))
          private$.populateClassSizeTable()
        
        # Model comparison만 조건부로 실행 (최적화)
        if (isTRUE(self$options$mc))
          private$.populateModelComparisonTable()
        
        if (isTRUE(self$options$mem))
          private$.populateClassMemberTable()
        
        # Generate plots if requested
        if (isTRUE(self$options$plot))
          private$.setPlot()
        
        if (isTRUE(self$options$plot1))
          private$.setPlot1()
      },
      
      # Model Comparison function ---- (최적화된 부분)
      .computeModelComparison = function() {
        if (is.null(private$.mc_cache)) {
          # 변수들을 미리 저장하여 반복 접근 최소화
          model_spec <- self$options$model
          data_df <- as.data.frame(self$data)
          miss_handling <- self$options$miss
          num_classes <- self$options$nc
          use_thresholds <- self$options$thr
          
          # 데이터 전처리를 한 번만 수행
          if (miss_handling == "listwise")
            data_df <- jmvcore::naOmit(data_df)
          
          # 결과 저장용 리스트 미리 할당
          results <- vector("list", num_classes)
          
          for (k in 1:num_classes) {
            model_k <- tidySEM::mx_growth_mixture(
              model = model_spec, 
              data = data_df,  # 전처리된 데이터 재사용
              classes = k, 
              thresholds = use_thresholds
            )
            
            if (!is.null(model_k)) {
              fit_stats <- tidySEM::table_fit(model_k)
              if (!is.null(fit_stats)) {
                results[[k]] <- data.frame(
                  classes = k,
                  AIC = fit_stats$AIC,
                  BIC = fit_stats$BIC
                )
              }
            }
          }
          
          # 유효한 결과만 병합
          valid_results <- results[!sapply(results, is.null)]
          if (length(valid_results) > 0) {
            private$.mc_cache <- do.call(rbind, valid_results)
          } else {
            private$.mc_cache <- data.frame(classes = integer(0), AIC = numeric(0), BIC = numeric(0))
          }
        }
        return(private$.mc_cache)
      },
      
      .populateModelComparisonTable = function() {
        if (!isTRUE(self$options$mc))
          return()
        
        tbl <- self$results$mc
        mc_data <- private$.computeModelComparison()
        
        if (nrow(mc_data) > 0) {
          for (i in seq_len(nrow(mc_data))) {
            row <- mc_data[i, ]
            tbl$addRow(
              rowKey = paste0("class_", row$classes),
              values = list(
                classes = row$classes,
                AIC = row$AIC,
                BIC = row$BIC
              )
            )
          }
        }
      },
      
      # Table population functions ----
      .populateDescTable = function() {
        if (!isTRUE(self$options$desc))
          return()
        
        vars <- self$options$vars
        table <- self$results$desc
        d <- as.data.frame(self$desc)
        
        for (i in seq_along(vars)) {
          row <- list(
            n = d[[2]][i],
            missing = d[[3]][i],
            mean = d[[4]][i],
            median = d[[5]][i],
            sd = d[[6]][i],
            min = d[[7]][i],
            max = d[[8]][i]
          )
          table$addRow(rowKey = vars[i], values = row)
        }
      },
      
      .populateFitTable = function() {
        if (!isTRUE(self$options$fit))
          return()
        
        table <- self$results$fit
        df <- as.data.frame(t(self$fit))
        
        for (name in rownames(df)) {
          row <- list(value = df[name, 1])
          table$addRow(rowKey = name, values = row)
        }
      },
      
      .populateEST = function() {
        if (!isTRUE(self$options$est))
          return()
        
        table <- self$results$est
        e <- as.data.frame(self$parameters)
        
        for (name in rownames(e)) {
          row <- list(
            cat = e[name, 1],
            lhs = e[name, 2],
            est = e[name, 3],
            se = e[name, 4],
            p = e[name, 5],
            ci = e[name, 6],
            na = e[name, 7]
          )
          table$addRow(rowKey = name, values = row)
        }
      },
      
      .populateClassSizeTable = function() {
        if (!isTRUE(self$options$cp))
          return()
        
        table <- self$results$cp
        nc <- self$options$nc
        vars <- self$options$vars
        d <- self$classProbabilities$summary
        
        for (i in seq_len(nc)) {
          row <- list(
            name = d[[1]][i],
            count = d[[2]][i],
            prop = d[[3]][i]
          )
          table$addRow(rowKey = vars[i], values = row)
        }
      },
      
      .populateClassMemberTable = function() {
        if (!isTRUE(self$options$mem))
          return()
        
        data <- self$data
        if (self$options$miss == 'listwise') {
          data <- jmvcore::naOmit(data)
        }
        
        mem <- self$classProbabilities$individual
        m <- as.factor(mem$predicted)
        
        if (self$results$mem$isNotFilled()) {
          self$results$mem$setRowNums(rownames(self$data))
          self$results$mem$setValues(m)
        }
      },
      
      # Plot functions ----
      .setPlot1 = function() {
        if (!isTRUE(self$options$plot1))
          return()
        
        data <- self$data
        if (self$options$miss == 'listwise') {
          data <- jmvcore::naOmit(data)
        }
        
        # Density plot data preparation
        long <- reshape(
          data,
          direction = "long",
          varying = list(names(data)),
          v.names = "value",
          idvar = "id",
          timevar = "time"
        )
        
        image <- self$results$plot1
        image$setState(long)
      },
      
      .setPlot = function() {
        if (!isTRUE(self$options$plot))
          return()
        
        image <- self$results$plot
        image$setState(self$res)
      },
      
      # Plot rendering functions ----
      .plot1 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        long <- image$state
        library(ggplot2)
        plot1 <- ggplot(long, aes(x = value)) +
          geom_density() +
          facet_wrap(~ time) + 
          theme_bw()
        
        plot1 <- plot1 + ggtheme
        print(plot1)
        TRUE
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        tra <- image$state
        plot <- tidySEM::plot_growth(
          tra,
          rawdata = self$options$raw,
          alpha_range = c(0, 0.05)
        )
        
        plot <- plot + ggtheme
        print(plot)
        TRUE
      }
    )
  )
