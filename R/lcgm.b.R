
# This file is a generated template, your changes will not be overwritten
#' @importFrom magrittr %>%

lcgmClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "lcgmClass",
    inherit = lcgmBase,
    
    # Active bindings---
    active = list(
      res = function() {
        if (is.null(private$.res_cache)) {
          data <- self$data
          if (self$options$miss == 'listwise') {
            data <- jmvcore::naOmit(data)
          }
          #private$.checkpoint()
          
          private$.res_cache <- tidySEM::mx_growth_mixture(
            model = self$options$model,
            data = as.data.frame(data),
            classes = self$options$nc,
            thresholds = self$options$thr
          )
        }
        return(private$.res_cache)
      },
      
      #---
      desc = function() {
        if (is.null(private$.desc_cache)) {
          data <- self$data
          desc <- tidySEM::descriptives(data)
          private$.desc_cache <- desc[, c("name", "n", "missing", "mean", "median", "sd", "min", "max")]
        }
        return(private$.desc_cache)
      }
    ),
    
    #---------------
    private = list(
      .htmlwidget = NULL,
      .results_cache = NULL,
      .res_cache = NULL,
      .desc_cache = NULL,
      
      #----------------------------------
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        
        if (is.null(self$data) | is.null(self$options$vars)) {
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
        
        if (isTRUE(self$options$plot1)) {
          width <- self$options$width1
          height <- self$options$height1
          self$results$plot1$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot)) {
          width <- self$options$width
          height <- self$options$height
          self$results$plot$setSize(width, height)
        }
        
        #
        private$.registerCallbacks()
        
      },
      
      #---
      .registerCallbacks = function() {
        callbacks <- list(
          desc = private$.populateDescTable,
          fit = private$.populateFitTable,
          cp = private$.populateClassSizeTable,
          mem = private$.populateClassMemberTable
        )
        
        for (name in names(callbacks)) {
          # check callback---
          
          if (name %in% names(self$results) &&
              !is.null(self$results[[name]]) &&
              "setCallback" %in% names(self$results[[name]])) {
            self$results[[name]]$setCallback(callbacks[[name]])
          }
        }
      },
      #---------
      
      .run = function() {
        vars <- self$options$vars
        
        if (is.null(self$options$vars) ||
            length(self$options$vars) < 3)
          return()
        
        
        data <- self$data
        if (self$options$miss == 'listwise') {
          data <- jmvcore::naOmit(data)
        }
        data <- as.data.frame(data)
        
        
        if (is.null(private$.results_cache)) {
          set.seed(1234)
          #private$.checkpoint()
          # compute using active binding
          res <- self$res
          desc <- self$desc
          
          # Get fit table
          fit <- tidySEM::table_fit(res)
          # Get parameter estimates
          para <- tidySEM::table_results(res, columns = NULL)
          para <- para[para$Category %in% c("Means", "Variances"), c("Category", "lhs", "est", "se", "pval", "confint", "name")]
          
          # class size
          cp1 <- tidySEM::class_prob(res)
          cp <- data.frame(cp1$sum.posterior)
          
          # save
          private$.results_cache <- list(
            data = data,
            res = res,
            desc = desc,
            fit = fit,
            para = para,
            cp = cp,
            cp1 = cp1
          )
          
          # inserting results---
          if (isTRUE(self$options$desc))
            private$.populateDescTable()
          if (isTRUE(self$options$fit))
            private$.populateFitTable()
          if (isTRUE(self$options$est))
            private$.populateEST()
          if (isTRUE(self$options$cp))
            private$.populateClassSizeTable()
          if (isTRUE(self$options$mem))
            private$.populateClassMemberTable()
          
          if (isTRUE(self$options$plot))
            private$.setPlot()
          if (isTRUE(self$options$plot1))
            private$.setPlot1()
          
          # ----------------------
          private$.registerCallbacks()
          
        }
      },
      
      
      # Descriptives---
      
      .populateDescTable = function() {
        if (!isTRUE(self$options$desc) ||
            is.null(private$.results_cache))
          return()
        
        vars <- self$options$vars
        
        table <- self$results$desc
        res <- private$.results_cache
        
        desc <- res$desc
        
        d <- data.frame(desc)
        
        for (i in seq_along(vars)) {
          # #if not rowKey, add rowkey---
          # if (!vars[i] %in% table$rowKeys) {
          #   table$addRow(rowKey = vars[i])
          # }
          #
          # if (table$getCell(rowKey=vars[i],'n')$isEmpty) {
          #
          #   table$setStatus('running')
          
          row <- list()
          row[["n"]] <- d[[2]][i]
          row[["missing"]] <- d[[3]][i]
          row[["mean"]] <- d[[4]][i]
          row[["median"]] <- d[[5]][i]
          row[["sd"]] <- d[[6]][i]
          row[["min"]] <- d[[7]][i]
          row[["max"]] <- d[[8]][i]
          table$addRow(rowKey = vars[i], values = row) #setRaw
          #  table$setStatus('complete')
          # }
        }
      },
      
      # model fit---
      .populateFitTable = function() {
        if (!isTRUE(self$options$fit) ||
            is.null(private$.results_cache))
          return()
        
        
        table <- self$results$fit
        res <- private$.results_cache
        
        fit <- res$fit
        fit <- t(fit)
        df <- as.data.frame(fit)
        #names <- dimnames(df)[[1]]
        names <- rownames(df)
        
        for (name in names) {
          # # if not rowKey, add rowkey---
          # if (!name %in% table$rowKeys) {
          #   table$addRow(rowKey = name)
          # }
          # if (table$getCell(rowKey=name,'value')$isEmpty) {
          #
          #   table$setStatus('running')
          
          row <- list()
          row[['value']] <- df[name, 1]
          
          table$addRow(rowKey = name, values = row)
          # table$setStatus('complete')
          # }
        }
      },
      
      
      
      # parameter estimates---
      .populateEST = function() {
        if (!isTRUE(self$options$fit) ||
            is.null(private$.results_cache))
          return()
        
        table <- self$results$est
        res <- private$.results_cache
        
        e <- res$para
        e <- data.frame(e)
        names <- dimnames(e)[[1]]
        
        for (name in names) {
          # # if not rowKey, add rowkey---
          # if (!name %in% table$rowKeys) {
          #   table$addRow(rowKey = name)
          # }
          #
          # if (table$getCell(rowKey=name,'cat')$isEmpty) {
          #
          #   table$setStatus('running')
          
          row <- list()
          row[['cat']] <- e[name, 1]
          row[['lhs']] <- e[name, 2]
          row[['est']] <- e[name, 3]
          row[['se']] <- e[name, 4]
          row[['p']] <- e[name, 5]
          row[['ci']] <- e[name, 6]
          row[['na']] <- e[name, 7]
          
          table$addRow(rowKey = name, values = row)
          # table$setStatus('complete')
          # }
        }
      },
      
      
      # class size---
      
      .populateClassSizeTable = function() {
        if (!isTRUE(self$options$cp) ||
            is.null(private$.results_cache))
          return()
        
        
        table <- self$results$cp
        nc <- self$options$nc
        vars <- self$options$vars
        
        res <- private$.results_cache
        d <- res$cp
        
        for (i in seq_along(1:nc)) {
          row <- list()
          
          row[["name"]] <- d[[1]][i]
          row[["count"]] <- d[[2]][i]
          row[["prop"]] <- d[[3]][i]
          
          table$addRow(rowKey = vars[i], values = row)
        }
        
      },
      
      
      # class member--
      .populateClassMemberTable = function() {
        if (!isTRUE(self$options$mem) ||
            is.null(private$.results_cache))
          return()
        
        res <- private$.results_cache
        data <- res$data
        
        cp1 <- res$cp1
        mem <- data.frame(cp1$individual)
        m <- mem$predicted
        
        m <- as.factor(m)
        
        if (self$options$mem
            && self$results$mem$isNotFilled()) {
          self$results$mem$setValues(m)
          self$results$mem$setRowNums(rownames(data))
        }
        
      },
      
      #plot----------
      .setPlot1 = function() {
        if (!isTRUE(self$options$plot1) ||
            is.null(private$.results_cache))
          return()
        
        res <- private$.results_cache
        data <- res$data
        
        # Density plot---
        long <- reshape(
          data,
          direction = "long",
          varying = list(names(data)),
          v.names = "value",
          idvar = "id",
          timevar = "time"
        )
        
        #self$results$text$setContent(long)
        image <- self$results$plot1
        image$setState(long)
      },
      
      .setPlot = function() {
        if (!isTRUE(self$options$plot1) ||
            is.null(private$.results_cache))
          return()
        #private$.checkpoint()
        
        res <- private$.results_cache
        
        # Trajectory plot---
        image <- self$results$plot
        image$setState(res$res)
      },
      
      
      # Plot---
      
      .plot1 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        long <- image$state
        
        library(ggplot2)
        plot1 <- ggplot(long, aes(x = value)) +
          geom_density() +
          facet_wrap( ~ time) + theme_bw()
        
        plot1 <- plot1 + ggtheme
        print(plot1)
        TRUE
        
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        tra <- image$state
        plot <- tidySEM::plot_growth(tra,
                                     rawdata = self$options$raw,
                                     alpha_range = c(0, 0.05))
        
        plot <- plot + ggtheme
        print(plot)
        TRUE
        
      }
      
    )
  )