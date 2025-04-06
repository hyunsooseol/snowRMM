
# This file is a generated template, your changes will not be overwritten


lcaordClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "lcaordClass",
    inherit = lcaordBase,
    
    # Active bindings---
    active = list(
      res = function() {
        if (is.null(private$.res_cache)) {
          data <- self$data
          if (self$options$miss == 'listwise') {
            data <- jmvcore::naOmit(data)
          }
          
          #private$.checkpoint()
          
          private$.res_cache <- tidySEM::mx_lca(data = as.data.frame(data), classes = self$options$nc)
        }
        return(private$.res_cache)
      },
      
      #---
      desc = function() {
        if (is.null(private$.desc_cache)) {
          data <- self$data
          desc <- tidySEM::descriptives(data)
          private$.desc_cache <- desc[, c("name", "type", "n", "missing", "unique", "mode")]
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
            '<li><b>tidySEM</b> R package is described in the <a href="https://cjvanlissa.github.io/tidySEM/articles/lca_ordinal.html" target = "_blank">page</a>.</li>',
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
      
#################################
      
      .run = function() {
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
          
          # addtional computing
          fit <- tidySEM::table_fit(res)
          cp1 <- tidySEM::class_prob(res)
          cp <- data.frame(cp1$sum.posterior)
          
          # save
          private$.results_cache <- list(
            data = data,
            res = res,
            desc = desc,
            fit = fit,
            cp = cp,
            cp1 = cp1
          )
          
          # inserting results---
          if (isTRUE(self$options$desc))
            private$.populateDescTable()
          if (isTRUE(self$options$fit))
            private$.populateFitTable()
          if (isTRUE(self$options$cp))
            private$.populateClassSizeTable()
          if (isTRUE(self$options$mem))
            private$.populateClassMemberTable()
          if (isTRUE(self$options$plot))
            private$.setResponseProbPlot()
          if (isTRUE(self$options$plot1))
            private$.setBarPlot()
          
          # ----------------------
          private$.registerCallbacks()
          
        }
      },
      
      # Descriptives---
      .populateDescTable = function() {
        if (!isTRUE(self$options$desc) || is.null(private$.results_cache))
          return()
        vars <- self$options$vars
        table <- self$results$desc
        d <- data.frame(private$.results_cache$desc)
        lapply(seq_along(vars), function(i) {
          row <- list(
            type = d[[2]][i],
            n = d[[3]][i],
            missing = d[[4]][i],
            unique = d[[5]][i],
            mode = d[[6]][i]
          )
          table$addRow(rowKey = vars[i], values = row)
        })
      },
      
      # model fit---
      .populateFitTable = function() {
        if (!isTRUE(self$options$fit) || is.null(private$.results_cache))
          return()
        
        retlist <- private$.results_cache
        table <- self$results$fit
        df <- as.data.frame(t(retlist$fit))
        lapply(rownames(df), function(name) {
          row <- list(value = df[name, 1])
          table$addRow(rowKey = name, values = row)
        })
      },
      
      # class size---
      .populateClassSizeTable = function() {
        if (!isTRUE(self$options$cp) || is.null(private$.results_cache))
          return()
        
        nc <- self$options$nc
        vars <- self$options$vars
        table <- self$results$cp
        d <- private$.results_cache$cp

        lapply(seq_len(nc), function(i) {
          row <- list(name = d[[1]][i],
                      count = d[[2]][i],
                      prop = d[[3]][i])
          table$addRow(rowKey = vars[i], values = row)
        })
      },
      
      # class member--
      .populateClassMemberTable = function() {
        if (!isTRUE(self$options$mem) ||
            is.null(private$.results_cache))
          return()
        
        retlist <- private$.results_cache
        data <- retlist$data
        
        #cp1<- tidySEM::class_prob(retlist$res)
        cp1 <- retlist$cp1
        
        mem <- data.frame(cp1$individual)
        m <- as.factor(mem$predicted)
        # 
        # m <- as.factor(m)
        if (self$options$mem
            && self$results$mem$isNotFilled()) {
          self$results$mem$setValues(m)
          self$results$mem$setRowNums(rownames(data))
        }
      },
      
      # Response probilities plot---
      .setResponseProbPlot = function() {
        if (!isTRUE(self$options$plot) ||
            is.null(private$.results_cache))
          return()
        
        #private$.checkpoint()
        retlist <- private$.results_cache
        image <- self$results$plot
        image$setState(retlist$res)
      },

      # Bar plot---
      .setBarPlot = function() {
        if (!isTRUE(self$options$plot1) ||
            is.null(private$.results_cache))
          return()
        
        retlist <- private$.results_cache
        data <- retlist$data
        
        df_plot <- data
        names(df_plot) <- paste0("Value.", names(df_plot))
        df_plot <- reshape(df_plot, varying = names(df_plot), direction = "long")
        
        image <- self$results$plot1
        image$setState(df_plot)
      },

      # plot---
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        res <- image$state
        plot <- tidySEM::plot_prob(res, bw = TRUE)
        plot <- plot + ggtheme
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        print(plot)
        TRUE
      },
      
      #Bar plot---
      
      .plot1 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        df_plot <- image$state
        
        library(ggplot2)
        plot1 <- ggplot(df_plot, aes(x = Value)) + geom_bar() +
          facet_wrap(~ time, scales = "free") + theme_bw()
        
        plot1 <- plot1 + ggtheme
        print(plot1)
        TRUE
      }
    )
  )

# .populateDescTable = function() {
#   if (!isTRUE(self$options$desc) ||
#       is.null(private$.results_cache))
#     return()
#
#   vars <- self$options$vars
#   retlist <- private$.results_cache
#
#   table <- self$results$desc
#   desc <- retlist$desc
#
#   d <- data.frame(desc)
#
#   for (i in seq_along(vars)) {
#     # # if not rowKey, add rowkey---
#     # if (!vars[i] %in% table$rowKeys) {
#     #   table$addRow(rowKey = vars[i])
#     # }
#     #
#     # if (table$getCell(rowKey=vars[i],'type')$isEmpty) {
#     #
#     #   table$setStatus('running')
#
#     row <- list()
#     row[["type"]] <- d[[2]][i]
#     row[["n"]] <- d[[3]][i]
#     row[["missing"]] <- d[[4]][i]
#     row[["unique"]] <- d[[5]][i]
#     row[["mode"]] <- d[[6]][i]
#     table$addRow(rowKey = vars[i], values = row)
#     # table$setStatus('complete')
#     # }
#   }
# },
#


# .populateFitTable = function() {
#   if (!isTRUE(self$options$fit) ||
#       is.null(private$.results_cache))
#     return()
#
#   retlist <- private$.results_cache
#   table <- self$results$fit
#
#   fit <- retlist$fit
#   fit <- t(fit)
#   df <- as.data.frame(fit)
#   names <- dimnames(df)[[1]]
#
#   for (name in names) {
#     # # if not rowKey, add rowkey---
#     # if (!name %in% table$rowKeys) {
#     #   table$addRow(rowKey = name)
#     # }
#     #
#     # if (table$getCell(rowKey=name,'value')$isEmpty) {
#     #
#     #   table$setStatus('running')
#
#     row <- list()
#     row[['value']] <- df[name, 1]
#     table$addRow(rowKey = name, values = row)
#     # table$setStatus('complete')
#     # }
#   }
# },

#
# .populateClassSizeTable = function() {
#   if (!isTRUE(self$options$cp) ||
#       is.null(private$.results_cache))
#     return()
#
#   nc <- self$options$nc
#   vars <- self$options$vars
#   retlist <- private$.results_cache
#
#   table <- self$results$cp
#   d <- retlist$cp
#
#   for (i in seq_along(1:nc)) {
#     row <- list()
#
#     row[["name"]] <- d[[1]][i]
#     row[["count"]] <- d[[2]][i]
#     row[["prop"]] <- d[[3]][i]
#
#     table$addRow(rowKey = vars[i], values = row)
#   }
#
# },
