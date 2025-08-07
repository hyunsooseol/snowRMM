
# This file is a generated template, your changes will not be overwritten


lcaordClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "lcaordClass",
    inherit = lcaordBase,
    
    active = list(
      res = function() {
        if (is.null(private$.res_cache)) {
          data <- self$data
          if (self$options$miss == 'listwise')
            data <- jmvcore::naOmit(data)
          
          private$.res_cache <- tidySEM::mx_lca(
            data    = as.data.frame(data),
            classes = self$options$nc
          )
        }
        private$.res_cache
      },
      
      desc = function() {
        if (is.null(private$.desc_cache)) {
          data <- self$data
          desc <- tidySEM::descriptives(data)
          private$.desc_cache <- desc[, c("name","type","n","missing","unique","mode")]
        }
        private$.desc_cache
      }
    ),
    
    private = list(
      .htmlwidget      = NULL,
      .results_cache   = NULL,
      .res_cache       = NULL,
      .desc_cache      = NULL,
      
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
              '<li><b>tidySEM</b> guide link.</li>',
              '<li>Report issues on GitHub.</li>',
              '</ul></div>'
            )
          )
        )
        
        if (isTRUE(self$options$plot1))
          self$results$plot1$setSize(self$options$width1, self$options$height1)
        
        if (isTRUE(self$options$plot))
          self$results$plot$setSize(self$options$width, self$options$height)
        
        private$.registerCallbacks()
      },
      
      .registerCallbacks = function() {
        callbacks <- list(
          desc = private$.populateDescTable,
          fit  = private$.populateFitTable,
          cp   = private$.populateClassSizeTable,
          mem  = private$.populateClassMemberTable
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
        if (is.null(self$options$vars) || length(self$options$vars) < 3)
          return()
        
        # Show progress bar
        self$results$progressBarHTML$setVisible(TRUE)
        html <- progressBarH(5,  100, 'Initializing analysis...')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
        
        data <- self$data
        if (self$options$miss == 'listwise')
          data <- jmvcore::naOmit(data)
        data <- as.data.frame(data)
        
        # Compute results once
        if (is.null(private$.results_cache)) {
          set.seed(1234)
          
          # 15%: model fitting
          html <- progressBarH(15, 100, 'Performing LCA model...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          res  <- self$res
          desc <- self$desc
          
          # 25%: descriptives
          html <- progressBarH(25, 100, 'Computing descriptives...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          fit <- tidySEM::table_fit(res)
          
          # 35%: class probabilities
          html <- progressBarH(35, 100, 'Computing class probabilities...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          cp1 <- tidySEM::class_prob(res)
          cp  <- data.frame(cp1$sum.posterior)
          
          private$.results_cache <- list(
            data = data,
            res  = res,
            desc = desc,
            fit  = fit,
            cp   = cp,
            cp1  = cp1
          )
          
          # 45%: populate desc
          if (isTRUE(self$options$desc)) {
            html <- progressBarH(45, 100, 'Populating descriptives table...')
            self$results$progressBarHTML$setContent(html)
            private$.checkpoint()
            private$.populateDescTable()
          }
          
          # 55%: populate fit
          if (isTRUE(self$options$fit)) {
            html <- progressBarH(55, 100, 'Populating fit table...')
            self$results$progressBarHTML$setContent(html)
            private$.checkpoint()
            private$.populateFitTable()
          }
          
          # 65%: populate class size
          if (isTRUE(self$options$cp)) {
            html <- progressBarH(65, 100, 'Populating class size table...')
            self$results$progressBarHTML$setContent(html)
            private$.checkpoint()
            private$.populateClassSizeTable()
          }
          
          # 75%: populate class member
          if (isTRUE(self$options$mem)) {
            html <- progressBarH(75, 100, 'Populating class members table...')
            self$results$progressBarHTML$setContent(html)
            private$.checkpoint()
            private$.populateClassMemberTable()
          }
          
          # 85%: plot response probabilities
          if (isTRUE(self$options$plot)) {
            html <- progressBarH(85, 100, 'Generating response probability plot...')
            self$results$progressBarHTML$setContent(html)
            private$.checkpoint()
            private$.setResponseProbPlot()
          }
          
          # 95%: plot bar
          if (isTRUE(self$options$plot1)) {
            html <- progressBarH(95,100,'Generating bar plot...')
            self$results$progressBarHTML$setContent(html)
            private$.checkpoint()
            private$.setBarPlot()
          }
        }
        
        # 100%: complete and hide
        html <- progressBarH(100,100,'Analysis complete!')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
        self$results$progressBarHTML$setVisible(FALSE)
        
        private$.registerCallbacks()
      },
      
      .populateDescTable = function() {
        vars  <- self$options$vars
        table <- self$results$desc
        d     <- data.frame(private$.results_cache$desc)
        lapply(seq_along(vars), function(i) {
          row <- list(
            type   = d[[2]][i],
            n      = d[[3]][i],
            missing= d[[4]][i],
            unique = d[[5]][i],
            mode   = d[[6]][i]
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
        vars  <- self$options$vars
        lapply(seq_len(nrow(d)), function(i) {
          table$addRow(rowKey = vars[i],
                       values = list(name = d[[1]][i], count = d[[2]][i], prop = d[[3]][i]))
        })
      },
      
      .populateClassMemberTable = function() {
        table <- self$results$mem
        mem   <- data.frame(private$.results_cache$cp1$individual)
        m     <- as.factor(mem$predicted)
        if (table$isNotFilled()) {
          table$setRowNums(rownames(self$data))
          table$setValues(m)
        }
      },
      
      .setResponseProbPlot = function() {
        image <- self$results$plot
        image$setState(private$.results_cache$res)
      },
      
      .setBarPlot = function() {
        df <- as.data.frame(private$.results_cache$data)
        names(df) <- paste0("Value.", names(df))
        df_long <- reshape(df, varying=names(df), direction="long")
        self$results$plot1$setState(df_long)
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) return(FALSE)
        p <- tidySEM::plot_prob(image$state, bw=TRUE) + ggtheme
        if (self$options$angle>0)
          p <- p + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=self$options$angle, hjust=1))
        print(p); TRUE
      },
      
      .plot1 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) return(FALSE)
        p <- ggplot(image$state, aes(x=Value)) +
          geom_bar() + facet_wrap(~time, scales="free") + theme_bw() + ggtheme
        print(p); TRUE
      }
    )
  )

# Progress Bar HTML  (R/progressBarH.R)
progressBarH <- function(progress=0, total=100, message='') {
  percentage <- round(progress/total*100)
  width      <- 400 * percentage/100
  html <- paste0(
    '<div style="text-align:center;padding:20px;">',
    '<div style="width:400px;height:20px;border:1px solid #ccc;',
    'background-color:#f8f9fa;margin:0 auto;border-radius:4px;">',
    '<div style="width:', width, 'px;height:18px;',
    'background-color:#999999;border-radius:3px;',
    'transition:width 0.3s ease;"></div>',
    '</div>',
    '<div style="margin-top:8px;font-size:12px;color:#666;">',
    message, ' (', percentage, '%)',
    '</div>',
    '</div>'
  )
  return(html)
}



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
