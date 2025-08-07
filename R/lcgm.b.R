
# This file is a generated template, your changes will not be overwritten
#' @importFrom magrittr %>%

lcgmClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "lcgmClass",
    inherit = lcgmBase,
    
    active = list(
      res = function() {
        if (is.null(private$.res_cache)) {
          data <- self$data
          if (self$options$miss == 'listwise')
            data <- jmvcore::naOmit(data)
          private$.res_cache <- tidySEM::mx_growth_mixture(
            model      = self$options$model,
            data       = as.data.frame(data),
            classes    = self$options$nc,
            thresholds = self$options$thr
          )
        }
        private$.res_cache
      },
      
      desc = function() {
        if (is.null(private$.desc_cache)) {
          data <- self$data
          private$.desc_cache <- tidySEM::descriptives(data)[,
                                                             c("name","n","missing","mean","median","sd","min","max")]
        }
        private$.desc_cache
      },
      
      fit = function() {
        if (is.null(private$.fit_cache))
          private$.fit_cache <- tidySEM::table_fit(self$res)
        private$.fit_cache
      },
      
      parameters = function() {
        if (is.null(private$.para_cache)) {
          para <- tidySEM::table_results(self$res, columns = NULL)
          private$.para_cache <- para[para$Category %in% c("Means","Variances"),
                                      c("Category","lhs","est","se","pval","confint","name")]
        }
        private$.para_cache
      },
      
      classProbabilities = function() {
        if (is.null(private$.cp_cache)) {
          cp1 <- tidySEM::class_prob(self$res)
          private$.cp_cache <- list(
            summary    = data.frame(cp1$sum.posterior),
            individual = data.frame(cp1$individual)
          )
        }
        private$.cp_cache
      }
    ),
    
    private = list(
      .htmlwidget   = NULL,
      .res_cache    = NULL,
      .desc_cache   = NULL,
      .fit_cache    = NULL,
      .para_cache   = NULL,
      .cp_cache     = NULL,
      .mc_cache     = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) || is.null(self$options$vars))
          self$results$instructions$setVisible(TRUE)
        
        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title   = "Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px;',
              ' padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<ul>',
              '<li><b>tidySEM</b> R package usage guide link.</li>',
              '<li>Set <b>Thresholds=TRUE</b> for ordinal data.</li>',
              '<li>Model comparison may take longer time.</li>',
              '<li>GitHub issues: snowRMM repository.</li>',
              '</ul></div>'
            )
          )
        )
        
        if (self$options$mc)
          self$results$mc$setNote("Note",
                                  "Entropy not shown due to instability in some models.")
        
        if (isTRUE(self$options$plot1))
          self$results$plot1$setSize(self$options$width1, self$options$height1)
        
        if (isTRUE(self$options$plot))
          self$results$plot$setSize(self$options$width, self$options$height)
      },
      
      .run = function() {
        if (is.null(self$options$vars) || length(self$options$vars) < 3)
          return()
        
        # Show and initialize progress bar
        self$results$progressBarHTML$setVisible(TRUE)
        html <- progressBarH(5,  100, 'Starting analysis...')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
        
        set.seed(1234)
        
        # Descriptives
        if (isTRUE(self$options$desc)) {
          html <- progressBarH(15, 100, 'Computing descriptives...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.populateDescTable()
        }
        
        # Fit statistics
        if (isTRUE(self$options$fit)) {
          html <- progressBarH(25, 100, 'Calculating fit statistics...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.populateFitTable()
        }
        
        # Parameter estimates
        if (isTRUE(self$options$est)) {
          html <- progressBarH(35, 100, 'Extracting parameter estimates...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.populateEST()
        }
        
        # Class probabilities
        if (isTRUE(self$options$cp)) {
          html <- progressBarH(45, 100, 'Computing class probabilities...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.populateClassSizeTable()
        }
        
        # Model comparison
        if (isTRUE(self$options$mc)) {
          html <- progressBarH(55, 100, 'Performing model comparison...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.populateModelComparisonTable()
        }
        
        # Class members
        if (isTRUE(self$options$mem)) {
          html <- progressBarH(65, 100, 'Listing class members...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.populateClassMemberTable()
        }
        
        # Plot1
        if (isTRUE(self$options$plot1)) {
          html <- progressBarH(75, 100, 'Preparing density plot...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.setPlot1()
        }
        
        # Plot2
        if (isTRUE(self$options$plot)) {
          html <- progressBarH(85, 100, 'Generating growth plot...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.setPlot()
        }
        
        # Complete
        html <- progressBarH(100,100, 'Analysis complete!')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
        self$results$progressBarHTML$setVisible(FALSE)
      },
      
      .populateDescTable = function() {
        vars  <- self$options$vars
        table <- self$results$desc
        d     <- as.data.frame(self$desc)
        for (i in seq_along(vars))
          table$addRow(rowKey = vars[i], values = as.list(d[i,2:8]))
      },
      
      .populateFitTable = function() {
        table <- self$results$fit
        df    <- as.data.frame(t(self$fit))
        for (nm in rownames(df))
          table$addRow(rowKey = nm, values = list(value = df[nm,1]))
      },
      
      .populateEST = function() {
        table <- self$results$est
        e     <- as.data.frame(self$parameters)
        for (nm in rownames(e))
          table$addRow(rowKey = nm, values = list(
            cat  = e[nm,1],
            lhs  = e[nm,2],
            est  = e[nm,3],
            se   = e[nm,4],
            p    = e[nm,5],
            ci   = e[nm,6],
            na   = e[nm,7]
          ))
      },
      
      .populateClassSizeTable = function() {
        table <- self$results$cp
        d     <- self$classProbabilities$summary
        for (i in seq_len(nrow(d)))
          table$addRow(rowKey = i, values = as.list(d[i,]))
      },
      
      .populateClassMemberTable = function() {
        table <- self$results$mem
        mem   <- self$classProbabilities$individual
        if (table$isNotFilled()) {
          table$setRowNums(rownames(self$data))
          table$setValues(as.factor(mem$predicted))
        }
      },
      
      .computeModelComparison = function() {
        if (!self$options$mc)
          return(data.frame())
        if (is.null(private$.mc_cache)) {
          data_df <- as.data.frame(self$data)
          if (self$options$miss == 'listwise')
            data_df <- jmvcore::naOmit(data_df)
          res_list <- list()
          for (k in seq_len(self$options$nc)) {
            m_k <- try(tidySEM::mx_growth_mixture(
              model      = self$options$model,
              data       = data_df,
              classes    = k,
              thresholds = self$options$thr
            ), silent=TRUE)
            if (!inherits(m_k, "try-error")) {
              fit_stats <- tidySEM::table_fit(m_k)
              res_list[[k]] <- data.frame(
                classes = k,
                AIC     = fit_stats$AIC,
                BIC     = fit_stats$BIC
              )
            }
          }
          private$.mc_cache <- do.call(rbind, res_list)
        }
        private$.mc_cache
      },
      
      .populateModelComparisonTable = function() {
        table <- self$results$mc
        mc_data <- private$.computeModelComparison()
        for (i in seq_len(nrow(mc_data)))
          table$addRow(rowKey = mc_data$classes[i],
                       values = as.list(mc_data[i,]))
      },
      
      .setPlot1 = function() {
        long <- reshape(self$data, direction="long",
                        varying=list(names(self$data)),
                        v.names="value",
                        idvar="id", timevar="time")
        self$results$plot1$setState(long)
      },
      
      .setPlot = function() {
        self$results$plot$setState(self$res)
      },
      
      .plot1 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) return(FALSE)
        long <- image$state
        p <- ggplot(long, aes(x=value)) +
          geom_density() + facet_wrap(~time) + theme_bw()
        print(p + ggtheme)
        TRUE
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) return(FALSE)
        p <- tidySEM::plot_growth(image$state,
                                  rawdata=self$options$raw,
                                  alpha_range=c(0,0.05))
        print(p + ggtheme)
        TRUE
      }
    )
  )

# Progress Bar HTML 함수 (R/progressBarH.R)
progressBarH <- function(progress = 0, total = 100, message = '') {
  percentage <- round(progress / total * 100)
  width      <- 400 * percentage / 100
  
  html <- paste0(
    '<div style="text-align: center; padding: 20px;">',
    '<div style="width: 400px; height: 20px; border: 1px solid #ccc;',
    ' background-color: #f8f9fa; margin: 0 auto; border-radius: 4px;">',
    '<div style="width: ', width, 'px; height: 18px;',
    ' background-color: #999999; border-radius: 3px;',
    ' transition: width 0.3s ease;"></div>',
    '</div>',
    '<div style="margin-top: 8px; font-size: 12px; color: #666;">',
    message, ' (', percentage, '%)',
    '</div>',
    '</div>'
  )
  
  return(html)
}
