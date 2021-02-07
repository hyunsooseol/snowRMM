
# This file is a generated template, your changes will not be overwritten

#' Mixture Rasch Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom mixRasch mixRasch
#' @importFrom mixRasch getEstDetails
#' @importFrom ShinyItemAnalysis ggWrightMap
#' @import ggplot2
#' @export


raschClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "raschClass",
    inherit = raschBase,
    private = list(
      
      
      .init = function() {
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(
          "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            
            <p> The standard Rasch model is performed by Jonint Maximum Liklihood(JML).</p>

            <p> Specify </b> the number of 'Step' and model 'Type'</b> in the 'Analysis option'.</p>

            <P> Step is defined as number of category-1. </p>

            <p> Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowRMM/'  target = '_blank'>GitHub</a></p>

            </div>
            </body>
            </html>"
        )
        
      },
      
      
      .run = function() {
        
        # get variables-------
        
        data <- self$data
        
        vars <- self$options$vars
        
        #Removing perfect score items before estimation (for example all 1 or 0)-------
        
        for (varName in self$options$vars) {
          var <- self$data[[varName]]
          if (length(unique(var)) < 2)
          stop(paste("Variable '", varName, "' contains all the same value and should be removed in the variable box."))
        }
        
        # Ready--------
        
        ready <- TRUE
        
        if (is.null(self$options$vars) |
            length(self$options$vars) < 2)
          
          ready <- FALSE
        
        if (ready) {
          data <- private$.cleanData()
          
          results <- private$.compute(data)
          
          #  populate Model information table-----
          
          private$.populateModelTable(results)
         
          # populate Item Statistics table-----
          
          private$.populateItemTable(results)
          
         
          # populate Person statistics table
          
          private$.populatePersonTable(results)
          
          # prepare wrightmap plot-----
          
          private$.prepareWrightmapPlot(data)
          
        }
      },
      
      .compute = function(data) {
        
        # get variables--------
        
        # data <- self$data
        
        vars <- self$options$vars
        
        step <- self$options$step
        
        type <- self$options$type
        
        
        # compute results------
        
        res <-
          mixRasch::mixRasch(
            data = data,
            steps = step,
            model = type,
            n.c = 1
          )
        
        # model information--------
        
        aic <- res$info.fit$AIC
        
        bic <- res$info.fit$BIC
        
        caic <- res$info.fit$CAIC
        
        # item statistics---------
        
        imean <- res$item.par$itemDescriptives
        
        imeasure <- res$item.par$delta.i
        
        ise <- res$item.par$SE.delta.i
        
        infit <- res$item.par$in.out
        
        outfit <- res$item.par$in.out
        
        pbis <- res$item.par$itemDescriptives
        
        # get number of class---------
        
        res0 <- mixRasch::getEstDetails(res)
        class <- res0$nC
        
        # person statistics---------
        
        total <- res$person.par$r
        
        pmeasure <- res$person.par$theta
        
        pse <- res$person.par$SE.theta
        
        pinfit <- res$person.par$infit
        
        poutfit <- res$person.par$outfit
        
        
        results <-
          list(
            'aic' = aic,
            'bic' = bic,
            'caic' = caic,
            'imean' = imean,
            'imeasure' = imeasure,
            'ise' = ise,
            'infit' = infit,
            'outfit' = outfit,
            'pbis' = pbis,
            'class' = class,
            'total' = total,
            'pmeasure' = pmeasure,
            'pse' = pse,
            'pinfit' = pinfit,
            'poutfit' = poutfit
            
          )
        
        
      },
      
     
      # populate Model information table-----
      
      .populateModelTable = function(results) {
        
        table <- self$results$item$model
        
        
        #results---------
        
        class <- results$class
        
        aic <- results$aic
        
        bic <- results$bic
        
        caic <- results$caic
        
        
        row <- list()
        
        row[["class"]] <- class
        row[["aic"]] <- aic
        row[["bic"]] <- bic
        row[["caic"]] <- caic
        
        table$setRow(rowNo = 1, values = row)
        
        
      },
      
      
      # populate Item Statistics table-----
      
      .populateItemTable = function(results) {
        
        table <- self$results$item$items
        
        vars <- self$options$vars
        
        
        #result---
        
        imean <- results$imean
        
        imeasure <- results$imeasure
        
        ise <- results$ise
        
        infit <- results$infit
        
        outfit <- results$outfit
        
        pbis <- results$pbis
        
        
        for (i in seq_along(vars)) {
          row <- list()
          
          
          row[["imean"]] <- imean[i, 1]
          
          row[["imeasure"]] <- imeasure[i]
          
          row[["ise"]] <- ise[i]
          
          row[["infit"]] <- infit[i, 1]
          
          row[["outfit"]] <- outfit[i, 3]
          
          row[["pbis"]] <- pbis[i, 2]
          
          table$setRow(rowKey = vars[i], values = row)
        }
        
      },
      
      
      # populate Person analysis table-----   
      
      .populatePersonTable = function(results) {
        
        data <- self$data
        
        table <- self$results$person$persons
        
        
        #result---
        
        total <- results$total
        
        pmeasure <- results$pmeasure
        
        pse <- results$pse
        
        pinfit <- results$pinfit
        
        poutfit <- results$poutfit
        
        
        
        for (i in 1:nrow(data)) {
          row <- list()
          
          
          row[["total"]] <- total[i]
          
          row[["pmeasure"]] <- pmeasure[i]
          
          row[["pse"]] <- pse[i]
          
          row[["pinfit"]] <- pinfit[i]
          
          row[["poutfit"]] <- poutfit[i]
          
          table$addRow(rowKey = i, values = row)
          
        }
        
      },
      
      
      ### wrightmap Plot functions -----------
      
      
      .prepareWrightmapPlot = function(data) {
        
        # get variables--------
        
       # data <- self$data
        
        step <- self$options$step
        
        type <- self$options$type
        
        
        #compute wright---
        
        res <-  mixRasch::mixRasch(
          data = data,
          steps = step,
          model = type,
          n.c = 1
        )
        
        imeasure <- res$item.par$delta.i
        pmeasure <- res$person.par$theta
        
        # plot---------
        
        image <- self$results$plot
        
        vars <- length(self$options$vars)
        
        # width <- 300 + vars * 30
        # 
        # image$setSize(width, 500)
        
        state <- list(pmeasure, imeasure)
        
        image$setState(state)
        
       
      },
      
      # wright map plot--------------
      
      .plot = function(image,ggtheme, theme, ...) {
        
        wrightmap <- self$options$wrightmap
        
        if (!wrightmap)
          return()
        
        pmeasure <- image$state[[1]]
        imeasure <- image$state[[2]]
        

        plot<- ShinyItemAnalysis::ggWrightMap(pmeasure, imeasure,
                                              color = "green")
        
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
