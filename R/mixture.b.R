
# This file is a generated template, your changes will not be overwritten

#' Mixture Rasch Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import mixRasch
#' @importFrom mixRasch mixRasch
#' @import mixRaschTools
#' @importFrom mixRaschTools mixRasch.plot
#' @importFrom mixRaschTools avg.theta
#' @import magicfor
#' @importFrom magicfor magic_for
#' @importFrom magicfor magic_result
#' @export


mixtureClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "mixtureClass",
    inherit = mixtureBase,
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
            <p>Welcome to Rasch Mixture Model.</p>

            <p><b>To get started:</b></p>

            <p>- First, specify the number of <b>'Class(more than 2 classes)', Step', and 'Type'</b> in the 'Analysis option'.</p>

            <p>- Second, highlight the variables and click the arrow to move it across into the 'Variables' box.</p>

            <p>- Rasch mixture model is estimated by Jonint Maximum Liklihood(JML).</p>

            <p>- Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowRMM/'  target = '_blank'>GitHub</a></p>

            <p>If you have any questions, please e-mail me: snow@cau.ac.kr</a></p>
            </div>
            </body>
            </html>"
        )
        
        
      },
      
      
      .run = function() {
        # get variables-------
        
        data <- self$data
        
        vars <- self$options$get('vars')
        
        
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
          
          # populate Average theta table----
          
          private$.populateAverageTable(results)
          
          # populate Person analysis table-------
          
          private$.populatePersonTable(results)
          
          # prepare item by class plot-----
          
          private$.prepareItemPlot(data)
          
          
        }
      },
      
      # compute results=====================================================
      
      .compute = function(data) {
        # get variables------
        
        data <- self$data
        
        vars <- self$options$vars
        
        nc <- self$options$nc
        
        step <- self$options$step
        
        type <- self$options$type
        
        # computing mixRasch-----------
        
        res1 <-
          mixRasch::mixRasch(
            data = data,
            steps = step,
            model = type,
            n.c = nc
          )
        
        # item statistics--------
       
        magicfor::magic_for(silent = TRUE)
        
        for(c in 1:nc){
        
        imean <-
          res1$LatentClass[[c]]$item.par$itemDescriptives

        imeasure <-
          res1$LatentClass[[c]]$item.par$delta.i

        ise <-
          res1$LatentClass[[c]]$item.par$SE.delta.i

        infit <- res1$LatentClass[[c]]$item.par$in.out

        outfit <-
          res1$LatentClass[[c]]$item.par$in.out

        pbis <-
          res1$LatentClass[[c]]$item.par$itemDescriptives
        
        put(imean, imeasure, ise, infit, outfit, pbis)

        }
        
        itemstat<- magicfor::magic_result()
        
        imean <-  itemstat$imean
        imeasure<- itemstat$imeasure
        ise <-  itemstat$ise
        infit <-  itemstat$infit
        outfit <-  itemstat$outfit
        pbis <-  itemstat$pbis
        
        # model information------
        
        aic <- res1$info.fit$AIC
        
        bic <- res1$info.fit$BIC
        
        caic <- res1$info.fit$CAIC
        
        
        # number of class
        
        res0 <- mixRasch::getEstDetails(res1)
        
        class <- res0$nC
        
        # Average Theta Values
        
        average <- mixRaschTools::avg.theta(res1)
        
        # person class
        
        pclass <- res1$class
        
        
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
            'average' = average,
            'pclass' = pclass
          )
        
      },
      
      # populate Model information table-----
      
      .populateModelTable = function(results) {
        table <- self$results$item$model
        nc <- self$options$nc
        
        
        #results---------
        
        class <- results$class
        aic <- results$aic
        bic <- results$bic
        caic <- results$caic
        
        
        for (i in seq_len(nc)) {
          row <- list()
          
          row[["class"]] <- class
          row[["aic"]] <- aic
          row[["bic"]] <- bic
          row[["caic"]] <- caic
          
          table$setRow(rowNo = 1, values = row)
          
        }
        
      },
      
      
      # populate Item Statistics table-----
      
      .populateItemTable = function(results) {
        
        
        table <- self$results$item$items
        
        nc <- self$options$nc
        
        vars <- self$options$vars
        
        
        #result---
        
       
        imean <- results$imean

        imeasure <- results$imeasure

        ise <- results$ise

        infit <- results$infit

        outfit <- results$outfit

        pbis <- results$pbis

        
        for(c in 1:nc){
          
          row <- list()
          vars <- nc[[i]]$vars
        
          imean <- imean[[c]]
          imeasure <- imeasure[[c]]
          ise <- ise[[c]]
          infit <- infit[[c]]
          outfit <- outfit[[c]]
          pbis <- pbis[[c]]
            
          
         for (i in seq_along(vars)) {
        
         #   row <- list()
         
          row[["imean"]] <- imean[i, 1]
          row[["imeasure"]] <- imeasure[i]
          row[["ise"]] <- ise[i]
          row[["infit"]] <- infit[i, 1]
          row[["outfit"]] <- outfit[i, 3]
          row[["pbis"]] <- pbis[i, 2]
          
          table$setRow(rowKey = vars[i], values = row)
        }
        }
        
      },
      
      # populate Average theta table-----
      
      .populateAverageTable = function(results) {
        nc <- self$options$nc
        
        table <- self$results$person$average
        
        #results---------
        
        value <- results$average
        value <- as.matrix(value)
        
        
        for (i in seq_len(nc)) {
          row <- list()
          
          row[["value"]] <- value[i, 1]
          
          table$setRow(rowNo = i, values = row)
        }
        
      },
      
      # populate Person Statistics table-----
      
      .populatePersonTable = function(results) {
        data <- self$data
        
        nc <- self$options$nc
        
        table <- self$results$person$persons
        
        
        # result-----------
        
        nclass <- results$class
        
        pclass <- results$pclass
        
        pclass <- as.data.frame(pclass)
        
        
        if (nclass > 1) {
          for (i in 2:nclass)
            
            table$addColumn(
              name = paste0("pc", i),
              title = as.character(i),
              type = 'number',
              superTitle = 'Class'
            )
          
        }
        
        for (i in 1:nrow(data)) {
          row <- list()
          
          
          for (j in 1:nclass) {
            row[[paste0("pc", j)]] <- pclass[i, j]
          }
          
          table$addRow(rowKey = i, values = row)
          
        }
        
      },
      
      
      ### Item Plot by Class ----
      
      
      .prepareItemPlot = function(data) {
        data <- self$data
        
        step <- self$options$step
        
        nc <- self$options$nc
        
        # computing mixRasch------------
        
        res2 <-
          mixRasch::mixRasch(
            data = data,
            steps = step,
            model = 'RSM',
            n.c = nc
          )
        
        # Prepare Data For Item Plot -------
        
        image <- self$results$iplot
        
        image$setState(res2)
        
      },
      
      # Item plot--------------
      
      .itemPlot = function(image, ...) {
        itemplot <- self$options$iplot
        
        nc <- self$options$nc
        
        
        res2 <- image$state
        
        
        if (is.null(res2))
          return()
        
        plot <- mixRaschTools::mixRasch.plot(res2)
        
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
