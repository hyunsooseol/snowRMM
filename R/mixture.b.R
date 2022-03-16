
# This file is a generated template, your changes will not be overwritten

#' Mixture Rasch Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import mixRasch
#' @importFrom mixRasch mixRasch
#' @importFrom mixRasch getEstDetails
#' @import mixRaschTools
#' @import RColorBrewer
#' @importFrom mixRaschTools avg.theta
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 aes
#' @importFrom WrightMap wrightMap
#' @importFrom tidyr gather
#' @export


mixtureClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "mixtureClass",
    inherit = mixtureBase,
    
   
    private = list(
 
      #####################
      
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
           
            <p><b>To get started:</b></p>
            <p>_____________________________________________________________________________________________</p>
            <p>1. First, specify the number of <b>'Class', Step', and 'Type'</b> in the 'Analysis option'.</p>
            <p>2. Second, highlight the variables and click the arrow to move it across into the 'Variables' box.</p>
            <p>3. The results of <b> Save </b> will be displayed in the datasheet.</p>
            <p>4. Rasch mixture model is estimated by Jonint Maximum Liklihood(JML).</p>
            <p>5. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowRMM/issues'  target = '_blank'>GitHub.</a></p>
            <p>_____________________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
        )
        
        
      },
      
      
      .run = function() {
        
        # # get variables-------
        # 
        # data <- self$data
        # 
        # vars <- self$options$vars
        # 
        
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
          
          private$.populateImeasureTable(results)
          private$.populateIseTable(results)
          private$.populateImeanTable(results)
          private$.populateInfitTable(results)
          private$.populateOutfitTable(results)
          private$.populatePbisTable(results)
          
          # populate Average theta table----
          
          private$.populateAverageTable(results)
          
          # populate Person analysis table-------
          
        #  private$.populatePersonTable(results)
          
          private$.populateOutputs(data)
          
          
          # prepare plot-----
          
          private$.prepareWrightmapPlot(data)
          
         
        }
      },
      
      # compute results=====================================================
      
      .compute = function(data) {
        
        # get variables------
        
        # data <- self$data
        
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
        
        imeasure <- sapply(res1$LatentClass, function(x) x$item.par$delta.i)
        imeasure <- as.data.frame(imeasure)
        
        
        ise <- sapply(res1$LatentClass, function(x) x$item.par$SE.delta.i)
        ise <- as.data.frame(ise)
        
        
        #  fit <- sapply(res1$LatentClass, function(x) x$item.par$in.out)
        
        infit <- sapply(res1$LatentClass, function(x) x$item.par$in.out[,1])   
        infit <- as.data.frame(infit)
        
        
        outfit <- sapply(res1$LatentClass, function(x) x$item.par$in.out[,3])
        outfit <- as.data.frame(outfit)
        
        
        # desc <- sapply(res1$LatentClass, function(x) x$item.par$itemDescriptives)
        
        imean <- sapply(res1$LatentClass, function(x) x$item.par$itemDescriptives[,1])
        imean <- as.data.frame(imean)
        
        
        pbis <- sapply(res1$LatentClass, function(x) x$item.par$itemDescriptives[,2])
        pbis <- as.data.frame(pbis)
        
        
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
        
        # pclass <- res1$class
        
        
        results <-
          list(
            'aic' = aic,
            'bic' = bic,
            'caic' = caic,
            'imeasure' = imeasure,
            'ise' = ise,
            'infit' = infit,
            'outfit' = outfit,
            'imean' = imean,
            'pbis'=pbis,
            'class' = class,
            'average' = average
            # 'pclass' = pclass
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
      
      .populateImeasureTable = function(results) {
        
        
        table <- self$results$item$imeasure
        
        nc <- self$options$nc
        
        vars <- self$options$vars
        
        
        #result---
        
        
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
          
          table$addRow(rowKey = i, values = row)
          
        }
        
        # Prepare Data For Item Plot -------
        
        image <- self$results$iplot
        
        imeasure$item <- seq.int(nrow(imeasure))
        
        data<- tidyr::gather(data =imeasure, class, measure, -item)
        
       
        image$setState(data)
        
        
      },
      
      .populateIseTable = function(results) {
        
        
        table <- self$results$item$ise
        
        nc <- self$options$nc
        
        vars <- self$options$vars
        
        #result---
        
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
          
          table$addRow(rowKey = i, values = row)
          
        }
      },
      
      
      .populateImeanTable = function(results) {
        
        
        table <- self$results$item$imean
        
        nc <- self$options$nc
        
        vars <- self$options$vars
        
        #result---
        
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
          
          table$addRow(rowKey = i, values = row)
          
        }
      },
      
      
      .populateInfitTable = function(results) {
        
        
        table <- self$results$item$infit
        
        nc <- self$options$nc
        
        vars <- self$options$vars
        
        #result---
        
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
          
          table$addRow(rowKey = i, values = row)
          
        }
      },
      
      .populateOutfitTable = function(results) {
        
        
        table <- self$results$item$outfit
        
        nc <- self$options$nc
        
        vars <- self$options$vars
        
        #result---
        
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
          
          table$addRow(rowKey = i, values = row)
          
        }
      },
      
      
      .populatePbisTable = function(results) {
        
        
        table <- self$results$item$pbis
        
        nc <- self$options$nc
        
        vars <- self$options$vars
        
        #result---
        
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
          
          table$addRow(rowKey = i, values = row)
          
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
      
      ##### Output variables for Person membership----------------
      
      
      .populateOutputs = function(data) {
        
        
        if (self$options$pclass
            && self$results$pclass$isNotFilled()) {
        
          
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
          
          pclass <- res1$class
          
          
          keys <- 1:self$options$nc
          titles <- paste("Class", 1:self$options$nc)
          descriptions <- paste("Class", 1:self$options$nc)
          measureTypes <- rep("continuous", self$options$nc)
          
          self$results$pclass$set(
            keys=keys,
            titles=titles,
            descriptions=descriptions,
            measureTypes=measureTypes
          )
          
          self$results$pclass$setRowNums(rownames(data))
          
        
          pclass <- as.data.frame(pclass)
         
          for (i in 1:self$options$nc) {
            scores <- as.numeric(pclass[, i])
            self$results$pclass$setValues(index=i, scores)
          }
        }
      },
      
      
      # populate Person Statistics table-----
      
      # .populatePersonTable = function(results) {
      #   data <- self$data
      #   
      #   nc <- self$options$nc
      #   
      #   table <- self$results$person$persons
      #   
      #   
      #   # result-----------
      #   
      #   nclass <- results$class
      #   
      #   pclass <- results$pclass
      #   
      #   pclass <- as.data.frame(pclass)
      #   
      #   
      #   if (nclass > 1) {
      #     for (i in 2:nclass)
      #       
      #       table$addColumn(
      #         name = paste0("pc", i),
      #         title = as.character(i),
      #         type = 'number',
      #         superTitle = 'Class'
      #       )
      #     
      #   }
      #   
      #   for (i in 1:nrow(data)) {
      #     row <- list()
      #     
      #     
      #     for (j in 1:nclass) {
      #       row[[paste0("pc", j)]] <- pclass[i, j]
      #     }
      #     
      #     table$addRow(rowKey = i, values = row)
      #     
      #   }
      #   
      # },
      # 
      # 
      # Item plot--------------
      
      .itemPlot = function(image, ggtheme, theme, ...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        
        itemplot <- self$options$iplot
        
        plotData <- image$state
        
        
        plot<- ggplot2::ggplot(plotData, aes(x=as.factor(item), y=measure, group=class)) +
          geom_line(aes(color=class))+
          geom_point(aes(color=class))+
          labs(title="Item parameters by class",
               x ="Item number", y = "Measure", color='Class')+
          ggtheme
        
        
        print(plot)
        TRUE
        
      },
      
      
      # multidimensional wrightmap plot----------------
      
      
      .prepareWrightmapPlot = function(data) {
        
        # get variables------
        
       # data <- self$data
        
        vars <- self$options$vars
        
        nc <- self$options$nc
        
        step <- self$options$step
        
        unidif <-
          mixRasch::mixRasch(
            data = data,
            steps = step,
            n.c = 1
          )  
        
        dif <- unidif$item.par$delta.i
        
        person <-
          
            mixRasch::mixRasch(
            data = data,
            steps = step,
            n.c = nc
          )  
        
        theta <- sapply(person$LatentClass, function(x) x$person.par$theta)
        theta <- as.data.frame(theta)
        
        # plot---------
        
        image <- self$results$plot
        
        vars <- length(self$options$vars)
        
        width <- 300 + vars * 30
        
        image$setSize(width, 500)
        
        state <- list(theta, dif)
        
        image$setState(state)
        
        
      },
     
       
      .plot = function(image,...) {
        
        wrightmap <- self$options$wrightmap
        
        if (!wrightmap)
          return()
        
        theta <- image$state[[1]]
        dif <- image$state[[2]]
        
        plot <- WrightMap::wrightMap(theta,dif,
                                     item.prop= 0.7,
                                     dim.color = brewer.pal(10, "Set1"),
                                     axis.persons = "Person distribution",
                                     show.thr.lab = FALSE
                                     , thr.sym.col.fg = rep(brewer.pal(10, "Set1"), each = 2)
                                     , thr.sym.col.bg = rep(brewer.pal(10, "Set1"), each = 2)
                                     , thr.sym.cex = 2)
                                     
                 
                                    
                                     
                                     
      },                             
      
      
      
      #### Helper functions =================================
      
      # .getDataRowNums = function() {
      #   if (is.null(private$.dataRowNums))
      #     private$.dataRowNums <- rownames(self$dataProcessed)
      #   
      #   return(private$.dataRowNums)
      # },
      # 
      
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
