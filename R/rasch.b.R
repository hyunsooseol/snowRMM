
# This file is a generated template, your changes will not be overwritten

#' Mixture Rasch Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom mixRasch mixRasch
#' @importFrom mixRasch getEstDetails
#' @importFrom ShinyItemAnalysis ggWrightMap
#' @importFrom eRm plotICC
#' @importFrom eRm RM
#' @import RColorBrewer
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
            
            <p><b>Instructions</b></p>
            <p>_____________________________________________________________________________________________</p>
            <p>1. The standard Rasch model is performed by Jonint Maximum Liklihood(JML).</p>
            <p>2. Specify </b> the number of 'Step' and model 'Type'</b> in the 'Analysis option'.</p>
            <P>3. Step is defined as number of <b>category-1</b>. </p>
            <p>4. The results of <b>Person Analysis</b> will be displayed in the datasheet.</p>
            <p>5. ICC plot can only be plotted for a dichotomous Rasch model.</p>
            <p>6. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowRMM/issues'  target = '_blank'>GitHub</a>.</p>
            <p>_____________________________________________________________________________________________</p>
            
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
          
         # private$.populatePersonTable(results)
          
          # populate output variables-----
          
          private$.populateOutputs(data)
          
          
          # prepare wrightmap plot-----
          
          private$.prepareWrightmapPlot(data)
          
          # prepare item fit plot-------
          
          private$.prepareInfitPlot(data)
          
          private$.prepareOutfitPlot(data)
          
          private$.prepareIccPlot(data)
          
          
        }
      },
      
      .compute = function(data) {
        
        # get variables--------
        
        
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
        
        loglik <- res$info.fit$loglik
        parm <- res$info.fit$N.parms
        person <- res$info.fit$N.persons
        
        # item statistics---------
        
        imean <- res$item.par$itemDescriptives
        
        imeasure <- res$item.par$delta.i
        
        ise <- res$item.par$SE.delta.i
        
        infit <- res$item.par$in.out[,1]
        
        outfit <- res$item.par$in.out[,3]
        
        pbis <- res$item.par$itemDescriptives
        
        # get number of class---------
        
        res0 <- mixRasch::getEstDetails(res)
        class <- res0$nC
        
       
        results <-
          list(
            'aic' = aic,
            'bic' = bic,
            'caic' = caic,
            'loglik'=loglik,
            'parm'=parm,
            'person'=person,
            'imean' = imean,
            'imeasure' = imeasure,
            'ise' = ise,
            'infit' = infit,
            'outfit' = outfit,
            'pbis' = pbis,
            'class' = class
            
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
        
        loglik <- results$loglik
        parm <- results$parm
        person <- results$person
        
        
        row <- list()
        
        row[["class"]] <- class
        row[["aic"]] <- aic
        row[["bic"]] <- bic
        row[["caic"]] <- caic
        row[["loglik"]] <- loglik
        row[["parm"]] <- parm
        row[["person"]] <- person
        
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
          
          row[["infit"]] <- infit[i]
          
          row[["outfit"]] <- outfit[i]
          
          row[["pbis"]] <- pbis[i, 2]
          
          table$setRow(rowKey = vars[i], values = row)
        }
        
      },
      
      ##### person statistics for output variable-------------------
      
      .populateOutputs= function(data) {
        
        if (self$options$total&& self$results$total$isNotFilled()){
          
          step <- self$options$step
          
          type <- self$options$type
          
          res <-
            mixRasch::mixRasch(
              data = data,
              steps = step,
              model = type,
              n.c = 1
            )
         
          total <- res$person.par$r
          
          self$results$total$setRowNums(rownames(data))
          self$results$total$setValues(total)
          
        }
        
        if (self$options$pmeasure&& self$results$pmeasure$isNotFilled()){
          
          step <- self$options$step
          
          type <- self$options$type
          
          res <-
            mixRasch::mixRasch(
              data = data,
              steps = step,
              model = type,
              n.c = 1
            )
          
          pmeasure <- res$person.par$theta
          
          self$results$pmeasure$setRowNums(rownames(data))
          self$results$pmeasure$setValues(pmeasure)
          
        }
        
        if (self$options$pse&& self$results$pse$isNotFilled()){
          
          step <- self$options$step
          
          type <- self$options$type
          
          res <-
            mixRasch::mixRasch(
              data = data,
              steps = step,
              model = type,
              n.c = 1
            )
        
          pse <- res$person.par$SE.theta
          
          self$results$pse$setRowNums(rownames(data))
          self$results$pse$setValues(pse)
          
        }
       
        
        if (self$options$pinfit&& self$results$pinfit$isNotFilled()){
          
          step <- self$options$step
          
          type <- self$options$type
          
          res <-
            mixRasch::mixRasch(
              data = data,
              steps = step,
              model = type,
              n.c = 1
            )
         
          pinfit <- res$person.par$infit
          
          self$results$pinfit$setRowNums(rownames(data))
          self$results$pinfit$setValues(pinfit)
          
        }
        
        
        if (self$options$poutfit&& self$results$poutfit$isNotFilled()){
          
          step <- self$options$step
          
          type <- self$options$type
          
          res <-
            mixRasch::mixRasch(
              data = data,
              steps = step,
              model = type,
              n.c = 1
            )
         
          poutfit <- res$person.par$outfit
          
          self$results$poutfit$setRowNums(rownames(data))
          self$results$poutfit$setValues(poutfit)
          
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
        
       # vars <- length(self$options$vars)
        
        # width <- 300 + vars * 30
        # 
        # image$setSize(width, 500)
        
        state <- list(pmeasure, imeasure)
        
        image$setState(state)
        
       
      },
      
      # wright map plot--------------
      
      .plot = function(image,...) {
        
        wrightmap <- self$options$wrightmap
        
        if (!wrightmap)
          return()
        
        pmeasure <- image$state[[1]]
        imeasure <- image$state[[2]]
        

        plot<- ShinyItemAnalysis::ggWrightMap(pmeasure, imeasure,
                                              color = "deepskyblue")
        
        print(plot)
        TRUE
        
      },
      
      .prepareInfitPlot=function(data){
        
        
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
        
        infit <- res$item.par$in.out[,1]
        
        item <- self$options$vars
        nitems <- length(item)
        
        
        infit <- NA
        
        for(i in 1:nitems){
          
          infit[i] <- res$item.par$in.out[,1][i]
          
        }
        
        infit1<- data.frame(item,infit)
        
        #self$results$text$setContent(infit1)
        
        
        image <- self$results$inplot
        image$setState(infit1)
        
        
      }, 
      
      .inPlot = function(image, ggtheme, theme,...) {
        
        
        inplot <- self$options$inplot
        
        if (!inplot)
          return()
        
        infit1 <- image$state
        
        
        plot <- ggplot(infit1, aes(x = item, y=infit)) + 
          geom_point(shape = 21, color = 'skyblue', 
                     fill = 'white', size = 3, stroke = 2) +
          geom_hline(yintercept = 1.5) +
          geom_hline(yintercept = 0.5) +
          ggtitle("Item Infit")
        
        plot <- plot+ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = self$options$angle, hjust = 1
            )
          )
        }
        
        
        print(plot)
        TRUE
        
      }, 
      
      .prepareOutfitPlot=function(data){
        
        
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
        
        outfit <- res$item.par$in.out[,3]
        
        item <- self$options$vars
        nitems <- length(item)
        
        
        outfit <- NA
        
        for(i in 1:nitems){
          
          outfit[i] <- res$item.par$in.out[,3][i]
          
        }
        
        outfit1<- data.frame(item,outfit)
        
        #self$results$text$setContent(infit1)
        
        
        image <- self$results$outplot
        image$setState(outfit1)
        
        
      },     
      
  
      .outPlot = function(image, ggtheme, theme,...) {
        
        outplot <- self$options$outplot
        
        if (!outplot)
          return()
        
        outfit1 <- image$state
        
        
        plot <- ggplot(outfit1, aes(x = item, y=outfit)) + 
          geom_point(shape = 21, color = 'skyblue', 
                     fill = 'white', size = 3, stroke = 2) +
          geom_hline(yintercept = 1.5) +
          geom_hline(yintercept = 0.5) +
          ggtitle("Item Outfit")
        
        plot <- plot+ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = self$options$angle, hjust = 1
            )
          )
        }
        
        
        
        print(plot)
        TRUE
        
      },    
      
      .prepareIccPlot=function(data){
        
        num <- self$options$num
      
     #   step <- self$options$step
        
        if (self$options$step!=1)
          return()
          
        erm.res <- eRm::RM(data)
      
        image <- self$results$plot1
        image$setState(erm.res)
        
      
      },
      
      .plot1 = function(image,...) {
        
        num <- self$options$num
        
        if (self$options$step!=1)
          return()
        
        # plot1 <- self$options$plot1
        # 
        # if (!plot1)
        #   return()
      
        erm.res <- image$state
        
        plot1 <- eRm::plotICC(erm.res, 
                              item.subset= num,
                              empICC=list("raw",type="b",col="blue",lty="dotted"),
                              empCI=list())
        
        
        print(plot1)
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
