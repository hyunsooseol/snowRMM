
# This file is a generated template, your changes will not be overwritten

#' Mixture Rasch Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom mixRasch mixRasch
#' @importFrom mixRasch getEstDetails
#' @importFrom ShinyItemAnalysis ggWrightMap
#' @importFrom eRm plotICC
#' @importFrom eRm RM
#' @importFrom eRm RSM
#' @importFrom eRm thresholds
#' @importFrom eRm PCM
#' @importFrom eRm plotPImap
#' @importFrom eRm LRtest
#' @importFrom  eRm Waldtest
#' @importFrom  eRm MLoef
#' @importFrom  eRm plotGOF
#' @importFrom  eRm person.parameter
#' @importFrom  eRm SepRel
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
            <p>- Specify </b> the number of 'Step' and model 'Type'</b> in the 'Analysis option'.</p>
            <p>- Step is defined as number of <b>category-1</b>. </p>
            <p>- The minimum and maximum values of a category must be the same across all items for <b>rating sclaes</b> with eRm R package.</p>
            <p>- <b>Person Analysis</b> will be displayed in the datasheet.</p>
            <p>- The <b>eRm</b> R package was used for the person-item map for PCM.</p>
            <p>- Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowRMM/issues'  target = '_blank'>GitHub</a>.</p>
            <p>_____________________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
        )
        
        # if (self$options$rsm)
        #   self$results$rsm$setNote(
        #     "Note",
        #     "The <b>eRm</b> R package was used for calculating thresholds."
        #     
        #   )
        # 
        # if (self$options$pcm)
        #   self$results$pcm$setNote(
        #     "Note",
        #     "The <b>eRm</b> R package was used for calculating thresholds."
        #     
        #   )
        
        if (self$options$ml1)
          self$results$ml1$setNote(
            "Note",
            "Number of categories should be the same for each item with eRm R package."
            
          )
        
        if (self$options$rel)
          self$results$rel$setNote(
            "Note",
            "SSD=Squared Standard Deviation; MSE=Mean Squared Error."
           
          )
        
        
        
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
        
        
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
        
        if (is.null(self$options$vars) ||
            length(self$options$vars) < 2)
          
          ready <- FALSE
        
        if (ready) {
          data <- private$.cleanData()
          
          results <- private$.compute(data)
          
          
          #  populate Model information table-----
          
          private$.populateModelTable(results)
         
          # populate Item Statistics table-----
          
          private$.populateItemTable(results)
          
         
          # populate reliability table----------
          
          private$.populateRelTable(results)
          
          # populate thresholds table-----
          
          private$.populateThrTable(results)
          
         
          # prepare wrightmap plot-----
          
          private$.prepareWrightmapPlot(data)
          
          # prepare person-item map
          private$.preparepiPlot(data)
          
          # prepare item fit plot-------
          
          private$.prepareInfitPlot(data)
          
          private$.prepareOutfitPlot(data)
          
          # ICC PLOT-------------------
          
          private$.prepareIccPlot(data)
          
          # private$.prepareRsmPlot(data)
          
          private$.preparePcmPlot(data)
          
          # populate Person analysis table-------
          private$.populatePtotalOutputs(results)
          private$.populatePmeasureOutputs(results)
          private$.populatePseOutputs(results)
          private$.populatePinfitOutputs(results)
          private$.populatePoutfitOutputs(results)
          
          
          
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
        
       # Person analysis-----------
        
        ptotal <- res$person.par$r
        pmeasure <- res$person.par$theta
        pse <- res$person.par$SE.theta
        pinfit <- res$person.par$infit
        poutfit <- res$person.par$outfit
        
        
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
        
        # person separation reliability using eRm R package---------
        
        if(self$options$step ==1){
        
        pers <- eRm::person.parameter(eRm::RM(data))
        rel <- eRm::SepRel(pers)
        } else if(self$options$step >1){
          pers <- eRm::person.parameter(eRm::PCM(data))
          rel <- eRm::SepRel(pers)
          
        }
        
        ssd <- rel$SSD.PS
        mse<-rel$MSE
        rel<- rel$sep.rel
       
        # thresholds(tau parameter)---------
        
        tau<- res$item.par$tau
        tau <- t(tau)
        tau<- as.data.frame(tau)
        
        ########################################################
        if(self$options$step ==1){
          
          rasch <- eRm::RM(data)
          
          
          lrsplit<- self$options$lrsplit
          
          # LR test----------
          lr <- eRm::LRtest(rasch, splitcr = lrsplit)
          #-------------------
          
          value<- lr$LR
          df<- lr$df
          p<- lr$pvalue
          
          table <- self$results$lr
          
          
          row <- list()
          
          row[['value']] <- value
          row[['df']] <- df
          row[['p']] <- p
          
          table$setRow(rowNo = 1, values = row)
          
          #Goodness-of-fit plot for LR test--------
          
          image <- self$results$gofplot
         
          image$setState(lr)
          
         
          ##############################################
          
          mlsplit<- self$options$mlsplit
          
          # Martin-lof test--------------
          
          ml<- eRm::MLoef(rasch,splitcr = mlsplit)
          #####################
          
          value<- ml$LR
          df<- ml$df
          p<- ml$p.value
          
          table <- self$results$ml
          
          
          row <- list()
          
          row[['value']] <- value
          row[['df']] <- df
          row[['p']] <- p
          
          table$setRow(rowNo = 1, values = row)
          
          
          # Wald test----------
          vars <- self$options$vars
          
          table <- self$results$wald
          
          waldsplit<- self$options$waldsplit
          #######################
          
          w<- eRm::Waldtest(rasch,splitcr = waldsplit)
          
          ###################
          w<- w$coef.table
          
          w <- as.data.frame(w)
          
          
          # Wald test table----------------------
          
          
          for (i in seq_along(vars)) {
            row <- list()
            
            
            row[["item"]] <- w[[1]][i]
            row[["p"]] <- w[[2]][i]
            
            table$addRow(rowKey = vars[i], values = row)
          }
          
          
        }
        
      
        if(self$options$rsm==TRUE || self$options$mlsplit1==TRUE || self$options$plot2==TRUE){
     
       ########################################
        
       rsm.res <- eRm::RSM(data)
       #######################################
        
        tab<- thresholds(rsm.res)
        tab<- tab$threshtable
        rsm<- data.frame(Reduce(rbind, tab))
        rsm<- rsm[,-1]
       
       
        # number of category---------
        
        nc <- ncol(rsm)
      
        
        table <- self$results$rsm

          nCategory <- nc

          vars <- self$options$vars


          if (nCategory > 1) {
            for (i in 1:nCategory)

              table$addColumn(
                name = paste0("name", i),
                title = as.character(i),
                superTitle = 'Thresholds',
                type = 'number'
              )
          }


          for (i in seq_along(vars)) {

            row <- list()


            for (j in 1:nCategory) {

              row[[paste0("name", j)]] <- rsm[i, j]


            }



            table$setRow(rowNo = i, values = row)
          }

          #RSM plot----------
          
          image <- self$results$plot2
          image$setState(rsm.res)
          
          ##################################################
          # Testing the Rasch model with Rating scale---
          
          mlsplit1<- self$options$mlsplit1
          
          #Martin-lof test--------------
          
          ml1<- eRm::MLoef(rsm.res,splitcr = mlsplit1)
          #####################
          
          value<- ml1$LR
          df<- ml1$df
          p<- ml1$p.value
          
          table <- self$results$ml1
          
          
          row <- list()
          
          row[['value']] <- value
          row[['df']] <- df
          row[['p']] <- p
          
          table$setRow(rowNo = 1, values = row)
          
        }
      
        if(self$options$pcm==TRUE){    
       
        #######################################
        pcm.res <- eRm::PCM(data)
        #####################################
        
        tab1<- thresholds(pcm.res)
        tab1<- tab1$threshtable
        pcm<- data.frame(Reduce(rbind, tab1))
        pcm<- pcm[,-1]
        
        nc <- ncol(pcm)
       
        nCategory <- nc
        
        table <- self$results$pcm


          nCategory <- nc

          vars <- self$options$vars


          if (nCategory > 1) {
            for (i in 1:nCategory)

              table$addColumn(
                name = paste0("name", i),
                title = as.character(i),
                superTitle = 'Thresholds',
                type = 'number'
              )
          }


          for (i in seq_along(vars)) {

            row <- list()


            for (j in 1:nCategory) {

              row[[paste0("name", j)]] <- pcm[i, j]


            }


            table$setRow(rowNo = i, values = row)
          }

          
          }
        
        ############################################
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
            'class' = class,
            'ssd' =ssd,
            'mse'=mse,
            'rel'=rel,
            'tau'=tau,
            'ptotal'=ptotal,
            'pmeasure'=pmeasure,
            'pse'=pse,
            'pinfit'=pinfit,
            'poutfit'=poutfit
          )
        
        
      },
      
      # populate thresholds table(tau parameter)---------
      
      .populateThrTable = function(results) {
        
        table <- self$results$thr
        
        tau <- results$tau
      
        #-------------
        nc <- ncol(tau)
       
        nCategory <- nc
        
        vars <- self$options$vars
        
        
        if (nCategory > 1) {
          for (i in 1:nCategory)
            
            table$addColumn(
              name = paste0("name", i),
              title = as.character(i),
              superTitle = 'Thresholds',
              type = 'number'
            )
        }
        
        
        for (i in seq_along(vars)) {
          
          row <- list()
          
          
          for (j in 1:nCategory) {
            
            row[[paste0("name", j)]] <- tau[i, j]
            
            
          }
          
          
          
          table$setRow(rowNo = i, values = row)
        
          }
        
      },
      
      
      ####################################
      .gofplot = function(image, ...) {
        
      
        lr <- image$state
        
        tlab <- self$options$tlab
        
        # additional 95 percent control line with user specified style
        gofplot<- plotGOF(lr, tlab=tlab, 
                          ctrline = list(gamma = 0.95, col = "red", lty = "dashed"))
        
        
        print(gofplot)
        TRUE
      },
      
      # populate reliability table---------
      
      .populateRelTable = function(results) {
        
        table <- self$results$rel
        
        
        #results---------
        
        ssd <- results$ssd
        mse <- results$mse
        rel <- results$rel
       
        row <- list()

        row[['SSD']] <- ssd
        row[['MSE']] <- mse
        row[['Reliability']] <- rel

        table$setRow(rowNo = 1, values = row)

        
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
      
      
      ##### person statistics -------------------
      
      
      .populatePtotalOutputs= function(results) {

        ptotal <- results$ptotal
        
        
        if (self$options$ptotal&& self$results$ptotal$isNotFilled()){

         
          self$results$ptotal$setRowNums(rownames(data))
          self$results$ptotal$setValues(ptotal)

        }
      },
      
        
        .populatePmeasureOutputs= function(results) {
          
          pmeasure <- results$pmeasure
          
          
          if (self$options$pmeasure&& self$results$pmeasure$isNotFilled()){
            
            
            self$results$pmeasure$setRowNums(rownames(data))
            self$results$pmeasure$setValues(pmeasure)
            
          }

        },
        
      .populatePseOutputs= function(results) {
        
        pse<- results$pse
        
        
        if (self$options$pse&& self$results$pse$isNotFilled()){
          
          
          self$results$pse$setRowNums(rownames(data))
          self$results$pse$setValues(pse)
          
        }
        
      },
        
      .populatePinfitOutputs= function(results) {
        
        pinfit<- results$pinfit
        
        
        if (self$options$pinfit&& self$results$pinfit$isNotFilled()){
          
          
          self$results$pinfit$setRowNums(rownames(data))
          self$results$pinfit$setValues(pinfit)
          
        }
        
      },
      
      .populatePoutfitOutputs= function(results) {
        
        poutfit<- results$poutfit
        
        
        if (self$options$poutfit&& self$results$poutfit$isNotFilled()){
          
          
          self$results$poutfit$setRowNums(rownames(data))
          self$results$poutfit$setValues(poutfit)
          
        }
        
      },
      
     
      ### wrightmap Plot functions -----------
      
      
      .prepareWrightmapPlot = function(data) {
        
       
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
        vars <- self$options$vars
        
        # plot---------
        
        image <- self$results$plot
        
       # vars <- length(self$options$vars)
        
        # width <- 300 + vars * 30
        # 
        # image$setSize(width, 500)
        
        state <- list(pmeasure, imeasure, vars)
        
        image$setState(state)
        
       
      },
      
      # wright map plot--------------
      
      .plot = function(image,...) {
        
        wrightmap <- self$options$wrightmap
        
        if (!wrightmap)
          return()
        
        pmeasure <- image$state[[1]]
        imeasure <- image$state[[2]]
        vars <- image$state[[3]]

        plot<- ShinyItemAnalysis::ggWrightMap(pmeasure, imeasure,
                                              item.names = vars,
                                              color = "deepskyblue")
        
        print(plot)
        TRUE
        
      },
      
     # PREPARE PERSON-ITEM PLOT FOR PCM-------------
     
     .preparepiPlot = function(data) {
       
       autopcm <- eRm::PCM(data)
       
       
       # Prepare Data For Plot -------
       
       image <- self$results$piplot
       image$setState(autopcm)
       
     },
     
     .piPlot= function(image, ...) {
       
       autopcm <- image$state
       
       if (is.null(autopcm))
         return()
       
       
       plot <- eRm::plotPImap(autopcm, sorted=TRUE,
                              warn.ord.colour = "red")
       
       print(plot)
       
       TRUE
       
     },
     
     
     ### fit plot----------------
     
     
     
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
      
         if (self$options$step>=2)
          return()
          
        erm.res <- eRm::RM(data)
      
        image <- self$results$plot1
        image$setState(erm.res)
        
      
      },
      
      .plot1 = function(image,...) {
        
        num <- self$options$num
        
        if (self$options$step>=2)
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
    
     
     .plot2 = function(image,...) {
       
       num <- self$options$num
       
       if (self$options$step<=1)
         return()
       
      
       rsm.res <- image$state
       
       plot2 <- eRm::plotICC(rsm.res,
                             legpos="top",
                             item.subset= num)
                            
       
       
       print(plot2)
       TRUE
       
       
     },
     
     .preparePcmPlot=function(data){
       
       num <- self$options$num
       
       #   step <- self$options$step
       
       if (self$options$step<=1)
         return()
       
       pcm.res <- eRm::PCM(data)
       
       #  rsm.res<- eRm::thresholds(rsm.res)
       
       image <- self$results$plot3
       image$setState(pcm.res)
       
       
     },
     
     .plot3 = function(image,...) {
       
       num <- self$options$num
       
       if (self$options$step<=1)
         return()
       
       
       pcm.res <- image$state
       
       plot3 <- eRm::plotICC(pcm.res, 
                             legpos="top",
                             item.subset= num)
       
       
       
       print(plot3)
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
