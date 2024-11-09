
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
#' @importFrom  pairwise pair
#' @importFrom  pairwise pers
#' @importFrom  pairwise rfa
#' @importFrom pairwise q3
#' @importFrom eRm NPtest
#' @import RColorBrewer
#' @import ggplot2
#' @export


raschClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "raschClass",
    inherit = raschBase,
    private = list(
      .htmlwidget = NULL, 
      
      .init = function() {
        
        private$.htmlwidget <- HTMLWidget$new() 
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        # self$results$instructions$setContent(
        #   "<html>
        #     <head>
        #     </head>
        #     <body>
        #     <div class='instructions'>
        #     
        #     <p>_____________________________________________________________________________________________</p>
        #     <p>1. The <b>mixRasch</b> R package was used for the Rasch model using joint maximum likelihood estimation(JMLE).</P>
        #     <p>2. Specify </b> the number of <b>Step</b> and model <b>Type</b> in the analysis option.</p>
        #     <p>3. Step is defined as number of <b>category-1</b>. </p>
        #     <p>4. The minimum and maximum values of a category must be the same across all items for <b>rating sclaes</b> with <b>eRm</b> R package.</p>
        #     <p>5. <b>Person Analysis</b> will be displayed in the datasheet.</p>
        #     <p>6. The <b>eRm</b> R package was used for the person-item map for PCM.</p>
        #     <p>7. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowRMM/issues'  target = '_blank'>GitHub</a>.</p>
        #     <p>_____________________________________________________________________________________________</p>
        #     
        #     </div>
        #     </body>
        #     </html>"
        # )
        
        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title="Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              '<ul>',
              '<li>The <b>mixRasch</b> R package was used for the Rasch model using joint maximum likelihood estimation(JMLE).</li>',
              '<li>Specify </b> the number of <b>Step</b> and model <b>Type</b> in the analysis option.</li>',
              '<li>Step is defined as number of <b>category-1</b>.</li>',
              '<li>The minimum and maximum values of a category must be the same across all items for <b>rating sclaes</b> with <b>eRm</b> R package.</li>',
              '<li><b>Person Analysis</b> will be displayed in the datasheet.</li>',
              '<li>The <b>eRm</b> R package was used for the person-item map for PCM.</li>',
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div></div>'
              
            )
            
          )
        )              
        
        
        if (self$options$ml1)
          self$results$tm$ml1$setNote(
            "Note",
            "Number of categories should be the same for each item with eRm R package."
            
          )
        
        if (self$options$rel)
          self$results$mf$rel$setNote(
            "Note",
            "SSD=Squared Standard Deviation; MSE=Mean Squared Error."
           
          )
        
        if(isTRUE(self$options$inplot)){
          
          width <- self$options$width
          height <- self$options$height
          
          self$results$inplot$setSize(width, height)
        }
        
        
        if(isTRUE(self$options$outplot)){
          
          width <- self$options$width
          height <- self$options$height
          
          self$results$outplot$setSize(width, height)
        }
        
        if(isTRUE(self$options$plot4)){
          
          width <- self$options$width1
          height <- self$options$height1
          
          self$results$plot4$setSize(width, height)
        }
        
        if(isTRUE(self$options$plot5)){
          
          width <- self$options$width2
          height <- self$options$height2
          
          self$results$plot5$setSize(width, height)
        }
        
        
        if(isTRUE(self$options$wrightmap)){
          
          width <- self$options$width3
          height <- self$options$height3
          
          self$results$plot$setSize(width, height)
        }
        
        if(isTRUE(self$options$piplot)){
          
          width <- self$options$width4
          height <- self$options$height4
          
          self$results$piplot$setSize(width, height)
        }
        
        if(isTRUE(self$options$plot2)){
          
          width <- self$options$width5
          height <- self$options$height5
          
          self$results$plot2$setSize(width, height)
        }
        
        if(isTRUE(self$options$plot3)){
          
          width <- self$options$width5
          height <- self$options$height5
          
          self$results$plot3$setSize(width, height)
        }
        
        if(isTRUE(self$options$plot1)){
          
          width <- self$options$width6
          height <- self$options$height6
          
          self$results$plot1$setSize(width, height)
        }
         
        if(isTRUE(self$options$gofplot)){
          
          width <- self$options$width7
          height <- self$options$height7
          
          self$results$gofplot$setSize(width, height)
        }
        
        if(isTRUE(self$options$plot8)){
          
          width <- self$options$width8
          height <- self$options$height8
          
          self$results$plot8$setSize(width, height)
        }
        
        if(isTRUE(self$options$plot9)){
          
          width <- self$options$width9
          height <- self$options$height9
          
          self$results$plot9$setSize(width, height)
        }
        
        
        # if (length(self$options$vars) <= 1)
        #   self$setStatus('complete')
        
        
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

        if(isTRUE(self$options$plot9)){
          private$.prepareciPlot(data)
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
         
        }
      },
      
      .compute = function(data) {
        
        # get variables--------
        
        
        vars <- self$options$vars
        step <- self$options$step
        type <- self$options$type
     
        # Cross-plot with 95% CIs---
        
        # if(isTRUE(self$options$plot9)){
        #   
        #   mea1 <- self$options$mea1
        #   mea2 <- self$options$mea2
        #   
        #   dat <- data.frame(a = mea1, b = mea2)
        #   
        #   # difference from diag.
        #   dat$diff <- dat$b - dat$a
        #   
        #   # mean and 95%
        #   mean_diff <- mean(dat$diff)
        #   sem_diff <- sd(dat$diff) / sqrt(length(dat$diff))
        #   ci_diff <- qt(0.975, df = length(dat$diff) - 1) * sem_diff
        #   
        #   # upper and lower bound
        #   dat$upper_bound <- dat$a + (mean_diff + ci_diff)
        #   dat$lower_bound <- dat$a + (mean_diff - ci_diff)
        #   
        #   image <- self$results$plot9
        #   
        #   state <- list(dat$a, dat$b, dat$upper_bound, dat$lower_bound)
        #   image$setState(state)
        #   
        # }
        # 
     # compute results------
      set.seed(1234)
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
        
        if (self$options$ptotal==TRUE){
         
          self$results$ptotal$setRowNums(rownames(data))
          self$results$ptotal$setValues(ptotal)
          
        }
        
        if (self$options$pmeasure==TRUE){
          
          self$results$pmeasure$setRowNums(rownames(data))
          self$results$pmeasure$setValues(pmeasure)
          
        }
        
        if (self$options$pse==TRUE){
          
          self$results$pse$setRowNums(rownames(data))
          self$results$pse$setValues(pse)
          
        }
        
        if (self$options$pinfit==TRUE){
          
          self$results$pinfit$setRowNums(rownames(data))
          self$results$pinfit$setValues(pinfit)
          
        }
        
        if (self$options$poutfit==TRUE){
          
          self$results$poutfit$setRowNums(rownames(data))
          self$results$poutfit$setValues(poutfit)
          
        }
        
        # Person fit plot3----------------------
        
        Measure <- pmeasure
        Infit <- pinfit
        Outfit <- poutfit
        
        daf <- data.frame(Measure,Infit,Outfit)
        
        pf<- reshape2::melt(daf,
                            id.vars='Measure',
                            variable.name="Fit",
                            value.name='Value')
        
        image <- self$results$plot4
        
        image$setState(pf)
        
        
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
        
########## eRm R package######################################

        if(self$options$step ==1){
          
          rasch<- eRm::RM(data) 
          
        } else if(self$options$step >1){
          
          pcm.res<- eRm::PCM(data)
          rsm.res <- eRm::RSM(data)
         
        }
        
# person separation reliability using eRm R package---------
        
        if(self$options$step ==1){
        
          #rasch<- eRm::RM(data)
          
        pers <- eRm::person.parameter(rasch)
        rel <- eRm::SepRel(pers)
        } else if(self$options$step >1){
          
          #pcm<- eRm::PCM(data)
          
          pers <- eRm::person.parameter(pcm.res)
          rel <- eRm::SepRel(pers)
          
        }
 ###################################################################       
        ssd <- rel$SSD.PS
        mse<-rel$MSE
        rel<- rel$sep.rel
       
        # thresholds(tau parameter)---------
        
        tau<- res$item.par$tau
        tau <- t(tau)
        tau<- as.data.frame(tau)
        
        ########################################################
        if(self$options$step ==1){
          
          #rasch <- eRm::RM(data)
          lrsplit<- self$options$lrsplit
          
          # LR test----------
          lr <- eRm::LRtest(rasch, splitcr = lrsplit)
          #-------------------
          
          value<- lr$LR
          df<- lr$df
          p<- lr$pvalue
          
          table <- self$results$tm$lr
          
          
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
          
          table <- self$results$tm$ml
          
          
          row <- list()
          
          row[['value']] <- value
          row[['df']] <- df
          row[['p']] <- p
          
          table$setRow(rowNo = 1, values = row)
          
          
          # Wald test----------
          vars <- self$options$vars
          
          table <- self$results$tm$wald
          
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
        
        if(self$options$step >1){
       # if(self$options$rsm==TRUE || self$options$mlsplit1==TRUE || self$options$plot2==TRUE){
     
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
         
          # Testing the Rasch model with Rating scale---
          
          mlsplit1<- self$options$mlsplit1
          
          #Martin-lof test--------------
          
          ml1<- eRm::MLoef(rsm.res,splitcr = mlsplit1)
          
          value<- ml1$LR
          df<- ml1$df
          p<- ml1$p.value
          
          table <- self$results$tm$ml1
          
          row <- list()
          
          row[['value']] <- value
          row[['df']] <- df
          row[['p']] <- p
          
          table$setRow(rowNo = 1, values = row)
          
        }
      
        if(self$options$pcm==TRUE){    
     
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
        
        # Rasch residual factor analysis using pairwise R package
        
        # pers_obj <- pers(pair(bfiN))
        # result <- rfa(pers_obj)
        # summary(result)
        # plot(result)
        
        if(isTRUE(self$options$plot5)){
        
        res <- self$options$res
          
        ip <- pairwise::pair(data)
        pers_obj <- pairwise::pers(ip)
        rf<- pairwise::rfa(pers_obj, res=res)
        summ<- summary(rf)
        
        self$results$text$setContent(summ)
        
        image5 <- self$results$plot5
        image5$setState(rf)
        
        }
        
        # Q3 fit statistics proposed by Yen(1984)

        if(self$options$q3==TRUE){
          
          res1 <- self$options$res1
          
          ip <- pairwise::pair(data)
          pers_obj <- pairwise::pers(ip)
         
          q<- pairwise::q3(pers_obj, res=res1)
       
          # Standardized correlation matrix
          ma<- q$resid_cor$cor 
          #self$results$text2$setContent(ma)
          
          if(isTRUE(self$options$cormatrix)){  
         
          table <- self$results$cormatrix
         
         ma <- as.data.frame(ma)
         names <- dimnames(ma)[[1]] 
         dims <- dimnames(ma)[[2]]
         
         # creating table----------------
         
         for (dim in dims) {
            table$addColumn(name = paste0(dim),
                           type = 'number')
         }
         
         for (name in names) {
            row <- list()
            for(j in seq_along(dims)){
             row[[dims[j]]] <- ma[name,j]
             }
           table$addRow(rowKey=name, values=row)
                    }
         
         }
         
          #------------------------------ 
          table <- self$results$q3
          
          if(is.null(self$options$q3))
            return()
          
          Mean<- q[["statistic"]]$Q3[1]
          Max<- q[["statistic"]]$Q3[2]
          Min<- q[["statistic"]]$Q3[3]
          Max_abs<- q[["statistic"]]$Q3[4]
          Min_abs<- q[["statistic"]]$Q3[5]
          Q3<- q[["statistic"]]$Q3[6]
          
          row <- list()
          
          row[['Mean']] <- Mean
          row[['Max']] <- Max
          row[['Min']] <- Min
          row[['Max_abs']] <- Max_abs
          row[['Min_abs']] <- Min_abs
          row[['Q3']] <- Q3
          
          table$setRow(rowNo = 1, values = row)
          
           
        }
      
        if(isTRUE(self$options$plot8)){
          
          # Rasch model, estimation of item and person parameters
          # Using eRm package
          # res <- RM(raschdat2)
          # p.res <- person.parameter(res)
          # item.fit <- eRm::itemfit(p.res)
          # std.resids <- item.fit$st.res
          
          if(self$options$step==1 && self$options$type=='RSM'){
           
            p.res <- person.parameter(rasch)
            
            item.fit <- eRm::itemfit(p.res)
            std.resids <- item.fit$st.res
          
          } 
          
          if(self$options$step>1 && self$options$type=='RSM'){
            
            #res <- eRm::RSM(data)
            p.res <- person.parameter(rsm.res)
            
            item.fit <- eRm::itemfit(p.res)
            std.resids <- item.fit$st.res
          }
          
          if(self$options$step>1 && self$options$type=='PCM'){  
            
            #res <- eRm::PCM(data)
            p.res <- person.parameter(pcm.res)
            
            item.fit <- eRm::itemfit(p.res)
            std.resids <- item.fit$st.res
            
          }
          
          image8 <- self$results$plot8
          image8$setState(std.resids)
        
        }
        
        if(isTRUE(self$options$nptest)){
          
          n <- self$options$matrix
          method <- self$options$npmethod    
          
          rmat <- as.matrix(data)
          res <- eRm::NPtest(rmat, n = n, method = method)
          
          self$results$text1$setContent(res)
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
        
        table <- self$results$mf$rel
        
        
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
        
        table <- self$results$mf$model
        
        
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
        
        table <- self$results$items
        
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
       
      # Plot of standardized residuals using eRm package------
      
      .plot8 = function(image8,...) {
        
        
        if (is.null(image8$state))
          return(FALSE)
      
        item.number <- self$options$num1
        std.resids <- image8$state
        
        
        # Before constructing the plots, find the maximum and minimum values of the standardized residuals to set limits for the axes:
        max.resid <- ceiling(max(std.resids))
        min.resid <- ceiling(min(std.resids))
        
        # The code below will produce standardized residual plots for each of the items:
       
          plot8<- plot(std.resids[, item.number], ylim = c(min.resid, max.resid),
               main = paste("Standardized Residuals for Item ", item.number, sep = ""),
               ylab = "Standardized Residual", xlab = "Person Index")
          abline(h = 0, col = "blue")
          abline(h=2, lty = 2, col = "red")
          abline(h=-2, lty = 2, col = "red")
          
          legend("topright", c("Std. Residual", "Observed = Expected", "+/- 2 SD"), pch = c(1, NA, NA), 
                 lty = c(NA, 1, 2),
                 col = c("black", "blue", "red"), cex = .8)
          
       
      print(plot8)
      TRUE 
      },
      
      
      
      ### wrightmap Plot functions -----------
      
      
      .prepareWrightmapPlot = function(data) {
        
       
        step <- self$options$step
        
        type <- self$options$type
        
        
        #compute wright---
        set.seed(1234)
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
        
        if (is.null(image$state))
          return(FALSE)
        # wrightmap <- self$options$wrightmap
        # 
        # if (!wrightmap)
        #   return()
        
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
        
        
        #compute mixRasch---
        
        set.seed(1234)
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
        
        if (is.null(image$state))
          return(FALSE)
        
        # inplot <- self$options$inplot
        # 
        # if (!inplot)
        #   return()
        
        infit1 <- image$state
        
        
        plot <- ggplot(infit1, aes(x = item, y=infit)) + 
          geom_point(shape = 4, color = 'black', 
                     fill = 'white', size = 3, stroke = 2) +
         
          geom_hline(yintercept = 1.5,linetype = "dotted", color='red', size=1.5) +
          geom_hline(yintercept = 0.5,linetype = "dotted", color='red', size=1.5) 
          #ggtitle("Item Infit")
        
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
        
        
        #compute mixRasch---
        set.seed(1234)
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
        
        # outplot <- self$options$outplot
        # 
        # if (!outplot)
        #   return()
        
        if (is.null(image$state))
          return(FALSE)
        
        outfit1 <- image$state
        
        
        plot <- ggplot(outfit1, aes(x = item, y=outfit)) + 
          geom_point(shape = 4, color = 'black', 
                     fill = 'white', size = 3, stroke = 2) +
         
          geom_hline(yintercept = 1.5,linetype = "dotted", color='red', size=1.5) +
          geom_hline(yintercept = 0.5,linetype = "dotted", color='red', size=1.5) 
          #ggtitle("Item Outfit")
        
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
     
      
     .plot4 = function(image,ggtheme, theme,...) {
       
       if (is.null(image$state))
         return(FALSE)
       
       pf <- image$state
       
       plot4<- ggplot2::ggplot(pf, aes(x = Measure, y = Value, shape = Fit))+
         geom_point(size=3, stroke=2)+
         
         ggplot2::scale_shape_manual(values=c(3, 4))+
         #ggplot2::scale_color_manual(values=c("red", "blue")+
         ggplot2::coord_cartesian(xlim=c(-4, 4), ylim=c(0,4))+
         ggplot2::geom_hline(yintercept = 1.5,linetype = "dotted", color='red', size=1.5)+ 
         ggplot2::geom_hline(yintercept = 0.5,linetype = "dotted", color='red', size=1.5)    
       
       
       plot4 <- plot4+ggtheme
       
       print(plot4)
       TRUE
     },
     
     .plot5 = function(image5,...) {
       
       if (is.null(image5$state))
         return(FALSE)
       
       Residuals <- image5$state
       
      
       plot5 <- plot(Residuals)

       print(plot5)
       TRUE
       
       
     },

.prepareciPlot = function(data) {
 
  data <- self$data
 
  D1 <- self$options$mea1
  D2 <- self$options$mea2
  SE1 <- self$options$se1
  SE2 <- self$options$se2
  
   data[[D1]] <- jmvcore::toNumeric(data[[D1]])
   data[[D2]] <- jmvcore::toNumeric(data[[D2]])
   data[[SE1]] <- jmvcore::toNumeric(data[[SE1]])
   data[[SE2]] <- jmvcore::toNumeric(data[[SE2]])

   dat <- data.frame(D1 = data[[D1]],
                     D2 = data[[D2]],
                     SE1 = data[[SE1]],
                     SE2 = data[[SE2]])

  # mean
  MEAN1 <- mean(dat$D1)
  MEAN2 <- mean(dat$D2)
 
  # Z-score for 95% confidence interval
  Z <- 1.96
  
  # SE AND Upper/Lower control line 
  SE12 <- sqrt(dat$SE1^2 + dat$SE2^2)
  
  UPPER1 <- (dat$D1 + dat$D2) / 2 + MEAN1 - Z * SE12 / 2
  UPPER2 <- (dat$D1 + dat$D2) / 2 + MEAN2 + Z * SE12 / 2
  LOWER1 <- (dat$D1 + dat$D2) / 2 + MEAN1 + Z * SE12 / 2
  LOWER2 <- (dat$D1 + dat$D2) / 2 + MEAN2 - Z * SE12 / 2
 
  dat2 <- data.frame(D1=dat$D1, D2=dat$D2,
                     UPPER1, UPPER2,
                     LOWER1, LOWER2)

  # arrange control line---
  control_upper <- dplyr::arrange(dat2,UPPER1)
  control_lower <- dplyr::arrange(dat2,LOWER1)
  
  image <- self$results$plot9
  
  state <- list(dat2, 
                control_upper,
                control_lower) 
                
  image$setState(state)
 
},

.plot9 = function(image, ggtheme, theme, ...) {
  
  if (is.null(image$state))
    return(FALSE)
  
  dat2 <- image$state[[1]]
  control_upper <- image$state[[2]]
  control_lower <- image$state[[3]]
  
  
  plot9<- ggplot() +
    geom_point(aes(x = dat2$D1, y = dat2$D2), 
               color = "blue", alpha = 0.6, size=2) +  
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +  
    geom_line(data = control_upper, 
              aes(x = dat2$UPPER1, y = dat2$UPPER2), 
              color = "red", 
              linetype = "solid") +  
    geom_line(data = control_lower, 
              aes(x = dat2$LOWER1, y = dat2$LOWER2), 
              color = "red", linetype = "solid") +  
    xlab("Measure 1") +
    ylab("Measure 2") 
   
  plot9 <- plot9 + ggtheme
  
  print(plot9)
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
