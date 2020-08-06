
# This file is a generated template, your changes will not be overwritten

#' Mixture Rasch Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import mixRasch
#' @importFrom mixRasch mixRasch
#' @importFrom mixRasch personItemPlot
#' @import boot
#' @importFrom boot boot
#' @importFrom boot boot.ci
#' @export


raschClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "raschClass",
    inherit = raschBase,
    private = list(
        
        .init = function() {
            
            
            if(is.null(self$data) | is.null(self$options$vars)){
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
            
            <p>- Identify </b> the number of 'Step' and model 'Type'</b> in the 'Analysis option'.</p>
            
            <P>- Step is defined as number of category-1. </p>
            
            <p>- Rasch model is estimated by Jonint Maximum Liklihood(JML).</p> 
            
            <p>- Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/RMM/'  target = '_blank'>GitHub</a></p>

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
                
                
                # populate Boot Fit table-------------
                
                  private$.populateBootTable(results)
                
                # populate Person statistics table
                
                private$.populatePersonTable(results)
                
                # prepare wrightmap plot-----
                
                private$.prepareWrightmapPlot(data)
                
               
                
            }
        },
        
        .compute = function(data) {
            
            
          # get variables--------
          
            data <- self$data
            
            vars <- self$options$vars
            
            step <- self$options$step
            
            type <- self$options$type
            
            
            # compute results------
            
            res <- mixRasch::mixRasch(data=data, steps=step, model=type, n.c=1)
            
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
            
            pbis <-res$item.par$itemDescriptives 
            
            
            # get number of class
            
            res0<- mixRasch::getEstDetails(res)
            class<- res0$nC
            
        
            # person statistics---------
            
            total <- res$person.par$r
            
            pmeasure <- res$person.par$theta 
            
            pse <- res$person.par$SE.theta 
            
            pinfit <- res$person.par$infit
            
            poutfit <- res$person.par$outfit
            
    
    
            
            ## Computing Bootstrap item fit ### 
        
        ### Computing boot infit-------------

        boot.infit<- function(data,indices){

                d = data[indices,]

           # estimate Rasch model
           res1 <- mixRasch::mixRasch(data=d, steps=step, model=type, n.c=1)


           # item infit
           infit<- res1$item.par$in.out[,1]

           return(infit)

           }

         boot.in<- boot::boot(data = data,statistic = boot.infit, R=10)

         binfit<- boot::boot.ci(boot.in, type="perc")

         #get boot infit
         
         binfit<- binfit$percent


         ### computing boot outfit------

         boot.outfit<- function(data,indices){

           d = data[indices,]

           # estimate Rasch model
           res1 <- mixRasch::mixRasch(data=d, steps=step, model=type, n.c=1)


           # item outfit

           outfit<- res1$item.par$in.out[,3]

           return(outfit)
         }

          boot.out<- boot::boot(data = data,statistic = boot.outfit, R=10)

          boutfit<- boot::boot.ci(boot.out, type="perc")

          # get boot outfit
          
           boutfit<- boutfit$percent



        results <-
            list(
                'aic' = aic,
                'bic' = bic,
                'caic' = caic,
                'imean'= imean,
                'imeasure' = imeasure,
                'ise' = ise,
                'infit' = infit,
                'outfit'= outfit,
                'pbis'= pbis,
                'class'=class,
                'binfit'=binfit,
                'boutfit'=boutfit,
                'total'=total,
                'pmeasure' = pmeasure,
                'pse' = pse,
                'pinfit' = pinfit,
                'poutfit'= poutfit
               
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
               
               table$setRow(rowNo=1, values=row)
               
          
           
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
               
               
               row[["imean"]] <- imean[i,1]
               
               row[["imeasure"]] <- imeasure[i]
               
               row[["ise"]] <- ise[i]
               
               row[["infit"]] <- infit[i,1]
               
               row[["outfit"]] <- outfit[i,3]
               
               row[["pbis"]] <- pbis[i,2]
               
               table$setRow(rowKey = vars[i], values = row)
           }
           
       },
       
       
       # populate Boot table--------
       
       .populateBootTable = function(results) {


           table <- self$results$item$bfit


           values <- list()

           binfit <- results$binfit

           boutfit <- results$boutfit


           values[['l[infit]']] <- binfit[4]
           values[['u[infit]']] <- binfit[5]

           values[['l[outfit]']] <- boutfit[4]
           values[['u[outfit]']] <- boutfit[5]


           table$setRow(rowNo=1, values)


       },

       
       # populate Person statistics table
       
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
               
               table$addRow(rowKey =i, values = row)
              
           }
           
       },
       
  
       ### wrightmap Plot functions ----
       
       
       .prepareWrightmapPlot = function(data) {
         
         # get variables--------
         
         data <- self$data
         
        step <- self$options$step
         
         type <- self$options$type
         
         
         #compute wright---
         
         wright = mixRasch::mixRasch(data=data, steps=step, model=type, n.c=1)
           
           
       # Prepare Data For wrightmap Plot -------
           
           image <- self$results$wrightmap
           image$setState(wright)
           
       },
       
       
       # wright map plot--------------
       
       .wrightmapPlot = function(image, ...) {
       
          wrightmap <- self$options$wrightmap
           
           if (!wrightmap)
               return()
           
           
           wright <- image$state
           
   plot <- mixRasch::personItemPlot(wright)
                                   
               
         print(plot)
           TRUE
           
       },
       

#### Helper functions =================================
        
        .cleanData = function() {
            items <- self$options$vars
            
            data <- list()
            
            for (item in items)
                data[[item]] <- jmvcore::toNumeric(self$data[[item]])
            
            attr(data, 'row.names') <- seq_len(length(data[[1]]))
            attr(data, 'class') <- 'data.frame'
            data <- jmvcore::naOmit(data)
            
            return(data)
        }          
            

           
        )
)
