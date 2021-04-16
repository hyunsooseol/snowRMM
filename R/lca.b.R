
# This file is a generated template, your changes will not be overwritten
#' Mixture Rasch Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import poLCA
#' @importFrom poLCA poLCA
#' @import MASS
#' @export


lcaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lcaClass",
    inherit = lcaBase,
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
           
            <p><b>To get started:</b></p>

            <p> jamovi treats all variables as qualitative/categorical/nominal.</p>

            <p> Variables must contain only integer values, and must be coded with consecutive values from 1 to the maximum number. </p>

            <p> The results of <b> Class membership </b> will be displayed in the datasheet.</p>
            
            <p> Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowRMM/'  target = '_blank'>GitHub.</a></p>

            </div>
            </body>
            </html>"
            )
            
            if (self$options$fit)
                self$results$fit$setNote(
                    "Note",
                    "Gsq:Likelihood ratio/deviance statistic.
                    Chisq: Pearson Chi-square goodness of fit statistic."
                )
            
            if (length(self$options$vars) <= 1)
                self$setStatus('complete')
            
            
            
        },
        
               .run = function() {

           
                   ready <- TRUE
                   
                   if (is.null(self$options$vars) ||
                       length(self$options$vars) < 2)
                       
                       ready <- FALSE
                   
                   if (ready) {
                       data <- private$.cleanData()
                       
                       results <- private$.compute(data)               
                   
                       # Populate Model table-----
                       
                       private$.populateFitTable(results)
                   
                       # populate output variables-----
                       
                       private$.populateOutputs(data)
                       
                   }
               },
                   
                   
                   
                   .compute = function(data) {
                       
                      nc<- self$options$nc
                       
                      data<- as.data.frame(data)
                      
                      vars <- colnames(data)
                      vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
                      vars <- paste0(vars, collapse=',')
                      formula <- as.formula(paste0('cbind(', vars, ')~1'))
                      
                      
                      # estimate ------------
                       
                      res<- poLCA::poLCA(formula,data,nclass=nc,calc.se = FALSE)
                       
                      # result-----------
                      
                      aic<- res$aic 
                      bic<- res$bic 
                      Gsq<- res$Gsq
                      Chisq<- res$Chisq 
                     # class <- res$predclass
                      
                      
                       results <-
                           list(
                               'aic' = aic,
                               'bic' = bic,
                               'Gsq' = Gsq,
                               'Chisq'=Chisq
                             #  'class'=class
                               
                           )
                       
                   },   
                   
                   # populate Model table-----
                   
                   .populateFitTable = function(results) {
                       
                       table <- self$results$fit
                       
                       aic <- results$aic
                       bic <- results$bic
                       Gsq <- results$Gsq
                       Chisq <- results$Chisq
                       
                       
                       
                       row <- list()
                       
                       row[['AIC']] <- aic
                       row[['BIC']] <- bic
                       row[['Gsq']] <- Gsq
                       row[['Chisq']] <- Chisq
                      
                       table$setRow(rowNo = 1, values = row)
                  
                  
        
                   },
                   
        .populateOutputs = function(data) {
            
            nc<- self$options$nc
            
            data<- as.data.frame(data)
            
            vars <- colnames(data)
            vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
            vars <- paste0(vars, collapse=',')
            formula <- as.formula(paste0('cbind(', vars, ')~1'))
            
            
            # estimate ------------
            
            res<- poLCA::poLCA(formula,data,nclass=nc,calc.se = FALSE)
            
            class <- res$predclass
            
            if (self$options$class
                && self$results$class$isNotFilled()) {

                
                self$results$class$setValues(class)
                
                self$results$class$setRowNums(rownames(data))
        
            }
            },
        
                   ### Helper functions =================================     
                   
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

                   
                   
                
                   
 
