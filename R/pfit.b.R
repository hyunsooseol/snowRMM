
# This file is a generated template, your changes will not be overwritten

#' @importFrom R6 R6Class
#' @import jmvcore
#' @import eRm 
#' @import ggplot2
#' @importFrom eRm RM
#' @importFrom eRm PCM
#' @importFrom iarm boot_fit


pfitClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "pfitClass",
    inherit = pfitBase,
    private = list(

        ###### .init function--------
        
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
            <p>1. This analysis computes Bootstrapping P Values for Outfit and Infit Statistics.</p>
            <p>2. Specify <b>Type and Bootstrap N</b> in the Analysis option.</p>
            <p>3. A fitted Rasch model or Partial Credit Model in R package <b>eRm</b> is used to compute bootstrap fit statistics.</p>
            <p>4. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowRMM/issues'  target = '_blank'>GitHub</a>.</p>
            <p>_____________________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
            
        },
        
   .run = function() {

       if (is.null(self$options$vars) |
           length(self$options$vars) < 2) return()
       
       
       
            data <- self$data
            data <- na.omit(data)
            
            vars <- self$options$vars
            
            bn <- self$options$bn
            
            type <- self$options$type
            
            adj <- self$options$adj
            
            # boot fit with RM an PCM ------
           
            if(self$options$type=='bi'){ 
            
                obj<- eRm::RM(data)
                
                fit<- iarm::boot_fit(obj,B=bn,p.adj=adj)
                
            }else{
                
                obj<- eRm::PCM(data)
                
                fit<- iarm::boot_fit(obj,B=bn,p.adj=adj)
            }
            
            # cREATING TABLE------------
            
            table <- self$results$outfit
            
            outfit<- fit[[1]][,1]
            outfit<- as.vector(outfit)
            
            pvalue<- fit[[1]][,2]
            pvalue<- as.vector(pvalue)
            
            padj<- fit[[1]][,3]
            padj<- as.vector(padj)
            
            
            for(i in seq_along(vars)){
                
                row <- list()
                
                row[["fit"]] <- outfit[i]
                
                row[["p"]] <- pvalue[i]
                
                row[["adp"]] <- padj[i]
                
                table$setRow(rowKey = vars[i], values = row)
                
            }
            
            
                table <- self$results$infit
                
                infit<- fit[[1]][,4]
                infit<- as.vector(infit)
                
                pvalue<- fit[[1]][,5]
                pvalue<- as.vector(pvalue)
                
                padj<- fit[[1]][,6]
                padj<- as.vector(padj)
                
                
                for(i in seq_along(vars)){
                    
                    row <- list()
                    
                    row[["fit"]] <- infit[i]
                    
                    row[["p"]] <- pvalue[i]
                    
                    row[["adp"]] <- padj[i]
                    
                    table$setRow(rowKey = vars[i], values = row)
                    
                    }
                
                
           
            # self$results$text$setContent(fit) 
            
            
        })
)
