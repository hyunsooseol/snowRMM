
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
        .run = function() {

            data <- self$data
            data <- na.omit(data)
            
            vars <- self$options$vars
            
            bn <- self$options$bn
            
            type <- self$options$type
            
            # boot fit------
           
            if(self$options$type=='bi'){ 
            
                obj<- eRm::RM(data)
                
                fit<- iarm::boot_fit(obj,B=bn)
                
            }else{
                
                obj<- eRm::PCM(data)
                
                fit<- iarm::boot_fit(obj,B=bn)
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
