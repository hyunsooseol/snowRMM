
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import tidySEM
#' @import blavaan
#' @import OpenMx
#' @importFrom tidySEM mx_growth_mixture
#' @importFrom tidySEM mx_mixture
#' @importFrom tidySEM table_fit
#' @import ggplot2
#' @export

lcgmClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lcgmClass",
    inherit = lcgmBase,
    private = list(
        .run = function() {

          
          if (is.null(self$options$vars) ||
              length(self$options$vars) < 3) return()
           
          vars <- self$options$vars
          model <- self$options$model
          nc <- self$options$nc
          type <- self$options$type
          variance <- self$options$variance  
          
          data <- self$data
          data <- na.omit(data)
          data <- as.data.frame(data)
          
         #----------------
        retlist <- private$.computeFIT()
        #self$results$text$setContent(fit)
        
        # model fit---
          
          if(isTRUE(self$options$fit)){ 
            
            table <- self$results$fit
            
            fit<- retlist$fit
            fit<- t(fit)
            df<- as.data.frame(fit)
            names <- dimnames(df)[[1]]
            
            for(name in names){
              row <- list()
              row[['value']] <- df[name,1]
              table$addRow(rowKey=name, values=row)
            }
          }
          
          # Trajectory plot---
          
          image <- self$results$plot
          image$setState(retlist$res)
          

        },

        # Plot---
        
        .plot = function(image, ggtheme, theme,...) {
          
          if (is.null(image$state))
            return(FALSE)
          
          tra <- image$state
          plot <- tidySEM::plot_growth(tra, 
                                    rawdata = self$options$raw, 
                                    alpha_range = c(0,0.05))
          
          plot <- plot+ggtheme
          print(plot)
          TRUE
          
        },        
        

        .computeFIT = function() {
          
          vars <- self$options$vars
          model <- self$options$model
          nc <- self$options$nc
          type <- self$options$type
          variance <- self$options$variance
          
          data <- self$data
          data <- jmvcore::naOmit(data)        
          data <- as.data.frame(data)
          
          if(type=='conti'){
          
          set.seed(1234)
          res <- tidySEM::mx_growth_mixture(model = model,
                                     classes = nc,
                                     data = data)
         
          }
          
          # Get fit table 
          fit <- tidySEM::table_fit(res)
         
        retlist <- list(res=res, fit=fit)
        return(retlist)
        
        } 
        
 )
)
