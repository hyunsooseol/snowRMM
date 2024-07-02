
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
            
            <p>_____________________________________________________________________________________________</p>
            <p>1. <b>tidyLPA</b> R package is described in the <a href='https://cran.r-project.org/web/packages/tidyLPA/vignettes/Introduction_to_tidyLPA.html' target = '_blank'>page</a>.</p>
            <p>2. Four models(1,2,3,6) are specified using <b>mclust</b> R package.</p>
            <p>3. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowRMM/issues'  target = '_blank'>GitHub</a>.</p>
            <p>_____________________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
        )
        
        if(isTRUE(self$options$plot)){
          
          width <- self$options$width
          height <- self$options$height
          
          self$results$plot$setSize(width, height)
        }      
      
      
      },
      
 #---------     
      
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
