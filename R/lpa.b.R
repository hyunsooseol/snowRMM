
# This file is a generated template, your changes will not be overwritten
# This file is a generated template, your changes will not be overwritten
#' Mixture Rasch Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import tidyLPA
#' @import ggplot2
#' @importFrom tidyLPA estimate_profiles
#' @importFrom tidyLPA get_fit
#' @importFrom tidyLPA plot_profiles
#' @importFrom tidyLPA plot_bivariate
#' @importFrom tidyLPA get_data
#' @export


lpaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lpaClass",
    inherit = lpaBase,
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
            <p>1. The R package <b>tidyLPA</b> is described in the <a href='https://cran.r-project.org/web/packages/tidyLPA/vignettes/Introduction_to_tidyLPA.html' target = '_blank'>page</a>.</p>
            <p>3. The result of <b>Person class</b> will be displayed in the datasheet.</p>
            <p>3. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowRMM/issues'  target = '_blank'>GitHub</a>.</p>
            <p>_____________________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
            
            
        },
        
        
        
.run = function() {
            
            
    if (length(self$options$vars)<1) return()
    
    
            vars <- self$options$vars
            
            nc <- self$options$nc
            
            
            data <- self$data
            
            data <- jmvcore::naOmit(data)
            
            # for (i in seq_along(vars))
            #     data[[i]] <- jmvcore::toNumeric(data[[i]])
            #
            # data <- jmvcore::select(data, self$options$vars)


            # res <- NA
            # 
            # for(i in 1:nc){
            # 
            #     res[i]<- tidyLPA::estimate_profiles(data,i)
            # 
            # 
            # }



         #   df <- as.data.frame(res)
            
            res<- tidyLPA::estimate_profiles(data,nc)
            
           
           #  fit <- tidyLPA::get_fit(res)

            self$results$text$setContent(res)

            
        # person class---------
            
            pc<- tidyLPA:: get_data(res)
            pc<- pc$Class
            
            
            if (self$options$pc
                && self$results$pc$isNotFilled()) {
                
                
                self$results$pc$setValues(pc)
                
                self$results$pc$setRowNums(rownames(data))
                
            }
            
            
            
            
            
            
            # plot----------
            
            image <- self$results$plot
            image$setState(res)
            
            # plot----------
            
            image <- self$results$plot1
            image$setState(res)
            
            
              
        },
        
        .plot = function(image, ggtheme, theme,...) {
            
            if (is.null(self$options$vars))
                return()
            
            res <- image$state

         plot <- tidyLPA::plot_bivariate(res)
            
         print(plot)
            TRUE
                
        },
        
        
        .plot1 = function(image, ggtheme, theme,...) {
            
            if (is.null(self$options$vars))
                return()
            
            res <- image$state
            
            
            plot1 <- tidyLPA::plot_profiles(res,  add_line = TRUE)
            
            
            print(plot1)
            TRUE
        
        
        
        
        })
)
