
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import equate
#' @importFrom equate freqtab
#' @importFrom equate equate
#' @export


linkingClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "linkingClass",
    inherit = linkingBase,
    private = list(

        .init = function() {
            
            if (is.null(self$data) | is.null(self$options$ind)  | is.null(self$options$dep)) {
                
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
            <p>1. This function creates a composite linking or equating.
            <p>2. The R package <b>equate</b>(Albano, 2018) is described in the <a href='https://cran.rstudio.com/web/packages/equate/equate.pdf' target = '_blank'>page.</a></p>
            <p>3. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowRMM/issues'  target = '_blank'>GitHub.</a></p>
            <p>4. This project has been supported by G-TELP Korea. </p>
            <p>_____________________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
            
            
        },
        
                .run = function() {
                    
                    
                    
                    if (length(self$options$ind)<1) return()
                    
                    if (length(self$options$dep)<1) return()
                    
                    
                    #get the data--------
                    
                    data <- self$data
                    
                    ind <- self$options$ind
                    
                    dep <- self$options$dep
                    
                    method <- self$options$method
                    
                    # convert to appropriate data types
                    
                    data[[ind]] <- jmvcore::toNumeric(data[[ind]])
                    
                    data[[dep]] <- jmvcore::toNumeric(data[[dep]])
                    
                    
                    data <- na.omit(data)
                    
                    # Computing  frequency table--------
                    
                    x<- equate::freqtab(data[[ind]])
                    y<- equate::freqtab(data[[dep]])
                    
                    
                    # computing equating---------------
                    
                    
                    res <- equate::equate(x, y, type = method)
                    
                    concord<- res$concordance
                    
                   
                    # making concordance table-------
                    
                    table<- self$results$con
                    
                    tab <- as.data.frame(concord)
                    
                    
                    for (i in 1:nrow(tab)) {
                        
                        row <- list()
                        
                        row[['x']] <- tab[i,1]
                        row[['yx']] <- tab[i,2]
                        
                        table$addRow(rowKey = i, values = row)
                        
                    }
                    
                    ## Plot==================================================
                    
                    
                    image <- self$results$plot
                    image$setState(res)
                    
                },
        
        .plot = function(image, ggtheme, theme, ...) {
            
                    
            # res <- image$state
            
            data <- self$data

            ind <- self$options$ind

            dep <- self$options$dep

            method <- self$options$method

            # convert to appropriate data types

            data[[ind]] <- jmvcore::toNumeric(data[[ind]])

            data[[dep]] <- jmvcore::toNumeric(data[[dep]])


            data <- na.omit(data)

            # Computing  frequency table--------

            x<- equate::freqtab(data[[ind]])
            y<- equate::freqtab(data[[dep]])

            # 
            # computing equating---------------
            
            
            lin <- equate::equate(x, y, type = "l")
            
            equ <- equate::equate(x, y, type = "e")
            
            
            plot <- plot(lin,equ,addident = FALSE) 
                    
            
            print(plot)
            TRUE
            
        })
)
