
# This file is a generated template, your changes will not be overwritten


#' @importFrom R6 R6Class
#' @import jmvcore
#' @import equi
#' @importFrom equi equi
#' @importFrom equi smoothtab
#' @export




equiClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "equiClass",
    inherit = equiBase,
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
            <p>____________________________________________________________________________________</p>
            <p> - The Form x is equated to the Form y with single or equivalent group design.
            <p> - The R package <b>equi</b>(Wolodzko, 2020) is described in the <a href='https://rdrr.io/github/twolodzko/equi/man/equi.html' target = '_blank'>page.</a></p>
            <p> - Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowRMM/issues'  target = '_blank'>GitHub.</a></p>
            <p> - This project has been supported by G-TELP Korea. </p>
            <p>____________________________________________________________________________________</p>
            
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
            
            group <- self$options$group
            
            design <- self$options$design
            
            # get the data
            
            data <- self$data
            
            # convert to appropriate data types
            
            data[[ind]] <- jmvcore::toNumeric(data[[ind]])
            
            data[[dep]] <- jmvcore::toNumeric(data[[dep]])
            
            
            data <- na.omit(data)
            
            # Computing equipercentile equating--------
            
            #providing single group design------
            
            if(self$options$design == 'single'){
            
            eq <- equi::equi(smoothtab(data[[ind]],  data[[dep]], presmoothing=TRUE))
           
            } else{
                 # data[[ind]] <- data[data[[group]], data[[ind]]]
                 # data[[dep]] <- data[data[[group]], data[[dep]]]
                
                eq <- equi::equi(smoothtab(data[[ind]]), smoothtab(data[[dep]]))
                
                
            }
            # making concordance-------
            
            table<- self$results$con
            
            tab <- as.data.frame(eq[[1]])
            
            
            for (i in 1:nrow(tab)) {
                
                row <- list()
                
                row[['x']] <- tab[i,1]
                row[['yx']] <- tab[i,2]
                
                table$addRow(rowKey = i, values = row)
                
            }
           
           
            
        }
        
        )
)
