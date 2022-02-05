
# This file is a generated template, your changes will not be overwritten

#' @importFrom R6 R6Class
#' @import jmvcore
#' @import equi
#' @importFrom equi lin
#' @export

equatingClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "equatingClass",
    inherit = equatingBase,
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
            <p> The rationale of test equating is described in the <a href='https://r-bloggers.com/2020/01/concordance-correlation-coefficient/?fbclid=IwAR2Txi_QrFTuDB9jH8NiJW8dEde_lw2Td08XqxNzoWqut9m8E-bE5RHUDiI' target = '_blank'>page.</a></p>
            <p> Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowRMM/'  target = '_blank'>GitHub.</a></p>

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
            
            
            # get the data
            
            data <- self$data
            
            # convert to appropriate data types
            
            data[[ind]] <- jmvcore::toNumeric(data[[ind]])
            
            data[[dep]] <- jmvcore::toNumeric(data[[dep]])
            
            
            data <- na.omit(data)
            
            # Computing linear equation
            
            leq <- equi::lin(data[[ind]], data[[dep]])
            
            
            table<- self$results$lineq
            
            inter <-  leq$intercept
            sl <-  leq$slope
            
            row <- list()
            
            row[['intercept']] <- inter
            row[['slope']] <- sl
           
            table$setRow(rowNo = 1, values = row)
            
        }
        
        
    )
)
        
