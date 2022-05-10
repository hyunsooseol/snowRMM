
# This file is a generated template, your changes will not be overwritten

#' @importFrom R6 R6Class
#' @import jmvcore
#' @import equi
#' @importFrom equi lin
#' @importFrom equi cdfplot
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
            
            <p><b>Instructions</b></p>
            <p>_____________________________________________________________________________________________</p>
            <p>1. The Form x is equated to the Form y with single group design.
            <p>2. The R package <b>equi</b>(Wolodzko, 2020) is described in the <a href='https://rdrr.io/github/twolodzko/equi/man/equi.html' target = '_blank'>page.</a></p>
            <p>3. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowRMM/issues'  target = '_blank'>GitHub</a>.</p>
            <p>4. This project has been supported by <a href='http://www.itsc-group.com' target = '_blank'>ITSC GROUP(G-TELP).</a></p>
            <p>_____________________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )

              if(self$options$lineq)
                self$results$lineq$setNote(
                    "Note",
                    " y = \u2026 + \u2026 x "
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
            #-------------------------
            
            table<- self$results$lineq
            
            inter <-  leq$intercept
            sl <-  leq$slope
            
            row <- list()
            
            row[['intercept']] <- inter
            row[['slope']] <- sl
           
            table$setRow(rowNo = 1, values = row)
            
            ## Note equation-------
            
            inter<- format(round(inter, 3), nsmall = 3)
            sl <- format(round(sl, 3), nsmall = 3)
            
            if(self$options$lineq)
            self$results$lineq$setNote(
                "Note",
                 paste('y =', inter, '+', sl,'x') 
            )
            
            
            
            # Contingency table of form x-----------
            
            conx <- equi::conttab(data[[ind]])
            
            table<- self$results$contabx
            
            tab <- data.frame(conx)
            
            
             for (i in 1:nrow(tab)) {
            
            row <- list()
            
            row[['score']] <- tab[i,1]
            row[['frequency']] <- tab[i,2]
            
            table$addRow(rowKey = i, values = row)
            
             }
            
            # Contingency table of form y-----------
            
            cony <- equi::conttab(data[[dep]])
            
            table<- self$results$contaby
            
            taby <- data.frame(cony)
            
            
            for (i in 1:nrow(taby)) {
                
                row <- list()
                
                row[['score']] <- taby[i,1]
                row[['frequency']] <- taby[i,2]
                
                table$addRow(rowKey = i, values = row)
                
            }
            
            
            # Contingency table of form x and y -----------
            
            # conxy <- equi::conttab(data[[ind]], data[[dep]])
            # 
            # #---------------
            # 
            # table <- self$results$contabxy
            # 
            # tabxy <- data.frame(conxy)
            # 
            # 
            # for (i in 1:nrow(tabxy)) {
            # 
            #     row <- list()
            # 
            #     row[['score_x']] <- tabxy[i,1]
            #     row[['score_y']] <- tabxy[i,2]
            #     row[['frequency']] <- tabxy[i,3]
            # 
            #     table$addRow(rowKey = i, values = row)
            # 
            # }

        # Equating score-------------
            
            table <- self$results$escore
            
            # leq <- equi::lin(data[[ind]], data[[dep]])
            
            es <- equi::lin(data[[ind]], leq)
           
            self$results$escore$setRowNums(rownames(data))
            self$results$escore$setValues(es)
            
            
            # for (i in 1:nrow(es)) {
            #     
            #     row <- list()
            #     
            #     row[['xz']] <- es1[i]
            #    
            #     table$addRow(rowKey = i, values = row)
            #     
            # }
            
            
            
            ## Plot==================================================
            
            
            image <- self$results$plot
            image$setState(leq)
            
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            
            # if (length(self$options$vars) < 1)
            #     return()
            
            # get the data--------
            
            data <- self$data
            data <- jmvcore::naOmit(data)
            
            ind <- self$options$ind
            
            dep <- self$options$dep
            
            leq <- image$state
            ecdf1 <- ecdf(data[[ind]])
            ecdf2 <- ecdf(data[[dep]])
            
            plot(ecdf1, verticals=TRUE, do.points=FALSE, xlab='Score', main='Empirical cumulative distribution ')
            plot(ecdf2, verticals=TRUE, do.points=FALSE, add=TRUE, col='red')            
           
             # plot <- equi::cdfplot(data[[ind]], xlab='Score')
             # plot <- equi::cdfplot(data[[dep]], add=TRUE)
             # 
            print(plot)
            TRUE
            
        }
         
    )
)
        
