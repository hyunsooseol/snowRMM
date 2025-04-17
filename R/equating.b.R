
# This file is a generated template, your changes will not be overwritten
#' @import equi

equatingClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "equatingClass",
    inherit = equatingBase,
    private = list(
       
      .htmlwidget = NULL, 
      
        .init = function() {
            
          if(self$options$mode=='simple'){  
          
            private$.htmlwidget <- HTMLWidget$new()  
            
            if (is.null(self$data) | is.null(self$options$ind)  | is.null(self$options$dep)) {
                
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            self$results$instructions$setContent(
              private$.htmlwidget$generate_accordion(
                title="Instructions",
                content = paste(
                  '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
                  '<div style="text-align:justify;">',
                  '<ul>',
                  '<li>The Form x is equated to the Form y with single group design.</li>',
                  '<li>The R package <b>equi</b>(Wolodzko, 2020) is described in the <a href="https://rdrr.io/github/twolodzko/equi/man/equi.html" target = "_blank">page</a>.</li>',
                  '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
                  '</ul></div></div>'
                  
                )
                
              )
            )             
              if(self$options$lineq)
                self$results$lineq$setNote(
                    "Note",
                    " y = \u2026 + \u2026 x "
                )
            if(isTRUE(self$options$plot)){
              
              width <- self$options$width
              height <- self$options$height
              
              self$results$plot$setSize(width, height)
            }
            
          }
          
          if(self$options$mode=='complex'){  
          
            private$.htmlwidget <- HTMLWidget$new()  
            
            if (is.null(self$data) | is.null(self$options$ind1)  | is.null(self$options$dep1)) {
              
              self$results$instructions1$setVisible(visible = TRUE)
              
            }

            self$results$instructions$setContent(
              private$.htmlwidget$generate_accordion(
                title="Instructions",
                content = paste(
                  '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
                  '<div style="text-align:justify;">',
                  '<ul>',
                  '<li>The Form x is equated to the Form y.</li>',
                  '<li>If an error message such as "could not find function ns", please chang Form x and Form y variables.</li>',
                  '<li>The R package <b>equi</b>(Wolodzko, 2020) is described in the <a href="https://rdrr.io/github/twolodzko/equi/man/equi.html" target = "_blank">page</a>.</li>',
                  '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
                  '</ul></div></div>'
                  
                )
                
              )
            )                       
            
            if(isTRUE(self$options$plot1)){
              
              width <- self$options$width1
              height <- self$options$height1
              
              self$results$plot1$setSize(width, height)
            }
            
          }  
            
            
        },
        
        
        .run = function() {
          
          
          if(self$options$mode=='simple'){
            
            
            if (length(self$options$ind)<1 | length(self$options$dep)<1) return()

            data <- self$data
            ind <- self$options$ind
            dep <- self$options$dep
            
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
        # Equating score-------------
            
            table <- self$results$escore
            es <- equi::lin(data[[ind]], leq)
           
            self$results$escore$setRowNums(rownames(data))
            self$results$escore$setValues(es)

            ## Plot==================================================
            image <- self$results$plot
            image$setState(leq)
          }
          
          if(self$options$mode=='complex'){
            
            if (length(self$options$ind1)<1 | length(self$options$dep1)<1) return()

            data <- self$data
            data <- na.omit(data)
            ind1 <- self$options$ind1
            dep1 <- self$options$dep1
            design <- self$options$design
            # convert to appropriate data types
            data[[ind1]] <- jmvcore::toNumeric(data[[ind1]])
            data[[dep1]] <- jmvcore::toNumeric(data[[dep1]])
            # Computing equipercentile equating--------
            
            # providing group design------
            
            if(self$options$design == 'single'){
              eq <- equi::equi(smoothtab(data[[ind1]],  data[[dep1]], presmoothing=TRUE))
            } else{
              eq <- equi::equi(smoothtab(data[[ind1]]), smoothtab(data[[dep1]]))
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
            
            # Contingency table of form x-----------
            
            conx <- equi::conttab(data[[ind1]])
            table<- self$results$contabx1
            tab <- data.frame(conx)
            for (i in 1:nrow(tab)) {
              row <- list()
              row[['score']] <- tab[i,1]
              row[['frequency']] <- tab[i,2]
              table$addRow(rowKey = i, values = row)
            }
            
            # Contingency table of form y-----------
            
            cony <- equi::conttab(data[[dep1]])
            table<- self$results$contaby1
            taby <- data.frame(cony)
            for (i in 1:nrow(taby)) {
              row <- list()
              row[['score']] <- taby[i,1]
              row[['frequency']] <- taby[i,2]
              table$addRow(rowKey = i, values = row)
            }
            
            # Equating score-------------
            
            table <- self$results$escore1
            es <- equi::equi(data[[ind1]], eq)
            self$results$escore1$setRowNums(rownames(data))
            self$results$escore1$setValues(es)
            
            ## Plot==================================================
            image1 <- self$results$plot1
            image1$setState(eq)
          }
        },
        
        .plot = function(image, ggtheme, theme, ...) {
          if (is.null(image$state))
            return(FALSE)  

            data <- self$data
            data <- jmvcore::naOmit(data)
            
            ind <- self$options$ind
            dep <- self$options$dep
            
            leq <- image$state
            ecdf1 <- stats::ecdf(data[[ind]])
            ecdf2 <- stats::ecdf(data[[dep]])
            
            plot(ecdf1, verticals=TRUE, do.points=FALSE, xlab='Score', main='Empirical cumulative distribution ')
            plot(ecdf2, verticals=TRUE, do.points=FALSE, add=TRUE, col='red')            
           
             # plot <- equi::cdfplot(data[[ind]], xlab='Score')
             # plot <- equi::cdfplot(data[[dep]], add=TRUE)
             # 
            print(plot)
            TRUE
            
        },
        
        .plot1 = function(image1, ggtheme, theme, ...) {
          
          if (is.null(image1$state))
            return(FALSE)
          
          # get the data--------
          
          data <- self$data
          data <- jmvcore::naOmit(data)
          ind1 <- self$options$ind1
          dep1 <- self$options$dep1
          eq <- image1$state
          st <- smoothtab(data[[ind1]],data[[dep1]],presmoothing=TRUE, postsmoothing=TRUE)
          plot1<- plot(st)
          print(plot1)
          TRUE
        }
         
    )
)
        
