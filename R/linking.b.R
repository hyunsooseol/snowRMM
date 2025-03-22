
linkingClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "linkingClass",
    inherit = linkingBase,
    private = list(

      .htmlwidget = NULL, 
      
        .init = function() {
            
          private$.htmlwidget <- HTMLWidget$new()
          
            if (is.null(self$data) | is.null(self$options$ind)  | is.null(self$options$dep)) {
                
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            # self$results$instructions$setContent(
            #     "<html>
            # <head>
            # </head>
            # <body>
            # <div class='instructions'>
            # 
            # <p>_____________________________________________________________________________________________</p>
            # <p>1. This function creates a composite linking or equating.
            # <p>2. The R package <b>equate</b>(Albano, 2018) is described in the <a href='https://cran.rstudio.com/web/packages/equate/equate.pdf' target = '_blank'>page.</a></p>
            # <p>3. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowRMM/issues'  target = '_blank'>GitHub</a>.</p>
            # <p>_____________________________________________________________________________________________</p>
            # 
            # </div>
            # </body>
            # </html>"
            # )
            
            
          self$results$instructions$setContent(
            private$.htmlwidget$generate_accordion(
              title="Instructions",
              content = paste(
                '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
                '<div style="text-align:justify;">',
                '<ul>',
                '<li>This function creates a composite linking or equating.</li>',
                '<li>The R package <b>equate</b>(Albano, 2018) is described in the <a href="https://cran.rstudio.com/web/packages/equate/equate.pdf" target = "_blank">page</a>.</li>',
                '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
                '</ul></div></div>'
                
              )
              
            )
          )              
          
          
            if(isTRUE(self$options$plot)){
              
              width <- self$options$width
              height <- self$options$height
              
              self$results$plot$setSize(width, height)
            }
            
           
            
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
                    
                    
                    lin <- equate::equate(x, y, type = 'l')
                    
                    eq <- equate::equate(x, y, type = 'e')
                    
                   
                    lincon<- lin$concordance
                    eqcon <- eq$concordance
                   
                    # making concordance table-------
                    
                    table<- self$results$con
                    
                    if(method=="linear"){
                    
                    tab <- as.data.frame(lincon)
                    
                    
                    for (i in 1:nrow(tab)) {
                        
                        row <- list()
                        
                        row[['x']] <- tab[i,1]
                        row[['yx']] <- tab[i,2]
                        
                        table$addRow(rowKey = i, values = row)
                        
                    }
                    
                    } 
                    
                    if(method=="equipercentile"){
                        
                        
                        tab <- as.data.frame(eqcon)
                        
                        
                        for (i in 1:nrow(tab)) {
                            
                            row <- list()
                            
                            row[['x']] <- tab[i,1]
                            row[['yx']] <- tab[i,2]
                            
                            table$addRow(rowKey = i, values = row)  
                        
                        }
                        
                    }
                    
                    
                    
                    ## Plot==================================================
                    
                    
                    image <- self$results$plot
                    image$setState(list(lin, eq))
                    
                },
        
        .plot = function(image,...) {
            
             
            # if (length(self$options$ind)<1) return()
            # 
            # if (length(self$options$dep)<1) return()
            # 
            
          if (is.null(image$state))
            return(FALSE)
          
            lin <- image$state[[1]]
            eq <- image$state[[2]]
            
            
            plot <- plot(lin,eq,lwd=2,addident = FALSE) 
                    
            
            print(plot)
            TRUE
            
        })
)
