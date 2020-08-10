
# This file is a generated template, your changes will not be overwritten

#' Mixture Rasch Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import mixRasch
#' @importFrom mixRasch mixRasch
#' @import boot
#' @importFrom boot boot
#' @importFrom boot boot.ci
#' @export


bfitClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "bfitClass",
    inherit = bfitBase,
    private = list(
        
        
        ###### .init function--------
        
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
          
            <p><b>To get started:</b></p>

            <p> The traditional Rasch model is performed by Jonint Maximum Liklihood(JML).</p>

            <p>- Specify <b>'Step', 'Type', and 'Bootstrap N'</b> in the 'Analysis option'.</p>

            <p>- Please, be patient. The bootstrapped confidence interval is <b>quite time-consuming !</b></p>

            <p>- Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowRMM/'  target = '_blank'>GitHub</a></p>

            <p> If you have any questions, please e-mail me: snow@cau.ac.kr</a></p>
            </div>
            </body>
            </html>"
            )
            
        },
        
        .run = function() {
            
            # get variables-------
            
            data <- self$data
            
            vars <- self$options$get('vars')
            
            
            # Ready--------
            
            ready <- TRUE
            
            if (is.null(self$options$vars) | length(self$options$vars) < 2)
                
                ready <- FALSE
            
            if (ready) {
                
                data <- private$.cleanData()
                
                results <- private$.compute(data)
                
                
                # populate Boot Fit table-------------
                
                    private$.populateBootTable(results)
                
                #  prepare infit plot-----
                # 
                # private$.prepareInPlot(data)
                # 
                #  prepare infit plot-----
                #  
                #  private$.prepareOutPlot(data)
            }
        },
            
     .compute = function(data) {
                    
         
         # get variables--------
                    
                    data <- self$data
                    
                    vars <- self$options$vars
                    
                    step <- self$options$step
                    
                    type <- self$options$type
                    
                    bn <- self$options$bn
            
     # Computing Bootstrap item fit 
            
            # Computing boot infit-------------
            
            boot.infit <- function(data, indices) {
                d = data[indices, ]
                
                # estimate Rasch model--------
                res1 <-
                    mixRasch::mixRasch(
                        data = d,
                        steps = step,
                        model = type,
                        n.c = 1
                    )
                
                
                # item infit--------
                infit <- res1$item.par$in.out[, 1]
                
                return(infit)
                
            }
            
            boot.in <-
                boot::boot(data = data,
                           statistic = boot.infit,
                           R = bn)
            
            binfit <- boot::boot.ci(boot.in, type = "perc")
            
            # get boot infit------
            
            binfit <- binfit$percent
            
            
            # computing boot outfit------
            
            boot.outfit <- function(data, indices) {
                d = data[indices, ]
                
                # estimate Rasch model
                res1 <-
                    mixRasch::mixRasch(
                        data = d,
                        steps = step,
                        model = type,
                        n.c = 1
                    )
                
                
                # item outfit-------
                
                outfit <- res1$item.par$in.out[, 3]
                
                return(outfit)
            }
            
            boot.out <-
                boot::boot(data = data,
                           statistic = boot.outfit,
                           R = bn)
            
            boutfit <- boot::boot.ci(boot.out, type = "perc")
            
            # get boot outfit-------
            
            boutfit <- boutfit$percent
            
        
            results <-
                list('boot.in'=boot.in,
                     'boot.out'=boot.out,
                     'binfit'=binfit,
                     'boutfit'=boutfit
                )
        
            
            },
        
 
   .populateBootTable = function(results) {
            
        table <- self$results$bfit
            
            
            values <- list()
            
            binfit <- results$binfit
            
            boutfit <- results$boutfit
            
            
            values[['l[infit]']] <- binfit[4]
            values[['u[infit]']] <- binfit[5]
            
            values[['l[outfit]']] <- boutfit[4]
            values[['u[outfit]']] <- boutfit[5]
            
            
            table$setRow(rowNo = 1, values)
            
            
        },
   
 
   # #### Plot functions ----
   # 
   # .prepareInPlot = function(data) {
   #     
   #    
   #     
   #     # Prepare Data For Plot -------
   #     image <- self$results$plot
   #     image$setState(partial)
   #     
   #     
   #     
   # }, 
   
   
   # Infit plot--------------
   
   .inPlot = function(image, ...) {
      
        inplot <- self$options$inplot
       
       if (!inplot)
           return()
       
        boot.in <- self$results$boot.in
        
        # infit Distribution--------
        
        boot.in$t[,1]
        
        hist(boot.in$t[,1], main = 'Infit Distribution ', 
             xlab = 'Infit', col = 'yellow', prob = T)
        
        lines(density(boot.in$t[,1]), col = 'blue')
        
        ci_H=binfit[c(4,5)]
        
        abline(v = ci_H, col = 'red')
        
            
        },
        
   # # wright <- image$state
   # 
   # 
   # plot <- private$.personItemPlot(wright)
   # 
   # 
   # 
   # print(plot)
   # TRUE  
   # 
   
   
     #### Helper functions =================================
   
   .cleanData = function() {
       items <- self$options$vars
       
       data <- list()
       
       for (item in items)
           data[[item]] <-
           jmvcore::toNumeric(self$data[[item]])
       
       attr(data, 'row.names') <- seq_len(length(data[[1]]))
       attr(data, 'class') <- 'data.frame'
       data <- jmvcore::naOmit(data)
       
       return(data)
   }
   
        
        
       )
)
