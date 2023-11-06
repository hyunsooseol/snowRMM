
# This file is a generated template, your changes will not be overwritten

# Differential Item Functioning by eRm package

#' @importFrom R6 R6Class
#' @import jmvcore
#' @import eRm
#' @importFrom eRm RM
#' @importFrom eRm Waldtest
#' @export


difClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "difClass",
    inherit = difBase,
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
            <p>____________________________________________________________________________________</p>
            <p>1. Performs DIF detection using <b>eRm</b> R package.
            <P>2. For Raju and MH method, the focal group should be coded as <b>1</b>.</P>
            <p>3. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowIRT/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            </div>
            </body>
            </html>"
        )      
      
        #https://bookdown.org/chua/new_rasch_demo2/DIF.html
        
        if(isTRUE(self$options$plot1)){
          width <- self$options$width1
          height <- self$options$height1
          self$results$plot1$setSize(width, height)
        }  
        
        
        
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
        
      },
      
  
      ###############################################################    
             .run = function() {

             #   vars <- self$options$vars
             #   facs <- self$options$facs
             #   #get the data--------
             #   data <- self$data
             #   data <- jmvcore::naOmit(data)
             #   # convert to appropriate data types
             #   for (i in seq_along(vars))
             #     data[[i]] <- jmvcore::toNumeric(data[[i]])
             # 
             #   for (fac in facs)
             #     data[[fac]] <- as.factor(data[[fac]])
             # # data is now all of the appropriate type we can begin!
             #   data <- na.omit(data)
             #   data <- jmvcore::select(data, self$options$vars)

              
               data <- self$data
               groupVarName <- self$options$facs
               vars <- self$options$vars
               varNames <- c(groupVarName, vars)

               if (is.null(groupVarName)) return()

               data <- select(self$data, varNames)
               for (var in vars)
               data[[var]] <- jmvcore::toNumeric(data[[var]])
              # exclude rows with missings in the grouping variable
               data <- data[!is.na(data[[groupVarName]]), ]

            # Example
               # dichot_model <- RM(raschdat1)
               # # Create subgroup classifications:
               # subgroups <- sample(1:2, 100, replace = TRUE)
               # # Calculate subgroup-specific item difficulty values:
               # subgroup_diffs <- Waldtest(dichot_model, splitcr = subgroups)
               # 
               # # Create objects for subgroup-specific item difficulties:
               # subgroup_1_diffs <- subgroup_diffs$betapar1
               # subgroup_2_diffs <- subgroup_diffs$betapar2
               # 
               # #store results from item comparisons in an object called "comparisons"
               # comparisons <- as.data.frame(subgroup_diffs$coef.table)
               # 
              
               dicho <- eRm::RM(data[,-1])
              
              sub <- eRm::Waldtest(dicho, splitcr = data[[groupVarName]])
               
              #self$results$text$setContent(sub) 
              
               
              table <- self$results$z
              items <- self$options$vars
              
              # get result---
              
              z <- as.vector(sub$coef.table)
              p <- as.vector(sub$coef.table[,2])
              
              
              for (i in seq_along(items)) {
                row <- list()
                
                row[["zstat"]] <- z[i]
                
                row[["p"]] <- p[i]
               
                
                table$setRow(rowKey = items[i], values = row)
              }
              
               
              #---------------
              
              # Create objects for subgroup-specific item difficulties:
               sub1 <- sub$betapar1
               sub2 <- sub$betapar2
              
               # #store results from item comparisons in an object called "comparisons"
               comparison <- as.data.frame(sub$coef.table)
              
               #self$results$text$setContent(comparison)
              
               # plot1---------
               
               image1 <- self$results$plot1
               image1$setState(comparison)
               
               
             },
      
      .plot1 = function(image1,ggtheme, theme, ...) {     
        
        
        if (is.null(image1$state))
          return(FALSE)
        
        comparisons <- image1$state
        
        
        min.y <- ifelse(ceiling(min(comparisons$`z-statistic`)) > -3, -3, 
                        ceiling(min(comparisons$`z-statistic`)))
        
        max.y <- ifelse(ceiling(max(comparisons$`z-statistic`)) < 3, 3, 
                        ceiling(max(comparisons$`z-statistic`)))
        
        plot(comparisons$`z-statistic`, ylim = c(min.y, max.y),
             ylab = "Z", xlab = "Item", main = "Test Statistics for Item Comparisons \nbetween Subgroup 1 and Subgroup 2")
        abline(h=2, col = "red", lty = 2)
        abline(h=-2, col = "red", lty = 2)
        
        legend("topright", c("Z Statistic", "Boundaries for Significant Difference"),
               pch = c(1, NA), lty = c(NA, 2), col = c("black", "red"), cex = .7)   
               
               
      
        }
      )
)
