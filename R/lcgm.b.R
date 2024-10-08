
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import tidySEM
#' @importFrom tidySEM mx_growth_mixture
#' @importFrom tidySEM mx_mixture
#' @importFrom tidySEM table_fit
#' @importFrom tidySEM descriptives
#' @import ggplot2
#' @export

lcgmClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lcgmClass",
    inherit = lcgmBase,
    private = list(
      .htmlwidget = NULL, 
      
      .init = function() {
      
        private$.htmlwidget <- HTMLWidget$new()
          
        if (is.null(self$data) | is.null(self$options$vars)) {
          
          self$results$instructions$setVisible(visible = TRUE)
          
        }
       
        # self$results$instructions$setContent(
        #   "<html>
        #     <head>
        #     </head>
        #     <body>
        #     <div class='instructions'>
        #     
        #     <p>_____________________________________________________________________________________________</p>
        #     <p>1. <b>tidySEM</b> R package is described in the <a href='https://cjvanlissa.github.io/tidySEM/articles/LCGA.html' target = '_blank'>page</a>.</p>
        #     <p>2. Please set <b>Thresholds=TRUE</b> when analyzing ordinal data.</p>
        #     <p>3. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowRMM/issues'  target = '_blank'>GitHub</a>.</p>
        #     <p>_____________________________________________________________________________________________</p>
        #     
        #     </div>
        #     </body>
        #     </html>"
        # )

        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title="Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              '<ul>',
              '<li><b>tidySEM</b> R package is described in the <a href="https://cjvanlissa.github.io/tidySEM/articles/LCGA.html" target = "_blank">page</a>.</li>',
              '<li>Please set <b>Thresholds=TRUE</b> when analyzing ordinal data.</li>',
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
          thr <- self$options$thr
       
          data <- self$data
          data <- na.omit(data)
          data <- as.data.frame(data)
          
       #---
        retlist <- private$.computeFIT()
       #---

          if(isTRUE(self$options$desc)){
            
            table <- self$results$desc
            desc <- retlist$desc
            
            d<- data.frame(desc)
 
           for(i in seq_along(vars)){
              row <- list()
              row[["mean"]] <- d[[2]][i]
              row[["median"]] <- d[[3]][i]
              row[["sd"]] <- d[[4]][i]
              row[["min"]] <- d[[5]][i]
              row[["max"]] <- d[[6]][i]
              table$addRow(rowKey = vars[i], values = row)
            }
                }
          
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
          
          # parameter estimates---
          
          if(isTRUE(self$options$est)){
            
            table <- self$results$est
            
            e<- retlist$para
            
            e<- data.frame(e)
            names<- dimnames(e)[[1]]
            
            for (name in names) {
              row <- list()
              
              row[['cat']] <- e[name, 1]
              row[['lhs']] <- e[name, 2]
              row[['est']] <- e[name, 3]
              row[['se']] <- e[name, 4]
              row[['p']] <- e[name, 5]
              row[['ci']] <- e[name, 6]
              row[['na']] <- e[name, 7]
             
              table$addRow(rowKey=name, values=row)
            }
          }
 
        # class size---
        
        if(isTRUE(self$options$cp)){ 
          
          table <- self$results$cp
          
          d<- retlist$cp
          
          for(i in seq_along(1:nc)){
            row <- list()
            
            row[["name"]] <- d[[1]][i]
            row[["count"]] <- d[[2]][i]
            row[["prop"]] <- d[[3]][i]
            
            table$addRow(rowKey = vars[i], values = row)
          }
          
        }
        
        # class member--
        if(isTRUE(self$options$mem)){
          
          cp<- tidySEM::class_prob(retlist$res)
          mem<- data.frame(cp$individual)
          m<- mem$predicted
          
          m <- as.factor(m)
          
          if (self$options$mem
              && self$results$mem$isNotFilled()) {
            
            self$results$mem$setValues(m)
            self$results$mem$setRowNums(rownames(data))
          }
          
        }
        
        if(isTRUE(self$options$plot1)){
          
          # Density plot---
          long <- reshape(data, direction = "long", 
                          varying = list(names(data)), 
                          v.names = "value", 
                          idvar = "id", 
                          timevar = "time")
          
          #self$results$text$setContent(long)
          image <- self$results$plot1
          image$setState(long)
        }
        
          if(isTRUE(self$options$plot)){
          # Trajectory plot---
          image <- self$results$plot
          image$setState(retlist$res)
          }

        },

# Plot---

 .plot1 = function(image, ggtheme, theme,...) {
   
   if (is.null(image$state))
     return(FALSE)
   
   long <- image$state
   
   plot1 <- ggplot(long, aes(x = value)) +
     geom_density() +
     facet_wrap(~time) + theme_bw()
   
   plot1 <- plot1+ggtheme
   print(plot1)
   TRUE
 
   },
   
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
          
          
          # R example---
          # data <- read.csv("empathy.csv")
          # set.seed(1234)
          # res<- tidySEM::mx_growth_mixture(model = "i =~ 1*ec1 + 1*ec2 + 1*ec3 +1*ec4 +1*ec5 +1*ec6
          #                  s =~ 0*ec1 + 1*ec2 + 2*ec3 +3*ec4 +4*ec5 +5*ec6
          #                  ec1 ~~ vec1*ec1
          #                  ec2 ~~ vec2*ec2
          #                  ec3 ~~ vec3*ec3
          #                  ec4 ~~ vec4*ec4
          #                  ec5 ~~ vec5*ec5
          #                  ec6 ~~ vec6*ec6
          #                   i ~~ 0*i
          #                   s ~~ 0*s
          #                   i ~~ 0*s
          #                        
          #                  # i ~~ i_var*i
          #                  # s ~~ s_var*s
          #                  # i ~~ s_cov*s
          #                        ",
          #                                  classes = 3,
          #                                  data = data) 
          # 
          
          vars <- self$options$vars
          model <- self$options$model
          nc <- self$options$nc
          thr <- self$options$thr
         
          data <- self$data
          data <- jmvcore::naOmit(data)        
          data <- as.data.frame(data)
          
          # Descriptives---
          desc <- tidySEM::descriptives(data)
          desc <- desc[, c("name", "mean", "median", "sd", "min", "max")]
         
          # Model--- 
          # Caution: Not possible to use 'classes=1:nc' using mx_mixture()
          
          set.seed(1234)
          res <- tidySEM::mx_growth_mixture(model = model,
                                            classes = nc,
                                            thresholds=thr,
                                            data=data)
          
          #self$results$text$setContent(res)                                                                  
         
          # Get fit table 
          fit <- tidySEM::table_fit(res)
          # Get parameter estimates
          para <- tidySEM::table_results(res, columns = NULL)
          para <- para[para$Category %in% c("Means", "Variances"), c("Category", "lhs", "est", "se", "pval", "confint", "name")]
           
          # class size
          cp<- tidySEM::class_prob(res)
          cp<- data.frame(cp$sum.posterior)
          
        retlist <- list(res=res, fit=fit, para=para, desc=desc, cp=cp)
        return(retlist)
        
        } 
        
 )
)
