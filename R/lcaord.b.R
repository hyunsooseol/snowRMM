
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import tidySEM
#' @importFrom tidySEM descriptives
#' @importFrom tidySEM mx_lca
#' @importFrom tidySEM table_fit
#' @importFrom tidySEM table_prob
#' @importFrom tidySEM plot_prob
#' @import ggplot2
#' @export


lcaordClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lcaordClass",
    inherit = lcaordBase,
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
        #     <p>1. <b>tidySEM</b> R package is described in the <a href='https://cjvanlissa.github.io/tidySEM/articles/lca_ordinal.html' target = '_blank'>page</a>.</p>
        #     <p>2. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowRMM/issues' target = '_blank'>GitHub</a>.</p>
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
              '<li><b>tidySEM</b> R package is described in the <a href="https://cjvanlissa.github.io/tidySEM/articles/lca_ordinal.html" target = "_blank">page</a>.</li>',
              '<li> The FIML method is applied to handle missing values.</li>',
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
        nc <- self$options$nc
       
        data <- self$data
        #data <- jmvcore::naOmit(data)  
        data <- as.data.frame(data)
        
        #---
        retlist <- private$.computeFIT()
        #---
        
        # Descriptives---
        
        if(isTRUE(self$options$desc)){
          
          table <- self$results$desc
          desc <- retlist$desc
          
          d<- data.frame(desc)
          
          for(i in seq_along(vars)){
            row <- list()
            row[["type"]] <- d[[2]][i]
            row[["n"]] <- d[[3]][i]
            row[["missing"]] <- d[[4]][i]
            row[["unique"]] <- d[[5]][i]
            row[["mode"]] <- d[[6]][i]
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
          
          #cp1<- tidySEM::class_prob(retlist$res)
          cp1<- retlist$cp1
          
          mem<- data.frame(cp1$individual)
          m<- mem$predicted
          
          m <- as.factor(m)
          
          if (self$options$mem
              && self$results$mem$isNotFilled()) {
            
            self$results$mem$setValues(m)
            self$results$mem$setRowNums(rownames(data))
          }
        
        }
        # Response probilities plot---
        
        if(isTRUE(self$options$plot)){
          
          # tab <- tidySEM::table_prob(retlist$res)
          # long<- reshape(tab,
          #                direction = "wide",
          #                v.names = "Probability",
          #                timevar = "group",
          #                idvar = c("Variable", "Category"))
          # 
          # self$results$text$setContent(long)
          
          image <- self$results$plot
          image$setState(retlist$res)
        } 
        
        # Bar plot---
        
        if(isTRUE(self$options$plot1)){
        
        df_plot <- data
        names(df_plot) <- paste0("Value.", names(df_plot))
        df_plot <- reshape(df_plot, varying = names(df_plot), direction = "long")
        
        image <- self$results$plot1
        image$setState(df_plot)
        
        }
        
        
      },
  
      # plot---    

   .plot = function(image, ggtheme, theme,...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        res <- image$state
       
        plot<- tidySEM::plot_prob(res, bw = TRUE)
        
        plot <- plot+ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = self$options$angle, hjust = 1
            )
          )
        }
        
        print(plot)
        TRUE
        
      },       
 
   .plot1 = function(image, ggtheme, theme,...) { 
   
     if (is.null(image$state))
       return(FALSE)
     
     df_plot <- image$state
     
     
     plot1<- ggplot(df_plot, aes(x = Value)) + geom_bar() + 
       facet_wrap(~time,
                  scales = "free") + theme_bw()
     
     plot1 <- plot1+ggtheme
     print(plot1)
     TRUE
     
   },
   
   
  .computeFIT = function() {

        vars <- self$options$vars
        nc <- self$options$nc
        
        data <- self$data
        
        #apply FIML method---
        #data <- jmvcore::naOmit(data)        
        data <- as.data.frame(data)
        
        # Descriptives---
        desc <- tidySEM::descriptives(data)
        desc <- desc[, c("name", "type", "n", "missing", "unique", "mode")]
                         
        
        # Model---
        # Not possible to use classes=1: nc now.
        set.seed(123)
        res <- tidySEM::mx_lca(data = data, 
                               classes = nc)
        
        #self$results$text$setContent(res)
        
        #fit
        fit <- tidySEM::table_fit(res)
       
        # class size
        cp1<- tidySEM::class_prob(res)
        cp<- data.frame(cp1$sum.posterior)
        
        retlist <- list(res=res, desc=desc, fit=fit, cp=cp, cp1=cp1)
        return(retlist)
        
      } 
      
        
        
        
        
        
        
        
        )
)
