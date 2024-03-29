
# This file is a generated template, your changes will not be overwritten
# This file is a generated template, your changes will not be overwritten
#' Mixture Rasch Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import tidyLPA
#' @import ggplot2
#' @importFrom tidyLPA estimate_profiles
#' @importFrom tidyLPA get_fit
#' @importFrom tidyLPA get_estimates
#' @importFrom tidyLPA plot_profiles
#' @importFrom tidyLPA plot_bivariate
#' @importFrom tidyLPA get_data
#' @importFrom reshape2 melt
#' @importFrom tidyLPA plot_density
#' @export


lpaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lpaClass",
    inherit = lpaBase,
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
            
            <p>_____________________________________________________________________________________________</p>
            <p>1. <b>tidyLPA</b> R package is described in the <a href='https://cran.r-project.org/web/packages/tidyLPA/vignettes/Introduction_to_tidyLPA.html' target = '_blank'>page</a>.</p>
            <p>2. Four models(1,2,3,6) are specified using <b>mclust</b> R package.</p>
            <p>3. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/snowRMM/issues'  target = '_blank'>GitHub</a>.</p>
            <p>_____________________________________________________________________________________________</p>
            
            </div>
            </body>
            </html>"
            )
            
            if(isTRUE(self$options$plot)){
              
              width <- self$options$width
              height <- self$options$height
              
              self$results$plot$setSize(width, height)
            }  
            
            if(isTRUE(self$options$plot1)){
              
              width <- self$options$width1
              height <- self$options$height1
              
              self$results$plot1$setSize(width, height)
            }  
            
            if(isTRUE(self$options$plot3)){
              
              width <- self$options$width2
              height <- self$options$height2
              
              self$results$plot3$setSize(width, height)
            }  
            
            if(isTRUE(self$options$plot2)){
              
              width <- self$options$width3
              height <- self$options$height3
              
              self$results$plot2$setSize(width, height)
            }  
             
            if(isTRUE(self$options$plot4)){
              
              width <- self$options$width4
              height <- self$options$height4
              
              self$results$plot4$setSize(width, height)
            }  
            
            
        },
        
        
        
.run = function() {
            
            
    if (length(self$options$vars)<1) return()
    
    
            vars <- self$options$vars
            nc <- self$options$nc
            
            variances <- self$options$variances
            covariances <- self$options$covariances
            
            data <- self$data
            
            data <- jmvcore::naOmit(data)
            
          
          #Estimates profile------------------------  
            
            res<- tidyLPA::estimate_profiles(data,
                                             nc,
                                             variances = variances,
                                             covariances = covariances)
           #------------------------------------------ 
           
           # self$results$text$setContent(res)

            res<- res[[1]]
            df<- as.data.frame(res$fit)
            
            names <- dimnames(df)[[1]]
            
            # populating fit measres----------
            
            table <- self$results$fit
            
            for(name in names){
              
              row <- list()
              
              row[['value']] <- df[name,1]
              
              table$addRow(rowKey=name, values=row)
              
            }
          
            # Model comparison-------
            
            out <- NULL
            
            for (i in 1:self$options$nc) {
              
              res<- tidyLPA::estimate_profiles(data,n_profiles=i,
                                               variances = variances,
                                               covariances = covariances)
              
              res<- res[[1]]
              df<- data.frame(res$fit)
              df<- t(df)
              
              
              model<- df[1]
              
              class <- df[2]
              
              log<- df[3]
              aic<- df[4]
              awe<- df[5]
              bic<- df[6]
              caic<- df[7]
              clc<- df[8]
              kic<- df[9]
              sabic<- df[10]
              icl<- df[11]
              entropy<- df[12]
              df<- data.frame(model,log, aic,
                              awe, bic,caic,
                              clc,kic,
                              sabic,icl,entropy,class)
              
              
              if (is.null(out)) {
                out <- df
              } else {
                out <- rbind(out, df)
              }
            }
            
            out <- out
            
           
            # populating table---------
             
             table <- self$results$best
             
            out <- data.frame(out)
            
            names <- dimnames(out)[[1]]
            

            for(name in names){

              row <- list()

              row[['model']] <- out[name, 1]
              row[['log']] <- out[name, 2]
              row[['aic']] <- out[name, 3]
              row[['awe']] <- out[name, 4]
              row[['bic']] <- out[name, 5]
              row[['caic']] <- out[name, 6]
              row[['clc']] <- out[name, 7]
              row[['kic']] <- out[name, 8]
              row[['sabic']] <- out[name, 9]
              row[['icl']] <- out[name, 10]
              row[['entropy']] <- out[name, 11]

              table$addRow(rowKey=name, values=row)

            }

            # get estimates--------------
            e<- tidyLPA::get_estimates(res)
            #################################
            
            e<- data.frame(e)
            
            table <- self$results$est
            
            names<- dimnames(e)[[1]]
            #dim<- dimnames(e)[[2]]
            
            # for (dim in dims) {
            #   
            #   table$addColumn(name = paste0(dim),
            #                   type = 'number')
            # }
            
            
            for (name in names) {
              
              row <- list()
              
              row[['cat']] <- e[name, 1]
              row[['par']] <- e[name, 2]
              row[['est']] <- e[name, 3]
              row[['se']] <- e[name, 4]
              row[['p']] <- e[name, 5]
              row[['cl']] <- e[name, 6]
              row[['model']] <- e[name, 7]
              row[['cla']] <- e[name, 8] 
              
              # for(j in seq_along(dims)){
              #   
              #   row[[dims[j]]] <- e[name,j]
              #   
              # }
              
              table$addRow(rowKey=name, values=row)
              
            }
            
            
            # person class---------
            
            base::options(max.print = .Machine$integer.max)
            
            pc<- tidyLPA:: get_data(res)
            pc<- pc$Class
            
            pc <- as.factor(pc)
            
            
            if (self$options$pc
                && self$results$pc$isNotFilled()) {
                
                
                self$results$pc$setValues(pc)
                
                self$results$pc$setRowNums(rownames(data))
                
            }
            
            
            # correlation plot----------
            
            image <- self$results$plot
            
            # nvars <- length(vars)
            # width <- 700 + nvars * 30
            #image$setSize(width, 700)
            
            
            image$setState(res)
            
            # Latent profile plot(Box plot)----------
            
            image1 <- self$results$plot1
            image1$setState(res)
            
            # Latent profile plot(Line plot)----------
            
            image4 <- self$results$plot4
            image4$setState(res)
            
            
            # elbow plot----------
            
            out1 <- out[,c(3:10,12),]
            
            colnames(out1) <- c('AIC','AWE','BIC',
                                'CAIC','CLC','KIC',
                                'SABIC','ICL','Class')
            
           
            elbow <- reshape2::melt(out1,
                                    id.vars='Class',
                                    variable.name="Fit",
                                    value.name='Value')
            
            
            #self$results$text$setContent(elbow)
            
            image <- self$results$plot2
            image$setState(elbow )
            
            # Density plot-----------
            
            res1<- tidyLPA::estimate_profiles(data,
                                             1:nc,
                                             variances = variances,
                                             covariances = covariances)
            #--------------------------------------
            
            image <- self$results$plot3
            image$setState(res1)
            
            
        },
        
        .plot = function(image, ggtheme, theme,...) {
            
          
          if (is.null(image$state))
            return(FALSE)
          
            # if (is.null(self$options$vars))
            #     return()
            
            res <- image$state

         plot <- tidyLPA::plot_bivariate(res)
            
         print(plot)
            TRUE
                
        },
        

.plot3 = function(image, ggtheme, theme,...) {
  
  # if (is.null(self$options$vars))
  #   return()
  
  if (is.null(image$state))
    return(FALSE)
  res1 <- image$state
  
  
  plot3 <- tidyLPA::plot_density(res1)
  
  
  print(plot3)
  TRUE
},

.plot2 = function(image, ggtheme, theme,...) {
  
  # if (is.null(self$options$vars))
  #   return()
  
  if (is.null(image$state))
    return(FALSE)
  
  elbow <- image$state
  
  plot2 <- ggplot2::ggplot(elbow, ggplot2::aes(x = Class, y = Value, color = Fit)) +
    ggplot2::geom_line(size = 1.1) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_x_continuous(breaks = seq(1, length(elbow$Class), by = 1))
  
  
  # plot2 <- ggplot2::ggplot(elbow,aes(x = Class, y = Value, group = Fit))+
  #   geom_line(size=1.1,aes(color=Fit))+
  #   geom_point(size=3,aes(color=Fit))
  # 
  
  plot2 <- plot2+ggtheme
  
  
  print(plot2)
  TRUE
  
},

      .plot1 = function(image1, ggtheme, theme,...) {
            
         
          if (is.null(image1$state))
            return(FALSE)
          
            res <- image1$state
            
            line <- self$options$line
            
            plot1 <- tidyLPA::plot_profiles(res,
                                            add_line = FALSE,
                                            rawdata = FALSE)
            
            if(line=='TRUE'){
              plot1 <- tidyLPA::plot_profiles(res,
                                              add_line = TRUE,
                                              rawdata = FALSE)
            } 
            
            
            
            if (self$options$angle > 0) {
              plot1 <- plot1 + ggplot2::theme(
                axis.text.x = ggplot2::element_text(
                  angle = self$options$angle, hjust = 1
                )
              )
            }
            
            print(plot1)
            TRUE
         },


.plot4 = function(image4, ggtheme, theme,...) {
  
  
  if (is.null(image4$state))
    return(FALSE)
  
  res <- image4$state
 
  plot4 <- tidyLPA::plot_profiles(res,
                                  ci=NULL,
                                  sd=FALSE,
                                  add_line = TRUE,
                                  rawdata = FALSE)
 
  if (self$options$angle > 0) {
    plot4 <- plot4 + ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = self$options$angle, hjust = 1
      )
    )
  }
  
  print(plot4)
  TRUE

}


)
)
