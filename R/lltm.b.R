#' @importFrom R6 R6Class
#' @import jmvcore
#' @import eRm
#' @importFrom eRm LLTM
#' @importFrom stats confint
#' @importFrom eRm LRtest
#' @export


lltmClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lltmClass",
    inherit = lltmBase,
    private = list(

        
        
                .run = function() {

           
            # get variables-------
            
            data <- self$data
            
            vars <- self$options$vars
            
            mat <- self$options$mat
            
            col <- self$options$col
            
            #Removing perfect score items before estimation (for example all 1 or 0)-------
            
            for (varName in self$options$vars) {
                var <- self$data[[varName]]
                if (length(unique(var)) < 2)
                    stop(paste("Variable '", varName, "' contains all the same value and should be removed in the variable box."))
            }    
            
           
            if (is.null(self$options$vars) |
                length(self$options$vars) < 2) return()
            
           
            #Rasch analysis###############################
            
            rasch <- eRm::RM(data)
            
            ############################
            
            rasch.item<- rasch$betapar
            
            rasch.item <- as.data.frame(rasch.item)
            
          #  self$results$text$setContent(rasch.item)
              
            table <- self$results$items
            
            vars <- self$options$vars
            
            #se.beta-------
            
            se.beta<- rasch$se.beta
            
            
            # 95% confidence intervals----------
            
            ci<- stats::confint(rasch, "beta")
            ci<- as.data.frame(ci)
           
            # Rasch beta results----------------------
            
            
            for (i in seq_along(vars)) {
                row <- list()
                
                
                row[["item"]] <- rasch.item[[1]][i]
                row[["se"]] <- se.beta[i]
                
                row[["lower"]] <- ci[[1]][i]
                row[["upper"]] <- ci[[2]][i]
               
                
                table$addRow(rowKey = vars[i], values = row)
            }
            
            # LR test#################
            
            lr <- eRm::LRtest(rasch, split = "mean")
            ##############################

            value<- lr$LR
            df<- lr$df
            p<- lr$pvalue

            table <- self$results$lr


            row <- list()

            row[['value']] <- value
            row[['df']] <- df
            row[['p']] <- p

            table$setRow(rowNo = 1, values = row)


            # Running LLTM-------------------------
        
            #Converting vectors into matrix------------
            
            
            mat<- strsplit(mat,',')
            mat<- unlist(mat)
            mat <- as.integer(mat)
            mat1 <- as.matrix(mat)
            
            
            mat1 <- matrix(mat1,ncol=self$options$col)
            
            # self$results$text$setContent(mat1)
            
            
            #LLTM analysis###########################################
               
            lltm <- eRm::LLTM(data, mat1)
              
            ############################################# 
            
            table <- self$results$eta
            
            # estimating eta parameters-----------
           
            lltm.eta<- lltm$etapar
            lltm.se.eta<- lltm$se.eta
            eta<- cbind(lltm.eta, lltm.se.eta)
            lltm.eta <- as.data.frame(eta)
            
            
            # 95% confidence intervals----------
            
            lltm.ci<- stats::confint(lltm, "eta")
            lltm.ci<- as.data.frame(lltm.ci)
            
            
            names<- dimnames(lltm.eta)[[1]]
            
            
            for (name in names) {
                
                row <- list()
                
                row[["item"]] <- lltm.eta[name,1]
                row[["se"]] <- lltm.eta[name,2]
                
                row[["lower"]] <- lltm.ci[name,1]
                row[["upper"]] <- lltm.ci[name,2]
                
                table$addRow(rowKey=name, values=row)
                
            }
            
        # LLTM item easiness parameters(beta)----------
            
            lltm.item<- lltm$betapar
            
            lltm.item <- as.data.frame(lltm.item)
            
            #  self$results$text$setContent(rasch.item)
            
            table <- self$results$beta
            
            vars <- self$options$vars
            
            #se.beta-------
            
            lltm.se<- lltm$se.beta
            
            
            # 95% confidence intervals----------
            
            lltm.ci<- stats::confint(lltm, "beta")
            lltm.ci<- as.data.frame(lltm.ci)
            
            # Rasch beta results----------------------
            
            
            for (i in seq_along(vars)) {
                row <- list()
                
                
                row[["item"]] <- lltm.item[[1]][i]
                row[["se"]] <- lltm.se[i]
                
                row[["lower"]] <- lltm.ci[[1]][i]
                row[["upper"]] <- lltm.ci[[2]][i]
                
                
                table$addRow(rowKey = vars[i], values = row)
            }
            
            #  plot----------
            
            image <- self$results$plot
            
            rm<- rasch$betapar
            lltm<- lltm$betapar
            
            state <- list(lltm, rm)
            
            image$setState(state)
            
            
                },
        
        .plot = function(image,ggtheme, theme, ...) {
            
            plot <- self$options$plot
            
            if (!plot)
                return()
            
            
            lltm <- image$state[[1]]
            rm <- image$state[[2]]
            
            plot <- 
                
                ggplot(data = NULL, aes(x=rm, y = lltm))+
               # geom_abline(slope = 1, intercept = 0)+
                geom_smooth(method = "lm")+
                geom_point()+
                labs(x = "Item Easiness Parameter-RM", 
                     y = "Item Easiness Parameter-LLTM")+
                theme_bw()
            
            
            print(plot)
            TRUE
        }
            
            
    )           
        )

