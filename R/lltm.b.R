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
            <p><b>Instructions</b></p>
            <p>____________________________________________________________________________________</p>
            <p>1. Each variable must be <b>coded as 0 or 1 with the type of numeric-continuous</b> in jamovi.</p>
            <p>2. The results of <b>Person Analysis</b> will be displayed in the datasheet.</p>
            <p>3. The result tables are estimated by Marginal Maximum Likelihood estimation(MMLE).</p>
            <p>4. The rationale of snowIRT module is described in the <a href='https://bookdown.org/dkatz/Rasch_Biome/' target = '_blank'>documentation</a>.</p>
            <p>5. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowIRT/issues'  target = '_blank'>GitHub</a>.</p>
            <p>____________________________________________________________________________________</p>
            </div>
            </body>
            </html>"
            )
            
            #  private$.initItemsTable()
            
            if (self$options$comp)
                self$results$comp$setNote(
                    "Note",
                    "LLs= Conditional log-likelihoods; npar= Number of parameters; 
                    LR= Likelihood ratio statistics."

                )
            
           
            
            if (length(self$options$vars) <= 1)
                self$setStatus('complete')
        },
        
        ###############################################
        
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
            
            
            # Model comparison------------
            
            table <- self$results$comp
            
            
            mod<- stats::anova(rasch, lltm)
            mod<- as.data.frame(mod$statistics)
            
            names<- dimnames(mod)[[1]]
            # 
            # ll<- mod$LLs
            # dev<- mod$dev
            # npar<- mod$npar
            # lr<- mod$LR
            # df<- mod$df
            # p <- mod$p
            
            for (name in names) {
                
                row <- list()
                
                row[["ll"]] <- mod[name,1]
                row[["dev"]] <- mod[name,2]
                row[["npar"]] <- mod[name,3]
                row[["lr"]] <- mod[name,4]
                row[["df"]] <- mod[name,5]
                row[["p"]] <- mod[name,6]
                
                
                table$addRow(rowKey=name, values=row)
                
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

