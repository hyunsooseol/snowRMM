
lltmClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lltmClass",
    inherit = lltmBase,
    private = list(
      .htmlwidget = NULL, 
      
      
        .init = function() {
          private$.htmlwidget <- HTMLWidget$new() 
          
            if (is.null(self$data) | is.null(self$options$vars)) {
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            # self$results$instructions$setContent(
            #     "<html>
            # <head>
            # </head>
            # <body>
            # <div class='instructions'>
            # <p>____________________________________________________________________________________</p>
            # <p>1. Performs Linear Logistic Test Model (LLTM) for binary item responses by using CML estimation.</p>
            # <p>2. Design matrix(W matrix) for the LLTM will be computed by specifying <b>Vectors and Number of columns</b>.</p>
            # <p>3. Artificial data matrix and R codes for creating W matrix can be found in Data Library>snowRMM folder.</p>
            # <p>4. A description of the LLTM is described in the <a href='https://scholarworks.umass.edu/cgi/viewcontent.cgi?article=1248&context=pare' target = '_blank'>paper</a>.</p>
            # <p>5. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowRMM/issues'  target = '_blank'>GitHub</a>.</p>
            # <p>____________________________________________________________________________________</p>
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
                '<li>Performs Linear Logistic Test Model (LLTM) for binary item responses by using CML estimation.</li>',
                '<li>Design matrix(W matrix) for the LLTM will be computed by specifying <b>Vectors and Number of columns</b>.</li>',
                '<li>Artificial data matrix and R codes for creating W matrix can be found in Data Library>snowRMM folder.</li>',
                '<li>A description of the LLTM is described in the <a href="https://scholarworks.umass.edu/cgi/viewcontent.cgi?article=1248&context=pare" target = "_blank">paper</a>.</li>',
                '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
                '</ul></div></div>'
                
              )
              
            )
          )              
          
          
          
            if (self$options$items)
                self$results$ra$items$setNote(
                    "Note",
                    "Easiness parameters have opposite signs to difficulty parameters."
                    
                )
            
            
            
            if (self$options$comp)
                self$results$ll$comp$setNote(
                    "Note",
                    "LLs= Conditional log-likelihoods; npar= Number of parameters; 
                    LR= Likelihood ratio statistics."

                )
            
          
            if(isTRUE(self$options$plot)){
              
              width <- self$options$width
              height <- self$options$height
              
              self$results$plot$setSize(width, height)
            }
            
             
            
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
            
           
            if(self$options$items | self$options$lr | self$options$ml | self$options$wald==TRUE){
            
            #Rasch analysis###############################
            
            rasch <- eRm::RM(data)
            
            ############################
            
            rasch.item<- rasch$betapar
            
            rasch.item <- as.data.frame(rasch.item)
            
          #  self$results$text$setContent(rasch.item)
              
            table <- self$results$ra$items
            
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
            
            lr <- eRm::LRtest(rasch)
            ##############################

            value<- lr$LR
            df<- lr$df
            p<- lr$pvalue

            table <- self$results$ra$lr


            row <- list()

            row[['value']] <- value
            row[['df']] <- df
            row[['p']] <- p

            table$setRow(rowNo = 1, values = row)


            # Martin-lof test--------------
            
            ml<- eRm::MLoef(rasch)
            #####################
            
            value<- ml$LR
            df<- ml$df
            p<- ml$p.value
            
            table <- self$results$ra$ml
            
            
            row <- list()
            
            row[['value']] <- value
            row[['df']] <- df
            row[['p']] <- p
            
            table$setRow(rowNo = 1, values = row)
            
            
            
            
            # Wald test----------
            vars <- self$options$vars
            
            table <- self$results$ra$wald
            
            #######################
            
            w<- eRm::Waldtest(rasch)
            
            ###################
            w<- w$coef.table
            
            w <- as.data.frame(w)
            
            
            # Wald test table----------------------
            
            
            for (i in seq_along(vars)) {
                row <- list()
                
                
                row[["item"]] <- w[[1]][i]
                row[["p"]] <- w[[2]][i]
               
                table$addRow(rowKey = vars[i], values = row)
            }
            
            
            } 
            ##########################################################
            
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
            
            table <- self$results$ll$eta
            
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
            
            table <- self$results$ll$beta
            
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
            
            table <- self$results$ll$comp
            
            
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
            
            # plot <- self$options$plot
            # 
            # if (!plot)
            #     return()
            # 
            
          
          if (is.null(image$state))
            return(FALSE)
          
            lltm <- image$state[[1]]
            rm <- image$state[[2]]
            
            plot <- 
                
                ggplot(data = NULL, aes(x=rm, y = lltm))+
                geom_abline(slope = 1, intercept = 0)+
                geom_smooth(method = "lm")+
                geom_point()+
                
                scale_x_continuous(limits = c(-4, 4)) +
                scale_y_continuous(limits = c(-4, 4)) +
                
                labs(x = "Item Easiness Parameter-RM", 
                     y = "Item Easiness Parameter-LLTM")+
                theme_bw()
            
            plot <- plot + ggtheme
            print(plot)
            TRUE
        }
            
            
    )           
        )

