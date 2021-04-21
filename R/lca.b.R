
# This file is a generated template, your changes will not be overwritten
#' Mixture Rasch Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import poLCA
#' @importFrom poLCA poLCA
#' @importFrom poLCA poLCA.entropy
#' @import MASS
#' @import scatterplot3d
#' @export


lcaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lcaClass",
    inherit = lcaBase,
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
           
            <p><b>To get started:</b></p>

            <p> jamovi treats all variables as qualitative/categorical/nominal.</p>

            <p> Variables must contain only integer values, and must be coded with consecutive values from 1 to the maximum number. </p>

            <p> The results of <b> Class membership </b> will be displayed in the datasheet.</p>
            
            <p> Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowRMM/'  target = '_blank'>GitHub.</a></p>

            </div>
            </body>
            </html>"
            )
            
            if (self$options$fit)
                self$results$fit$setNote(
                    "Note",
                    "Gsq=the likelihood-ratio statistic; Chisq=Pearson Chi-square goodness of fit statistic."
                )
            
            if (length(self$options$vars) <= 1)
                self$setStatus('complete')
            
            
            
        },
        
               .run = function() {

           
                   ready <- TRUE
                   
                   if (is.null(self$options$vars) ||
                       length(self$options$vars) < 2)
                       
                       ready <- FALSE
                   
                   if (ready) {
                       data <- private$.cleanData()
                       
                       results <- private$.compute(data)               
                   
                       # Populate Model table-----
                       
                       private$.populateFitTable(results)
                   
                       # populate output variables-----
                       
                       private$.populateOutputs(data)
                       
                       # prepare plot-----
                       
                       private$.preparePlot(data)
                       
                       
                   }
               },
                   
                   
                   
                   .compute = function(data) {
                       
                      nc<- self$options$nc
                       
                      data<- as.data.frame(data)
                      
                      vars <- colnames(data)
                      vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
                      vars <- paste0(vars, collapse=',')
                      formula <- as.formula(paste0('cbind(', vars, ')~1'))
                      
                      
                      # estimate ------------
                       
                      res<- poLCA::poLCA(formula,data,nclass=nc,calc.se = FALSE)
                       
                      
                      entro<- poLCA::poLCA.entropy(res)
                      
                      # result-----------
                      
                      aic<- res$aic 
                      bic<- res$bic 
                      Chisq<- res$Chisq 
                      Gsq <- res$Gsq
                      
                      
                       results <-
                           list(
                               'aic' = aic,
                               'bic' = bic,
                               'Chisq'=Chisq,
                               'Gsq'=Gsq,
                               'entro'=entro
                             
                               
                           )
                       
                   },   
                   
                   # populate Model table-----
                   
                   .populateFitTable = function(results) {
                       
                       table <- self$results$fit
                       
                       nc <- self$options$nc
                       aic <- results$aic
                       bic <- results$bic
                       entro <- results$entro
                       Gsq <- results$Gsq
                       Chisq <- results$Chisq
                       
                       
                       row <- list()
                       
                       row[['Class']] <- nc
                       row[['AIC']] <- aic
                       row[['BIC']] <- bic
                       row[['Entropy']] <- entro
                       row[['Gsq']] <- Gsq
                       row[['Chisq']] <- Chisq
                      
                       table$setRow(rowNo = 1, values = row)
                  
                  
        
                   },
                   
        .populateOutputs = function(data) {
            
            nc<- self$options$nc
            
            data<- as.data.frame(data)
            
            vars <- colnames(data)
            vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
            vars <- paste0(vars, collapse=',')
            formula <- as.formula(paste0('cbind(', vars, ')~1'))
            
            
            # estimate ------------
            
            res<- poLCA::poLCA(formula,data,nclass=nc,calc.se = FALSE)
            
            cm <- res$predclass
            
            if (self$options$cm
                && self$results$cm$isNotFilled()) {

                
                self$results$cm$setValues(cm)
                
                self$results$cm$setRowNums(rownames(data))
        
            }
            },
        
        .preparePlot = function(data) {
            
            nc<- self$options$nc
            
            data<- as.data.frame(data)
            
            vars <- colnames(data)
            vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
            vars <- paste0(vars, collapse=',')
            formula <- as.formula(paste0('cbind(', vars, ')~1'))
            
            
            # estimate ------------
            
            x <- poLCA::poLCA(formula,data,nclass=nc, calc.se = FALSE)
            
            
            # plot----------
            
            image <- self$results$plot
            image$setState(x)
            
            
        },
        
        .plot = function(image,...) {
            
            if (is.null(self$options$vars))
                return()
            
            x <- image$state
            
            ### plot function-----
            
            poLCA.makeplot.dich <-
                function(probs,P,y,ti) {
                    R <- nrow(probs[[1]])
                    pi.class <- matrix(NA,nrow=length(probs),ncol=R)
                    for (j in 1:length(probs)) {
                        pi.class[j,] <- probs[[j]][,2]
                    }
                    dimnames(pi.class) <- list(names(y),round(P,4))
                    ds.plot <- data.frame(Classes=as.vector(col(pi.class)),Manifest.variables=as.vector(row(pi.class)),value=as.vector(pi.class))
                    vis <- scatterplot3d(ds.plot,type="h",lwd=5,pch=" ",x.ticklabs=colnames(pi.class),y.ticklabs=colnames(y),z.ticklabs=" ",
                                         xlab="Classes; population share",ylab="Manifest variables",zlab="Pr(outcome)",color=2,main=ti,y.margin.add=0.2,
                                         mar=c(6,3,3,3),lab=c(R-1,ncol(y)-1),zlim=c(0,1),box=FALSE,cex.main=1,angle=83)
                }
            
            poLCA.makeplot.poly <-
                function(probs,r,y,K.j,ti) {
                    pi.class <- matrix(NA,nrow=length(probs),ncol=max(K.j))
                    for (j in 1:length(probs)) {
                        pi.class[j,1:K.j[j]] <- probs[[j]][r,]
                    }
                    dimnames(pi.class) <- list(as.character(c(1:ncol(y))),as.character(c(1:max(K.j))))
                    ds.plot <- data.frame(Manifest.variables=as.vector(row(pi.class)),Outcomes=as.vector(col(pi.class)),value=as.vector(pi.class))
                    vis <- scatterplot3d(ds.plot,type="h",lwd=5,pch=" ",x.ticklabs=colnames(y),y.ticklabs=colnames(pi.class),z.ticklabs=" ",
                                         xlab="Manifest variables",zlab="Pr(outcome)",main=ti,cex.main=1.5,color=2,lab=c(ncol(y)-1,max(K.j)-1),zlim=c(0,1),box=FALSE,
                                         angle=75,mar=c(3,3,2,3))
                }
            
            plot.poLCA <-
                function(x, ...) {
                    K.j <- sapply(x$probs,ncol)
                    R <- length(x$P)
                    if (max(K.j)==2) {
                        poLCA.makeplot.dich(x$probs,x$P,x$y,NULL)
                    } else {
                        layout(matrix(seq(1,(R+1)),R+1,1),heights=c(rep(5,R),1))
                        for (r in 1:R) {
                            poLCA.makeplot.poly(x$probs,r,x$y,K.j,paste("Class ",r,": population share = ",round(x$P[r],3),sep=""))
                        }
                    }
                    par(mfrow=c(1,1),mar=c(5,4,4,2)+0.1)
                }
            
          plot<-  plot.poLCA(x)
            
            print(plot)
            TRUE
            
            
        },
        
        
        ### Helper functions =================================     
                   
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

                   
                   
                
                   
 