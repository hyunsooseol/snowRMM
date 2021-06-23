
# This file is a generated template, your changes will not be overwritten
#' Mixture Rasch Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import poLCA
#' @importFrom poLCA poLCA
#' @importFrom poLCA poLCA.entropy
#' @importFrom stats aggregate
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
            
            <p> The output columm can NOT be used as an input to the same analysis.</p>
            
            <p> Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowRMM/'  target = '_blank'>GitHub.</a></p>

            </div>
            </body>
            </html>"
            )
            
            if (self$options$fit)
                self$results$fit$setNote(
                    "Note",
                    "Gsq=the likelihood-ratio statistic; Chisq=Pearson Chi-square goodness of fit statistic; Entropy=non-normalized entropy which ranges between 0 and infinity."
                )
            # if (self$options$cp)
            #     self$results$cp$setNote(
            #         "Note",
            #         "Sizes of each latent class."
            #     )
            # 
            # if (self$options$ip)
            #     self$results$ip$setNote(
            #         "Note",
            #         "Pr.=Probability."
            #     )


            
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
                   
                       # populate class probability table-----
                       
                       private$.populateClassTable(results)
                       
                       # populate item probability table-------
                       
                        private$.populateItemTable(results)
                       
                       # Populate Model table-----
                       
                       private$.populateFitTable(results)
                   
                       
                       # populate cell frequencies---------
                       
                       
                       private$.populateCfTable(results)
                       
                       # populate output variables-----
                       
                       private$.populateOutputs(data)
                       
                       # populated cell percentages in a latent class model-----
                       
                       private$.populateCellOutputs(data)
                       
                       # prepare plot-----
                       
                       private$.preparePlot(data)
                       
                       
                       # prepare plot1(profile)-----
                       
                       private$.preparePlot1()
                       
                       
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
                       
                      res<- poLCA::poLCA(formula,data,nclass=nc,maxiter = 2000,calc.se = FALSE)
                       
                      
                      entro<- poLCA::poLCA.entropy(res)
                      
                     
                      # result-----------
                      
                      classprob<- res$P
                      
                      
                    #  self$results$ip$setContent(res$probs)
                       itemprob<- res$probs
                      
                      aic<- res$aic 
                      bic<- res$bic 
                      Chisq<- res$Chisq 
                      Gsq <- res$Gsq
                      
                      # cell frequencies-------
                      
                      cell<- res$predcell 
                      
                      
                       results <-
                           list(
                               'classprob'=classprob,
                                'itemprob'=itemprob,
                               'aic' = aic,
                               'bic' = bic,
                               'Chisq'=Chisq,
                               'Gsq'=Gsq,
                               'entro'=entro,
                               'cell'= cell
                             
                               
                           )
                       
                   },   
                   
        
         # populate class probability table---------------
        
        .populateClassTable= function(results){
            
            classprob <- results$classprob
            
            classprob<- as.data.frame(classprob)
            
            names<- dimnames(classprob)[[1]]
            
            #creating table--------
            
            table <- self$results$cp
            
            for (name in names) {
                
                row <- list()
                
                row[['value']] <- classprob[name,1]
                
                table$addRow(rowKey=name, values=row)
                
            }
            
         
        },
        
        # populate item probability table---------------
        
        .populateItemTable= function(results){
        
            nc <- self$options$nc
            
            itemprob <- results$itemprob
            
            itemprob<- as.data.frame(itemprob)
            itemprob<- t(itemprob)
            
            names<- dimnames(itemprob)[[1]]
            
            #creating table--------
            
            table <- self$results$ip
            
            for (i in 1:nc)
                
                table$addColumn(
                    name = paste0("pc", i),
                    title = as.character(i),
                    type = 'number',
                    superTitle = 'Class'
                )
            
            for (name in names) {
                
                row <- list()
                
                
                for (j in seq_along(1:nc)) {
                    row[[paste0("pc", j)]] <- itemprob[name, j]
                }
                
                
                table$addRow(rowKey=name, values=row)
                
            }
            
            
            
            
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
        
        
        # populate cell frequencies------------
        
        .populateCfTable = function(results) {
        
            table <- self$results$cf
            
            cell <- results$cell
            
            cell<- as.data.frame(cell)
            
            names <-  dimnames(cell)[[1]]
            dims <- dimnames(cell)[[2]]
            
            
            for (dim in dims) {
                
                table$addColumn(name = paste0(dim),
                                type = 'number')
            }
            
            
            for (name in names) {
                
                row <- list()
                
                for(j in seq_along(dims)){
                    
                    row[[dims[j]]] <- cell[name,j]
                    
                }
                
                table$addRow(rowKey=name, values=row)
                
            }
            
            
            
        },
            
        
           # populate class membership-------
        
        
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
        
        # Predicted cell percentages in a latent class model---
        
        .populateCellOutputs = function(data) {
            
            nc<- self$options$nc
            
            data<- as.data.frame(data)
            
            vars <- colnames(data)
            vars <- vapply(vars, function(x) jmvcore::composeTerm(x), '')
            vars <- paste0(vars, collapse=',')
            formula <- as.formula(paste0('cbind(', vars, ')~1'))
            
            
            # estimate ------------
            
            res<- poLCA::poLCA(formula,data,nclass=nc,calc.se = FALSE)
            
           #Predicted cell percentages in a latent class model
            pc<- poLCA::poLCA.predcell(lc=res,res$y)
            
            
            if (self$options$pc
                && self$results$pc$isNotFilled()) {
                
                
                self$results$pc$setValues(pc)
                
                self$results$pc$setRowNums(rownames(data))
                
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
        
       
        # profile plot1---------------
        
        .preparePlot1 = function() {
        
        if(is.null(self$options$group))
            return()
            
            if(!is.null(self$options$group))
            
            {
                data <- self$data
                data <- as.data.frame(data)
                data <- jmvcore::naOmit(data)
                
                nc<- self$options$nc
                vars <- self$options$vars
                nVars <- length(vars)
                
                for (var in vars) {
                    data[[var]] <-as.numeric(as.character(data[[var]])) 
                }
                # Using aggregate to calculate mean across class variable-----
                ave <-  stats::aggregate(data[,self$options$vars], list(data[,self$options$group]), mean)
                
               
                names(ave)[1]   <-  self$options$group
                
                self$results$text$setContent(ave)
                 
                # The means of class table-------
               
                ave1 <- ave[,-1]
                
                names<- dimnames(ave1)[[1]]
                
                table <- self$results$mc
                
                for (i in seq_along(vars)) {

                    var <- vars[[i]]

                    table$addColumn(name = paste0(var),
                                    type = 'number',
                                    format = 'zto')

                }
                
                for (name in names) {
                    
                    row <- list()
                    
                    
                    for(j in seq_along(vars)){
                        
                        var <- vars[[j]]
                        
                        row[[var]] <- ave1[name, j]
                        
                    }
                    
                    table$addRow(rowKey=name, values=row)
                    
                    
                }
                
                
                # reshape to long for ggplot
                plotData1       <-  reshape2::melt(ave, id.vars=self$options$group)
                names(plotData1)[1]   <-  self$options$group
                
                # self$results$text$setContent(plotData1)
                
                # plot data function---------
                
                image   <-  self$results$plot1
                image$setState(plotData1)

            }
        
        },
        
        .plot1 = function(image, ggtheme, theme, ...) {
            
            
            if(is.null(self$options$group))
                return()
            
            plotData1 <- image$state
            
            if (!is.null(plotData1))
            {
                plot1 <-
                    ggplot(plotData1,
                           ggplot2::aes_string(
                               x = "variable",
                               y = "value",
                               group = self$options$group,
                               colour = self$options$group
                           )) +
                    geom_path(size = 1.2) +
                    geom_point(size = 4) +
                    xlab("Variable") +
                    ylab("Mean value") +
                    ggtheme
                
                print(plot1)
                TRUE
            }
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

                   
                   
                
                   
 
