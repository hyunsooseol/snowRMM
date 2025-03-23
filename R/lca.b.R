
# This file is a generated template, your changes will not be overwritten


lcaClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "lcaClass",
    inherit = lcaBase,
    private = list(
      .allCache = NULL,
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        private$.allCache <- NULL
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Latent Class Analysis based on <b>poLCA(Linzer & Lewis, 2022)</b> R package.</li>',
            '<li>Variables must contain integer values, and must be coded with consecutive values from <b>1</b> to the maximum number.</li>',
            '<li><b> Membership table</b> will be shown in the datasheet.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
          
        ))
        
        if (self$options$fit)
          self$results$mf$fit$setNote(
            "Note",
            "G\u00B2=Likelihood ratio statistic; \u03C7\u00B2=Pearson Chi-square goodness of fit statistic; Entropy=entropy R^2 statistic (Vermunt & Magidson, 2013, p. 71)"
          )
        
        if (isTRUE(self$options$plot)) {
          width <- self$options$width
          height <- self$options$height
          
          self$results$plot$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot1)) {
          width <- self$options$width1
          height <- self$options$height1
          
          self$results$plot1$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot2)) {
          width <- self$options$width2
          height <- self$options$height2
          
          self$results$plot2$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot3)) {
          width <- self$options$width3
          height <- self$options$height3
          
          self$results$plot3$setSize(width, height)
        }
        
        
        
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
        
        
        
      },
      
      .run = function() {
        ready <- TRUE
        
        if (is.null(self$options$vars) ||
            length(self$options$vars) < 2)
          ready <- FALSE
        
        if (ready) {
          if (is.null(private$.allCache)) {
            data <- private$.cleanData()
            private$.allCache <- private$.compute(data)
          }
          
          results <- private$.allCache
          
          # populate Model comparison-----------
          
          private$.populateModelTable(results)
          
          # populate class probability table-----
          
          private$.populateClassTable(results)
          
          # populate item probability table-------
          
          private$.populateItemTable(results)
          
          # Populate Model table-----
          
          private$.populateFitTable(results)
          
          
          # populate cell frequencies---------
          
          
          private$.populateCfTable(results)
          
          # populate output variables-----
          
          private$.populateOutputs(results)
          
          # populated cell percentages in a latent class model-----
          
          # private$.populateCellOutputs(results)
          
          # populated posterior probabilities--
          
          # private$.populatePosteriorOutputs(results)
          
          
          
        }
      },
      
      .compute = function(data) {
        # library(poLCA)
        # data(values)
        # f <- cbind(A,B,C,D)~1
        # res<- poLCA::poLCA(f,values,nclass=2, na.rm = F,calc.se = FALSE)
        
        
        nc <- self$options$nc
        
        ############ Construct formula###################
        
        vars <- self$options$vars
        
        
        vars <- vapply(vars, function(x)
          jmvcore::composeTerm(x), '')
        vars <- paste0(vars, collapse = ',')
        formula <- as.formula(paste0('cbind(', vars, ')~1'))
        
        
        if (length(self$options$covs) >= 1) {
          # if( !is.null(self$options$covs) ) {
          covs <- self$options$covs
          covs <- vapply(covs, function(x)
            jmvcore::composeTerm(x), '')
          covs <- paste0(covs, collapse = '+')
          
          formula <- as.formula(paste0('cbind(', vars, ') ~', covs))
        }
        
        
        ################ Model Estimates############################
        set.seed(1126)
        res <- poLCA::poLCA(formula,
                            data,
                            nclass = nc,
                            na.rm = FALSE,
                            calc.se = FALSE)
        
        
        ###############################################################
        
        
        #if( !is.null(self$options$covs) ) {
        if (length(self$options$covs) >= 1 &&
            isTRUE(self$options$coef)) {
          R <- length(res$P)
          
          mzout <- function() {
            cat("_________________________________________________________\n")
            cat("Fit for", R, "latent classes\n")
            cat("_________________________________________________________\n")
            
            for (r in 2:R) {
              cat(paste(r, "/ 1\n"))
              disp <- data.frame(
                coeff = round(res$coeff[, (r - 1)], 5),
                se = round(res$coeff.se[, (r - 1)], 5),
                tval = round(res$coeff[, (r - 1)] / res$coeff.se[, (r -
                                                                      1)], 3),
                pr = round(1 - (2 * abs(
                  pt(res$coeff[, (r - 1)] / res$coeff.se[, (r - 1)], res$resid.df) - 0.5
                )), 3)
              )
              
              colnames(disp) <- c("Coefficient", " Std. error", " t value", " Pr(>|t|)")
              print(disp)
              cat("_________________________________________________________\n")
            }
          }
          out <- utils::capture.output(mzout())
          self$results$text$setContent(out)
        }
        
        
        # Model Fit---------
        
        aic <- res$aic
        bic <- res$bic
        Chisq <- res$Chisq
        Gsq <- res$Gsq
        
        #sample-size adjusted bic
        SABIC = (-2 * res$llik) + (log((res$N + 2) / 24) * res$npar)
        
        CAIC = (-2 * res$llik) + res$npar * (1 + log(res$N))
        
        # Akaike Information Criterion 3
        aic3 <- (-2 * res$llik) + (3 * res$npar)
        
        
        # self$results$text1$setContent(SABIC)
        
        # Model comparison-------
        
        out <- NULL
        
        for (i in 1:self$options$nc) {
          #########################################
          res <- poLCA::poLCA(
            formula,
            data,
            nclass = i,
            na.rm = FALSE,
            calc.se = FALSE
          )
          ##########################################
          
          aic <- res$aic
          aic3 <- (-2 * res$llik) + (3 * res$npar)
          bic <- res$bic
          loglik <- res$llik
          Chisq <- res$Chisq
          Gsq <- res$Gsq
          
          SABIC = (-2 * res$llik) + (log((res$N + 2) / 24) * res$npar)
          
          CAIC = (-2 * res$llik) + res$npar * (1 + log(res$N))
          
          df <- data.frame(aic, aic3, bic, SABIC, CAIC, loglik, Chisq, Gsq)
          
          
          if (is.null(out)) {
            out <- df
          } else {
            out <- rbind(out, df)
          }
        }
        
        out <- out
        
        # Elbow plot-------------
        
        out1 <- out[, c(1:5)]
        
        
        cla <- c(1:self$options$nc)
        
        out1 <- data.frame(out1, cla)
        
        # self$results$text1$setContent(out1)
        
        colnames(out1) <- c('AIC', 'AIC3', 'BIC', 'SABIC', 'CAIC', 'Class')
        
        elbow <- reshape2::melt(
          out1,
          id.vars = 'Class',
          variable.name = "Fit",
          value.name = 'Value'
        )
        
        image <- self$results$plot3
        image$setState(elbow)
        
        
        # Caculating Chi and Gsp p values----------
        
        y <- res$y
        K.j <- t(matrix(apply(y, 2, max)))
        C <- max(K.j)
        J <- ncol(y)
        #I <- J # number of items
        
        df <- C ^ J - res$npar - 1 # Degrees of freedom
        
        cp <- 1 - pchisq(res$Chisq, df)
        gp <- 1 - pchisq(res$Gsq, df)
        
        # pvalue-----
        
        # C <- max(K.j) # number of categories
        # I <- J # number of items
        # df <- C^I - ret$npar - 1 # Degrees of freedom
        # Chisq.pvalue <- 1-pchisq(ret$Chisq,df)
        # Gsq.pvalue <- 1-pchisq(ret$Gsq,df)
        
        
        #########################################
        # ref:https://stackoverflow.com/questions/33000511/entropy-measure-polca-mplus
        # caculating R2_Entropy
        
        
        entropy <- function (p)
          sum(na.omit(-p * log(p))) #sum(-p*log(p))
        error_prior <- entropy(res$P) # Class proportions
        error_post <- mean(apply(res$posterior, 1, entropy))
        entro <- (error_prior - error_post) / error_prior
        #########################################################################
        
        ####### result###############################
        
        classprob <- res$P
        itemprob <- res$probs
        
        
        # cell frequencies-------
        cell <- res$predcell
        
        
        # output results------------
        
        base::options(max.print = .Machine$integer.max)
        cm <- res$predclass
        
        
        #self$results$text1$setContent(cm)
        
        
        
        #Predicted cell percentages in a latent class model
        # pc<- poLCA::poLCA.predcell(lc=res,res$y)
        
        # Posterior probabilities---------------
        #post <- res$posterior
        
        
        # plot----------
        
        image <- self$results$plot
        image$setState(res)
        
        # plot1------
        
        lcModelProbs <- reshape2::melt(res$probs)
        colnames(lcModelProbs) <- c("Class", "Level", "value", "L1")
        
        image1 <- self$results$plot1
        image1$setState(lcModelProbs)
        
        # profile plot2------
        
        profile <- reshape2::melt(res$probs)
        colnames(profile) <- c("Class", "Level", "value", "Variable")
        levels(profile$Class) <- c(1:self$options$nc)
        
        image2 <- self$results$plot2
        
        # nvars <- length(vars)
        # width <- 700 + nvars * 40
        # image2$setSize(width, 300)
        
        image2$setState(profile)
        
        
        # log-likelihood--------
        
        like <- res[["llik"]]
        
        # R output----------------------
        
        if (isTRUE(self$options$r)) {
          self$results$r$setContent(res)
        }
        
        results <-
          list(
            'classprob' = classprob,
            'itemprob' = itemprob,
            'out' = out,
            'aic' = aic,
            'aic3' = aic3,
            'bic' = bic,
            'Chisq' = Chisq,
            'Gsq' = Gsq,
            'entro' = entro,
            'cell' = cell,
            'cp' = cp,
            'gp' = gp,
            'cm' = cm,
            'df' = df,
            'like' = like,
            'SABIC' = SABIC,
            'CAIC' = CAIC
            
          )
        
      },
      
      
      # Model table-----
      
      .populateFitTable = function(results) {
        table <- self$results$mf$fit
        
        nc <- self$options$nc
        
        like <- results$like
        df <- results$df
        aic <- results$aic
        
        aic3 <- results$aic3
        
        bic <- results$bic
        entro <- results$entro
        resid.df <- results$df
        Gsq <- results$Gsq
        gp <- results$gp
        Chisq <- results$Chisq
        cp <- results$cp
        
        SABIC <- results$SABIC
        CAIC <- results$CAIC
        
        row <- list()
        
        row[['Class']] <- nc
        row[['Log-likelihood']] <- like
        row[['Resid.df']] <- df
        row[['AIC']] <- aic
        row[['AIC3']] <- aic3
        row[['BIC']] <- bic
        
        row[['SABIC']] <- SABIC
        row[['CAIC']] <- CAIC
        
        row[['Entropy']] <- entro
        row[['Resid.df']] <- resid.df
        row[['G\u00B2']] <- Gsq
        row[['G\u00B2 p']] <- gp
        row[['\u03C7\u00B2']] <- Chisq
        row[['\u03C7\u00B2 p']] <- cp
        
        
        table$setRow(rowNo = 1, values = row)
        
        
        
      },
      
      # Model comparison table----------
      
      
      .populateModelTable = function(results) {
        table <- self$results$mf$comp
        
        nc <- self$options$nc
        
        out <- results$out
        
        
        fit <- data.frame(out)
        
        
        names <- dimnames(fit)[[1]]
        
        
        for (name in names) {
          row <- list()
          
          row[["aic"]]   <-  fit[name, 1]
          row[["aic3"]]   <-  fit[name, 2]
          row[["bic"]] <-  fit[name, 3]
          
          row[["SABIC"]] <-  fit[name, 4]
          row[["CAIC"]] <-  fit[name, 5]
          
          row[["loglik"]] <-  fit[name, 6]
          row[["Chisq"]] <-  fit[name, 7]
          row[["Gsq"]] <-  fit[name, 8]
          
          table$addRow(rowKey = name, values = row)
          
        }
      },
      
      
      
      # Multinomial logit coefficients--------
      
      #  .populateLoTable= function(results) {
      #
      #
      #    table <- self$results$lo
      #
      #    lo <- results$lo
      #
      #    coef<- as.data.frame(lo)
      #
      #
      #    names <- dimnames(coef)[[1]]
      #
      #
      #    dims <- dimnames(coef)[[2]]
      #
      #
      #     for (dim in dims) {
      #
      #       table$addColumn(name = paste0(dim),
      #                       type = 'text')
      #     }
      #
      #
      #     for (name in names) {
      #
      #       row <- list()
      #
      #       for(j in seq_along(dims)){
      #
      #         row[[dims[j]]] <- coef[name,j]
      #
      #       }
      #
      #       table$addRow(rowKey=name, values=row)
      #
      #     }
      #
      #  },
      #
      #
      # # S.E of coefficients--------
      #
      # .populateLeTable= function(results) {
      #
      #   table <- self$results$le
      #
      #   le <- results$le
      #
      #   coef<- as.data.frame(le)
      #
      #
      #   names <- dimnames(coef)[[1]]
      #
      #
      #   dims <- dimnames(coef)[[2]]
      #
      #   for (dim in dims) {
      #
      #     table$addColumn(name = paste0(dim),
      #                     type = 'text')
      #   }
      #
      #
      #   for (name in names) {
      #
      #     row <- list()
      #
      #     for(j in seq_along(dims)){
      #
      #       row[[dims[j]]] <- coef[name,j]
      #
      #     }
      #
      #     table$addRow(rowKey=name, values=row)
      #
      #   }
      #
      # },
      #
      
      
      
      # populate class probability table---------------
      
      .populateClassTable = function(results) {
        classprob <- results$classprob
        
        classprob <- as.data.frame(classprob)
        
        names <- dimnames(classprob)[[1]]
        
        #creating table--------
        
        table <- self$results$pro$cp
        
        for (name in names) {
          row <- list()
          
          row[['value']] <- classprob[name, 1]
          
          table$addRow(rowKey = name, values = row)
          
        }
        
      },
      
      # populate item probability table---------------
      
      .populateItemTable = function(results) {
        tables <- self$results$ip
        
        itemprob <- results$itemprob
        
        vars <- self$options$vars
        
        for (i in seq_along(vars)) {
          item <- results$item[[vars[i]]]
          
          
          table <- tables[[i]]
          
          names <- row.names(item)
          dims <- colnames(item)
          
          
          for (dim in dims) {
            table$addColumn(name = paste0(dim),
                            type = 'text',
                            combineBelow = TRUE)
          }
          
          
          for (name in names) {
            row <- list()
            
            for (j in seq_along(dims)) {
              row[[dims[j]]] <- item[name, j]
              
            }
            
            table$addRow(rowKey = name, values = row)
            
          }
          
        }
        
      },
      
      
      
      # populate cell frequencies------------
      
      .populateCfTable = function(results) {
        table <- self$results$pro$cf
        
        cell <- results$cell
        
        cell <- as.data.frame(cell)
        
        names <-  dimnames(cell)[[1]]
        dims <- dimnames(cell)[[2]]
        
        
        for (dim in dims) {
          table$addColumn(name = paste0(dim), type = 'number')
        }
        
        
        for (name in names) {
          row <- list()
          
          for (j in seq_along(dims)) {
            row[[dims[j]]] <- cell[name, j]
            
          }
          
          table$addRow(rowKey = name, values = row)
          
        }
        
        
        
      },
      
      
      # populate class membership-------
      
      
      .populateOutputs = function(results) {
        cm <- results$cm
        
        if (self$options$cm
            && self$results$cm$isNotFilled()) {
          self$results$cm$setValues(cm)
          
          self$results$cm$setRowNums(rownames(data))
          
        }
      },
      
      # Predicted cell percentages in a latent class model---
      
      # .populateCellOutputs = function(results) {
      #
      #   pc <- results$pc
      #
      #   if (self$options$pc
      #       && self$results$pc$isNotFilled()) {
      #
      #
      #     self$results$pc$setValues(pc)
      #
      #     self$results$pc$setRowNums(rownames(data))
      #
      #   }
      # },
      
      # Posterior probabilities---------
      
      
      # .populatePosteriorOutputs= function(results) {
      #
      #   post <- results$post
      #
      #   if (self$options$post
      #       && self$results$post$isNotFilled()) {
      #
      #     keys <- 1:self$options$nc
      #     measureTypes <- rep("continuous", self$options$nc)
      #
      #     titles <- paste(.("Class"), keys)
      #     descriptions <- paste(.("Class"), keys)
      #
      #     self$results$post$set(
      #       keys=keys,
      #       titles=titles,
      #       descriptions=descriptions,
      #       measureTypes=measureTypes
      #     )
      #
      #     self$results$post$setRowNums(rownames(data))
      #
      #     for (i in 1:self$options$nc) {
      #       scores <- as.numeric(post[, i])
      #       self$results$post$setValues(index=i, scores)
      #     }
      #
      #
      #   }
      # },
      #
      #
      .plot = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        
        x <- image$state
        
        ### plot function-----
        
        poLCA.makeplot.dich <-
          function(probs, P, y, ti) {
            R <- nrow(probs[[1]])
            pi.class <- matrix(NA, nrow = length(probs), ncol = R)
            for (j in 1:length(probs)) {
              pi.class[j, ] <- probs[[j]][, 2]
            }
            dimnames(pi.class) <- list(names(y), round(P, 4))
            ds.plot <- data.frame(
              Classes = as.vector(col(pi.class)),
              Manifest.variables = as.vector(row(pi.class)),
              value = as.vector(pi.class)
            )
            vis <- scatterplot3d::scatterplot3d(
              ds.plot,
              type = "h",
              lwd = 5,
              pch = " ",
              x.ticklabs = colnames(pi.class),
              y.ticklabs = colnames(y),
              z.ticklabs = " ",
              xlab = "Classes; population share",
              ylab = "Manifest variables",
              zlab = "Pr(outcome)",
              color = 2,
              main = ti,
              y.margin.add = 0.2,
              mar = c(6, 3, 3, 3),
              lab = c(R - 1, ncol(y) - 1),
              zlim = c(0, 1),
              box = FALSE,
              cex.main = 1,
              angle = 83
            )
          }
        
        poLCA.makeplot.poly <-
          function(probs, r, y, K.j, ti) {
            pi.class <- matrix(NA, nrow = length(probs), ncol = max(K.j))
            for (j in 1:length(probs)) {
              pi.class[j, 1:K.j[j]] <- probs[[j]][r, ]
            }
            dimnames(pi.class) <- list(as.character(c(1:ncol(y))), as.character(c(1:max(K.j))))
            ds.plot <- data.frame(
              Manifest.variables = as.vector(row(pi.class)),
              Outcomes = as.vector(col(pi.class)),
              value = as.vector(pi.class)
            )
            vis <- scatterplot3d::scatterplot3d(
              ds.plot,
              type = "h",
              lwd = 5,
              pch = " ",
              x.ticklabs = colnames(y),
              y.ticklabs = colnames(pi.class),
              z.ticklabs = " ",
              xlab = "Manifest variables",
              zlab = "Pr(outcome)",
              main = ti,
              cex.main = 1.5,
              color = 2,
              lab = c(ncol(y) - 1, max(K.j) - 1),
              zlim = c(0, 1),
              box = FALSE,
              angle = 75,
              mar = c(3, 3, 2, 3)
            )
          }
        
        plot.poLCA <-
          function(x, ...) {
            K.j <- sapply(x$probs, ncol)
            R <- length(x$P)
            if (max(K.j) == 2) {
              poLCA.makeplot.dich(x$probs, x$P, x$y, NULL)
            } else {
              layout(matrix(seq(1, (R + 1)), R + 1, 1), heights = c(rep(5, R), 1))
              for (r in 1:R) {
                poLCA.makeplot.poly(x$probs,
                                    r,
                                    x$y,
                                    K.j,
                                    paste("Class ", r, ": population share = ", round(x$P[r], 3), sep = ""))
              }
            }
            par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
          }
        
        plot <-  plot.poLCA(x)
        
        print(plot)
        TRUE
        
        
      },
      
      
      .plot1 = function(image1, ggtheme, theme, ...) {
        if (is.null(image1$state))
          return(FALSE)
        
        
        lcModelProbs <- image1$state
        
        plot1 <- ggplot2::ggplot(lcModelProbs, aes(x = Class, y = value, fill = Level)) +
          geom_bar(stat = "identity", position = "stack") +
          facet_wrap( ~ L1) +
          scale_x_discrete("Class", expand = c(0, 0)) +
          scale_y_continuous("Proportion", expand = c(0, 0)) +
          #  scale_fill_discrete("Factor Level") +
          theme_bw()
        
        plot1 <- plot1 + ggtheme
        
        if (self$options$angle > 0) {
          plot1 <- plot1 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        
        
        print(plot1)
        TRUE
        
      },
      
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (is.null(image2$state))
          return(FALSE)
        
        
        profile <- image2$state
        
        plot2 <- ggplot2::ggplot(profile, aes(x = Variable, y = value, group = Class)) +
          facet_wrap( ~ Level) +
          geom_line(aes(color = Class), size = 1.1) +
          geom_point(aes(color = Class), size = 3)
        
        plot2 <- plot2 + ggtheme
        
        if (self$options$angle > 0) {
          plot2 <- plot2 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        
        print(plot2)
        TRUE
        
      },
      
      .plot3 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        elbow <- image$state
        
        
        plot3 <- ggplot2::ggplot(elbow, ggplot2::aes(x = Class, y = Value, color = Fit)) +
          ggplot2::geom_line(size = 1.1) +
          ggplot2::geom_point(size = 3) +
          ggplot2::scale_x_continuous(breaks = seq(1, length(elbow$Class), by = 1))
        
        # plot3 <- ggplot2::ggplot(elbow,aes(x = Class, y = Value, group = Fit))+
        #   geom_line(size=1.1,aes(color=Fit))+
        #   geom_point(size=3,aes(color=Fit))
        #
        
        plot3 <- plot3 + ggtheme
        
        
        print(plot3)
        TRUE
        
      },
      
      ### Helper functions =================================
      
      .cleanData = function() {
        data <- list()
        
        if (!is.null(self$options$covs))
          
          for (cov in self$options$covs)
            data[[cov]] <- jmvcore::toNumeric(self$data[[cov]])
        
        for (var in self$options$vars)
          
          data[[var]] <- jmvcore::toNumeric(self$data[[var]])
        
        attr(data, 'row.names') <- seq_len(length(data[[1]]))
        attr(data, 'class') <- 'data.frame'
        
        if (!is.null(self$options$covs))
          for (cov in self$options$covs)
            data <- data[!is.na(data[[cov]]), ]
        
        return(data)
      }
      
      
      
    )
  )
