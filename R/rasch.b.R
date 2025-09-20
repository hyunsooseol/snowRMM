# This file is a generated template, your changes will not be overwritten
#' @import ggplot2


raschClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "raschClass",
    inherit = raschBase,
    
    private = list(
      .resCache = NULL,
      .ermCache = NULL,
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        private$.resCache <- NULL
        private$.ermCache <- NULL
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>The mixRasch R package was used for the Rasch model using joint maximum likelihood estimation(JMLE).</li>',
            '<li> Person analysis was also performed by using mixRasch R package .</li>',
            '<li>Specify </b> the number of <b>Step</b> and model <b>Type</b> in the analysis option.</li>',
            '<li>Step is defined as number of <b>category-1</b>.</li>',
            '<li>The minimum and maximum values of a category must be the same across all items for <b>rating sclaes</b> with <b>eRm</b> R package.</li>',
            '<li>The <b>eRm</b> R package was used for the person-item map for PCM.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
        ))
        
        if (self$options$rel)
          self$results$mf$rel$setNote("Note",
                                      "SSD=Squared Standard Deviation; MSE=Mean Squared Error.")
        
        if (isTRUE(self$options$inplot)) {
          width <- self$options$width
          height <- self$options$height
          
          self$results$inplot$setSize(width, height)
        }
        if (isTRUE(self$options$outplot)) {
          width <- self$options$width
          height <- self$options$height
          
          self$results$outplot$setSize(width, height)
        }
        if (isTRUE(self$options$plot4)) {
          width <- self$options$width1
          height <- self$options$height1
          
          self$results$plot4$setSize(width, height)
        }
        if (isTRUE(self$options$plot5)) {
          width <- self$options$width2
          height <- self$options$height2
          
          self$results$plot5$setSize(width, height)
        }
        if (isTRUE(self$options$wrightmap)) {
          width <- self$options$width3
          height <- self$options$height3
          
          self$results$plot$setSize(width, height)
        }
        if (isTRUE(self$options$piplot)) {
          width <- self$options$width4
          height <- self$options$height4
          
          self$results$piplot$setSize(width, height)
        }
        if (isTRUE(self$options$plot2)) {
          width <- self$options$width5
          height <- self$options$height5
          
          self$results$plot2$setSize(width, height)
        }
        if (isTRUE(self$options$plot3)) {
          width <- self$options$width5
          height <- self$options$height5
          
          self$results$plot3$setSize(width, height)
        }
        if (isTRUE(self$options$plot1)) {
          width <- self$options$width6
          height <- self$options$height6
          
          self$results$plot1$setSize(width, height)
        }
        if (isTRUE(self$options$gofplot)) {
          width <- self$options$width7
          height <- self$options$height7
          
          self$results$gofplot$setSize(width, height)
        }
        if (isTRUE(self$options$plot8)) {
          width <- self$options$width8
          height <- self$options$height8
          
          self$results$plot8$setSize(width, height)
        }
        if (isTRUE(self$options$plot9)) {
          width <- self$options$width9
          height <- self$options$height9
          
          self$results$plot9$setSize(width, height)
        }
      },
      
      .run = function() {
        
        if (is.null(self$options$vars) || length(self$options$vars) < 2)
          return()
        
        #Removing perfect score items before estimation (for example all 1 or 0)-------
        for (varName in self$options$vars) {
          var <- self$data[[varName]]
          if (length(unique(var)) < 2)
            stop(
              paste(
                "Variable '",
                varName,
                "' contains all the same value and should be removed in the variable box."
              )
            )
        }
        
        vars <- self$options$vars
        step <- self$options$step
        type <- self$options$type
        
        if (is.null(private$.resCache)) {
          private$.resCache <- private$.computeRES()
        }
        
        res <- private$.resCache
        
        if (is.null(private$.ermCache)) {
          private$.ermCache <- private$.computeERM()
        }
        erm <- private$.ermCache
        
        # item statistics---------
        imean <- res$item.par$itemDescriptives
        imeasure <- res$item.par$delta.i
        ise <- res$item.par$SE.delta.i
        infit <- res$item.par$in.out[, 1]
        outfit <- res$item.par$in.out[, 3]
        pbis <- res$item.par$itemDescriptives
        
        # populate Item Statistics table-----
        table <- self$results$items
        vars <- self$options$vars
        
        for (i in seq_along(vars)) {
          table$setRow(
            rowKey = vars[i],
            values = list(
              imean = imean[i, 1],
              imeasure = imeasure[i],
              ise = ise[i],
              infit = infit[i],
              outfit = outfit[i],
              pbis = pbis[i, 2]
            )
          )
        }
        
        # populate Model information table-----
        set.seed(1234)
        res0 <- mixRasch::getEstDetails(res)
        class <- res0$nC
        
        if (isTRUE(self$options$fit)) {
          aic <- res$info.fit$AIC
          bic <- res$info.fit$BIC
          caic <- res$info.fit$CAIC
          loglik <- res$info.fit$loglik
          parm <- res$info.fit$N.parms
          person <- res$info.fit$N.persons
          
          self$results$mf$model$setRow(
            rowNo = 1,
            values = list(
              class = class,
              aic = aic,
              bic = bic,
              caic = caic,
              loglik = loglik,
              parm = parm,
              person = person
            )
          )
        }
        
        # 수정된 Person analysis 부분 ---------
        # 클린 데이터와 결측값이 있는 원본 데이터 모두 필요
        cleanData <- private$.cleanData()
        
        ptotal <- res$person.par$r
        pmeasure <- res$person.par$theta
        pse <- res$person.par$SE.theta
        pinfit <- res$person.par$infit
        poutfit <- res$person.par$outfit
        
        # 원본 데이터의 행 수와 클린 데이터의 행 수가 다를 경우 처리
        originalRowCount <- nrow(self$data)
        cleanRowCount <- nrow(cleanData)
        
        # 결측값으로 인해 제거된 행의 인덱스 찾기
        if (originalRowCount != cleanRowCount) {
          # 결측값이 없는 행의 인덱스를 찾기
          complete_cases_index <- complete.cases(self$data[self$options$vars])
          valid_row_names <- rownames(self$data)[complete_cases_index]
          
          # Person analysis 결과를 원본 데이터 구조에 맞게 확장
          extended_ptotal <- rep(NA, originalRowCount)
          extended_pmeasure <- rep(NA, originalRowCount)
          extended_pse <- rep(NA, originalRowCount)
          extended_pinfit <- rep(NA, originalRowCount)
          extended_poutfit <- rep(NA, originalRowCount)
          
          # 유효한 케이스에만 값 할당
          extended_ptotal[complete_cases_index] <- ptotal
          extended_pmeasure[complete_cases_index] <- pmeasure
          extended_pse[complete_cases_index] <- pse
          extended_pinfit[complete_cases_index] <- pinfit
          extended_poutfit[complete_cases_index] <- poutfit
          
          # 확장된 벡터 사용
          ptotal <- extended_ptotal
          pmeasure <- extended_pmeasure
          pse <- extended_pse
          pinfit <- extended_pinfit
          poutfit <- extended_poutfit
        }
        
        if (isTRUE(self$options$ptotal)) {
          self$results$ptotal$setRowNums(rownames(self$data))
          self$results$ptotal$setValues(ptotal)
        }
        
        if (isTRUE(self$options$pmeasure)) {
          self$results$pmeasure$setRowNums(rownames(self$data))
          self$results$pmeasure$setValues(pmeasure)
        }
        
        if (isTRUE(self$options$pse)) {
          self$results$pse$setRowNums(rownames(self$data))
          self$results$pse$setValues(pse)
        }
        
        if (isTRUE(self$options$pinfit)) {
          self$results$pinfit$setRowNums(rownames(self$data))
          self$results$pinfit$setValues(pinfit)
        }
        
        if (isTRUE(self$options$poutfit)) {
          self$results$poutfit$setRowNums(rownames(self$data))
          self$results$poutfit$setValues(poutfit)
        }
        
        # Person fit plot4 수정----------------------
        if (isTRUE(self$options$plot4)) {
          # 결측값이 있는 경우 유효한 데이터만 사용
          valid_indices <- complete.cases(self$data[self$options$vars])
          valid_pmeasure <- res$person.par$theta
          valid_pinfit <- res$person.par$infit
          valid_poutfit <- res$person.par$outfit
          
          Measure <- valid_pmeasure
          Infit <- valid_pinfit
          Outfit <- valid_poutfit
          daf <- data.frame(Measure, Infit, Outfit)
          pf <- reshape2::melt(
            daf,
            id.vars = 'Measure',
            variable.name = "Fit",
            value.name = 'Value'
          )
          image <- self$results$plot4
          image$setState(pf)
        }
        
        ########## eRm R package######################################
        
        # person separation reliability using eRm R package---------
        
        if (self$options$step == 1) {
          set.seed(1234)
          pers <- eRm::person.parameter(erm$rasch)
          rel <- eRm::SepRel(pers)
        } else if (self$options$step > 1) {
          set.seed(1234)
          pers <- eRm::person.parameter(erm$pcm.res)
          rel <- eRm::SepRel(pers)
        }
        
        if (isTRUE(self$options$rel)) {
          ssd <- rel$SSD.PS
          mse <- rel$MSE
          re <- rel$sep.rel
          
          self$results$mf$rel$setRow(rowNo = 1,
                                     values = list(
                                       SSD = ssd,
                                       MSE = mse,
                                       Reliability = re
                                     ))
        }
        
        if (isTRUE(self$options$thr)) {
          tau <- res$item.par$tau
          tau <- t(tau)
          tau <- as.data.frame(tau)
          
          table <- self$results$thr
          nc <- ncol(tau)
          nCategory <- nc
          vars <- self$options$vars
          if (nCategory > 1) {
            for (i in 1:nCategory)
              table$addColumn(
                name = paste0("name", i),
                title = as.character(i),
                superTitle = 'Thresholds',
                type = 'number'
              )
          }
          for (i in seq_along(vars)) {
            row <- list()
            for (j in 1:nCategory) {
              row[[paste0("name", j)]] <- tau[i, j]
            }
            table$setRow(rowNo = i, values = row)
          }
        }
        
        ########################################################
        if (self$options$step == 1) {
          # LR test----------
          set.seed(1234)
          lr <- eRm::LRtest(erm$rasch, splitcr = self$options$lrsplit)
          table <- self$results$tm$lr
          table$setRow(rowNo = 1,
                       values = list(
                         value = lr$LR,
                         df = lr$df,
                         p = lr$pvalue
                       ))
          #Goodness-of-fit plot for LR test--------
          image <- self$results$gofplot
          image$setState(lr)
          
          # Martin-lof test--------------
          set.seed(1234)
          ml <- eRm::MLoef(erm$rasch, splitcr = self$options$mlsplit)
          self$results$tm$ml$setRow(rowNo = 1,
                                    values = list(
                                      value = ml$LR,
                                      df = ml$df,
                                      p = ml$p.value
                                    ))
          # Wald test----------
          vars <- self$options$vars
          set.seed(1234)
          w <- as.data.frame(eRm::Waldtest(erm$rasch, splitcr = self$options$waldsplit)$coef.table)
          table <- self$results$tm$wald
          for (i in seq_along(vars)) {
            table$addRow(rowKey = vars[i], values = list(item = w[[1]][i], p = w[[2]][i]))
          }
        }
        
        if (self$options$step > 1) {
          if (isTRUE(self$options$rsm)) {
            tab <- eRm::thresholds(erm$rsm.res)
            tab <- tab$threshtable
            rsm <- data.frame(Reduce(rbind, tab))
            rsm <- rsm[, -1]
            # number of category---------
            nc <- ncol(rsm)
            table <- self$results$rsm
            nCategory <- nc
            vars <- self$options$vars
            if (nCategory > 1) {
              for (i in 1:nCategory)
                table$addColumn(
                  name = paste0("name", i),
                  title = as.character(i),
                  superTitle = 'Thresholds',
                  type = 'number'
                )
            }
            for (i in seq_along(vars)) {
              row <- list()
              for (j in 1:nCategory) {
                row[[paste0("name", j)]] <- rsm[i, j]
              }
              table$setRow(rowNo = i, values = row)
            }
          }
          
          #RSM plot----------
          if (isTRUE(self$options$plot2)) {
            image <- self$results$plot2
            image$setState(erm$rsm.res)
          }
          
          # Testing LR test with Rating scale---
          
          if (isTRUE(self$options$lr1)) {
            set.seed(1234)
            lr1 <- eRm::LRtest(erm$rsm.res, splitcr = self$options$lrsplit1)
            
            table <- self$results$tm$lr1
            row <- list(value = lr1$LR,
                        df = lr1$df,
                        p = lr1$pvalue)
            table$setRow(rowNo = 1, values = row)
          }
        }
        
        if (isTRUE(self$options$pcm)) {
          tab1 <- eRm::thresholds(erm$pcm.res)
          tab1 <- tab1$threshtable
          pcm <- data.frame(Reduce(rbind, tab1))
          pcm <- pcm[, -1]
          nc <- ncol(pcm)
          nCategory <- nc
          table <- self$results$pcm
          nCategory <- nc
          vars <- self$options$vars
          if (nCategory > 1) {
            for (i in 1:nCategory)
              table$addColumn(
                name = paste0("name", i),
                title = as.character(i),
                superTitle = 'Thresholds',
                type = 'number'
              )
          }
          for (i in seq_along(vars)) {
            row <- list()
            for (j in 1:nCategory) {
              row[[paste0("name", j)]] <- pcm[i, j]
            }
            table$setRow(rowNo = i, values = row)
          }
        }
        
        # Rasch residual factor analysis using pairwise R package
        
        if (isTRUE(self$options$plot5)) {
          
          rf <- pairwise::rfa(
            pairwise::pers(pairwise::pair(private$.cleanData())),
            res = self$options$res
          )
          
          summ <- capture.output(summary(rf))
          
          body <- gsub("\\$eigen.values", 
                       "\n\n[Eigenvalues]\n", 
                       paste(summ, collapse="\n"))
          body <- gsub("\\$loadings", 
                       "\n\n[Loadings]\n", body)
          body <- gsub("\\$variance.proportion", 
                       "\n\n[Variance proportion]\n", body)
          body <- gsub("\\$variance.total", 
                       "\n\n[Variance total]\n", body)
          body <- gsub("\\$transposed", 
                       "\n\n[Transposed]\n", body)
          
          self$results$text$setContent(body)
          self$results$plot5$setState(rf)
        }
        
        
        
        
        
        # Q3 fit statistics proposed by Yen(1984)
        
        if (isTRUE(self$options$q3)) {
          
          data <- private$.cleanData()
          set.seed(1234)
          ip <- pairwise::pair(data)
          pers_obj <- pairwise::pers(ip)
          q <- pairwise::q3(pers_obj, 
                            res = self$options$res1)
          
          # Standardized correlation matrix
          ma <- q$resid_cor$cor
          
          if (isTRUE(self$options$cormatrix)) {
            table <- self$results$cormatrix
            
            ma <- as.data.frame(ma)
            names <- dimnames(ma)[[1]]
            dims <- dimnames(ma)[[2]]
            
            # creating table----------------
            
            for (dim in dims) {
              table$addColumn(name = paste0(dim), type = 'number')
            }
            
            for (name in names) {
              row <- list()
              for (j in seq_along(dims)) {
                row[[dims[j]]] <- ma[name, j]
              }
              table$addRow(rowKey = name, values = row)
            }
            
          }
          
          table <- self$results$q3
          
          if (is.null(self$options$q3))
            return()
          
          Mean <- q[["statistic"]]$Q3[1]
          Max <- q[["statistic"]]$Q3[2]
          Min <- q[["statistic"]]$Q3[3]
          Max_abs <- q[["statistic"]]$Q3[4]
          Min_abs <- q[["statistic"]]$Q3[5]
          Q3 <- q[["statistic"]]$Q3[6]
          
          row <- list()
          
          row[['Mean']] <- Mean
          row[['Max']] <- Max
          row[['Min']] <- Min
          row[['Max_abs']] <- Max_abs
          row[['Min_abs']] <- Min_abs
          row[['Q3']] <- Q3
          
          table$setRow(rowNo = 1, values = row)
        }
        
        if (isTRUE(self$options$plot8)) {
          # Rasch model, estimation of item and person parameters
          # Using eRm package
          
          if (self$options$step == 1 &&
              (self$options$type == 'RSM' ||
               self$options$type == 'PCM')) {
            set.seed(1234)
            p.res <- eRm::person.parameter(erm$rasch)
            item.fit <- eRm::itemfit(p.res)
            std.resids <- item.fit$st.res
            
          }
          
          if (self$options$step > 1 && self$options$type == 'RSM') {
            set.seed(1234)
            p.res <- eRm::person.parameter(erm$rsm.res)
            item.fit <- eRm::itemfit(p.res)
            std.resids <- item.fit$st.res
          }
          
          if (self$options$step > 1 && self$options$type == 'PCM') {
            set.seed(1234)
            p.res <- eRm::person.parameter(erm$pcm.res)
            item.fit <- eRm::itemfit(p.res)
            std.resids <- item.fit$st.res
          }
          image8 <- self$results$plot8
          image8$setState(std.resids)
        }
        
        if (isTRUE(self$options$nptest)) {
          n <- self$options$matrix
          method <- self$options$npmethod
          
          data <- private$.cleanData()
          rmat <- as.matrix(data)
          res <- eRm::NPtest(rmat, n = n, method = method)
          
          self$results$text1$setContent(res)
        }
        
      },
      
      # plot---
      
      .gofplot = function(image, ...) {
        lr <- image$state
        tlab <- self$options$tlab
        # additional 95 percent control line with user specified style
        gofplot <- eRm::plotGOF(lr,
                                tlab = tlab,
                                ctrline = list(
                                  gamma = 0.95,
                                  col = "red",
                                  lty = "dashed"
                                ))
        print(gofplot)
        TRUE
      },
      
      # Plot of standardized residuals using eRm package------
      
      .plot8 = function(image8, ...) {
        if (is.null(image8$state))
          return(FALSE)
        
        item.number <- self$options$num1
        std.resids <- image8$state
        # Before constructing the plots, find the maximum and minimum values of the standardized residuals to set limits for the axes:
        max.resid <- ceiling(max(std.resids))
        min.resid <- ceiling(min(std.resids))
        # The code below will produce standardized residual plots for each of the items:
        plot8 <- plot(
          std.resids[, item.number],
          ylim = c(min.resid, max.resid),
          main = paste("Standardized Residuals for Item ", item.number, sep = ""),
          ylab = "Standardized Residual",
          xlab = "Person Index"
        )
        abline(h = 0, col = "blue")
        abline(h = 2, lty = 2, col = "red")
        abline(h = -2,
               lty = 2,
               col = "red")
        
        legend(
          "topright",
          c("Std. Residual", "Observed = Expected", "+/- 2 SD"),
          pch = c(1, NA, NA),
          lty = c(NA, 1, 2),
          col = c("black", "blue", "red"),
          cex = .8
        )
        print(plot8)
        TRUE
      },
      
      # wrightmap Plot-----------
      
      .plot = function(image, ...) {
        res <- private$.computeRES()
        
        pmeasure <- res$person.par$theta
        imeasure <- res$item.par$delta.i
        itemNames <- self$options$vars
        
        plot <- ShinyItemAnalysis::ggWrightMap(
          theta = pmeasure,
          b = imeasure,
          item.names = itemNames,
          color = "deepskyblue"
        )
        print(plot)
        TRUE
      },
      
      # fit plot---
      
      .inPlot = function(image, ggtheme, theme, ...) {
        res <- private$.computeRES()
        infit <- res$item.par$in.out[, 1]
        
        item <- self$options$vars
        nitems <- length(item)
        
        infit_values <- numeric(nitems)
        
        for (i in 1:nitems) {
          infit_values[i] <- res$item.par$in.out[i, 1]  # 인덱싱 수정
        }
        infit1 <- data.frame(item = item, infit = infit_values)
        
        plot <- ggplot(infit1, aes(x = item, y = infit)) +
          # Refined circular point style
          geom_point(
            shape = 21,
            color = '#2c3e50',
            fill = '#3498db',
            size = 3.2,
            stroke = 1.1,
            alpha = 0.85
          ) +
          # Clean boundary lines
          geom_hline(
            yintercept = 1.5,
            linetype = "solid",
            color = '#e74c3c',
            linewidth = 1,
            alpha = 0.7
          ) +
          geom_hline(
            yintercept = 0.5,
            linetype = "solid",
            color = '#e74c3c',
            linewidth = 1,
            alpha = 0.7
          ) +
          ggtitle("") +
          theme_minimal() +
          theme(
            # Background settings
            panel.background = element_rect(fill = "#fafafa", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            
            # Grid line settings
            panel.grid.major.y = element_line(color = "#e8e8e8", linewidth = 0.4),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            
            # Title and axis label styling
            plot.title = element_text(
              hjust = 0.5,
              size = 15,
              face = "bold",
              color = "#2c3e50",
              margin = margin(b = 15)
            ),
            axis.title = element_text(size = 11, color = "#34495e"),
            axis.text = element_text(size = 9.5, color = "#7f8c8d"),
            axis.text.x = element_text(margin = margin(t = 6)),
            axis.text.y = element_text(margin = margin(r = 6)),
            
            # Border settings
            panel.border = element_rect(color = "#bdc3c7", fill = NA, linewidth = 0.4),
            
            # Margin adjustments
            plot.margin = margin(15, 15, 15, 15)
          )
        
        plot <- plot + ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        print(plot)
        TRUE
      },
      
      .outPlot = function(image, ggtheme, theme, ...) {
        res <- private$.computeRES()
        
        outfit <- res$item.par$in.out[, 3]
        item <- self$options$vars
        nitems <- length(item)
        outfit <- NA
        for (i in 1:nitems) {
          outfit[i] <- res$item.par$in.out[, 3][i]
        }
        outfit1 <- data.frame(item, outfit)
        
        plot <- ggplot(outfit1, aes(x = item, y = outfit)) +
          # Refined circular point style
          geom_point(
            shape = 21,
            color = '#2c3e50',
            fill = '#3498db',
            size = 3.2,
            stroke = 1.1,
            alpha = 0.85
          ) +
          # Clean boundary lines
          geom_hline(
            yintercept = 1.5,
            linetype = "solid",
            color = '#e74c3c',
            linewidth = 1,
            alpha = 0.7
          ) +
          geom_hline(
            yintercept = 0.5,
            linetype = "solid",
            color = '#e74c3c',
            linewidth = 1,
            alpha = 0.7
          ) +
          ggtitle("") +
          theme_minimal() +
          theme(
            # Background settings
            panel.background = element_rect(fill = "#fafafa", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            
            # Grid line settings
            panel.grid.major.y = element_line(color = "#e8e8e8", linewidth = 0.4),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            
            # Title and axis label styling
            plot.title = element_text(
              hjust = 0.5,
              size = 15,
              face = "bold",
              color = "#2c3e50",
              margin = margin(b = 15)
            ),
            axis.title = element_text(size = 11, color = "#34495e"),
            axis.text = element_text(size = 9.5, color = "#7f8c8d"),
            axis.text.x = element_text(margin = margin(t = 6)),
            axis.text.y = element_text(margin = margin(r = 6)),
            
            # Border settings
            panel.border = element_rect(color = "#bdc3c7", fill = NA, linewidth = 0.4),
            
            # Margin adjustments
            plot.margin = margin(15, 15, 15, 15)
          )
        
        plot <- plot + ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        print(plot)
        TRUE
      },
      
      # PREPARE PERSON-ITEM PLOT FOR PCM-------------
      
      .piPlot = function(image, ...) {
        erm <- private$.computeERM()
        autopcm <- erm$pcm.res
        plot <- eRm::plotPImap(autopcm, sorted = TRUE, warn.ord.colour = "red")
        print(plot)
        TRUE
      },
      
      .plot1 = function(image, ...) {
        erm <- private$.computeERM()
        rasch <- erm$rasch
        num <- self$options$num
        
        plot1 <- eRm::plotICC(
          rasch,
          item.subset = num,
          empICC = list(
            "raw",
            type = "b",
            col = "blue",
            lty = "dotted"
          ),
          empCI = list()
        )
        print(plot1)
        TRUE
      },
      
      .plot2 = function(image, ...) {
        num <- self$options$num
        if (self$options$step <= 1)
          return()
        rsm.res <- image$state
        plot2 <- eRm::plotICC(rsm.res, legpos = "top", item.subset = num)
        print(plot2)
        TRUE
      },
      
      .plot3 = function(image, ...) {
        num <- self$options$num
        if (self$options$step <= 1)
          return()
        
        erm <- private$.computeERM()
        pcm.res <- erm$pcm.res
        plot3 <- eRm::plotICC(pcm.res, legpos = "top", item.subset = num)
        print(plot3)
        TRUE
      },
      
      .plot4 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        pf <- image$state
        
        plot4 <- ggplot2::ggplot(pf, aes(x = Measure, y = Value, shape = Fit, color = Fit)) +
          # Modern point styling with refined shapes
          geom_point(
            size = 2.8,
            stroke = 1.2,
            alpha = 0.8
          ) +
          # Elegant shape and color mapping
          ggplot2::scale_shape_manual(
            values = c(16, 17),  # Circle and triangle instead of + and X
            name = "Fit"
          ) +
          ggplot2::scale_color_manual(
            values = c('#2980b9', '#e74c3c'),  # Blue and red
            name = "Fit"
          ) +
          # Clean boundary lines
          ggplot2::geom_hline(
            yintercept = 1.5,
            linetype = "solid",
            color = '#95a5a6',
            linewidth = 0.8,
            alpha = 0.8
          ) +
          ggplot2::geom_hline(
            yintercept = 0.5,
            linetype = "solid",
            color = '#95a5a6',
            linewidth = 0.8,
            alpha = 0.8
          ) +
          ggplot2::coord_cartesian(xlim = c(-4, 4), ylim = c(0, 4)) +
          ggtitle("Task Fit Plot") +
          labs(
            x = "Measure",
            y = "Value"
          ) +
          theme_minimal() +
          theme(
            # Background settings
            panel.background = element_rect(fill = "#fafafa", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            
            # Grid line settings
            panel.grid.major = element_line(color = "#f0f0f0", linewidth = 0.3),
            panel.grid.minor = element_blank(),
            
            # Title and axis label styling
            plot.title = element_text(
              hjust = 0.5,
              size = 15,
              face = "bold",
              color = "#2c3e50",
              margin = margin(b = 15)
            ),
            axis.title = element_text(size = 11, color = "#34495e"),
            axis.text = element_text(size = 9.5, color = "#7f8c8d"),
            
            # Legend styling
            legend.position = "right",
            legend.title = element_text(size = 11, color = "#34495e", face = "bold"),
            legend.text = element_text(size = 10, color = "#7f8c8d"),
            legend.background = element_rect(fill = "white", color = "#ecf0f1", linewidth = 0.3),
            legend.key = element_rect(fill = "transparent"),
            legend.margin = margin(10, 10, 10, 10),
            
            # Border settings
            panel.border = element_rect(color = "#bdc3c7", fill = NA, linewidth = 0.4),
            
            # Margin adjustments
            plot.margin = margin(15, 15, 15, 15)
          )
        
        plot4 <- plot4 + ggtheme
        print(plot4)
        TRUE
      },
      
      .plot5 = function(image5, ...) {
        if (is.null(image5$state))
          return(FALSE)
        Residuals <- image5$state
        plot5 <- plot(Residuals)
        print(plot5)
        TRUE
      },
      
      .prepareciPlot = function(data) {
        
        data <- private$.cleanData()
        
        D1 <- self$options$mea1
        D2 <- self$options$mea2
        SE1 <- self$options$se1
        SE2 <- self$options$se2
        
        data[[D1]] <- jmvcore::toNumeric(data[[D1]])
        data[[D2]] <- jmvcore::toNumeric(data[[D2]])
        data[[SE1]] <- jmvcore::toNumeric(data[[SE1]])
        data[[SE2]] <- jmvcore::toNumeric(data[[SE2]])
        
        dat <- data.frame(
          D1 = data[[D1]],
          D2 = data[[D2]],
          SE1 = data[[SE1]],
          SE2 = data[[SE2]]
        )
        
        # mean
        MEAN1 <- mean(dat$D1)
        MEAN2 <- mean(dat$D2)
        
        # Z-score for 95% confidence interval
        Z <- 1.96
        
        # SE AND Upper/Lower control line
        SE12 <- sqrt(dat$SE1 ^ 2 + dat$SE2 ^ 2)
        
        UPPER1 <- (dat$D1 + dat$D2) / 2 + MEAN1 - Z * SE12 / 2
        UPPER2 <- (dat$D1 + dat$D2) / 2 + MEAN2 + Z * SE12 / 2
        LOWER1 <- (dat$D1 + dat$D2) / 2 + MEAN1 + Z * SE12 / 2
        LOWER2 <- (dat$D1 + dat$D2) / 2 + MEAN2 - Z * SE12 / 2
        
        dat2 <- data.frame(D1 = dat$D1,
                           D2 = dat$D2,
                           UPPER1,
                           UPPER2,
                           LOWER1,
                           LOWER2)
        
        # arrange control line---
        control_upper <- dplyr::arrange(dat2, UPPER1)
        control_lower <- dplyr::arrange(dat2, LOWER1)
        
        image <- self$results$plot9
        
        state <- list(dat2, control_upper, control_lower)
        
        image$setState(state)
        
      },
      
      .plot9 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        dat2 <- image$state[[1]]
        control_upper <- image$state[[2]]
        control_lower <- image$state[[3]]
        
        plot9 <- ggplot() +
          geom_point(
            aes(x = dat2$D1, y = dat2$D2),
            color = "blue",
            alpha = 0.6,
            size = 2
          ) +
          geom_abline(slope = 1,
                      intercept = 0,
                      linetype = "dashed") +
          geom_line(
            data = control_upper,
            aes(x = dat2$UPPER1, y = dat2$UPPER2),
            color = "red",
            linetype = "solid"
          ) +
          geom_line(
            data = control_lower,
            aes(x = dat2$LOWER1, y = dat2$LOWER2),
            color = "red",
            linetype = "solid"
          ) +
          xlab("Measure 1") +
          ylab("Measure 2")
        
        plot9 <- plot9 + ggtheme
        print(plot9)
        TRUE
      },
      
      #### Helper functions =================================
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
      },
      
      .computeRES = function(data = NULL) {
        if (is.null(data))
          data <- private$.cleanData()
        set.seed(1234)
        
        res <- mixRasch::mixRasch(
          data = data,
          steps = self$options$step,
          model = self$options$type,
          n.c = 1
        )
        return(res)
      },
      
      .computeERM = function(data = NULL) {
        if (is.null(data))
          data <- private$.cleanData()
        
        rasch <- NULL
        pcm.res <- NULL
        rsm.res <- NULL
        
        if (self$options$step == 1) {
          set.seed(1234)
          rasch <- eRm::RM(data)
          
        } else if (self$options$step > 1) {
          set.seed(1234)
          pcm.res <- eRm::PCM(data)
          rsm.res <- eRm::RSM(data)
        }
        
        retlist <- list(rasch = rasch,
                        pcm.res = pcm.res,
                        rsm.res = rsm.res)
        return(retlist)
      }
    )
  )
