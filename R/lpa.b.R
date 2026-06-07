
#' @importFrom tidyLPA get_data

# ---------- 범용 텍스트 테이블 함수 ----------
text_table <- function(df, title=NULL) {
  stopifnot(is.data.frame(df))
  out <- capture.output(print(df, row.names=FALSE))
  if (!is.null(title))
    out <- c(title, out)
  paste(out, collapse="\n")
}


lpaClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "lpaClass",
    inherit = lpaBase,
    
    private = list(
      .allCache   = NULL,
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) || is.null(self$options$vars))
          self$results$instructions$setVisible(TRUE)
        
        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title = "Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              '<ul>',
              '<li><b>tidyLPA</b> R package is described in the <a href="https://cran.r-project.org/web/packages/tidyLPA/vignettes/Introduction_to_tidyLPA.html" target = "_blank">page</a>.</li>',
              '<li>Four models(1,2,3,6) are specified using <b>mclust</b> R package.</li>',
              '<li>3-step auxiliary results are provided as approximate posterior-probability-based comparisons and should be interpreted with caution for strict methodological applications.</li>',
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div></div>'
            )
          )
        )
        
      },
      
      .run = function() {
        
        if (!isTRUE(self$options$run))
          return()
        
        if (length(self$options$vars) < 2)
          return()
        
        if (is.null(private$.allCache)) {
          private$.allCache <- private$.computeRES()
        }
        
        all <- private$.allCache
        
        if (is.null(all) || is.null(all$res))
          return()
        
        # Overall model fit table---
        if (isTRUE(self$options$overall)) {
          table <- self$results$overall
          f <- all$bestfit
          if (!is.null(f)) {
            lapply(rownames(f), function(name) {
              row <- list(
                model = f[name, 1],
                classes = f[name, 2],
                log = f[name, 3],
                aic = f[name, 4],
                awe = f[name, 5],
                bic = f[name, 6],
                caic = f[name, 7],
                clc = f[name, 8],
                kic = f[name, 9],
                sabic = f[name, 10],
                icl = f[name, 11],
                entropy = f[name, 12]
              )
              table$addRow(rowKey = name, values = row)
            })
          }
        }
        
        # Fit measures----------
        if (isTRUE(self$options$fit)) {
          table <- self$results$fit
          df <- as.data.frame(all$res[[1]]$fit)
          lapply(rownames(df), function(name) {
            row <- list(value = df[name, 1])
            table$addRow(rowKey = name, values = row)
          })
        }
        
        # Estimates---
        if (isTRUE(self$options$est)) {
          table <- self$results$est
          set.seed(1234)
          e <- as.data.frame(tidyLPA::get_estimates(all$res))
          lapply(rownames(e), function(name) {
            row <- list(
              cat = e[name, 1],
              par = e[name, 2],
              est = e[name, 3],
              se = e[name, 4],
              p = e[name, 5],
              cl = e[name, 6],
              model = e[name, 7],
              cla = e[name, 8]
            )
            table$addRow(rowKey = name, values = row)
          })
        }
        
        # person class---------
        if (isTRUE(self$options$pc)) {
          base::options(max.print = .Machine$integer.max)
          #pc_data <- tidyLPA::get_data(all$res)
          pc_data <- all$class_data
          
          # n_row <- nrow(self$data)
          # not_na_idx <- which(stats::complete.cases(self$data[, self$options$vars, drop=FALSE]))
          # pc_vec <- rep(NA, n_row)
          # pc_vec[not_na_idx] <- as.factor(pc_data$Class)
          # self$results$pc$setRowNums(rownames(self$data))
          # self$results$pc$setValues(pc_vec)
          n_row <- nrow(self$data)
          analysis_rows <- all$analysis_rows
          
          pc_vec <- rep(NA, n_row)
          pc_vec[analysis_rows] <- as.factor(pc_data$Class)
          
          self$results$pc$setRowNums(rownames(self$data))
          self$results$pc$setValues(pc_vec)
          
          }
        
        if (isTRUE(self$options$plot)) {
          pc_data <- tidyLPA::get_data(all$res)
          # n_row <- nrow(self$data)
          # not_na_idx <- which(stats::complete.cases(self$data[, self$options$vars, drop=FALSE]))
          # pc_vec <- rep(NA, n_row)
          # pc_vec[not_na_idx] <- as.factor(pc_data$Class)
          # self$results$plot$setState(pc_vec)
          n_row <- nrow(self$data)
          analysis_rows <- all$analysis_rows
          
          pc_vec <- rep(NA, n_row)
          pc_vec[analysis_rows] <- as.factor(pc_data$Class)
          
          self$results$plot$setState(pc_vec)        
          
          }
        
        # Posterior probabilities---
        if (isTRUE(self$options$post)) {
          #post_data <- tidyLPA::get_data(all$res, "posterior_probabilities")
          post_data <- all$post_data
          
          post_cols <- grep("^CPROB", names(post_data))
          K <- if (!is.null(self$options$nc)) self$options$nc else if (!is.null(self$options$nclass)) self$options$nclass else 2
          if (length(post_cols) == 0) post_cols <- seq_len(min(K, ncol(post_data)))
          post_data <- post_data[, post_cols, drop = FALSE]
          
          # n_row <- nrow(self$data)
          # not_na_idx <- which(stats::complete.cases(self$data[, self$options$vars, drop=FALSE]))
          # post_mat <- matrix(NA, nrow = n_row, ncol = ncol(post_data))
          # post_mat[not_na_idx, ] <- as.matrix(post_data)
          n_row <- nrow(self$data)
          analysis_rows <- all$analysis_rows
          
          post_mat <- matrix(NA, nrow = n_row, ncol = ncol(post_data))
          post_mat[analysis_rows, ] <- as.matrix(post_data)
          
                    
          if (self$results$post$isNotFilled()) {
            keys <- seq_len(ncol(post_data))
            self$results$post$set(
              keys = keys,
              titles = paste("Class", keys),
              descriptions = paste("Class", keys),
              measureTypes = rep("continuous", length(keys))
            )
            self$results$post$setRowNums(rownames(self$data))
            for (i in seq_len(ncol(post_data))) {
              self$results$post$setValues(index = i, as.numeric(post_mat[, i]))
            }
          }
        }
        
        # Latent profile plots ----------
        if (isTRUE(self$options$plot1)) self$results$plot1$setState(all$res)
        if (isTRUE(self$options$plot4)) self$results$plot4$setState(all$res)
        if (isTRUE(self$options$plot5)) self$results$plot5$setState(all$res)
        
        if (isTRUE(self$options$plot2)) {
          out <- private$.allCache$elbow_data
          out1 <- out[, c(3:10, 12)]
          colnames(out1) <- c('AIC','AWE','BIC','CAIC','CLC','KIC','SABIC','ICL','Class')
          elbow <- reshape2::melt(out1, id.vars = 'Class', variable.name = "Fit", value.name = 'Value')
          self$results$plot2$setState(elbow)
        }
        if (isTRUE(self$options$plot3)) self$results$plot3$setState(all$res)
        
        # =========================
        # 3-step auxiliary analysis (BCH / DCAT)
        # =========================
        if (isTRUE(self$options$use3step)) {
          aux_name <- self$options$auxVar
          if (!is.null(aux_name) && aux_name %in% names(self$data)) {
            
            #post_df <- try(tidyLPA::get_data(all$res, "posterior_probabilities"), silent = TRUE)
            post_df <- all$post_data
            
            if (!inherits(post_df, "try-error") && !is.null(post_df)) {
              
              post_cols <- grep("^CPROB", names(post_df))
              K <- if (!is.null(self$options$nc)) self$options$nc else if (!is.null(self$options$nclass)) self$options$nclass else 2
              if (length(post_cols) == 0) post_cols <- seq_len(min(K, ncol(post_df)))
              P_core <- as.matrix(post_df[, post_cols, drop = FALSE])
              
              # not_na_idx <- which(stats::complete.cases(self$data[, self$options$vars, drop=FALSE]))
              # P <- matrix(NA_real_, nrow(self$data), ncol(P_core))
              # P[not_na_idx, ] <- P_core
              # colnames(P) <- paste0("Class", seq_len(ncol(P)))
              analysis_rows <- all$analysis_rows
              
              if (length(analysis_rows) != nrow(P_core)) {
                jmvcore::reject("3-step analysis failed: posterior probabilities and analysis rows do not match.")
              }
              
              P <- matrix(NA_real_, nrow(self$data), ncol(P_core))
              P[analysis_rows, ] <- P_core
              colnames(P) <- paste0("Class", seq_len(ncol(P)))
              
              
              
              aux <- self$data[[aux_name]]
              if (is.character(aux))
                aux <- factor(aux)
              
              # -------------------
              # Numeric distal (BCH)
              # -------------------
              if (is.numeric(aux)) {
                means_table <- self$results$lpa3_means
                omnibus_table <- self$results$lpa3_omnibus
                pairwise_table <- self$results$lpa3_pairwise
                
                if (!is.null(means_table)) {
                  means_table$setTitle("3-step auxiliary: Class-weighted distal summaries")
                }
                
                if (!is.null(omnibus_table)) {
                  omnibus_table$setTitle(
                    "3-step auxiliary: Overall test (V applies to categorical distal outcomes only)"
                  )
                }
                
                if (!is.null(pairwise_table)) {
                  pairwise_table$setTitle(
                    "3-step auxiliary: Pairwise comparisons (V applies to categorical distal outcomes only)"
                  )
                }
                
                # Class-weighted summaries for numeric distal outcome
                # Note: This is an approximate 3-step distal comparison
                # based on posterior-probability weights.
                
                mu <- sd <- se <- Neff <- rep(NA_real_, ncol(P))
                
                for (k in seq_len(ncol(P))) {
                  w <- P[, k]
                  v <- aux
                  ok <- (!is.na(w)) & (!is.na(v))
                  
                  if (sum(ok) > 0 && sum(w[ok], na.rm = TRUE) > 0) {
                    
                    mu[k] <- stats::weighted.mean(v[ok], w[ok])
                    
                    Neff[k] <- (sum(w[ok], na.rm = TRUE)^2) /
                      sum(w[ok]^2, na.rm = TRUE)
                    
                    s2 <- sum(w[ok] * (v[ok] - mu[k])^2, na.rm = TRUE) /
                      sum(w[ok], na.rm = TRUE)
                    
                    sd[k] <- sqrt(s2)
                    se[k] <- sd[k] / sqrt(Neff[k])
                  }
                }
                
                # Output class-weighted mean, SD, SE, and effective N
                if (!is.null(means_table)) {
                  means_table$setTitle("3-step auxiliary: Class-weighted distal summaries")
                  
                  rk <- 1
                  for (k in seq_along(mu)) {
                    
                    vals <- list(
                      Mean = mu[k],
                      SD = sd[k],
                      SE = se[k],
                      `Effective N` = Neff[k]
                    )
                    
                    for (nm in names(vals)) {
                      means_table$addRow(
                        rowKey = rk,
                        values = list(
                          class = paste0("Class ", k),
                          category = nm,
                          value = as.numeric(vals[[nm]])
                        )
                      )
                      rk <- rk + 1
                    }
                  }
                }
                
                # Wald-type omnibus test for equality of class-specific means
                valid <- is.finite(mu) & is.finite(se) & se > 0
                
                if (sum(valid) >= 2) {
                  
                  mu_v <- mu[valid]
                  se_v <- se[valid]
                  K_v <- length(mu_v)
                  
                  C <- cbind(diag(K_v - 1), -1)
                  diff <- C %*% mu_v
                  Vmat <- C %*% diag(se_v^2, nrow = K_v) %*% t(C)
                  
                  chiW <- try(
                    as.numeric(t(diff) %*% solve(Vmat, diff)),
                    silent = TRUE
                  )
                  
                  if (!inherits(chiW, "try-error") && is.finite(chiW)) {
                    
                    dfW <- K_v - 1
                    pW <- stats::pchisq(chiW, dfW, lower.tail = FALSE)
                    
                    if (!is.null(omnibus_table)) {
                      omnibus_table$addRow(
                        rowKey = 1,
                        values = list(
                          method = "PP-weighted Wald omnibus (approx.)",
                          statistic = as.numeric(chiW),
                          df = as.numeric(dfW),
                          p = as.numeric(pW)
                         )
                      )
                    }
                  }
                }
                
                
                labs <- c()
                X2vec <- c()
                pvec <- c()
                #Vvec <- c()
                
                for (a in 1:(ncol(P)-1)) for (b in (a+1):ncol(P)) {
                  if (is.finite(mu[a]) && is.finite(mu[b])) {
                    z <- (mu[a] - mu[b]) / sqrt(se[a]^2 + se[b]^2)
                    pz <- 2 * stats::pnorm(-abs(z))
                    labs <- c(labs, sprintf("%d vs %d", a, b))
                    X2vec <- c(X2vec, z^2)
                    pvec <- c(pvec, pz)
                    #Vvec <- c(Vvec, NA_real_)
                  }
                }
                
                if (length(pvec) && !is.null(pairwise_table)) {
                  p_bh <- p.adjust(pvec, "BH")
                  p_bonf <- p.adjust(pvec, "bonf")
                  for (i in seq_along(pvec)) {
                    pairwise_table$addRow(
                      rowKey = i,
                      values = list(
                        comparison = labs[i],
                        chi2 = as.numeric(X2vec[i]),
                        df = 1,
                        p = as.numeric(pvec[i]),
                        p_bh = as.numeric(p_bh[i]),
                        p_bonf = as.numeric(p_bonf[i])
                        #V = Vvec[i]
                      )
                    )
                  }
                }
              }
              
              # -------------------
              # Categorical distal (DCAT)
              # -------------------
              if (is.factor(aux)) {
                means_table <- self$results$lpa3_means
                omnibus_table <- self$results$lpa3_omnibus
                pairwise_table <- self$results$lpa3_pairwise
                
                if (!is.null(means_table))
                  means_table$setTitle("3-step auxiliary: Class-by-category proportions")
                
                Lv <- levels(aux)
                counts <- matrix(
                  0,
                  nrow = ncol(P),
                  ncol = length(Lv),
                  dimnames = list(paste0("Class", 1:ncol(P)), Lv)
                )
                rowsum <- rep(0, ncol(P))
                
                for (k in seq_len(ncol(P))) {
                  w <- P[,k]
                  ok <- (!is.na(w)) & (!is.na(aux))
                  rowsum[k] <- sum(w[ok])
                  tmp <- tapply(w[ok], aux[ok], sum)
                  counts[k, names(tmp)] <- tmp
                }
                
                if (!is.null(means_table)) {
                  rk <- 1
                  for (k in seq_len(nrow(counts))) {
                    for (j in seq_len(ncol(counts))) {
                      means_table$addRow(
                        rowKey = rk,
                        values = list(
                          class = paste0("Class ", k),
                          category = colnames(counts)[j],
                          #value = as.numeric(counts[k, j])
                          value = as.numeric(counts[k, j] / max(rowsum[k], .Machine$double.eps))
                        )
                      )
                      rk <- rk + 1
                    }
                  }
                }
                
                colsum <- colSums(counts)
                grand <- sum(rowsum)
                E <- outer(rowsum, colsum) / grand
                #chi <- sum((counts - E)^2 / E)
                chi <- sum((counts - E)^2 / pmax(E, .Machine$double.eps), na.rm = TRUE)
                df <- (ncol(P) - 1) * (length(Lv) - 1)
                pchi <- stats::pchisq(chi, df, lower.tail = FALSE)
                V <- sqrt(chi / (grand * min(ncol(P) - 1, length(Lv) - 1)))
                
                if (!is.null(omnibus_table)) {
                  omnibus_table$addRow(
                    rowKey = 1,
                    values = list(
                      method = "DCAT omnibus",
                      statistic = as.numeric(chi),
                      df = as.numeric(df),
                      p = as.numeric(pchi),
                      V = as.numeric(V)
                    )
                  )
                }
                
                labs <- c()
                X2vec <- c()
                pvec <- c()
                Vvec <- c()
                
                for (a in 1:(ncol(P)-1)) for (b in (a+1):ncol(P)) {
                  sub <- rbind(counts[a,], counts[b,])
                  rs <- rowSums(sub)
                  cs <- colSums(sub)
                  g <- sum(rs)
                  Eab <- outer(rs, cs) / g
                  #chiab <- sum((sub - Eab)^2 / Eab)
                  chiab <- sum((sub - Eab)^2 / pmax(Eab, .Machine$double.eps), na.rm = TRUE)
                  dfab <- length(Lv) - 1
                  pab <- stats::pchisq(chiab, dfab, lower.tail = FALSE)
                  Vab <- sqrt(chiab / (g * 1))
                  labs <- c(labs, sprintf("%d vs %d", a, b))
                  X2vec <- c(X2vec, chiab)
                  pvec <- c(pvec, pab)
                  Vvec <- c(Vvec, Vab)
                }
                
                if (length(pvec) && !is.null(pairwise_table)) {
                  p_bh <- p.adjust(pvec, "BH")
                  p_bonf <- p.adjust(pvec, "bonf")
                  for (i in seq_along(pvec)) {
                    pairwise_table$addRow(
                      rowKey = i,
                      values = list(
                        comparison = labs[i],
                        chi2 = as.numeric(X2vec[i]),
                        df = as.numeric(dfab),
                        p = as.numeric(pvec[i]),
                        p_bh = as.numeric(p_bh[i]),
                        p_bonf = as.numeric(p_bonf[i]),
                        V = as.numeric(Vvec[i])
                      )
                    )
                  }
                }
              }
            }
          }
        }
        
      },
      
      .computeRES = function() {
        
        vars <- self$options$vars
        if (is.null(vars) || length(vars) < 2)
          jmvcore::reject("Select at least two indicator variables.")
        
        # datX <- self$data[, vars, drop = FALSE]
        # non_num <- names(datX)[!vapply(datX, is.numeric, logical(1))]
        # if (length(non_num) > 0)
        #   jmvcore::reject(paste0("Indicators must be numeric only. Non-numeric: ",
        #                          paste(non_num, collapse = ", ")))
        # datX <- jmvcore::naOmit(datX)
        datX0 <- self$data[, vars, drop = FALSE]
        
        non_num <- names(datX0)[!vapply(datX0, is.numeric, logical(1))]
        if (length(non_num) > 0)
          jmvcore::reject(paste0("Indicators must be numeric only. Non-numeric: ",
                                 paste(non_num, collapse = ", ")))
        
        analysis_rows <- which(stats::complete.cases(datX0))
        datX <- datX0[analysis_rows, , drop = FALSE]
        
        if (nrow(datX) < 2)
          jmvcore::reject("Too few complete cases for the selected indicator variables.")
        
                
        nc <- if (!is.null(self$options$nc)) self$options$nc else if (!is.null(self$options$nclass)) self$options$nclass else 2
        variances <- self$options$variances
        covariances <- self$options$covariances
        
        # Progress bar 시작
        self$results$progressBarHTML$setVisible(TRUE)
        self$results$progressBarHTML$setContent(
        appleSpinnerH('Computing profile estimation...')
        )
        private$.checkpoint()
        
        set.seed(1234)
        out <- NULL
        res <- NULL
        
        for (i in 1:nc) {

          temp_res <- tidyLPA::estimate_profiles(
            datX,
            n_profiles = i,
            variances = variances,
            covariances = covariances
          )
          
          if (i == nc)
            res <- temp_res
          
          tr <- temp_res[[1]]
          df <- data.frame(tr$fit)
          df <- t(df)
          df <- data.frame(
            model = df[1],
            log = df[3],
            aic = df[4],
            awe = df[5],
            bic = df[6],
            caic = df[7],
            clc = df[8],
            kic = df[9],
            sabic = df[10],
            icl = df[11],
            entropy = df[12],
            class = df[2]
          )
          out <- if (is.null(out)) df else rbind(out, df)
        }
        
        #res <- tidyLPA::estimate_profiles(datX, nc, variances = variances, covariances = covariances)
        
        # Best fit 비교/요약
        best <- tidyLPA::estimate_profiles(datX, n_profiles = 2:nc, models = c(1, 2, 3, 6))
        sol <- tidyLPA::compare_solutions(best)
        self$results$text$setContent(sol)
        bestfit <- as.data.frame(tidyLPA::get_fit(best))
        
        
        # 100%: complete and hide
        self$results$progressBarHTML$setVisible(FALSE)
        
        class_data <- tidyLPA::get_data(res)
        post_data  <- tidyLPA::get_data(res, "posterior_probabilities")
        
        list(
          res = res,
          bestfit = bestfit,
          elbow_data = out,
          class_data = class_data,
          post_data = post_data,
          analysis_rows = analysis_rows
        )        

      },
      
      # ----- Plotters -----
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        Class <- image$state
        Class <- Class[!is.na(Class)]
        
        if (length(Class) == 0)
          return(FALSE)
        
        freq_table <- as.data.frame(table(Class))
        names(freq_table) <- c("Class", "Freq")
        
        freq_table$Percentage <- (freq_table$Freq / sum(freq_table$Freq)) * 100
        freq_table$Label <- sprintf("%d (%.1f%%)", freq_table$Freq, freq_table$Percentage)
        
        max_freq <- max(freq_table$Freq, na.rm = TRUE)
        y_pad <- max(3, max_freq * 0.08)
        
        # 큰 막대는 라벨을 막대 안쪽에 배치
        # 작은 막대는 라벨을 막대 위쪽에 배치
        freq_table$label_inside <- freq_table$Freq > max_freq * 0.20
        
        freq_table$label_y <- ifelse(
          freq_table$label_inside,
          freq_table$Freq - y_pad,
          freq_table$Freq + y_pad
        )
        
        freq_table$label_vjust <- ifelse(
          freq_table$label_inside,
          1,
          0
        )
        
        y_top <- max(
          max_freq * 1.18,
          max(freq_table$label_y, na.rm = TRUE) + y_pad
        )
        
        plot <- ggplot(freq_table, aes(x = Class, y = Freq)) +
          geom_bar(
            stat = "identity",
            fill = "deepskyblue"
          ) +
          geom_text(
            aes(
              y = label_y,
              label = Label,
              vjust = label_vjust
            ),
            size = 3.5
          ) +
          labs(
            title = "",
            x = "Class",
            y = "Frequency"
          ) +
          coord_cartesian(
            ylim = c(0, y_top),
            clip = "off"
          ) +
          theme_minimal() +
          theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.margin = margin(25, 25, 25, 25)
          )
        
        print(
          plot + ggtheme +
            theme(
              plot.margin = margin(25, 25, 25, 25)
            )
        )
        
        TRUE
      },
      
      .plot3 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        res1 <- image$state
        plot3 <- tidyLPA::plot_density(res1)
        print(plot3)
        TRUE
      },
      
      .plot2 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        elbow <- image$state
        plot2 <- ggplot2::ggplot(elbow, ggplot2::aes(x = Class, y = Value, color = Fit)) +
          ggplot2::geom_line(size = 1.1) +
          ggplot2::geom_point(size = 3) +
          ggplot2::scale_x_continuous(breaks = seq(1, length(elbow$Class), by = 1))
        plot2 <- plot2 + ggtheme
        print(plot2)
        TRUE
      },
      
      .plot1 = function(image1, ggtheme, theme, ...) {
        if (is.null(image1$state))
          return(FALSE)
        res <- image1$state
        line <- self$options$line
        plot1 <- tidyLPA::plot_profiles(res, add_line = FALSE, rawdata = FALSE)
        if (line == 'TRUE') {
          plot1 <- tidyLPA::plot_profiles(res, add_line = TRUE, rawdata = FALSE)
        }
        if (self$options$angle > 0) {
          plot1 <- plot1 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        print(plot1)
        TRUE
      },
      
      .plot4 = function(image4, ggtheme, theme, ...) {
        if (is.null(image4$state))
          return(FALSE)
        res <- image4$state
        plot4 <- tidyLPA::plot_profiles(
          res,
          ci = NULL,
          sd = FALSE,
          add_line = TRUE,
          rawdata = FALSE
        ) +
          aes(linetype = 'solid', linewidth = 1.3, size=3) +
          scale_linewidth_identity() +
          scale_linetype_identity() +
          scale_size_identity()
        if (self$options$angle > 0) {
          plot4 <- plot4 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        print(plot4)
        TRUE
      },
      
      .plot5 = function(image5, ggtheme, theme, ...) {
        if (is.null(image5$state))
          return(FALSE)
        
        res <- image5$state
        
        model_candidates <- names(res)[vapply(names(res), function(nm) {
          obj <- res[[nm]]
          is.list(obj) && !is.null(obj[["estimates"]])
        }, logical(1))]
        
        if (length(model_candidates) == 0)
          return(FALSE)
        
        model_name <- NULL
        for (nm in model_candidates) {
          est <- res[[nm]][["estimates"]]
          if (!is.null(est) && "Category" %in% names(est)) {
            if (any(est$Category == "Means", na.rm = TRUE)) {
              model_name <- nm
              break
            }
          }
        }
        
        if (is.null(model_name))
          return(FALSE)
        
        estimates <- res[[model_name]][["estimates"]]
        if (is.null(estimates))
          return(FALSE)
        
        means_df <- estimates[estimates$Category == "Means", c("Class", "Parameter", "Estimate")]
        if (nrow(means_df) == 0)
          return(FALSE)
        
        means_df$Estimate <- suppressWarnings(as.numeric(as.character(means_df$Estimate)))
        means_df <- means_df[!is.na(means_df$Estimate), , drop = FALSE]
        if (nrow(means_df) == 0)
          return(FALSE)
        
        means_df$Centered <- stats::ave(
          means_df$Estimate,
          means_df$Parameter,
          FUN = function(x) x - mean(x, na.rm = TRUE)
        )
        
        plot5 <- ggplot(means_df, aes(x = Parameter, y = Centered, group = Class, color = factor(Class))) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          geom_hline(yintercept = 0, linetype = "dashed") +
          labs(y = "Deviation from Variable Mean", x = "Variables", color = "Class") +
          theme_minimal()
        
        if (self$options$angle > 0) {
          plot5 <- plot5 + ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1)
          )
        }
        
        print(plot5)
        TRUE
      }
    )
  )

# Progress Bar HTML 함수
appleSpinnerH <- function(message = '') {
  paste0(
    '<div style="text-align:center;padding:24px;">',
    
    '<style>',
    '@keyframes snowsoftAppleDotPulse {',
    '0%, 80%, 100% { transform: scale(0.72); opacity: 0.55; }',
    '40% { transform: scale(1.20); opacity: 1; }',
    '}',
    '</style>',
    
    '<div style="margin-bottom:10px;">',
    
    '<span style="',
    'display:inline-block;',
    'width:12px;',
    'height:12px;',
    'margin:0 5px;',
    'border-radius:50%;',
    'background:#007AFF;',
    'animation:snowsoftAppleDotPulse 1.2s infinite ease-in-out;',
    'vertical-align:middle;',
    '"></span>',
    
    '<span style="',
    'display:inline-block;',
    'width:12px;',
    'height:12px;',
    'margin:0 5px;',
    'border-radius:50%;',
    'background:#34C759;',
    'animation:snowsoftAppleDotPulse 1.2s infinite ease-in-out;',
    'animation-delay:0.15s;',
    'vertical-align:middle;',
    '"></span>',
    
    '<span style="',
    'display:inline-block;',
    'width:12px;',
    'height:12px;',
    'margin:0 5px;',
    'border-radius:50%;',
    'background:#FF9500;',
    'animation:snowsoftAppleDotPulse 1.2s infinite ease-in-out;',
    'animation-delay:0.30s;',
    'vertical-align:middle;',
    '"></span>',
    
    '</div>',
    
    '<div style="font-size:12px;color:#666;">',
    message,
    '</div>',
    
    '</div>'
  )
}