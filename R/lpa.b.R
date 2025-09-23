
#' @importFrom tidyLPA get_data

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
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div></div>'
            )
          )
        )
        
        if (isTRUE(self$options$plot))  self$results$plot$setSize(self$options$width,  self$options$height)
        if (isTRUE(self$options$plot1)) self$results$plot1$setSize(self$options$width1, self$options$height1)
        if (isTRUE(self$options$plot2)) self$results$plot2$setSize(self$options$width2, self$options$height2)
        if (isTRUE(self$options$plot3)) self$results$plot3$setSize(self$options$width3, self$options$height3)
        if (isTRUE(self$options$plot4)) self$results$plot4$setSize(self$options$width4, self$options$height4)
        if (isTRUE(self$options$plot5)) self$results$plot5$setSize(self$options$width5, self$options$height5)
      },
      
      .run = function() {
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
          pc_data <- tidyLPA::get_data(all$res)
          n_row <- nrow(self$data)
          not_na_idx <- which(stats::complete.cases(self$data[, self$options$vars, drop=FALSE]))
          pc_vec <- rep(NA, n_row)
          pc_vec[not_na_idx] <- as.factor(pc_data$Class)
          self$results$pc$setRowNums(rownames(self$data))
          self$results$pc$setValues(pc_vec)
        }
        
        if (isTRUE(self$options$plot)) {
          pc_data <- tidyLPA::get_data(all$res)
          n_row <- nrow(self$data)
          not_na_idx <- which(stats::complete.cases(self$data[, self$options$vars, drop=FALSE]))
          pc_vec <- rep(NA, n_row)
          pc_vec[not_na_idx] <- as.factor(pc_data$Class)
          self$results$plot$setState(pc_vec)
        }
        
        # Posterior probabilities---
        if (isTRUE(self$options$post)) {
          post_data <- tidyLPA::get_data(all$res, "posterior_probabilities")
          post_cols <- grep("^CPROB", names(post_data))
          K <- if (!is.null(self$options$nc)) self$options$nc else if (!is.null(self$options$nclass)) self$options$nclass else 2
          if (length(post_cols) == 0) post_cols <- seq_len(min(K, ncol(post_data)))
          post_data <- post_data[, post_cols, drop = FALSE]
          
          n_row <- nrow(self$data)
          not_na_idx <- which(stats::complete.cases(self$data[, self$options$vars, drop=FALSE]))
          post_mat <- matrix(NA, nrow = n_row, ncol = ncol(post_data))
          post_mat[not_na_idx, ] <- as.matrix(post_data)
          
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
        # 3-step 보조분석 (BCH / DCAT)
        # =========================
        if (isTRUE(self$options$use3step)) {
          aux_name <- self$options$auxVar
          
          if (!is.null(aux_name) && aux_name %in% names(self$data)) {
            post_df <- try(tidyLPA::get_data(all$res, "posterior_probabilities"), silent = TRUE)
            if (!inherits(post_df, "try-error") && !is.null(post_df)) {
              
              # posterior 열
              post_cols <- grep("^CPROB", names(post_df))
              K <- if (!is.null(self$options$nc)) self$options$nc else if (!is.null(self$options$nclass)) self$options$nclass else 2
              if (length(post_cols) == 0) post_cols <- seq_len(min(K, ncol(post_df)))
              P_core <- as.matrix(post_df[, post_cols, drop = FALSE])
              
              # 지표 결측 제외 행과 정합
              not_na_idx <- which(stats::complete.cases(self$data[, self$options$vars, drop=FALSE]))
              P <- matrix(NA_real_, nrow(self$data), ncol(P_core))
              P[not_na_idx, ] <- P_core
              colnames(P) <- paste0("Class", seq_len(ncol(P)))
              
              aux <- self$data[[aux_name]]
              lines <- c()
              
              if (is.numeric(aux)) {
                # -------- BCH: class-wise weighted means + 유의성 + 다중비교 보정 + Cohen's d --------
                lines <- c(lines, sprintf("Distal: %s (numeric, BCH)", aux_name))
                
                mu <- se <- Neff <- rep(NA_real_, ncol(P))
                SSW <- 0
                w_all_sum <- 0; y_w_sum <- 0
                for (k in seq_len(ncol(P))) {
                  w <- P[, k]; v <- aux
                  ok <- (!is.na(w)) & (!is.na(v))
                  if (sum(ok) > 0 && sum(w[ok], na.rm=TRUE) > 0) {
                    mu[k]  <- stats::weighted.mean(v[ok], w[ok], na.rm=TRUE)
                    vcent  <- v[ok] - mu[k]
                    Neff[k] <- (sum(w[ok], na.rm=TRUE)^2) / pmax(sum(w[ok]^2, na.rm=TRUE), .Machine$double.eps)
                    s2     <- sum(w[ok] * vcent^2, na.rm=TRUE) / pmax(sum(w[ok], na.rm=TRUE), .Machine$double.eps)
                    se[k]  <- sqrt(s2 / max(1, Neff[k]))
                    SSW    <- SSW + sum(w[ok] * vcent^2, na.rm=TRUE)
                    # overall mean용 누적
                    w_all_sum <- w_all_sum + sum(w[ok], na.rm=TRUE)
                    y_w_sum   <- y_w_sum + sum(w[ok] * v[ok], na.rm=TRUE)
                  } else {
                    mu[k] <- se[k] <- Neff[k] <- NA_real_
                  }
                  lines <- c(lines, sprintf("  Class %d: mean=%.4f, SE=%.4f, N_eff=%.1f", k, mu[k], se[k], Neff[k]))
                }
                # Overall weighted ANOVA 근사
                mu_all <- y_w_sum / pmax(w_all_sum, .Machine$double.eps)
                SSB <- sum(Neff * (mu - mu_all)^2, na.rm=TRUE)
                df1 <- max(1, ncol(P) - 1)
                df2 <- max(1, sum(Neff, na.rm=TRUE) - ncol(P))
                Fval <- (SSB/df1) / (SSW/df2)
                pF   <- stats::pf(Fval, df1, df2, lower.tail = FALSE)
                lines <- c(lines, sprintf("Overall test (approx. weighted ANOVA): F(%d, %.1f)=%.3f, p=%.3g", df1, df2, Fval, pF))
                
                # Pairwise: z, p, BH/Bonf, Cohen's d
                if (ncol(P) >= 2) {
                  labs <- c(); zvec <- c(); pvec <- c(); dvec <- c()
                  for (a in 1:(ncol(P)-1)) for (b in (a+1):ncol(P)) {
                    if (is.finite(mu[a]) && is.finite(mu[b]) && is.finite(se[a]) && is.finite(se[b])) {
                      z  <- (mu[a] - mu[b]) / sqrt(se[a]^2 + se[b]^2)
                      pz <- 2*stats::pnorm(-abs(z))
                      # Cohen's d (weighted pooled SD, Neff 기반 근사)
                      sp <- sqrt(((Neff[a]-1)* (se[a]^2 * max(1, Neff[a])) + (Neff[b]-1)* (se[b]^2 * max(1, Neff[b]))) /
                                   pmax(Neff[a] + Neff[b] - 2, 1))
                      # 위 sp 계산은 s^2 ≈ se^2 * n 근사를 사용 (가중근사 안정화)
                      d  <- (mu[a] - mu[b]) / sp
                      labs <- c(labs, sprintf("%d vs %d", a, b))
                      zvec <- c(zvec, z); pvec <- c(pvec, pz); dvec <- c(dvec, d)
                    }
                  }
                  if (length(pvec)) {
                    pBH   <- stats::p.adjust(pvec, method = "BH")
                    pBonf <- stats::p.adjust(pvec, method = "bonferroni")
                    lines <- c(lines, "Pairwise (z, p, p_BH, p_Bonf, Cohen's d):")
                    for (i in seq_along(labs)) {
                      lines <- c(lines, sprintf("  Class %s: z=%.3f, p=%.3g, p_BH=%.3g, p_Bonf=%.3g, d=%.3f",
                                                labs[i], zvec[i], pvec[i], pBH[i], pBonf[i], dvec[i]))
                    }
                  }
                }
                
              } else {
                # -------- DCAT: 전체 χ² + Cramér's V, 쌍별 χ² + BH/Bonf + V --------
                f <- droplevels(as.factor(aux))
                lines <- c(lines, sprintf("Distal: %s (categorical, DCAT)", aux_name))
                
                Kc <- ncol(P); Lv <- levels(f)
                counts <- matrix(0, nrow=Kc, ncol=length(Lv), dimnames=list(paste0("Class", 1:Kc), Lv))
                rowsum <- rep(0, Kc)
                for (k in seq_len(Kc)) {
                  w <- P[, k]
                  ok <- (!is.na(w)) & (!is.na(f))
                  if (sum(ok) > 0) {
                    rowsum[k] <- sum(w[ok], na.rm=TRUE)
                    tmp <- tapply(w[ok], f[ok], sum)
                    tmp[is.na(tmp)] <- 0
                    counts[k, names(tmp)] <- as.numeric(tmp)
                    prop <- counts[k, ] / ifelse(rowsum[k] > 0, rowsum[k], NA_real_)
                    lines <- c(lines, sprintf("  Class %d:", k))
                    for (j in seq_along(Lv))
                      lines <- c(lines, sprintf("    %s : %.4f", Lv[j], prop[j]))
                  } else {
                    lines <- c(lines, sprintf("  Class %d: insufficient data", k))
                  }
                }
                # Overall chi-square
                colsum <- colSums(counts, na.rm=TRUE)
                grand  <- sum(rowsum, na.rm=TRUE)
                E <- outer(rowsum, colsum) / ifelse(grand > 0, grand, NA_real_)
                chi <- sum((counts - E)^2 / pmax(E, .Machine$double.eps), na.rm=TRUE)
                df  <- (Kc - 1) * (length(Lv) - 1)
                pchi <- stats::pchisq(chi, df=df, lower.tail=FALSE)
                Vall <- sqrt( chi / (grand * max(1, min(Kc-1, length(Lv)-1))) )
                lines <- c(lines, sprintf("Overall test (weighted chi-square): χ²(%d)=%.3f, p=%.3g, Cramer's V=%.3f",
                                          df, chi, pchi, Vall))
                
                # Pairwise 2×C
                if (Kc >= 2) {
                  labs <- c(); pvec <- c(); X2vec <- c(); dfvec <- c(); Vvec <- c()
                  for (a in 1:(Kc-1)) for (b in (a+1):Kc) {
                    sub <- rbind(counts[a, ], counts[b, ])
                    rs  <- rowSums(sub); cs <- colSums(sub); g <- sum(rs)
                    Eab <- outer(rs, cs) / ifelse(g > 0, g, NA_real_)
                    chiab <- sum((sub - Eab)^2 / pmax(Eab, .Machine$double.eps), na.rm=TRUE)
                    dfab  <- length(Lv) - 1
                    pab   <- stats::pchisq(chiab, df=dfab, lower.tail=FALSE)
                    Vab   <- sqrt( chiab / pmax(g * 1, .Machine$double.eps) ) # 2×C → min=1
                    labs  <- c(labs, sprintf("%d vs %d", a, b))
                    pvec  <- c(pvec, pab); X2vec <- c(X2vec, chiab); dfvec <- c(dfvec, dfab); Vvec <- c(Vvec, Vab)
                  }
                  if (length(pvec)) {
                    pBH   <- stats::p.adjust(pvec, method = "BH")
                    pBonf <- stats::p.adjust(pvec, method = "bonferroni")
                    lines <- c(lines, "Pairwise (chi-square; p, p_BH, p_Bonf, Cramer's V):")
                    for (i in seq_along(labs)) {
                      lines <- c(lines, sprintf("  Class %s: χ²(%d)=%.3f, p=%.3g, p_BH=%.3g, p_Bonf=%.3g, V=%.3f",
                                                labs[i], dfvec[i], X2vec[i], pvec[i], pBH[i], pBonf[i], Vvec[i]))
                    }
                  }
                }
              }
              
              # ==== 결과를 r.yaml의 use3step(Preformatted) 슬롯에 출력 ====
              if (!is.null(self$results$use3step)) {
                self$results$use3step$setContent(paste(lines, collapse = "\n"))
              }
            }
          } else {
            if (!is.null(self$results$use3step))
              self$results$use3step$setContent("[3-step] auxVar not selected or invalid — skipped.")
          }
        }
        
        
      },
      
      .computeRES = function() {
        # --- 지표만 추출, numeric만 허용 ---
        vars <- self$options$vars
        if (is.null(vars) || length(vars) < 2)
          jmvcore::reject("Select at least two indicator variables.")
        
        datX <- self$data[, vars, drop = FALSE]
        non_num <- names(datX)[!vapply(datX, is.numeric, logical(1))]
        if (length(non_num) > 0)
          jmvcore::reject(paste0("Indicators must be numeric only. Non-numeric: ",
                                 paste(non_num, collapse = ", ")))
        datX <- jmvcore::naOmit(datX)
        
        nc <- if (!is.null(self$options$nc)) self$options$nc else if (!is.null(self$options$nclass)) self$options$nclass else 2
        variances <- self$options$variances
        covariances <- self$options$covariances
        
        # Progress bar 시작
        self$results$progressBarHTML$setVisible(TRUE)
        self$results$progressBarHTML$setContent(progressBarH(10, 100, 'Starting profile estimation...'))
        private$.checkpoint()
        
        set.seed(1234)
        out <- NULL
        for (i in 1:nc) {
          self$results$progressBarHTML$setContent(progressBarH(10 + (i / nc) * 40, 100, paste('Computing model for', i, 'classes...')))
          private$.checkpoint()
          
          temp_res <- tidyLPA::estimate_profiles(
            datX,
            n_profiles = i,
            variances = variances,
            covariances = covariances
          )
          
          if (i == nc) res <- temp_res
          
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
        
        # 최종 모델 추정
        self$results$progressBarHTML$setContent(progressBarH(60, 100, 'Computing final profile model...'))
        private$.checkpoint()
        res <- tidyLPA::estimate_profiles(datX, nc, variances = variances, covariances = covariances)
        
        # Best fit 비교/요약
        self$results$progressBarHTML$setContent(progressBarH(80, 100, 'Comparing model solutions...'))
        private$.checkpoint()
        best <- tidyLPA::estimate_profiles(datX, n_profiles = 2:nc, models = c(1, 2, 3, 6))
        sol <- tidyLPA::compare_solutions(best)
        self$results$text$setContent(sol)
        bestfit <- as.data.frame(tidyLPA::get_fit(best))
        
        # 완료
        self$results$progressBarHTML$setContent(progressBarH(100, 100, 'Profile estimation complete!'))
        private$.checkpoint()
        self$results$progressBarHTML$setVisible(FALSE)
        
        list(
          res = res,
          bestfit = bestfit,
          elbow_data = out
        )
      },
      
      # ----- Plotters -----
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        Class <- image$state
        freq_table <- as.data.frame(table(Class))
        freq_table$Percentage <- (freq_table$Freq / sum(freq_table$Freq)) * 100
        freq_table$Label <- sprintf("%d (%.1f%%)", freq_table$Freq, freq_table$Percentage)
        plot <- ggplot(freq_table, aes(x = Class, y = Freq)) +
          geom_bar(stat = "identity", fill = "deepskyblue") +
          geom_text(aes(label = Label, vjust = -0.5)) +
          labs(title = "", x = "Class", y = "Frequency") +
          theme_minimal()
        plot <- plot + ggtheme
        print(plot)
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
        if (is.null(image5$state)) return(FALSE)
        res <- image5$state
        
        model_name <- names(res)[grepl("^model_1_class_", names(res))][1]
        if (is.na(model_name)) return(FALSE)
        
        estimates <- res[[model_name]][["estimates"]]
        if (is.null(estimates)) return(FALSE)
        
        means_df <- estimates[estimates$Category == "Means", c("Class", "Parameter", "Estimate")]
        if (nrow(means_df) == 0) return(FALSE)
        
        means_df$Estimate <- as.numeric(as.character(means_df$Estimate))
        means_df <- means_df[!is.na(means_df$Estimate), ]
        means_df$Centered <- stats::ave(means_df$Estimate, means_df$Parameter, FUN = function(x) x - mean(x))
        
        plot5 <- ggplot(means_df, aes(x = Parameter, y = Centered, group = Class, color = factor(Class))) +
          geom_line(size = 1.2) + geom_point(size = 3) + geom_hline(yintercept = 0, linetype = "dashed") +
          labs(y = "Deviation from Variable Mean", x = "Variables", color = "Class") + theme_minimal()
        
        if (self$options$angle > 0) {
          plot5 <- plot5 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        
        print(plot5)
        TRUE
      }
    )
  )

# Progress Bar HTML 함수
progressBarH <- function(progress = 0, total = 100, message = '') {
  percentage <- round(progress / total * 100)
  width <- 400 * percentage / 100
  
  paste0(
    '<div style="text-align: center; padding: 20px;">',
    '<div style="width: 400px; height: 20px; border: 1px solid #ccc; ',
    'background-color: #f8f9fa; margin: 0 auto; border-radius: 4px;">',
    '<div style="width: ', width, 'px; height: 18px; ',
    'background-color: #999999; border-radius: 3px; ',
    'transition: width 0.3s ease;"></div>',
    '</div>',
    '<div style="margin-top: 8px; font-size: 12px; color: #666;">',
    message, ' (', percentage, '%)</div>',
    '</div>'
  )
}
