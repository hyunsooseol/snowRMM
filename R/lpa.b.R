
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
              '<li>Models with many indicators or classes, particularly those with complex covariance structures, may require substantial computation time or may fail to converge. In such cases, consider fewer classes or a simpler variance-covariance structure.</li>',
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
                model = f[name, "Model"],
                classes = f[name, "Classes"],
                log = f[name, "LogLik"],
                aic = f[name, "AIC"],
                awe = f[name, "AWE"],
                bic = f[name, "BIC"],
                caic = f[name, "CAIC"],
                clc = f[name, "CLC"],
                kic = f[name, "KIC"],
                sabic = f[name, "SABIC"],
                icl = f[name, "ICL"],
                entropy = f[name, "Entropy"]
              )
              table$addRow(rowKey = name, values = row)
            })
          }
        }
        
        # Fit measures----------
        if (isTRUE(self$options$fit)) {
          table <- self$results$fit
          fit_df <- as.data.frame(all$res[[1]]$fit)
          
          if (nrow(fit_df) > 0) {
            lapply(names(fit_df), function(name) {
              value <- suppressWarnings(
                as.numeric(as.character(fit_df[[name]][1]))
              )
              
              if (length(value) == 1 && is.finite(value)) {
                table$addRow(
                  rowKey = name,
                  values = list(value = value)
                )
              }
            })
          }
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
          K <- if (!is.null(self$options$nc)) self$options$nc else 2
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
      
      # Recalculate mclust fit indices with the authoritative number of
      # estimated parameters. This avoids covariance parameters being counted
      # twice in tidyLPA versions whose EEE/VVV parameter-count calculation
      # uses both triangles of the symmetric covariance matrix.
      .correctMclustFit = function(profile, nvars) {
        if (is.null(profile$fit))
          stop("The LPA profile does not contain fit information.")
        
        fit <- as.data.frame(profile$fit)
        required <- c(
          "Model", "Classes", "LogLik", "parameters", "n", "Entropy",
          "AIC", "AWE", "BIC", "CAIC", "CLC", "KIC", "SABIC", "ICL"
        )
        missing <- setdiff(required, names(fit))
        if (nrow(fit) < 1 || length(missing) > 0) {
          stop(
            paste0(
              "The LPA fit output is missing required values: ",
              paste(missing, collapse = ", "), "."
            )
          )
        }
        
        model_number <- as.integer(fit$Model[1])
        classes <- as.integer(fit$Classes[1])
        model_name <- c(
          `1` = "EEI",
          `2` = "VVI",
          `3` = "EEE",
          `6` = "VVV"
        )[as.character(model_number)]
        
        if (length(model_name) != 1 || is.na(model_name))
          stop(paste0("Unsupported mclust model number: ", model_number, "."))
        
        loglik <- as.numeric(fit$LogLik[1])
        n <- as.numeric(fit$n[1])
        entropy <- as.numeric(fit$Entropy[1])
        
        if (
          !is.finite(loglik) || !is.finite(n) || n <= 0 ||
          !is.finite(entropy) || !is.finite(nvars) || nvars < 1 ||
          !is.finite(classes) || classes < 1
        ) {
          stop("The LPA fit output contains invalid values.")
        }
        
        parameters <- as.numeric(
          mclust::nMclustParams(
            modelName = unname(model_name),
            d = as.integer(nvars),
            G = classes
          )
        )
        
        if (length(parameters) != 1 || !is.finite(parameters))
          stop("The number of mclust parameters could not be determined.")
        
        aic <- -2 * loglik + 2 * parameters
        awe <- -2 * (loglik + entropy) +
          2 * parameters * (3 / 2 + log(n))
        bic <- -2 * loglik + parameters * log(n)
        caic <- -2 * loglik + parameters * (log(n) + 1)
        clc <- -2 * loglik + 2 * entropy
        kic <- -2 * loglik + 3 * (parameters + 1)
        sabic <- -2 * loglik + parameters * log((n + 2) / 24)
        
        # tidyLPA defines ICL as -BIC plus a posterior-classification term.
        if (classes == 1) {
          icl <- -bic
        } else {
          post_prob <- try(profile$model$z, silent = TRUE)
          if (
            inherits(post_prob, "try-error") ||
            is.null(dim(post_prob)) || nrow(post_prob) < 1
          )
            stop("Posterior probabilities required for ICL are unavailable.")
          
          max_prob <- apply(
            post_prob,
            1,
            function(x) x[which.max(x)]
          )
          if (any(!is.finite(max_prob)))
            stop("Posterior probabilities required for ICL are invalid.")
          
          class_indicator <- post_prob == apply(post_prob, 1, max)
          icl <- -bic +
            2 * sum(class_indicator * log(max_prob + 1e-12))
        }
        
        corrected <- c(
          parameters = parameters,
          AIC = aic,
          AWE = awe,
          BIC = bic,
          CAIC = caic,
          CLC = clc,
          KIC = kic,
          SABIC = sabic,
          ICL = icl
        )
        
        for (name in names(corrected))
          fit[1, name] <- unname(corrected[name])
        
        # Internal consistency check for every recalculated fit index.
        expected <- c(
          AIC = -2 * loglik + 2 * parameters,
          AWE = -2 * (loglik + entropy) +
            2 * parameters * (3 / 2 + log(n)),
          BIC = -2 * loglik + parameters * log(n),
          CAIC = -2 * loglik + parameters * (log(n) + 1),
          CLC = -2 * loglik + 2 * entropy,
          KIC = -2 * loglik + 3 * (parameters + 1),
          SABIC = -2 * loglik + parameters * log((n + 2) / 24)
        )
        actual <- vapply(
          names(expected),
          function(name) as.numeric(fit[[name]][1]),
          numeric(1)
        )
        tolerance <- 1e-8 * pmax(1, abs(expected))
        if (any(!is.finite(actual)) || any(abs(actual - expected) > tolerance))
          stop("The corrected LPA fit indices failed the consistency check.")
        
        profile$fit <- fit
        profile
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
          appleSpinnerH('Computing LPA...')
        )
        on.exit(
          self$results$progressBarHTML$setVisible(FALSE),
          add = TRUE
        )
        private$.checkpoint()
        
        set.seed(1234)
        
        if (!is.finite(nc) || nc < 2)
          jmvcore::reject("The number of classes must be at least 2.")
        
        selected_model <- if (identical(variances, "equal") && identical(covariances, "zero")) {
          1L
        } else if (identical(variances, "varying") && identical(covariances, "zero")) {
          2L
        } else if (identical(variances, "equal") && identical(covariances, "equal")) {
          3L
        } else if (identical(variances, "varying") && identical(covariances, "varying")) {
          6L
        } else {
          jmvcore::reject(
            paste0(
              "The selected variance/covariance combination is not available with mclust: ",
              "variances = '", variances, "', covariances = '", covariances, "'."
            )
          )
        }
        
        # Model 1, 2, 3, and 6 are retained for Overall Model Fit.
        # Each model is estimated once, and the selected K-class solution is
        # reused for the detailed output.
        comparison_models <- c(1L, 2L, 3L, 6L)
        best <- list()
        
        for (model_number in comparison_models) {
          self$results$progressBarHTML$setContent(
            appleSpinnerH(
              paste0(
                "Computing LPA... Model ", model_number,
                " (classes 2-", as.integer(nc), ")"
              )
            )
          )
          private$.checkpoint()
          
          model_results <- tryCatch(
            tidyLPA::estimate_profiles(
              datX,
              n_profiles = seq.int(2L, as.integer(nc)),
              models = model_number
            ),
            error = function(e) {
              jmvcore::reject(
                paste0(
                  "LPA estimation failed for Model ", model_number, ": ",
                  conditionMessage(e)
                )
              )
            }
          )
          
          model_results_class <- class(model_results)
          for (result_index in seq_along(model_results)) {
            model_results[[result_index]] <- private$.correctMclustFit(
              model_results[[result_index]],
              nvars = ncol(datX)
            )
          }
          class(model_results) <- model_results_class
          
          best <- c(best, unclass(model_results))
          private$.checkpoint()
        }
        class(best) <- c("tidyLPA", "list")
        
        bestfit <- as.data.frame(tidyLPA::get_fit(best))
        required_fit_columns <- c(
          "Model", "Classes", "LogLik", "AIC", "AWE", "BIC",
          "CAIC", "CLC", "KIC", "SABIC", "ICL", "Entropy"
        )
        missing_fit_columns <- setdiff(required_fit_columns, names(bestfit))
        if (length(missing_fit_columns) > 0) {
          jmvcore::reject(
            paste0(
              "LPA fit output is missing required columns: ",
              paste(missing_fit_columns, collapse = ", "), "."
            )
          )
        }
        
        selected_key <- paste0(
          "model_", selected_model, "_class_", as.integer(nc)
        )
        if (!selected_key %in% names(best)) {
          jmvcore::reject(
            paste0("The selected LPA solution was not returned: ", selected_key, ".")
          )
        }
        
        selected_profile <- best[[selected_key]]
        selected_fit <- as.data.frame(selected_profile$fit)
        if (
          is.null(selected_profile$dff) ||
          nrow(selected_fit) == 0 ||
          !"LogLik" %in% names(selected_fit) ||
          !is.finite(as.numeric(selected_fit$LogLik[1]))
        ) {
          jmvcore::reject(
            paste0(
              "The selected LPA solution did not converge (Model ",
              selected_model, ", ", as.integer(nc), " classes)."
            )
          )
        }
        
        res <- best[selected_key]
        class(res) <- class(best)
        
        # Elbow data for the selected variance/covariance model.
        selected_rows <-
          bestfit$Model == selected_model &
          bestfit$Classes >= 2 &
          bestfit$Classes <= nc
        selected_bestfit <- bestfit[selected_rows, , drop = FALSE]
        
        out <- data.frame(
          model = selected_bestfit$Model,
          log = selected_bestfit$LogLik,
          aic = selected_bestfit$AIC,
          awe = selected_bestfit$AWE,
          bic = selected_bestfit$BIC,
          caic = selected_bestfit$CAIC,
          clc = selected_bestfit$CLC,
          kic = selected_bestfit$KIC,
          sabic = selected_bestfit$SABIC,
          icl = selected_bestfit$ICL,
          entropy = selected_bestfit$Entropy,
          class = selected_bestfit$Classes
        )
        
        if (isTRUE(self$options$plot2)) {
          one_profile <- tryCatch(
            tidyLPA::estimate_profiles(
              datX,
              n_profiles = 1,
              variances = variances,
              covariances = covariances
            ),
            error = function(e) {
              jmvcore::reject(
                paste0("The one-class LPA solution failed: ", conditionMessage(e))
              )
            }
          )
          private$.checkpoint()
          
          one_profile_class <- class(one_profile)
          for (result_index in seq_along(one_profile)) {
            one_profile[[result_index]] <- private$.correctMclustFit(
              one_profile[[result_index]],
              nvars = ncol(datX)
            )
          }
          class(one_profile) <- one_profile_class
          
          one_fit <- as.data.frame(tidyLPA::get_fit(one_profile))
          missing_one_fit_columns <- setdiff(required_fit_columns, names(one_fit))
          if (length(missing_one_fit_columns) > 0) {
            jmvcore::reject(
              paste0(
                "The one-class LPA fit output is missing required columns: ",
                paste(missing_one_fit_columns, collapse = ", "), "."
              )
            )
          }
          
          out_one <- data.frame(
            model = one_fit$Model,
            log = one_fit$LogLik,
            aic = one_fit$AIC,
            awe = one_fit$AWE,
            bic = one_fit$BIC,
            caic = one_fit$CAIC,
            clc = one_fit$CLC,
            kic = one_fit$KIC,
            sabic = one_fit$SABIC,
            icl = one_fit$ICL,
            entropy = one_fit$Entropy,
            class = one_fit$Classes
          )
          out <- rbind(out_one, out)
        }
        
        out <- out[order(out$class), , drop = FALSE]
        rownames(out) <- NULL
        
        sol <- tryCatch(
          tidyLPA::compare_solutions(best),
          error = function(e) {
            jmvcore::reject(
              paste0("LPA model comparison failed: ", conditionMessage(e))
            )
          }
        )
        self$results$text$setContent(sol)
        
        
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
        
        plot <- ggplot2::ggplot(freq_table, ggplot2::aes(x = Class, y = Freq)) +
          ggplot2::geom_bar(
            stat = "identity",
            fill = "deepskyblue"
          ) +
          ggplot2::geom_text(
            ggplot2::aes(
              y = label_y,
              label = Label,
              vjust = label_vjust
            ),
            size = 3.5
          ) +
          ggplot2::labs(
            title = "",
            x = "Class",
            y = "Frequency"
          ) +
          ggplot2::coord_cartesian(
            ylim = c(0, y_top),
            clip = "off"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            panel.grid.minor.y = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            plot.margin = ggplot2::margin(25, 25, 25, 25)
          )
        
        plot <- plot + ggtheme +
          ggplot2::theme(
            plot.margin = ggplot2::margin(25, 25, 25, 25)
          )
        
        print(plot)
        TRUE
      },
      
      .plot3 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        res1 <- image$state
        
        plot3 <- tidyLPA::plot_density(res1)
        plot3 <- plot3 + ggtheme
        
        print(plot3)
        TRUE
      },
      
      .plot2 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        elbow <- image$state
        
        if (is.null(elbow) || nrow(elbow) == 0)
          return(FALSE)
        
        plot2 <- ggplot2::ggplot(
          elbow,
          ggplot2::aes(x = Class, y = Value, color = Fit)
        ) +
          ggplot2::geom_line(linewidth = 1.1) +
          ggplot2::geom_point(size = 3) +
          ggplot2::scale_x_continuous(
            breaks = sort(unique(elbow$Class))
          ) +
          ggplot2::labs(
            x = "Class",
            y = "Fit index",
            color = "Fit"
          )
        
        plot2 <- plot2 + ggtheme
        
        print(plot2)
        TRUE
      },
      
      .plot1 = function(image1, ggtheme, theme, ...) {
        if (is.null(image1$state))
          return(FALSE)
        
        res <- image1$state
        
        add_line <- isTRUE(self$options$line)
        
        plot1 <- tidyLPA::plot_profiles(
          res,
          add_line = add_line,
          rawdata = FALSE
        )
        
        plot1 <- plot1 + ggtheme
        
        if (self$options$angle > 0) {
          plot1 <- plot1 + ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = self$options$angle,
              hjust = 1
            )
          )
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
          ggplot2::aes(
            linetype = 'solid',
            linewidth = 1.3,
            size = 3
          ) +
          ggplot2::scale_linewidth_identity() +
          ggplot2::scale_linetype_identity() +
          ggplot2::scale_size_identity()
        
        plot4 <- plot4 + ggtheme
        
        if (self$options$angle > 0) {
          plot4 <- plot4 + ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = self$options$angle,
              hjust = 1
            )
          )
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
        
        means_df <- estimates[
          estimates$Category == "Means",
          c("Class", "Parameter", "Estimate")
        ]
        
        if (nrow(means_df) == 0)
          return(FALSE)
        
        means_df$Estimate <- suppressWarnings(
          as.numeric(as.character(means_df$Estimate))
        )
        
        means_df <- means_df[!is.na(means_df$Estimate), , drop = FALSE]
        
        if (nrow(means_df) == 0)
          return(FALSE)
        
        means_df$Class <- factor(means_df$Class)
        
        means_df$Centered <- stats::ave(
          means_df$Estimate,
          means_df$Parameter,
          FUN = function(x) x - mean(x, na.rm = TRUE)
        )
        
        plot5 <- ggplot2::ggplot(
          means_df,
          ggplot2::aes(
            x = Parameter,
            y = Centered,
            group = Class,
            color = Class
          )
        ) +
          ggplot2::geom_line(linewidth = 1.2) +
          ggplot2::geom_point(size = 3) +
          ggplot2::geom_hline(
            yintercept = 0,
            linetype = "dashed"
          ) +
          ggplot2::labs(
            y = "Deviation from Variable Mean",
            x = "Variables",
            color = "Class"
          ) +
          ggplot2::theme_minimal()
        
        plot5 <- plot5 + ggtheme
        
        if (self$options$angle > 0) {
          plot5 <- plot5 + ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = self$options$angle,
              hjust = 1
            )
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
