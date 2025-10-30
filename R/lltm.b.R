
lltmClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "lltmClass",
    inherit = lltmBase,
    private = list(
      .allCache = NULL,
      .htmlwidget = NULL,
      
      # ---------- helper: default contrasts (F1-F2, F2-F3, ...) ----------
      .buildDefaultContrasts = function(k, facNames = NULL) {
        if (k < 2L) return(matrix(numeric(0), nrow = 0, ncol = k))
        L <- matrix(0, nrow = k - 1L, ncol = k)
        for (j in 1:(k - 1L)) { L[j, j] <- 1; L[j, j + 1L] <- -1 }
        if (is.null(facNames)) facNames <- paste0("F", seq_len(k))
        colnames(L) <- facNames
        rownames(L) <- paste0(facNames[1:(k - 1L)], " - ", facNames[2:k])
        L
      },
      
      # ---------- helper: extract MSQ from eRm::itemfit() ----------
      # ---------- helper: extract MSQ from eRm::itemfit() ----------
      .extractMSQ = function(model, item_names) {
        # person.parameter() 객체 생성 필요
        p_res <- tryCatch(
          eRm::person.parameter(model),
          error = function(e) {
            warning("person.parameter() failed: ", e$message)
            return(NULL)
          }
        )
        if (is.null(p_res)) return(NULL)
        
        # person parameter 객체에 itemfit() 적용
        fit <- tryCatch(
          eRm::itemfit(p_res),
          error = function(e) {
            warning("itemfit() failed: ", e$message)
            return(NULL)
          }
        )
        if (is.null(fit)) return(NULL)
        
        infit_vals <- NULL
        outfit_vals <- NULL
        
        # 시도 1: 직접 필드 접근 (가장 일반적)
        if (!is.null(fit$i.infitMSQ)) infit_vals <- fit$i.infitMSQ
        if (!is.null(fit$i.outfitMSQ)) outfit_vals <- fit$i.outfitMSQ
        
        # 시도 2: i.fit 데이터프레임 내부
        if (is.null(infit_vals) && !is.null(fit$i.fit)) {
          df <- as.data.frame(fit$i.fit)
          cn <- tolower(colnames(df))
          
          outfit_idx <- which(cn %in% c("outfit", "outfitmsq", "outfit.msq", "outfitms"))[1]
          if (!is.na(outfit_idx)) outfit_vals <- df[[outfit_idx]]
          
          infit_idx <- which(cn %in% c("infit", "infitmsq", "infit.msq", "infitms"))[1]
          if (!is.na(infit_idx)) infit_vals <- df[[infit_idx]]
        }
        
        # 시도 3: 대문자 필드명
        if (is.null(infit_vals) && !is.null(fit$InfitMSQ)) infit_vals <- fit$InfitMSQ
        if (is.null(outfit_vals) && !is.null(fit$OutfitMSQ)) outfit_vals <- fit$OutfitMSQ
        
        # 시도 4: Outfit, Infit (컬럼명만)
        if (is.null(infit_vals) && !is.null(fit$Infit)) infit_vals <- fit$Infit
        if (is.null(outfit_vals) && !is.null(fit$Outfit)) outfit_vals <- fit$Outfit
        
        # 값이 없으면 NULL 반환
        if (is.null(infit_vals) || is.null(outfit_vals)) {
          warning("Could not extract Infit/Outfit MSQ from itemfit()")
          return(NULL)
        }
        
        # 길이 검증
        if (length(infit_vals) != length(item_names) || 
            length(outfit_vals) != length(item_names)) {
          warning(sprintf(
            "MSQ length mismatch: infit=%d, outfit=%d, items=%d",
            length(infit_vals), length(outfit_vals), length(item_names)
          ))
          return(NULL)
        }
        
        # 결과 데이터프레임 (rownames = item_names)
        data.frame(
          infit = as.numeric(infit_vals),
          outfit = as.numeric(outfit_vals),
          row.names = item_names,
          stringsAsFactors = FALSE
        )
      }, 
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
        }
        
        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title = "Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              '<ul>',
              '<li>Performs Linear Logistic Test Model (LLTM) for binary item responses by using CML estimation.</li>',
              '<li>Design matrix(W matrix) for the LLTM will be computed by specifying <b>Vectors and Number of columns</b>.</li>',
              '<li>Artificial data matrix and R codes for creating W matrix can be found in Data Library>snowRMM folder.</li>',
              '<li>A description of the LLTM is described in the <a href="https://share.google/C20YJlOBWOX3FfiAH" target = "_blank">paper</a>.</li>',
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div></div>'
            )
          )
        )
        
        if (self$options$items)
          self$results$ra$items$setNote(
            "Note",
            "Easiness parameters have opposite signs to difficulty parameters. Infit/Outfit MSQ indicate item-level model fit (values between 0.7–1.3 are typically acceptable)."
          )
        
        if (self$options$comp)
          self$results$ll$comp$setNote(
            "Note",
            "LLs= Conditional log-likelihoods; npar= Number of parameters; LR= Likelihood ratio statistics."
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
        
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
      },
      
      # --------------------------------------------------------------------
      .run = function() {
        vars <- self$options$vars
        
        # 변수 검증
        for (varName in vars) {
          var <- self$data[[varName]]
          if (length(unique(var)) < 2)
            stop(paste0(
              "Variable '", varName,
              "' contains all the same value and should be removed in the variable box."
            ))
        }
        
        if (is.null(vars) || length(vars) < 2) return()
        
        # 캐시
        if (is.null(private$.allCache)) {
          private$.allCache <- private$.computeRES()
        }
        all <- private$.allCache
        
        # -------- W diagnostics --------
        if (isTRUE(self$options$wdiag)) {
          tbl <- self$results$wdiag
          W <- all$W
          
          if (is.null(colnames(W))) colnames(W) <- paste0("F", seq_len(ncol(W)))
          if (is.null(rownames(W))) rownames(W) <- vars
          
          rnk <- tryCatch(qr(W)$rank, error = function(e) NA_integer_)
          kappa_val <- tryCatch(kappa(W), error = function(e) NA_real_)
          nz_by_factor <- colSums(W != 0)
          nz_by_item <- rowSums(W != 0)
          sparsity <- 1 - mean(W != 0)
          
          add <- function(m, v, n = "") {
            tbl$addRow(rowKey = m, values = list(metric = m, value = v, note = n))
          }
          
          add("rank(W)", rnk, 
              if (!is.na(rnk) && rnk < ncol(W)) "⚠ not full rank" else "OK")
          add("condition number", kappa_val,
              if (!is.na(kappa_val) && kappa_val > 30) "⚠ possible collinearity" else "OK")
          add("mean non-zero / factor", mean(nz_by_factor),
              if (!is.na(mean(nz_by_factor)) && mean(nz_by_factor) > 0) "OK" else "⚠ check data")
          add("mean non-zero / item", mean(nz_by_item),
              if (!is.na(mean(nz_by_item)) && mean(nz_by_item) > 0) "OK" else "⚠ check data")
          add("sparsity (1 - density)", sparsity,
              if (!is.na(sparsity) && sparsity < 0.9) "OK" else "⚠ too sparse")
        }
        
        # --------------------- Rasch Model ---------------------
        rasch <- all$rasch
        
        if (isTRUE(self$options$items)) {
          table <- self$results$ra$items
          rasch.item <- as.data.frame(rasch$betapar)
          se.beta <- rasch$se.beta
          ci <- as.data.frame(stats::confint(rasch, "beta"))
          
          # MSQ 값 초기화
          infit <- rep(NA_real_, length(vars))
          outfit <- rep(NA_real_, length(vars))
          
          # 캐시에서 MSQ 가져오기
          if (!is.null(all$rasch_msq)) {
            msq <- all$rasch_msq
            for (i in seq_along(vars)) {
              if (vars[i] %in% rownames(msq)) {
                infit[i] <- msq[vars[i], "infit"]
                outfit[i] <- msq[vars[i], "outfit"]
              }
            }
          }
          
          # 테이블에 행 추가
          for (i in seq_along(vars)) {
            row <- list(
              item = rasch.item[[1]][i],
              se = se.beta[i],
              lower = ci[[1]][i],
              upper = ci[[2]][i],
              infit = infit[i],
              outfit = outfit[i]
            )
            table$addRow(rowKey = vars[i], values = row)
          }
        }
        
        # LR test
        if (isTRUE(self$options$lr)) {
          lr <- all$lr
          self$results$ra$lr$setRow(
            rowNo = 1,
            values = list(value = lr$LR, df = lr$df, p = lr$pvalue)
          )
        }
        
        # Martin–Loef
        if (isTRUE(self$options$ml)) {
          ml <- all$ml
          self$results$ra$ml$setRow(
            rowNo = 1,
            values = list(value = ml$LR, df = ml$df, p = ml$p.value)
          )
        }
        
        # Wald test
        if (isTRUE(self$options$wald)) {
          w <- all$w
          table <- self$results$ra$wald
          ww <- as.data.frame(w$coef.table)
          for (i in seq_along(vars)) {
            row <- list(item = ww[[1]][i], p = ww[[2]][i])
            table$addRow(rowKey = vars[i], values = row)
          }
        }
        
        # --------------------- LLTM ---------------------
        lltm <- all$lltm
        
        if (isTRUE(self$options$eta)) {
          table <- self$results$ll$eta
          lltm.eta <- as.data.frame(cbind(lltm$etapar, lltm$se.eta))
          lltm.ci <- as.data.frame(stats::confint(lltm, "eta"))
          names <- dimnames(lltm.eta)[[1]]
          
          for (name in names) {
            row <- list(
              item = lltm.eta[name, 1],
              se = lltm.eta[name, 2],
              lower = lltm.ci[name, 1],
              upper = lltm.ci[name, 2]
            )
            table$addRow(rowKey = name, values = row)
          }
        }
        
        if (isTRUE(self$options$beta)) {
          table <- self$results$ll$beta
          lltm.item <- as.data.frame(lltm$betapar)
          lltm.se <- lltm$se.beta
          lltm.ci <- as.data.frame(stats::confint(lltm, "beta"))
          
          for (i in seq_along(vars)) {
            row <- list(
              item = lltm.item[[1]][i],
              se = lltm.se[i],
              lower = lltm.ci[[1]][i],
              upper = lltm.ci[[2]][i]
            )
            table$addRow(rowKey = vars[i], values = row)
          }
        }
        
        if (isTRUE(self$options$comp)) {
          table <- self$results$ll$comp
          mod <- as.data.frame(stats::anova(rasch, lltm)$statistics)
          names <- dimnames(mod)[[1]]
          
          for (name in names) {
            row <- list(
              ll = mod[name, 1],
              dev = mod[name, 2],
              npar = mod[name, 3],
              lr = mod[name, 4],
              df = mod[name, 5],
              p = mod[name, 6]
            )
            table$addRow(rowKey = name, values = row)
          }
        }
        
        # -------- η Contrast Test --------
        if (isTRUE(self$options$con)) {
          table <- self$results$con
          
          eta_hat <- as.numeric(lltm$etapar)
          names_eta <- names(lltm$etapar)
          if (is.null(names_eta)) names_eta <- paste0("F", seq_along(eta_hat))
          
          # eta 공분산 행렬
          V_eta <- NULL
          cand <- list(lltm$etavar, lltm$etacov, lltm$vcov.eta, lltm$cov.eta)
          for (obj in cand) {
            if (!is.null(obj)) {
              V_eta <- tryCatch(as.matrix(obj), error = function(e) NULL)
              if (!is.null(V_eta)) break
            }
          }
          if (is.null(V_eta)) {
            se_eta <- as.numeric(lltm$se.eta)
            V_eta <- diag(se_eta^2, nrow = length(se_eta), ncol = length(se_eta))
          }
          
          # 인접 대비 행렬
          L <- private$.buildDefaultContrasts(length(eta_hat), facNames = names_eta)
          
          if (nrow(L) > 0) {
            est <- as.numeric(L %*% eta_hat)
            se <- sqrt(diag(L %*% V_eta %*% t(L)))
            z <- est / se
            p <- 2 * stats::pnorm(-abs(z))
            
            for (i in seq_len(nrow(L))) {
              rk <- rownames(L)[i]
              table$addRow(
                rowKey = rk,
                values = list(contrast = rk, est = est[i], se = se[i], z = z[i], p = p[i])
              )
            }
          } else {
            table$addRow(
              rowKey = "NA",
              values = list(contrast = "\u2014", est = NA, se = NA, z = NA, p = NA)
            )
          }
        }
        
        # ---------------- Plots ----------------
        if (isTRUE(self$options$plot)) {
          image <- self$results$plot
          image$setState(list(lltm$betapar, rasch$betapar))
        }
        
        if (isTRUE(self$options$plot1)) {
          self$results$plot1$setState(list(W = all$W))
        }
        
        # -------- Residual table --------
        if (isTRUE(self$options$resid)) {
          tbl <- self$results$resid
          
          rm_b <- as.numeric(rasch$betapar)
          lltm_b <- as.numeric(lltm$betapar)
          
          n <- min(length(rm_b), length(lltm_b), length(vars))
          if (n > 0) {
            resid <- rm_b[seq_len(n)] - lltm_b[seq_len(n)]
            absres <- abs(resid)
            
            for (i in seq_len(n)) {
              note_i <- if (is.finite(resid[i])) {
                if (abs(resid[i]) >= 2) {
                  if (resid[i] > 0) "⚠ possible misfit (LLTM underestimates)"
                  else "⚠ possible misfit (LLTM overestimates)"
                } else {
                  if (resid[i] > 0) "OK (slight underestimation)"
                  else if (resid[i] < 0) "OK (slight overestimation)"
                  else "OK (neutral)"
                }
              } else ""
              
              tbl$addRow(
                rowKey = vars[i],
                values = list(
                  item = as.character(vars[i]),
                  resid = resid[i],
                  absres = absres[i],
                  flag = note_i
                )
              )
            }
            tbl$setNote(
              "Rule",
              "Flag if |Residual| ≥ 2 (heuristic): 'underestimates' = LLTM predicts easier; 'overestimates' = LLTM predicts harder."
            )
          }
        }
        
        # -------- Residual plot --------
        if (isTRUE(self$options$plot2)) {
          self$results$plot2$setState(list(
            rm_beta = as.numeric(rasch$betapar),
            lltm_beta = as.numeric(lltm$betapar),
            items = vars
          ))
        }
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) return(FALSE)
        lltm <- image$state[[1]]
        rm <- image$state[[2]]
        
        p <- ggplot(data = NULL, aes(x = rm, y = lltm)) +
          geom_abline(slope = 1, intercept = 0) +
          geom_smooth(method = "lm") +
          geom_point() +
          scale_x_continuous(limits = c(-4, 4)) +
          scale_y_continuous(limits = c(-4, 4)) +
          labs(
            x = "Item Easiness Parameter-RM",
            y = "Item Easiness Parameter-LLTM"
          ) +
          theme_bw()
        
        p <- p + ggtheme
        print(p)
        TRUE
      },
      
      .plot1 = function(image, ggtheme, theme, ...) {
        st <- image$state
        if (is.null(st) || is.null(st$W)) return(FALSE)
        W <- st$W
        
        if (is.null(rownames(W))) {
          rn <- self$options$vars
          rownames(W) <- if (!is.null(rn) && length(rn) == nrow(W)) {
            rn
          } else {
            paste0("I", seq_len(nrow(W)))
          }
        }
        if (is.null(colnames(W))) {
          colnames(W) <- paste0("F", seq_len(ncol(W)))
        }
        
        rnk <- tryCatch(qr(W)$rank, error = function(e) NA_integer_)
        svals <- tryCatch(round(svd(W)$d, 3), error = function(e) NULL)
        subtxt <- paste0(
          "rank(W) = ", rnk,
          if (!is.null(svals)) paste0(" | singular values: ", paste(svals, collapse = ", "))
        )
        
        df <- as.data.frame(as.table(W))
        names(df) <- c("Item", "Factor", "Value")
        
        nz_counts <- stats::aggregate(
          as.integer(df$Value != 0),
          by = list(Item = df$Item),
          FUN = sum
        )
        names(nz_counts) <- c("Item", "NonZero")
        df$Item <- factor(
          df$Item,
          levels = rev(nz_counts$Item[order(nz_counts$NonZero)])
        )
        
        show_text <- (nrow(W) * ncol(W) <= 120)
        lab_txt <- if (show_text) sprintf("%.2g", df$Value) else ""
        if (show_text) lab_txt[df$Value == 0] <- ""
        
        p1 <- ggplot2::ggplot(df, ggplot2::aes(x = Factor, y = Item, fill = Value)) +
          ggplot2::geom_tile(color = NA) +
          ggplot2::coord_fixed() +
          ggplot2::labs(
            title = "Design Matrix (W)",
            subtitle = subtxt,
            x = "Factor",
            y = "Item"
          ) +
          ggplot2::theme_classic(base_size = 11) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, vjust = .5, hjust = 1),
            plot.title = ggplot2::element_text(face = "bold"),
            plot.subtitle = ggplot2::element_text(size = 9, color = "gray40")
          )
        
        if (show_text)
          p1 <- p1 + ggplot2::geom_text(ggplot2::aes(label = lab_txt), size = 3)
        
        if (!is.null(ggtheme)) p1 <- p1 + ggtheme
        
        p1 <- p1 + ggplot2::scale_fill_gradient2(
          low = "#2166ac",
          mid = "white",
          high = "#b2182b",
          midpoint = 0,
          name = "W",
          limits = c(-1, 1),
          breaks = c(-1, 0, 1),
          guide = ggplot2::guide_colorbar(barwidth = 0.4, barheight = 6)
        )
        
        has_gridExtra <- requireNamespace("gridExtra", quietly = TRUE)
        nz_by_factor <- stats::aggregate(
          as.integer(df$Value != 0),
          by = list(Factor = df$Factor),
          FUN = sum
        )
        names(nz_by_factor) <- c("Factor", "NonZero")
        nz_by_factor$Factor <- factor(nz_by_factor$Factor, levels = colnames(W))
        
        p2 <- ggplot2::ggplot(nz_by_factor, ggplot2::aes(x = Factor, y = NonZero)) +
          ggplot2::geom_col(width = 0.7, fill = "#737373") +
          ggplot2::geom_text(ggplot2::aes(label = NonZero), vjust = -0.4, size = 3.5) +
          ggplot2::labs(title = "Non-zero count by Factor", x = NULL, y = "Count") +
          ggplot2::theme_classic(base_size = 11) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, vjust = .5, hjust = 1),
            plot.title = ggplot2::element_text(size = 11, face = "bold")
          )
        
        if (!is.null(ggtheme)) p2 <- p2 + ggtheme
        
        if (has_gridExtra) {
          g <- gridExtra::arrangeGrob(p1, p2, ncol = 2, widths = c(3, 2))
          grid::grid.newpage()
          grid::grid.draw(g)
        } else {
          print(p1)
        }
        TRUE
      },
      
      .plot2 = function(image, ggtheme, theme, ...) {
        st <- image$state
        if (is.null(st)) return(FALSE)
        
        rm_b <- as.numeric(st$rm_beta)
        lltm_b <- as.numeric(st$lltm_beta)
        items <- st$items
        
        n <- min(length(rm_b), length(lltm_b), length(items))
        if (n <= 0) return(FALSE)
        
        resid <- rm_b[seq_len(n)] - lltm_b[seq_len(n)]
        
        df <- data.frame(
          Item = items[seq_len(n)],
          Residual = resid,
          stringsAsFactors = FALSE
        )
        
        p <- ggplot2::ggplot(df, ggplot2::aes(x = Item, y = Residual)) +
          ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray55") +
          ggplot2::geom_point(size = 2.8) +
          ggplot2::geom_text(
            ggplot2::aes(label = round(Residual, 3)),
            vjust = -0.9,
            size = 3
          ) +
          ggplot2::labs(
            title = "Item Parameter Residuals (RM \u2212 LLTM)",
            y = "Residual (β_RM − β_LLTM)",
            x = "Item"
          ) +
          ggplot2::theme_minimal(base_size = 11)
        
        if (!is.null(ggtheme)) p <- p + ggtheme
        print(p)
        TRUE
      },
      
      .computeRES = function() {
        data <- self$data
        data <- na.omit(data)
        vars <- self$options$vars
        mat <- self$options$mat
        col <- self$options$col
        
        # Rasch model
        rasch <- eRm::RM(data)
        lr <- eRm::LRtest(rasch)
        ml <- eRm::MLoef(rasch)
        w <- eRm::Waldtest(rasch)
        
        # Person parameters (MSQ 계산용)
        rasch_person <- tryCatch(
          eRm::person.parameter(rasch),
          error = function(e) {
            warning("person.parameter() failed for Rasch: ", e$message)
            return(NULL)
          }
        )
        
        # MSQ 추출 (person parameter 필요)
        rasch_msq <- if (!is.null(rasch_person)) {
          private$.extractMSQ(rasch, item_names = vars)
        } else {
          NULL
        }
        
        # W matrix 생성
        mat <- strsplit(mat, ',')
        mat <- unlist(mat)
        mat <- as.integer(mat)
        mat1 <- matrix(as.matrix(mat), ncol = col)
        
        # LLTM
        lltm <- eRm::LLTM(data, mat1)
        
        # LLTM person parameters
        lltm_person <- tryCatch(
          eRm::person.parameter(lltm),
          error = function(e) {
            warning("person.parameter() failed for LLTM: ", e$message)
            return(NULL)
          }
        )
        
        # MSQ 추출 (LLTM)
        lltm_msq <- if (!is.null(lltm_person)) {
          private$.extractMSQ(lltm, item_names = vars)
        } else {
          NULL
        }
        
        list(
          rasch = rasch,
          lr = lr,
          ml = ml,
          w = w,
          lltm = lltm,
          W = mat1,
          rasch_msq = rasch_msq,
          lltm_msq = lltm_msq,
          rasch_person = rasch_person,
          lltm_person = lltm_person
        )
      }
    )
  )