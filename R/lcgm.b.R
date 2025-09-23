
# This file is a generated template, your changes will not be overwritten
#' @importFrom magrittr %>%

lcgmClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "lcgmClass",
    inherit = lcgmBase,
    
    active = list(
      
      res = function() {
        if (is.null(private$.res_cache)) {
          data <- self$data
          if (self$options$miss == 'listwise')
            data <- jmvcore::naOmit(data)
          private$.res_cache <- tidySEM::mx_growth_mixture(
            model      = self$options$model,
            data       = as.data.frame(data),
            classes    = self$options$nc,
            thresholds = self$options$thr
          )
        }
        private$.res_cache
      },
      
      desc = function() {
        if (is.null(private$.desc_cache)) {
          data <- self$data
          private$.desc_cache <- tidySEM::descriptives(data)[,
                                                             c("name","n","missing","mean","median","sd","min","max")]
        }
        private$.desc_cache
      },
      
      fit = function() {
        if (is.null(private$.fit_cache))
          private$.fit_cache <- tidySEM::table_fit(self$res)
        private$.fit_cache
      },
      
      parameters = function() {
        if (is.null(private$.para_cache)) {
          para <- tidySEM::table_results(self$res, columns = NULL)
          private$.para_cache <- para[para$Category %in% c("Means","Variances"),
                                      c("Category","lhs","est","se","pval","confint","name")]
        }
        private$.para_cache
      },
      
      classProbabilities = function() {
        if (is.null(private$.cp_cache)) {
          cp1 <- tidySEM::class_prob(self$res)
          individual_data <- data.frame(cp1$individual)   # predicted & posterior probs
          predicted_classes <- individual_data$predicted
          
          class_counts <- table(predicted_classes)
          total_n <- sum(class_counts)
          
          summary_df <- data.frame(
            Class = as.numeric(names(class_counts)),
            Count = as.numeric(class_counts),
            Proportion = round(as.numeric(class_counts) / total_n, 3)
          )
          
          private$.cp_cache <- list(
            summary    = summary_df,
            individual = individual_data
          )
        }
        private$.cp_cache
      }
    ),
    
    private = list(
      .htmlwidget   = NULL,
      .res_cache    = NULL,
      .desc_cache   = NULL,
      .fit_cache    = NULL,
      .para_cache   = NULL,
      .cp_cache     = NULL,
      
      # ---------- 안전 텍스트 출력 ----------
      .setTextSafe = function(txt) {
        slots <- c("three", "threeStep", "threeStepText", "text1", "text", "auxText")
        for (id in slots) {
          out <- try(eval(parse(text = sprintf("self$results$%s$setContent", id))), silent = TRUE)
          if (!inherits(out, "try-error")) {
            try(eval(parse(text = sprintf("self$results$%s$setContent(txt)", id))), silent = TRUE)
            return(invisible(TRUE))
          }
        }
        try(self$results$instructions$setContent(txt), silent = TRUE)
        invisible(TRUE)
      },
      
      # ---------- 3-STEP 계산 (효과크기 + 다중비교 보정 포함) ----------
      .computeThreeStep = function() {
        auxName <- self$options$auxVar
        if (is.null(auxName) || !nzchar(auxName))
          return(NULL)
        
        dat <- as.data.frame(self$data)
        if (!(auxName %in% names(dat)))
          return(list(msg = sprintf("[3-step] '%s' not found in data.", auxName)))
        
        # posterior 가져오기
        ind <- self$classProbabilities$individual
        
        # posterior 열 탐색
        .findPosteriorCols <- function(nms) {
          pats <- c(
            "^Class[_\\.]?[0-9]+$", "^class[_\\.]?[0-9]+$",
            "^C[0-9]+$", "^c[0-9]+$", "^p[0-9]+$", "^P[0-9]+$",
            "^prob(?:ability)?[_\\.]?[0-9]+$", "^posterior[_\\.]?[0-9]+$"
          )
          hits <- unique(unlist(lapply(pats, function(p) grep(p, nms))))
          nms[hits]
        }
        pcols <- .findPosteriorCols(names(ind))
        if (length(pcols) == 0) {
          num <- names(ind)[vapply(ind, is.numeric, TRUE)]
          cand <- setdiff(num, c("id","ID","predicted","Predicted"))
          if (length(cand) > 1) {
            S <- rowSums(ind[, cand, drop = FALSE], na.rm = TRUE)
            if (all(is.finite(S)) && mean(abs(S - 1)) < 1e-3)
              pcols <- cand
          }
        }
        if (length(pcols) == 0)
          return(list(msg = "[3-step] posterior columns not found (tried C1/Class_1/p1/etc.)."))
        
        K <- length(pcols)
        cls_levels <- seq_len(K)
        
        # ---- 보조변수 타입 결정 ----
        yraw <- dat[[auxName]]
        n <- NROW(yraw)
        if (n != NROW(ind))
          return(list(msg = "[3-step] data/posterior row mismatch."))
        
        notes <- character(0)
        is_cat_like <- function(x) {
          if (!is.numeric(x)) return(FALSE)
          ux <- sort(unique(x[is.finite(x)]))
          length(ux) <= 6 && all(abs(ux - round(ux)) < 1e-8)
        }
        
        if (is.factor(yraw) || is.ordered(yraw) || is.character(yraw) || is.logical(yraw)) {
          ytype <- "categorical"; yfac <- as.factor(yraw)
        } else if (is_cat_like(yraw)) {
          ytype <- "categorical"; yfac <- factor(yraw)
          notes <- c(notes, sprintf("[note] '%s' has few integer levels; treated as categorical (DCAT).", auxName))
        } else if (is.numeric(yraw)) {
          ytype <- "numeric"
        } else {
          return(list(msg = sprintf("[3-step] Unsupported type for '%s'.", auxName)))
        }
        
        # ---- 결측 처리: y 결측이면 해당 행 가중치 0 ----
        if (ytype == "numeric") {
          y <- yraw; wmask <- is.finite(y)
        } else {
          y <- yfac; wmask <- !is.na(y)
        }
        for (pc in pcols) ind[[pc]][!wmask] <- 0
        
        # =========================
        # BCH (연속형)
        # =========================
        if (ytype == "numeric") {
          long <- lapply(seq_len(K), function(k) {
            data.frame(class = factor(k, levels = cls_levels),
                       y = y, w = ind[[pcols[k]]])
          })
          long <- do.call(rbind, long)
          
          eff_nk <- tapply(long$w, long$class, sum)
          if (any(eff_nk < 1e-6))
            notes <- c(notes, "[warn] One or more classes have (near-)zero effective weight for the distal variable.")
          if (stats::var(long$y[long$w > 0], na.rm = TRUE) < 1e-10)
            notes <- c(notes, "[warn] Near-zero variance in distal variable (BCH may be unreliable).")
          
          # 클래스별 평균/분산/SE/N_eff
          wmean <- tapply(long$y * long$w, long$class, sum) / pmax(eff_nk, .Machine$double.eps)
          # 가중 분산
          wvar <- sapply(levels(long$class), function(k) {
            idx <- long$class == k
            wk <- long$w[idx]; yk <- long$y[idx]
            mk <- if (eff_nk[k] > 0) sum(wk * yk) / eff_nk[k] else NA_real_
            if (!is.finite(mk) || eff_nk[k] <= 0) return(NA_real_)
            sum(wk * (yk - mk)^2) / eff_nk[k]
          })
          wse <- sqrt(wvar / pmax(eff_nk, 1))
          
          # 전체 F 근사
          wbar <- sum(long$y * long$w) / sum(long$w)
          ssb  <- sum(eff_nk * (wmean - wbar)^2, na.rm=TRUE)
          long$gmean <- wmean[as.character(long$class)]
          ssw  <- sum(long$w * (long$y - long$gmean)^2, na.rm=TRUE)
          dfb  <- K - 1
          dfw  <- max(1, sum(eff_nk, na.rm=TRUE) - K)
          msb  <- ssb / dfb
          msw  <- ssw / dfw
          Fst  <- if (msw > 0) msb / msw else Inf
          pF   <- tryCatch(stats::pf(Fst, dfb, dfw, lower.tail = FALSE), error = function(e) NA_real_)
          
          # 텍스트: 클래스별
          lines <- c(
            "3-step analysis (BCH/DCAT)",
            "=== 3-STEP auxiliary (tidySEM, BCH_basic) ===",
            sprintf("[Distal: %s, numeric] K=%d", auxName, K),
            sprintf("Class-weighted means: %s",
                    paste(paste0("C", names(wmean), "=", round(wmean, 3)), collapse = ", ")),
            sprintf("Wald/ANOVA approx: F(%d,%.1f)=%.3f, p=%.4f", dfb, dfw, Fst, pF)
          )
          
          # 쌍별 비교: z, p, p_BH, p_Bonf, Cohen's d
          if (K >= 2) {
            pair_lab <- c(); p_raw <- c(); zvals <- c(); dvals <- c()
            for (a in 1:(K-1)) for (b in (a+1):K) {
              za <- (wmean[a] - wmean[b]) / sqrt(wse[a]^2 + wse[b]^2)
              pa <- 2*stats::pnorm(-abs(za))
              # Cohen's d (가중 pooled SD)
              spooled <- sqrt( ((eff_nk[a]-1)*wvar[a] + (eff_nk[b]-1)*wvar[b]) /
                                 pmax(eff_nk[a]+eff_nk[b]-2, 1) )
              d_ab <- (wmean[a] - wmean[b]) / spooled
              pair_lab <- c(pair_lab, sprintf("%d vs %d", a, b))
              p_raw    <- c(p_raw, pa)
              zvals    <- c(zvals, za)
              dvals    <- c(dvals, d_ab)
            }
            p_bh   <- stats::p.adjust(p_raw, method = "BH")
            p_bonf <- stats::p.adjust(p_raw, method = "bonferroni")
            lines <- c(lines, "Pairwise differences (z, p, p_BH, p_Bonf, Cohen's d):")
            for (i in seq_along(pair_lab)) {
              lines <- c(lines, sprintf("  Class %s: z=%.3f, p=%.3g, p_BH=%.3g, p_Bonf=%.3g, d=%.3f",
                                        pair_lab[i], zvals[i], p_raw[i], p_bh[i], p_bonf[i], dvals[i]))
            }
          }
          
          if (length(notes)) lines <- c(lines, notes)
          return(list(text = paste(lines, collapse = "\n")))
        }
        
        # =========================
        # DCAT (범주형)
        # =========================
        cats <- levels(y)
        W <- matrix(0, nrow = K, ncol = length(cats),
                    dimnames = list(paste0("C", cls_levels), cats))
        for (k in seq_len(K)) {
          for (j in seq_along(cats)) {
            W[k, j] <- sum( (y == cats[j]) * ind[[pcols[k]]] )
          }
        }
        
        # 전체 χ² + Cramér's V
        rowsum <- rowSums(W, na.rm=TRUE)
        colsum <- colSums(W, na.rm=TRUE)
        grand  <- sum(rowsum, na.rm=TRUE)
        E <- outer(rowsum, colsum) / ifelse(grand > 0, grand, NA_real_)
        if (any(E < 1))
          notes <- c(notes, "[warn] Some expected weighted counts < 1 (chi-square may be inaccurate).")
        if (mean(E < 5) > 0.2)
          notes <- c(notes, "[note] >20% of expected weighted counts < 5 (use caution).")
        
        chi  <- sum((W - E)^2 / pmax(E, .Machine$double.eps), na.rm=TRUE)
        df   <- (K - 1) * (length(cats) - 1)
        pchi <- stats::pchisq(chi, df=df, lower.tail=FALSE)
        Vall <- sqrt( chi / (grand * max(1, min(K-1, length(cats)-1))) )
        
        lines <- c(
          "3-step analysis (BCH/DCAT)",
          "=== 3-STEP auxiliary (tidySEM, DCAT) ===",
          sprintf("[Distal: %s, categorical] K=%d, J=%d", auxName, K, ncol(W)),
          sprintf("Wald/Chi-square: X^2(%d)=%.3f, p=%.4f, Cramer's V=%.3f", df, chi, pchi, Vall)
        )
        
        # 쌍별 2×C: χ², p, 보정 p, V
        if (K >= 2) {
          pair_lab <- c(); p_raw <- c(); chis <- c(); df2 <- c(); Vpair <- c()
          for (a in 1:(K-1)) for (b in (a+1):K) {
            sub <- rbind(W[a, ], W[b, ])
            rs  <- rowSums(sub); cs <- colSums(sub); g <- sum(rs)
            Eab <- outer(rs, cs) / ifelse(g > 0, g, NA_real_)
            chiab <- sum((sub - Eab)^2 / pmax(Eab, .Machine$double.eps), na.rm=TRUE)
            dfab  <- ncol(W) - 1
            pab   <- stats::pchisq(chiab, df=dfab, lower.tail=FALSE)
            Vab   <- sqrt( chiab / pmax(g, .Machine$double.eps) )  # 2×C → min(r-1,c-1)=1
            pair_lab <- c(pair_lab, sprintf("%d vs %d", a, b))
            p_raw    <- c(p_raw, pab)
            chis     <- c(chis, chiab)
            df2      <- c(df2, dfab)
            Vpair    <- c(Vpair, Vab)
          }
          p_bh   <- stats::p.adjust(p_raw, method = "BH")
          p_bonf <- stats::p.adjust(p_raw, method = "bonferroni")
          lines <- c(lines, "Pairwise association (chi-square; p, p_BH, p_Bonf, Cramer's V):")
          for (i in seq_along(pair_lab)) {
            lines <- c(lines, sprintf("  Class %s: X^2(%d)=%.3f, p=%.3g, p_BH=%.3g, p_Bonf=%.3g, V=%.3f",
                                      pair_lab[i], df2[i], chis[i], p_raw[i], p_bh[i], p_bonf[i], Vpair[i]))
          }
        }
        
        if (length(notes)) lines <- c(lines, notes)
        return(list(text = paste(lines, collapse = "\n")))
      },
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) || is.null(self$options$vars))
          self$results$instructions$setVisible(TRUE)
        
        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title   = "Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px;',
              ' padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<ul>',
              '<li>Set <b>Thresholds=TRUE</b> for ordinal data.</li>',
              '<li>To select the optimal number of classes, change the Classes option and compare the Model fit values.</li>',
              '<li>Latent class growth analysis is described in the ',
              '<a href="https://cjvanlissa.github.io/tidySEM/articles/lca_lcga.html" target="_blank">tidySEM article</a>.</li>',
              '<li>Feature requests and bug reports: ',
              '<a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub Issues</a>.</li>',
              '</ul></div>'
            )
          )
        )
        
        if (isTRUE(self$options$plot1))
          self$results$plot1$setSize(self$options$width1, self$options$height1)
        
        if (isTRUE(self$options$plot))
          self$results$plot$setSize(self$options$width, self$options$height)
        
        if (isTRUE(self$options$plot2))
          self$results$plot2$setSize(self$options$width2, self$options$height2)
      },
      
      .run = function() {
        if (is.null(self$options$vars) || length(self$options$vars) < 3)
          return()
        
        self$results$progressBarHTML$setVisible(TRUE)
        html <- progressBarH(5,  100, 'Starting analysis...')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
        
        set.seed(1234)
        
        if (isTRUE(self$options$desc)) {
          html <- progressBarH(15, 100, 'Computing descriptives...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.populateDescTable()
        }
        
        if (isTRUE(self$options$fit)) {
          html <- progressBarH(30, 100, 'Calculating fit statistics...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.populateFitTable()
        }
        
        if (isTRUE(self$options$est)) {
          html <- progressBarH(45, 100, 'Extracting parameter estimates...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.populateEST()
        }
        
        if (isTRUE(self$options$cp)) {
          html <- progressBarH(60, 100, 'Computing class probabilities...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.populateClassSizeTable()
        }
        
        if (isTRUE(self$options$mem)) {
          html <- progressBarH(70, 100, 'Listing class members...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.populateClassMemberTable()
        }
        
        if (isTRUE(self$options$plot2)) {
          html <- progressBarH(80, 100, 'Creating class size plot...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.setPlot2()
        }
        
        if (isTRUE(self$options$plot1)) {
          html <- progressBarH(88, 100, 'Preparing density plot...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.setPlot1()
        }
        
        if (isTRUE(self$options$plot)) {
          html <- progressBarH(96, 100, 'Generating growth plot...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.setPlot()
        }
        
        # ---------- 3-STEP 실행 ----------
        if (isTRUE(self$options$use3step) && !is.null(self$options$auxVar)) {
          html <- progressBarH(98, 100, 'Running 3-step auxiliary (BCH/DCAT)...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          
          res3 <- private$.computeThreeStep()
          
          sep_line <- strrep("━", 60)  # 굵은 유니코드 줄
          
          if (is.null(res3)) {
            self$results$threeStep$setContent(
              paste0(sep_line, "\n[3-step] No auxiliary variable specified.\n", sep_line)
            )
          } else if (!is.null(res3$text)) {
            formatted <- paste0(sep_line, "\n", res3$text, "\n", sep_line)
            self$results$threeStep$setContent(formatted)
          } else if (!is.null(res3$msg)) {
            self$results$threeStep$setContent(
              paste0(sep_line, "\n", res3$msg, "\n", sep_line)
            )
          } else {
            self$results$threeStep$setContent(
              paste0(sep_line, "\n[3-step] Completed.\n", sep_line)
            )
          }
        }
        
        html <- progressBarH(100,100, 'Analysis complete!')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
        self$results$progressBarHTML$setVisible(FALSE)
      },
      
      .populateDescTable = function() {
        vars  <- self$options$vars
        table <- self$results$desc
        d     <- as.data.frame(self$desc)
        for (i in seq_along(vars))
          table$addRow(rowKey = vars[i], values = as.list(d[i,2:8]))
      },
      
      .populateFitTable = function() {
        table <- self$results$fit
        df    <- as.data.frame(t(self$fit))
        for (nm in rownames(df))
          table$addRow(rowKey = nm, values = list(value = df[nm,1]))
      },
      
      .populateEST = function() {
        table <- self$results$est
        e     <- as.data.frame(self$parameters)
        for (nm in rownames(e))
          table$addRow(rowKey = nm, values = list(
            cat  = e[nm,1],
            lhs  = e[nm,2],
            est  = e[nm,3],
            se   = e[nm,4],
            p    = e[nm,5],
            ci   = e[nm,6],
            na   = e[nm,7]
          ))
      },
      
      .populateClassSizeTable = function() {
        table <- self$results$cp
        individual_data <- self$classProbabilities$individual
        predicted_classes <- individual_data$predicted
        class_counts <- table(predicted_classes)
        total_n <- sum(class_counts)
        
        for (class_num in sort(unique(predicted_classes))) {
          count_val <- as.integer(class_counts[as.character(class_num)])
          prop_val <- round(count_val / total_n, 3)
          table$addRow(rowKey = as.integer(class_num), values = list(
            count = count_val,
            prop  = prop_val
          ))
        }
      },
      
      .populateClassMemberTable = function() {
        table <- self$results$mem
        mem   <- self$classProbabilities$individual
        if (table$isNotFilled()) {
          table$setRowNums(rownames(self$data))
          table$setValues(as.factor(mem$predicted))
        }
      },
      
      .setPlot2 = function() {
        individual_data <- self$classProbabilities$individual
        predicted_classes <- individual_data$predicted
        class_counts <- table(predicted_classes)
        total_n <- sum(class_counts)
        
        plot_data <- data.frame(
          Class = factor(names(class_counts), levels = sort(as.numeric(names(class_counts)))),
          Count = as.numeric(class_counts),
          Proportion = round(as.numeric(class_counts) / total_n, 3),
          Percentage = round(as.numeric(class_counts) / total_n * 100, 1)
        )
        
        self$results$plot2$setState(plot_data)
      },
      
      .setPlot1 = function() {
        df   <- as.data.frame(self$data)
        vars <- self$options$vars
        if (is.null(vars) || length(vars) < 1) return()
        isNum <- vapply(df[vars], is.numeric, TRUE)
        xvars <- vars[isNum]
        if (length(xvars) < 1) return()
        if (!("id" %in% names(df))) df$id <- seq_len(nrow(df))
        long <- reshape(df[, c("id", xvars), drop = FALSE],
                        direction = "long",
                        varying   = xvars,
                        v.names   = "value",
                        idvar     = "id",
                        timevar   = "time")
        long$time <- factor(long$time, labels = xvars)
        self$results$plot1$setState(long)
      },
      
      .setPlot = function() {
        self$results$plot$setState(self$res)
      },
      
      .plot2 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) return(FALSE)
        plot_data <- image$state
        p <- ggplot(plot_data, aes(x = Class, y = Count, fill = Class)) +
          geom_bar(stat = "identity", alpha = 0.8, color = "white", size = 0.5) +
          geom_text(aes(label = paste0("n = ", Count, "\n(", Percentage, "%)")),
                    vjust = -0.5, size = 3.5, fontface = "bold", color = "black") +
          scale_fill_brewer(type = "qual", palette = "Set3") +
          labs(title = "", x = "Latent Class", y = "Number of Participants") +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 13, face = "bold", margin = margin(b = 20)),
            axis.text.x = element_text(size = 11),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 11, face = "bold"),
            legend.position = "none",
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.margin = margin(10, 10, 10, 10)
          ) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
        print(p + ggtheme); TRUE
      },
      
      .plot1 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) return(FALSE)
        long <- image$state
        long <- long[is.finite(long$value), , drop = FALSE]
        p <- ggplot(long, aes(x = value)) +
          geom_density() +
          facet_wrap(~ time, ncol = 2) +   
          theme_bw()
        print(p + ggtheme)
        TRUE
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) return(FALSE)
        p <- tidySEM::plot_growth(image$state,
                                  rawdata = self$options$raw,
                                  alpha_range = c(0, 0.05))
        print(p + ggtheme); TRUE
      }
    )
  )


# Progress Bar HTML (R/progressBarH.R)
progressBarH <- function(progress = 0, total = 100, message = '') {
  percentage <- round(progress / total * 100)
  width      <- 400 * percentage / 100
  
  html <- paste0(
    '<div style="text-align: center; padding: 20px;">',
    '<div style="width: 400px; height: 20px; border: 1px solid #ccc;',
    ' background-color: #f8f9fa; margin: 0 auto; border-radius: 4px;">',
    '<div style="width: ', width, 'px; height: 18px;',
    ' background-color: #999999; border-radius: 3px;',
    ' transition: width 0.3s ease;"></div>',
    '</div>',
    '<div style="margin-top: 8px; font-size: 12px; color: #666;">',
    message, ' (', percentage, '%)',
    '</div>',
    '</div>'
  )
  
  return(html)
}
