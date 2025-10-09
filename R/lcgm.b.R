# 변경 요약: 캐시 무효화/정리 루틴과 데이터 접근 헬퍼를 도입하여 중복 객체를 줄이고 실행 후 대용량 상태를 해제합니다.
#' @importFrom magrittr %>%

lcgmClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "lcgmClass",
    inherit = lcgmBase,

    active = list(

      res = function() {
        cache <- private$.cacheGet("res")
        if (!is.null(cache))
          return(cache)

        data <- private$.data_model()
        result <- tidySEM::mx_growth_mixture(
          model      = self$options$model,
          data       = data,
          classes    = self$options$nc,
          thresholds = self$options$thr
        )

        private$.cacheSet("res", result)
        result
      },

      desc = function() {
        cache <- private$.cacheGet("desc")
        if (!is.null(cache))
          return(cache)

        data <- private$.data_raw()
        desc_df <- tidySEM::descriptives(data)
        cols <- c("name", "n", "missing", "mean", "median", "sd", "min", "max")
        desc_df <- desc_df[, cols, drop = FALSE]
        rownames(desc_df) <- desc_df$name

        private$.cacheSet("desc", desc_df)
        desc_df
      },

      fit = function() {
        cache <- private$.cacheGet("fit")
        if (!is.null(cache))
          return(cache)

        fit_tbl <- tidySEM::table_fit(self$res)
        private$.cacheSet("fit", fit_tbl)
        fit_tbl
      },

      parameters = function() {
        cache <- private$.cacheGet("parameters")
        if (!is.null(cache))
          return(cache)

        para <- tidySEM::table_results(self$res, columns = NULL)
        keep <- para$Category %in% c("Means", "Variances")
        cols <- c("Category", "lhs", "est", "se", "pval", "confint", "name")
        para <- para[keep, cols, drop = FALSE]

        private$.cacheSet("parameters", para)
        para
      },

      classProbabilities = function() {
        cache <- private$.cacheGet("classProbabilities")
        if (!is.null(cache))
          return(cache)

        cp_raw <- tidySEM::class_prob(self$res)
        individual <- private$.normalizeIndividual(cp_raw$individual)

        predicted_classes <- individual$predicted
        class_counts <- table(predicted_classes)
        total_n <- sum(class_counts)

        summary_df <- data.frame(
          Class = as.numeric(names(class_counts)),
          Count = as.numeric(class_counts),
          Proportion = round(as.numeric(class_counts) / total_n, 3),
          stringsAsFactors = FALSE
        )

        result <- list(
          summary    = summary_df,
          individual = individual
        )

        private$.cacheSet("classProbabilities", result)
        result
      }
    ),

    private = list(
      .htmlwidget = NULL,
      .cache_env = NULL,

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

      .cacheEnsure = function() {
        if (is.null(private$.cache_env))
          private$.cache_env <- new.env(parent = emptyenv())
      },

      .cacheGet = function(key) {
        private$.cacheEnsure()
        if (exists(key, envir = private$.cache_env, inherits = FALSE))
          get(key, envir = private$.cache_env, inherits = FALSE)
        else
          NULL
      },

      .cacheSet = function(key, value) {
        private$.cacheEnsure()
        if (is.null(value)) {
          if (exists(key, envir = private$.cache_env, inherits = FALSE))
            rm(list = key, envir = private$.cache_env)
          return(NULL)
        }
        assign(key, value, envir = private$.cache_env)
        value
      },

      .cacheReset = function(keys = NULL) {
        private$.cacheEnsure()
        if (is.null(keys)) {
          rm(list = ls(envir = private$.cache_env, all.names = TRUE), envir = private$.cache_env)
        } else {
          keys <- intersect(keys, ls(envir = private$.cache_env, all.names = TRUE))
          if (length(keys) > 0)
            rm(list = keys, envir = private$.cache_env)
        }
        invisible(TRUE)
      },

      .data_raw = function() {
        cache <- private$.cacheGet("data_raw")
        if (!is.null(cache))
          return(cache)

        df <- as.data.frame(self$data)
        private$.cacheSet("data_raw", df)
        df
      },

      .data_model = function() {
        cache <- private$.cacheGet("data_model")
        if (!is.null(cache))
          return(cache)

        df <- private$.data_raw()
        if (identical(self$options$miss, 'listwise'))
          df <- jmvcore::naOmit(df)

        private$.cacheSet("data_model", df)
        df
      },

      .normalizeIndividual = function(x) {
        if (!is.data.frame(x))
          x <- as.data.frame(x)
        rownames(x) <- NULL
        if (!"predicted" %in% names(x) && "Predicted" %in% names(x))
          names(x)[names(x) == "Predicted"] <- "predicted"
        if (!is.null(x$predicted)) {
          x$predicted <- as.integer(x$predicted)
        }
        x
      },

      .findPosteriorCols = function(nms) {
        patterns <- c(
          "^Class[_\\.]?[0-9]+$", "^class[_\\.]?[0-9]+$",
          "^C[0-9]+$", "^c[0-9]+$", "^p[0-9]+$", "^P[0-9]+$",
          "^prob(?:ability)?[_\\.]?[0-9]+$", "^posterior[_\\.]?[0-9]+$"
        )
        hits <- unique(unlist(lapply(patterns, function(p) grep(p, nms))))
        nms[hits]
      },

      .computeThreeStep = function() {
        auxName <- self$options$auxVar
        if (is.null(auxName) || !nzchar(auxName))
          return(NULL)

        dat <- private$.data_raw()
        if (!(auxName %in% names(dat)))
          return(list(msg = sprintf("[3-step] '%s' not found in data.", auxName)))

        cp <- self$classProbabilities
        ind <- cp$individual

        pcols <- private$.findPosteriorCols(names(ind))
        if (length(pcols) == 0) {
          num <- names(ind)[vapply(ind, is.numeric, TRUE)]
          cand <- setdiff(num, c("id", "ID", "predicted", "Predicted"))
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

        if (ytype == "numeric") {
          y <- yraw; wmask <- is.finite(y)
        } else {
          y <- yfac; wmask <- !is.na(y)
        }
        for (pc in pcols) ind[[pc]][!wmask] <- 0

        if (ytype == "numeric") {
          long <- lapply(seq_len(K), function(k) {
            data.frame(class = factor(k, levels = cls_levels),
                       y = yraw,
                       w = ind[[pcols[k]]],
                       stringsAsFactors = FALSE)
          })
          long <- do.call(rbind, long)

          eff_nk <- tapply(long$w, long$class, sum)
          if (any(eff_nk < 1e-6))
            notes <- c(notes, "[warn] One or more classes have (near-)zero effective weight for the distal variable.")
          if (stats::var(long$y[long$w > 0], na.rm = TRUE) < 1e-10)
            notes <- c(notes, "[warn] Near-zero variance in distal variable (BCH may be unreliable).")

          wmean <- tapply(long$y * long$w, long$class, sum) / pmax(eff_nk, .Machine$double.eps)
          wvar <- vapply(levels(long$class), function(k) {
            idx <- long$class == k
            wk <- long$w[idx]; yk <- long$y[idx]
            mk <- if (eff_nk[k] > 0) sum(wk * yk) / eff_nk[k] else NA_real_
            if (!is.finite(mk) || eff_nk[k] <= 0) return(NA_real_)
            sum(wk * (yk - mk)^2) / eff_nk[k]
          }, numeric(1))
          wse <- sqrt(wvar / pmax(eff_nk, 1))

          wbar <- sum(long$y * long$w) / sum(long$w)
          ssb  <- sum(eff_nk * (wmean - wbar)^2, na.rm = TRUE)
          long$gmean <- wmean[as.character(long$class)]
          ssw  <- sum(long$w * (long$y - long$gmean)^2, na.rm = TRUE)
          dfb  <- K - 1
          dfw  <- max(1, sum(eff_nk, na.rm = TRUE) - K)
          msb  <- ssb / dfb
          msw  <- ssw / dfw
          Fst  <- if (msw > 0) msb / msw else Inf
          pF   <- tryCatch(stats::pf(Fst, dfb, dfw, lower.tail = FALSE), error = function(e) NA_real_)

          lines <- c(
            "3-step analysis (BCH/DCAT)",
            "=== 3-STEP auxiliary (tidySEM, BCH_basic) ===",
            sprintf("[Distal: %s, numeric] K=%d", auxName, K),
            sprintf("Class-weighted means: %s",
                    paste(paste0("C", names(wmean), "=", round(wmean, 3)), collapse = ", ")),
            sprintf("Wald/ANOVA approx: F(%d,%.1f)=%.3f, p=%.4f", dfb, dfw, Fst, pF)
          )

          if (K >= 2) {
            pair_lab <- character()
            p_raw <- numeric()
            zvals <- numeric()
            dvals <- numeric()
            for (a in 1:(K - 1)) for (b in (a + 1):K) {
              za <- (wmean[a] - wmean[b]) / sqrt(wse[a]^2 + wse[b]^2)
              pa <- 2 * stats::pnorm(-abs(za))
              spooled <- sqrt(((eff_nk[a] - 1) * wvar[a] + (eff_nk[b] - 1) * wvar[b]) /
                                pmax(eff_nk[a] + eff_nk[b] - 2, 1))
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

        cats <- levels(y)
        W <- matrix(0, nrow = K, ncol = length(cats),
                    dimnames = list(paste0("C", cls_levels), cats))
        for (k in seq_len(K)) {
          for (j in seq_along(cats)) {
            W[k, j] <- sum((y == cats[j]) * ind[[pcols[k]]])
          }
        }

        rowsum <- rowSums(W, na.rm = TRUE)
        colsum <- colSums(W, na.rm = TRUE)
        grand  <- sum(rowsum, na.rm = TRUE)
        E <- outer(rowsum, colsum) / ifelse(grand > 0, grand, NA_real_)
        if (any(E < 1))
          notes <- c(notes, "[warn] Some expected weighted counts < 1 (chi-square may be inaccurate).")
        if (mean(E < 5) > 0.2)
          notes <- c(notes, "[note] >20% of expected weighted counts < 5 (use caution).")

        chi  <- sum((W - E)^2 / pmax(E, .Machine$double.eps), na.rm = TRUE)
        df   <- (K - 1) * (length(cats) - 1)
        pchi <- stats::pchisq(chi, df = df, lower.tail = FALSE)
        Vall <- sqrt(chi / (grand * max(1, min(K - 1, length(cats) - 1))))

        lines <- c(
          "3-step analysis (BCH/DCAT)",
          "=== 3-STEP auxiliary (tidySEM, DCAT) ===",
          sprintf("[Distal: %s, categorical] K=%d, J=%d", auxName, K, ncol(W)),
          sprintf("Wald/Chi-square: X^2(%d)=%.3f, p=%.4f, Cramer's V=%.3f", df, chi, pchi, Vall)
        )

        if (K >= 2) {
          pair_lab <- character()
          p_raw <- numeric()
          chis <- numeric()
          df2 <- numeric()
          Vpair <- numeric()
          for (a in 1:(K - 1)) for (b in (a + 1):K) {
            sub <- rbind(W[a, ], W[b, ])
            rs  <- rowSums(sub)
            cs  <- colSums(sub)
            g <- sum(rs)
            Eab <- outer(rs, cs) / ifelse(g > 0, g, NA_real_)
            chiab <- sum((sub - Eab)^2 / pmax(Eab, .Machine$double.eps), na.rm = TRUE)
            dfab  <- ncol(W) - 1
            pab   <- stats::pchisq(chiab, df = dfab, lower.tail = FALSE)
            Vab   <- sqrt(chiab / pmax(g, .Machine$double.eps))
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
        list(text = paste(lines, collapse = "\n"))
      },

      .init = function() {
        private$.cacheReset()
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

        private$.cacheReset()

        self$results$progressBarHTML$setVisible(TRUE)
        on.exit({
          self$results$progressBarHTML$setVisible(FALSE)
          private$.cacheReset(c("data_raw", "data_model"))
        }, add = TRUE)

        html <- progressBarH(5, 100, 'Starting analysis...')
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

        if (isTRUE(self$options$use3step) && !is.null(self$options$auxVar)) {
          html <- progressBarH(98, 100, 'Running 3-step auxiliary (BCH/DCAT)...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()

          res3 <- private$.computeThreeStep()
          sep_line <- strrep("━", 60)

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

        html <- progressBarH(100, 100, 'Analysis complete!')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
      },

      .populateDescTable = function() {
        vars  <- self$options$vars
        table <- self$results$desc
        desc_df <- self$desc
        for (i in seq_along(vars)) {
          row <- desc_df[vars[i], , drop = FALSE]
          if (nrow(row) == 0)
            next
          table$addRow(rowKey = vars[i], values = as.list(row[1, c("n", "missing", "mean", "median", "sd", "min", "max")]))
        }
      },

      .populateFitTable = function() {
        table <- self$results$fit
        df    <- as.data.frame(t(self$fit))
        for (nm in rownames(df))
          table$addRow(rowKey = nm, values = list(value = df[nm, 1]))
      },

      .populateEST = function() {
        table <- self$results$est
        e     <- self$parameters
        for (i in seq_len(nrow(e))) {
          table$addRow(rowKey = rownames(e)[i], values = list(
            cat = e$Category[i],
            lhs = e$lhs[i],
            est = e$est[i],
            se  = e$se[i],
            p   = e$pval[i],
            ci  = e$confint[i],
            na  = e$name[i]
          ))
        }
      },

      .populateClassSizeTable = function() {
        table <- self$results$cp
        cp    <- self$classProbabilities
        summary_df <- cp$summary
        for (i in seq_len(nrow(summary_df))) {
          table$addRow(rowKey = summary_df$Class[i], values = list(
            count = summary_df$Count[i],
            prop  = summary_df$Proportion[i]
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
        cp <- self$classProbabilities
        plot_data <- data.frame(
          Class = factor(cp$summary$Class, levels = sort(cp$summary$Class)),
          Count = cp$summary$Count,
          Proportion = cp$summary$Proportion,
          Percentage = round(cp$summary$Proportion * 100, 1)
        )
        self$results$plot2$setState(plot_data)
      },

      .setPlot1 = function() {
        df   <- private$.data_raw()
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
        long <- long[is.finite(long$value), , drop = FALSE]
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

  html
}
