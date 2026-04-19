
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
          
          if (self$options$miss == "listwise")
            data <- jmvcore::naOmit(data)
          
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
          individual_data <- data.frame(cp1$individual)
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
      .plot1_cache  = NULL,
#--------------------------------------------
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
      
      .clearThreeStepTables = function() {
        for (nm in c("threeStepSummary", "threeStepClassStats", "threeStepPairwise")) {
          obj <- try(eval(parse(text = sprintf("self$results$%s", nm))), silent = TRUE)
          if (!inherits(obj, "try-error"))
            try(obj$clear(), silent = TRUE)
        }
        invisible(TRUE)
      },
      
      .populateThreeStepTables = function(res3) {
        private$.clearThreeStepTables()
        
        if (is.null(res3))
          return(invisible(NULL))
        
        if (!is.null(res3$msg)) {
          private$.setTextSafe(res3$msg)
          return(invisible(NULL))
        }
        
        # jamovi numeric cells: NA를 그대로 넣으면 NaN처럼 보일 수 있어 빈값 처리
        cell_num <- function(x) {
          if (length(x) == 0 || is.null(x) || is.na(x) || is.nan(x))
            return(NULL)
          x
        }
        cell_txt <- function(x) {
          if (length(x) == 0 || is.null(x) || is.na(x))
            return("")
          as.character(x)
        }
        
        if (!is.null(res3$summary) && nrow(res3$summary) > 0) {
          tab <- self$results$threeStepSummary
          for (i in seq_len(nrow(res3$summary))) {
            rw <- res3$summary[i, , drop = FALSE]
            vals <- list(
              distal      = cell_txt(rw$distal[[1]]),
              dtype       = cell_txt(rw$dtype[[1]]),
              method      = cell_txt(rw$method[[1]]),
              k           = cell_num(rw$k[[1]]),
              j           = cell_num(rw$j[[1]]),
              test        = cell_txt(rw$test[[1]]),
              df1         = cell_num(rw$df1[[1]]),
              df2         = cell_num(rw$df2[[1]]),
              statistic   = cell_num(rw$statistic[[1]]),
              p           = cell_num(rw$p[[1]]),
              effect      = cell_num(rw$effect[[1]]),
              effectLabel = cell_txt(rw$effectLabel[[1]])
            )
            vals <- vals[!vapply(vals, is.null, logical(1))]
            tab$addRow(rowKey = i, values = vals)
          }
        }
        
        if (!is.null(res3$classStats) && nrow(res3$classStats) > 0) {
          tab <- self$results$threeStepClassStats
          for (i in seq_len(nrow(res3$classStats))) {
            rw <- res3$classStats[i, , drop = FALSE]
            vals <- list(
              class     = cell_txt(rw$class[[1]]),
              category  = cell_txt(rw$category[[1]]),
              estimate  = cell_num(rw$estimate[[1]]),
              statLabel = cell_txt(rw$statLabel[[1]])
            )
            vals <- vals[!vapply(vals, is.null, logical(1))]
            tab$addRow(rowKey = i, values = vals)
          }
        }
        
        if (!is.null(res3$pairwise) && nrow(res3$pairwise) > 0) {
          tab <- self$results$threeStepPairwise
          for (i in seq_len(nrow(res3$pairwise))) {
            rw <- res3$pairwise[i, , drop = FALSE]
            vals <- list(
              comparison  = cell_txt(rw$comparison[[1]]),
              test        = cell_txt(rw$test[[1]]),
              df1         = cell_num(rw$df1[[1]]),
              df2         = cell_num(rw$df2[[1]]),
              statistic   = cell_num(rw$statistic[[1]]),
              p           = cell_num(rw$p[[1]]),
              pBH         = cell_num(rw$pBH[[1]]),
              pBonf       = cell_num(rw$pBonf[[1]]),
              effect      = cell_num(rw$effect[[1]]),
              effectLabel = cell_txt(rw$effectLabel[[1]])
            )
            vals <- vals[!vapply(vals, is.null, logical(1))]
            tab$addRow(rowKey = i, values = vals)
          }
        }
        
        if (!is.null(res3$notes) && length(res3$notes) > 0)
          private$.setTextSafe(paste(res3$notes, collapse = "\n"))
        
        invisible(TRUE)
      },

      .getPlot1Data = function() {
        if (!is.null(private$.plot1_cache))
          return(private$.plot1_cache)
        
        df   <- as.data.frame(self$data)
        vars <- self$options$vars
        
        if (is.null(vars) || length(vars) < 1)
          return(NULL)
        
        isNum <- vapply(df[vars], is.numeric, TRUE)
        xvars <- vars[isNum]
        
        if (length(xvars) < 1)
          return(NULL)
        
        if (!("id" %in% names(df)))
          df$id <- seq_len(nrow(df))
        
        long <- reshape(
          df[, c("id", xvars), drop = FALSE],
          direction = "long",
          varying   = xvars,
          v.names   = "value",
          idvar     = "id",
          timevar   = "time"
        )
        
        long$time <- factor(long$time, labels = xvars)
        
        private$.plot1_cache <- long
        private$.plot1_cache
      },
      
      .computeThreeStep = function() {
        auxName <- self$options$auxVar
        if (is.null(auxName) || !nzchar(auxName))
          return(NULL)
        
        dat <- as.data.frame(self$data)
        if (!(auxName %in% names(dat)))
          return(list(msg = sprintf("[3-step] '%s' not found in data.", auxName)))
        
        ind <- self$classProbabilities$individual
        
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
          ytype <- "categorical"
          yfac <- as.factor(yraw)
        } else if (is_cat_like(yraw)) {
          ytype <- "categorical"
          yfac <- factor(yraw)
          notes <- c(notes, sprintf("[note] '%s' has few integer levels; treated as categorical (DCAT).", auxName))
        } else if (is.numeric(yraw)) {
          ytype <- "numeric"
        } else {
          return(list(msg = sprintf("[3-step] Unsupported type for '%s'.", auxName)))
        }
        
        if (ytype == "numeric") {
          y <- yraw
          wmask <- is.finite(y)
        } else {
          y <- yfac
          wmask <- !is.na(y)
        }
        for (pc in pcols)
          ind[[pc]][!wmask] <- 0
        
        # =========================
        # BCH (numeric)
        # =========================
        if (ytype == "numeric") {
          long <- lapply(seq_len(K), function(k) {
            data.frame(
              class = factor(k, levels = cls_levels),
              y = y,
              w = ind[[pcols[k]]]
            )
          })
          long <- do.call(rbind, long)
          
          eff_nk <- tapply(long$w, long$class, sum)
          if (any(eff_nk < 1e-6))
            notes <- c(notes, "[warn] One or more classes have (near-)zero effective weight for the distal variable.")
          if (stats::var(long$y[long$w > 0], na.rm = TRUE) < 1e-10)
            notes <- c(notes, "[warn] Near-zero variance in distal variable (BCH may be unreliable).")
          
          wmean <- tapply(long$y * long$w, long$class, sum) / pmax(eff_nk, .Machine$double.eps)
          
          wvar <- sapply(levels(long$class), function(k) {
            idx <- long$class == k
            wk <- long$w[idx]
            yk <- long$y[idx]
            mk <- if (eff_nk[k] > 0) sum(wk * yk) / eff_nk[k] else NA_real_
            if (!is.finite(mk) || eff_nk[k] <= 0)
              return(NA_real_)
            sum(wk * (yk - mk)^2) / eff_nk[k]
          })
          
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
          
          summary_df <- data.frame(
            distal      = auxName,
            dtype       = "numeric",
            method      = "BCH",
            k           = K,
            j           = NA_integer_,
            test        = "Wald/ANOVA approx",
            df1         = as.integer(dfb),
            df2         = as.numeric(dfw),
            statistic   = as.numeric(Fst),
            p           = as.numeric(pF),
            effect      = NA_real_,
            effectLabel = "",
            stringsAsFactors = FALSE
          )
          
          class_df <- data.frame(
            class     = paste0("Class ", seq_len(K)),
            category  = "",
            estimate  = as.numeric(wmean),
            statLabel = "Weighted mean",
            stringsAsFactors = FALSE
          )
          
          pair_df <- NULL
          if (K >= 2) {
            pair_lab <- c()
            p_raw <- c()
            zvals <- c()
            dvals <- c()
            
            for (a in 1:(K-1)) for (b in (a+1):K) {
              za <- (wmean[a] - wmean[b]) / sqrt(wse[a]^2 + wse[b]^2)
              pa <- 2 * stats::pnorm(-abs(za))
              
              spooled <- sqrt(((eff_nk[a]-1) * wvar[a] + (eff_nk[b]-1) * wvar[b]) /
                                pmax(eff_nk[a] + eff_nk[b] - 2, 1))
              d_ab <- if (is.finite(spooled) && spooled > 0)
                (wmean[a] - wmean[b]) / spooled
              else
                NA_real_
              
              pair_lab <- c(pair_lab, sprintf("Class %d vs %d", a, b))
              p_raw    <- c(p_raw, pa)
              zvals    <- c(zvals, za)
              dvals    <- c(dvals, d_ab)
            }
            
            p_bh   <- stats::p.adjust(p_raw, method = "BH")
            p_bonf <- stats::p.adjust(p_raw, method = "bonferroni")
            
            pair_df <- data.frame(
              comparison  = pair_lab,
              test        = "z",
              df1         = NA_integer_,
              df2         = NA_real_,
              statistic   = as.numeric(zvals),
              p           = as.numeric(p_raw),
              pBH         = as.numeric(p_bh),
              pBonf       = as.numeric(p_bonf),
              effect      = as.numeric(dvals),
              effectLabel = "Cohen's d",
              stringsAsFactors = FALSE
            )
          }
          
          return(list(
            summary    = summary_df,
            classStats = class_df,
            pairwise   = pair_df,
            notes      = notes
          ))
        }
        
        # =========================
        # DCAT (categorical)
        # =========================
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
        
        summary_df <- data.frame(
          distal      = auxName,
          dtype       = "categorical",
          method      = "DCAT",
          k           = K,
          j           = length(cats),
          test        = "Wald/Chi-square",
          df1         = as.integer(df),
          df2         = NA_real_,
          statistic   = as.numeric(chi),
          p           = as.numeric(pchi),
          effect      = as.numeric(Vall),
          effectLabel = "Cramer's V",
          stringsAsFactors = FALSE
        )
        
        class_list <- lapply(seq_len(K), function(k) {
          props <- if (sum(W[k, ]) > 0) W[k, ] / sum(W[k, ]) else rep(NA_real_, length(cats))
          data.frame(
            class     = paste0("Class ", k),
            category  = cats,
            estimate  = as.numeric(props),
            statLabel = "Proportion",
            stringsAsFactors = FALSE
          )
        })
        class_df <- do.call(rbind, class_list)
        
        pair_df <- NULL
        if (K >= 2) {
          pair_lab <- c()
          p_raw <- c()
          chis <- c()
          df2v <- c()
          Vpair <- c()
          
          for (a in 1:(K-1)) for (b in (a+1):K) {
            sub <- rbind(W[a, ], W[b, ])
            rs  <- rowSums(sub)
            cs  <- colSums(sub)
            g   <- sum(rs)
            Eab <- outer(rs, cs) / ifelse(g > 0, g, NA_real_)
            chiab <- sum((sub - Eab)^2 / pmax(Eab, .Machine$double.eps), na.rm = TRUE)
            dfab  <- ncol(W) - 1
            pab   <- stats::pchisq(chiab, df = dfab, lower.tail = FALSE)
            Vab   <- sqrt(chiab / pmax(g, .Machine$double.eps))
            
            pair_lab <- c(pair_lab, sprintf("Class %d vs %d", a, b))
            p_raw    <- c(p_raw, pab)
            chis     <- c(chis, chiab)
            df2v     <- c(df2v, dfab)
            Vpair    <- c(Vpair, Vab)
          }
          
          p_bh   <- stats::p.adjust(p_raw, method = "BH")
          p_bonf <- stats::p.adjust(p_raw, method = "bonferroni")
          
          pair_df <- data.frame(
            comparison  = pair_lab,
            test        = "Chi-square",
            df1         = as.integer(df2v),
            df2         = NA_real_,
            statistic   = as.numeric(chis),
            p           = as.numeric(p_raw),
            pBH         = as.numeric(p_bh),
            pBonf       = as.numeric(p_bonf),
            effect      = as.numeric(Vpair),
            effectLabel = "Cramer's V",
            stringsAsFactors = FALSE
          )
        }
        
        return(list(
          summary    = summary_df,
          classStats = class_df,
          pairwise   = pair_df,
          notes      = notes
        ))
      },
      
#----------------------------------------------------      
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
              '<li>3-step auxiliary results are provided as approximate posterior-probability-based comparisons and should be interpreted with caution for strict methodological applications.</li>',
              '<li>Latent class growth analysis is described in the ',
              '<a href="https://cjvanlissa.github.io/tidySEM/articles/lca_lcga.html" target="_blank">tidySEM article</a>.</li>',
              '<li>Feature requests and bug reports: ',
              '<a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub Issues</a>.</li>',
              '</ul></div>'
            )
          )
        )
      },
      
      .run = function() {
        
        if (!isTRUE(self$options$run))
          return()
        
        if (is.null(self$options$vars) || length(self$options$vars) < 3)
          return()
        
        self$results$progressBarHTML$setVisible(TRUE)
        html <- progressBarH(5, 100, 'Starting analysis...')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
        
        set.seed(1234)
        private$.plot1_cache <- NULL
        
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
        
        if (isTRUE(self$options$use3step) && !is.null(self$options$auxVar) && nzchar(self$options$auxVar)) {
          html <- progressBarH(98, 100, 'Running 3-step auxiliary (BCH/DCAT)...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          
          res3 <- private$.computeThreeStep()
          private$.populateThreeStepTables(res3)
        }
        
        html <- progressBarH(100, 100, 'Analysis complete!')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
        self$results$progressBarHTML$setVisible(FALSE)
      },
      
      .populateDescTable = function() {
        vars  <- self$options$vars
        table <- self$results$desc
        d     <- as.data.frame(self$desc)
        for (i in seq_along(vars))
          table$addRow(rowKey = vars[i], values = as.list(d[i, 2:8]))
      },
      
      .populateFitTable = function() {
        table <- self$results$fit
        df    <- as.data.frame(t(self$fit))
        for (nm in rownames(df))
          table$addRow(rowKey = nm, values = list(value = df[nm, 1]))
      },
      
      .populateEST = function() {
        table <- self$results$est
        e     <- as.data.frame(self$parameters)
        for (nm in rownames(e))
          table$addRow(rowKey = nm, values = list(
            cat  = e[nm, 1],
            lhs  = e[nm, 2],
            est  = e[nm, 3],
            se   = e[nm, 4],
            p    = e[nm, 5],
            ci   = e[nm, 6],
            na   = e[nm, 7]
          ))
      },
      
    .populateClassSizeTable = function() {
      table <- self$results$cp
      cpdf  <- as.data.frame(self$classProbabilities$summary)
      
      for (i in seq_len(nrow(cpdf))) {
        table$addRow(rowKey = as.integer(cpdf$Class[i]), values = list(
          count = as.integer(cpdf$Count[i]),
          prop  = as.numeric(cpdf$Proportion[i])
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
        cpdf <- as.data.frame(self$classProbabilities$summary)
        
        plot_data <- data.frame(
          Class = factor(cpdf$Class, levels = sort(unique(cpdf$Class))),
          Count = as.numeric(cpdf$Count),
          Proportion = as.numeric(cpdf$Proportion),
          Percentage = round(as.numeric(cpdf$Proportion) * 100, 1)
        )
        
        self$results$plot2$setState(plot_data)
      },
      
      .setPlot1 = function() {
        long <- private$.getPlot1Data()
        if (is.null(long))
          return()
        
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
        p <- tidySEM::plot_growth(
          image$state,
          rawdata = self$options$raw,
          alpha_range = c(0, 0.05)
        )
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
  
  return(html)
}