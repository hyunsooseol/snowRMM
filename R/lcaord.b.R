
# This file is a generated template, your changes will not be overwritten

#' @importFrom utils capture.output

lcaordClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "lcaordClass",
    inherit = lcaordBase,
    
    active = list(
      res = function() {
        if (is.null(private$.res_cache)) {
          # --- 전체 데이터(보조분석용) & 지표 데이터(모형용) 분리 ---
          data_all <- self$data
          if (self$options$miss == 'listwise')
            data_all <- jmvcore::naOmit(data_all)
          data_all <- as.data.frame(data_all)
          
          ind_names <- self$options$vars
          if (is.null(ind_names) || length(ind_names) < 3)
            stop("Select at least 3 ordinal indicators in 'Variables'.")
          
          data_ind <- data_all[, ind_names, drop = FALSE]
          
          # mx_lca 요구사항: 모든 지표는 binary 또는 ordered factor
          data_ind[] <- lapply(data_ind, function(x) {
            if (is.ordered(x)) return(x)
            if (is.factor(x))  return(ordered(x))
            stop("All indicators must be ordinal (binary or ordered factors).")
          })
          
          private$.res_cache <- tidySEM::mx_lca(
            data    = data_ind,
            classes = self$options$nc
          )
        }
        private$.res_cache
      },
      
      desc = function() {
        if (is.null(private$.desc_cache)) {
          # 지표 변수만 요약
          data_all <- self$data
          if (self$options$miss == 'listwise')
            data_all <- jmvcore::naOmit(data_all)
          data_all <- as.data.frame(data_all)
          
          ind_names <- self$options$vars
          data_ind  <- data_all[, ind_names, drop = FALSE]
          
          desc <- tidySEM::descriptives(data_ind)
          private$.desc_cache <- desc[, c("name","type","n","missing","unique","mode")]
        }
        private$.desc_cache
      }
    ),
    
    private = list(
      .htmlwidget      = NULL,
      .results_cache   = NULL,
      .res_cache       = NULL,
      .desc_cache      = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) || is.null(self$options$vars))
          self$results$instructions$setVisible(TRUE)
        
        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title = "Instructions",
            content = paste(
              '<div style="border:2px solid #e6f4fe;border-radius:15px;padding:15px;',
              'background-color:#e6f4fe;margin-top:10px;"><ul>',
              '<li>Latent class analysis for ordinal indicators is described in the ',              
              '<a href="https://cjvanlissa.github.io/tidySEM/articles/lca_ordinal.html" target="_blank">tidySEM article</a>.</li>',
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div>'
            )
          )
        )
        
        if (isTRUE(self$options$plot1))
          self$results$plot1$setSize(self$options$width1, self$options$height1)
        
        if (isTRUE(self$options$plot))
          self$results$plot$setSize(self$options$width, self$options$height)
        
        private$.registerCallbacks()
      },
      
      .registerCallbacks = function() {
        callbacks <- list(
          desc = private$.populateDescTable,
          fit  = private$.populateFitTable,
          cp   = private$.populateClassSizeTable,
          mem  = private$.populateClassMemberTable
        )
        for (name in names(callbacks)) {
          if (name %in% names(self$results) &&
              !is.null(self$results[[name]]) &&
              "setCallback" %in% names(self$results[[name]])) {
            self$results[[name]]$setCallback(callbacks[[name]])
          }
        }
      },
      
      .run = function() {
        if (is.null(self$options$vars) || length(self$options$vars) < 3)
          return()
        
        # Show progress bar
        self$results$progressBarHTML$setVisible(TRUE)
        html <- progressBarH(5,  100, 'Initializing analysis...')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
        
        # --- 전체 데이터 & 지표 데이터 분리(캐시에 둘 다 보관) ---
        # (!) formula/auxVar에 등장하는 변수까지 강제로 로드 시도
        ind_names <- self$options$vars
        needed <- ind_names
        
        aux <- self$options$auxVar
        if (!is.null(aux) && nzchar(aux))
          needed <- unique(c(needed, aux))
        
        fstr_needed <- self$options$auxFormula
        if (!is.null(fstr_needed) && nzchar(fstr_needed)) {
          f_need <- try(stats::as.formula(fstr_needed), silent = TRUE)
          if (!inherits(f_need, "try-error"))
            needed <- unique(c(needed, all.vars(f_need)))
        }
        
        # 실제 존재하는 컬럼만 선택 (없는 이름은 드롭하고 기록)
        all_cols <- try(colnames(self$data), silent = TRUE)
        if (inherits(all_cols, "try-error") || is.null(all_cols))
          all_cols <- names(as.data.frame(self$data))
        
        needed <- unique(needed)
        missing_cols <- setdiff(needed, all_cols)
        needed_present <- intersect(needed, all_cols)
        
        # 지표(필수) 중 빠진 것이 있으면 바로 에러
        miss_ind <- setdiff(ind_names, needed_present)
        if (length(miss_ind) > 0)
          stop(sprintf("Indicators not found in data: %s", paste(miss_ind, collapse = ", ")))
        
        # 존재하는 것만 먼저 subset → jamovi가 해당 컬럼을 로드
        data_all <- self$data[needed_present]
        
        # 이후 결측처리/프레임화
        if (self$options$miss == 'listwise')
          data_all <- jmvcore::naOmit(data_all)
        data_all <- as.data.frame(data_all)
        
        # LCA 지표 데이터(ordinal 강제)
        data_ind  <- data_all[, ind_names, drop = FALSE]
        data_ind[] <- lapply(data_ind, function(x) {
          if (is.ordered(x)) x else if (is.factor(x)) ordered(x) else x
        })
        
        # Compute results once
        if (is.null(private$.results_cache)) {
          set.seed(1234)
          
          # 15%: model fitting
          html <- progressBarH(15, 100, 'Performing LCA model...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          res  <- self$res
          desc <- self$desc
          
          # 25%: descriptives
          html <- progressBarH(25, 100, 'Computing descriptives...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          fit <- tidySEM::table_fit(res)
          
          # 35%: class probabilities
          html <- progressBarH(35, 100, 'Computing class probabilities...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          cp1 <- tidySEM::class_prob(res)
          cp  <- data.frame(cp1$sum.posterior)
          
          private$.results_cache <- list(
            data_all = data_all,   # 보조분석/회귀용(연속/범주 변수 포함)
            data_ind = data_ind,   # LCA 지표용(모두 ordinal)
            res  = res,
            desc = desc,
            fit  = fit,
            cp   = cp,
            cp1  = cp1,
            missing_aux_cols = missing_cols  # formula/auxVar 중 데이터에 없던 컬럼 기록
          )
          
          # 45%: populate desc
          if (isTRUE(self$options$desc)) {
            html <- progressBarH(45, 100, 'Populating descriptives table...')
            self$results$progressBarHTML$setContent(html)
            private$.checkpoint()
            private$.populateDescTable()
          }
          
          # 55%: populate fit
          if (isTRUE(self$options$fit)) {
            html <- progressBarH(55, 100, 'Populating fit table...')
            self$results$progressBarHTML$setContent(html)
            private$.checkpoint()
            private$.populateFitTable()
          }
          
          # 65%: populate class size
          if (isTRUE(self$options$cp)) {
            html <- progressBarH(65, 100, 'Populating class size table...')
            self$results$progressBarHTML$setContent(html)
            private$.checkpoint()
            private$.populateClassSizeTable()
          }
          
          # 75%: populate class member
          if (isTRUE(self$options$mem)) {
            html <- progressBarH(75, 100, 'Populating class members table...')
            self$results$progressBarHTML$setContent(html)
            private$.checkpoint()
            private$.populateClassMemberTable()
          }
          
          # 85%: plot response probabilities
          if (isTRUE(self$options$plot)) {
            html <- progressBarH(85, 100, 'Generating response probability plot...')
            self$results$progressBarHTML$setContent(html)
            private$.checkpoint()
            private$.setResponseProbPlot()
          }
          
          # 95%: plot bar
          if (isTRUE(self$options$plot1)) {
            html <- progressBarH(95,100,'Generating bar plot...')
            self$results$progressBarHTML$setContent(html)
            private$.checkpoint()
            private$.setBarPlot()
          }
          
          # ===================== 96%: 3-STEP (텍스트 출력) =====================
          if (isTRUE(self$options$use3step)) {
            html <- progressBarH(96, 100, 'Running 3-STEP (BCH/DCAT)...')
            self$results$progressBarHTML$setContent(html)
            private$.checkpoint()
            
            dat <- private$.results_cache$data_all  # 보조변수 포함 전체 데이터
            
            ## (A) Distal 결과 텍스트 (효과크기 + 보정 포함)
            out_lines <- c("=== 3-STEP auxiliary (tidySEM) ===")
            vname <- self$options$auxVar
            if (!is.null(vname) && nzchar(vname)) {
              tres <- private$.computeThreeStep(dat, vname)
              out_lines <- c(out_lines, tres)
            } else {
              out_lines <- c(out_lines, "[Distal] no variable selected.")
            }
            
            ## (B) Class-wise 회귀 텍스트: 옵션 reg가 켜진 경우에만 별도 출력
            reg_lines <- NULL
            if (isTRUE(self$options$reg)) {
              fstr <- self$options$auxFormula
              reg_lines <- c("=== Class-wise regression (3-STEP BCH) ===")
              if (!is.null(fstr) && nzchar(fstr)) {
                rres <- private$.run3stepReg(self$res, dat, fstr)
                reg_lines <- c(reg_lines, rres$msg)
              } else {
                reg_lines <- c(reg_lines, "[Regression] no formula.")
              }
            }
            
            ## 데이터에 없던 보조/회귀 변수 각주
            miss_aux <- private$.results_cache$missing_aux_cols
            if (!is.null(miss_aux) && length(miss_aux) > 0) {
              note <- sprintf("[Note] columns not found and ignored: %s",
                              paste(miss_aux, collapse = ", "))
              out_lines <- c(out_lines, note)
              if (!is.null(reg_lines)) reg_lines <- c(reg_lines, note)
            }
            
            ## r.yaml: name = use3step / reg (Preformatted)에 출력
            if (!is.null(self$results$use3step))
              self$results$use3step$setContent(paste(out_lines, collapse = "\n"))
            if (!is.null(reg_lines) && !is.null(self$results$reg))
              self$results$reg$setContent(paste(reg_lines, collapse = "\n"))
          }
          # ===================================================================
        }
        
        # 100%: complete and hide
        html <- progressBarH(100,100,'Analysis complete!')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
        self$results$progressBarHTML$setVisible(FALSE)
        
        private$.registerCallbacks()
      },
      
      .populateDescTable = function() {
        vars  <- self$options$vars
        table <- self$results$desc
        d     <- data.frame(private$.results_cache$desc)
        lapply(seq_along(vars), function(i) {
          row <- list(
            type   = d[[2]][i],
            n      = d[[3]][i],
            missing= d[[4]][i],
            unique = d[[5]][i],
            mode   = d[[6]][i]
          )
          table$addRow(rowKey = vars[i], values = row)
        })
      },
      
      .populateFitTable = function() {
        table <- self$results$fit
        df    <- as.data.frame(t(private$.results_cache$fit))
        lapply(rownames(df), function(name) {
          table$addRow(rowKey = name, values = list(value = df[name,1]))
        })
      },
      
      .populateClassSizeTable = function() {
        table <- self$results$cp
        d     <- private$.results_cache$cp
        vars  <- self$options$vars
        lapply(seq_len(nrow(d)), function(i) {
          table$addRow(rowKey = vars[i],
                       values = list(name = d[[1]][i], count = d[[2]][i], prop = d[[3]][i]))
        })
      },
      
      .populateClassMemberTable = function() {
        table <- self$results$mem
        mem   <- data.frame(private$.results_cache$cp1$individual)
        m     <- as.factor(mem$predicted)
        if (table$isNotFilled()) {
          table$setRowNums(rownames(self$data))
          table$setValues(m)
        }
      },
      
      .setResponseProbPlot = function() {
        image <- self$results$plot
        image$setState(private$.results_cache$res)
      },
      
      .setBarPlot = function() {
        # 지표 데이터만 사용해야 막대그래프가 올바름
        df <- as.data.frame(private$.results_cache$data_ind)
        names(df) <- paste0("Value.", names(df))
        df_long <- reshape(df, varying = names(df), direction = "long")
        self$results$plot1$setState(df_long)
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) return(FALSE)
        p <- tidySEM::plot_prob(image$state, bw=TRUE) + ggtheme
        if (self$options$angle>0)
          p <- p + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=self$options$angle, hjust=1))
        print(p); TRUE
      },
      
      .plot1 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) return(FALSE)
        p <- ggplot(image$state, aes(x=Value)) +
          geom_bar() + facet_wrap(~time, scales="free") + theme_bw() + ggtheme
        print(p); TRUE
      },
      
      # ------------------------------------------------------------------
      # 3-STEP (BCH/DCAT) with effect sizes & multiple-comparison control
      # ------------------------------------------------------------------
      .computeThreeStep = function(dat_all, auxName) {
        # posterior 가져오기 (results_cache가 있으면 거기서)
        ind <- NULL
        if (!is.null(private$.results_cache$cp1$individual)) {
          ind <- data.frame(private$.results_cache$cp1$individual)
        } else {
          cp <- try(tidySEM::class_prob(self$res), silent = TRUE)
          if (!inherits(cp, "try-error") && !is.null(cp$individual))
            ind <- data.frame(cp$individual)
        }
        if (is.null(ind))
          return("[3-step] posterior probabilities not found.")
        
        # posterior 열 찾기
        .findPcols <- function(nms) {
          pats <- c("^CPROB[0-9]+$","^Class[_\\.]?[0-9]+$","^C[0-9]+$",
                    "^p[0-9]+$","^posterior[_\\.]?[0-9]+$")
          hits <- unique(unlist(lapply(pats, function(p) grep(p, nms))))
          if (length(hits)) return(nms[hits])
          num <- nms[vapply(ind, is.numeric, TRUE)]
          cand <- setdiff(num, c("id","ID","predicted","Predicted"))
          if (length(cand) > 1) {
            S <- rowSums(ind[, cand, drop=FALSE], na.rm=TRUE)
            if (all(is.finite(S)) && mean(abs(S-1)) < 1e-3) return(cand)
          }
          character(0)
        }
        pcols <- .findPcols(names(ind))
        if (length(pcols) == 0)
          return("[3-step] posterior columns could not be identified.")
        
        K <- length(pcols)
        
        if (is.null(dat_all[[auxName]]))
          return(sprintf("[Distal] variable '%s' not found.", auxName))
        
        yraw <- dat_all[[auxName]]
        # y 결측이면 그 행의 모든 가중치 0으로
        ok <- if (is.numeric(yraw)) is.finite(yraw) else !is.na(yraw)
        for (pc in pcols) ind[[pc]][!ok] <- 0
        
        # ====== BCH (numeric) ======
        if (is.numeric(yraw) && !(is.factor(yraw) || is.ordered(yraw))) {
          y <- yraw
          eff <- sapply(pcols, function(pc) sum(ind[[pc]], na.rm=TRUE))
          m   <- sapply(pcols, function(pc) sum(ind[[pc]]*y, na.rm=TRUE) / pmax(sum(ind[[pc]],na.rm=TRUE), .Machine$double.eps))
          v   <- mapply(function(pc, mu) {
            w <- ind[[pc]]; sum(w*(y-mu)^2, na.rm=TRUE) / pmax(sum(w,na.rm=TRUE), .Machine$double.eps)
          }, pcols, m)
          se  <- sqrt(v / pmax(eff,1))
          
          # 전체 F
          wbar <- sum(y * rowSums(ind[, pcols, drop=FALSE]), na.rm=TRUE) /
            sum(rowSums(ind[, pcols, drop=FALSE]), na.rm=TRUE)
          ssb  <- sum(eff * (m - wbar)^2, na.rm=TRUE)
          ssw  <- 0
          for (i in seq_along(pcols)) {
            w <- ind[[pcols[i]]]; mu <- m[i]
            ssw <- ssw + sum(w*(y-mu)^2, na.rm=TRUE)
          }
          df1 <- K - 1; df2 <- max(1, sum(eff,na.rm=TRUE) - K)
          Fst <- (ssb/df1) / (ssw/df2); pF <- stats::pf(Fst, df1, df2, lower.tail = FALSE)
          
          lines <- c(
            "3-step analysis (BCH/DCAT)",
            "=== 3-STEP auxiliary (ordinal LCA, BCH) ===",
            sprintf("[Distal: %s, numeric] K=%d", auxName, K),
            sprintf("Class-weighted means: %s",
                    paste(paste0("C", seq_len(K), "=", round(m,3)), collapse=", ")),
            sprintf("Wald/ANOVA approx: F(%d,%.1f)=%.3f, p=%.4f", df1, df2, Fst, pF)
          )
          
          # 쌍별: z, p, BH/Bonf, Cohen's d
          if (K >= 2) {
            lab <- c(); z <- c(); p <- c(); d <- c()
            for (a in 1:(K-1)) for (b in (a+1):K) {
              za <- (m[a]-m[b]) / sqrt(se[a]^2 + se[b]^2)
              pa <- 2*stats::pnorm(-abs(za))
              sp <- sqrt(((eff[a]-1)*v[a] + (eff[b]-1)*v[b]) /
                           pmax(eff[a]+eff[b]-2, 1))
              d  <- c(d, (m[a]-m[b]) / sp); lab <- c(lab, sprintf("%d vs %d", a,b))
              z  <- c(z, za); p <- c(p, pa)
            }
            p_bh <- stats::p.adjust(p, "BH")
            p_bonf <- stats::p.adjust(p, "bonferroni")
            lines <- c(lines, "Pairwise (z, p, p_BH, p_Bonf, Cohen's d):")
            for (i in seq_along(lab))
              lines <- c(lines, sprintf("  Class %s: z=%.3f, p=%.3g, p_BH=%.3g, p_Bonf=%.3g, d=%.3f",
                                        lab[i], z[i], p[i], p_bh[i], p_bonf[i], d[i]))
          }
          return(paste(lines, collapse="\n"))
        }
        
        # ====== DCAT (categorical/ordinal) ======
        f <- as.factor(yraw)
        J <- nlevels(f)
        W <- matrix(0, nrow=K, ncol=J, dimnames=list(paste0("C",1:K), levels(f)))
        for (k in 1:K) for (j in 1:J)
          W[k,j] <- sum( (f==levels(f)[j]) * ind[[pcols[k]]], na.rm=TRUE )
        
        rs <- rowSums(W); cs <- colSums(W); N <- sum(rs)
        E  <- outer(rs, cs) / ifelse(N>0, N, NA_real_)
        chi <- sum((W - E)^2 / pmax(E, .Machine$double.eps), na.rm=TRUE)
        df  <- (K-1)*(J-1)
        pchi <- stats::pchisq(chi, df=df, lower.tail=FALSE)
        Vall <- sqrt( chi / (N * max(1, min(K-1, J-1))) )
        
        lines <- c(
          "3-step analysis (BCH/DCAT)",
          "=== 3-STEP auxiliary (ordinal LCA, DCAT) ===",
          sprintf("[Distal: %s, categorical/ordinal] K=%d, J=%d", auxName, K, J),
          sprintf("Wald/Chi-square: X^2(%d)=%.3f, p=%.4f, Cramer's V=%.3f", df, chi, pchi, Vall)
        )
        
        if (K >= 2) {
          lab <- c(); p <- c(); X2 <- c(); dfv <- c(); Vpair <- c()
          for (a in 1:(K-1)) for (b in (a+1):K) {
            sub <- rbind(W[a,], W[b,])
            rs2 <- rowSums(sub); cs2 <- colSums(sub); N2 <- sum(rs2)
            E2  <- outer(rs2, cs2) / ifelse(N2>0, N2, NA_real_)
            chi2 <- sum((sub - E2)^2 / pmax(E2, .Machine$double.eps), na.rm=TRUE)
            df2  <- J-1; p2 <- stats::pchisq(chi2, df=df2, lower.tail=FALSE)
            V2   <- sqrt( chi2 / pmax(N2, .Machine$double.eps) )   # 2×J → min=1
            lab <- c(lab, sprintf("%d vs %d", a,b)); p <- c(p,p2)
            X2 <- c(X2, chi2); dfv <- c(dfv, df2); Vpair <- c(Vpair, V2)
          }
          p_bh <- stats::p.adjust(p, "BH")
          p_bonf <- stats::p.adjust(p, "bonferroni")
          lines <- c(lines, "Pairwise (chi-square; p, p_BH, p_Bonf, Cramer's V):")
          for (i in seq_along(lab))
            lines <- c(lines, sprintf("  Class %s: X^2(%d)=%.3f, p=%.3g, p_BH=%.3g, p_Bonf=%.3g, V=%.3f",
                                      lab[i], dfv[i], X2[i], p[i], p_bh[i], p_bonf[i], Vpair[i]))
        }
        paste(lines, collapse="\n")
      },
      
      # ---- 회귀(기존 유지) ----------------------------------------------------
      .run3stepReg = function(res_final, dat, formula_str) {
        if (is.null(formula_str) || !nzchar(formula_str))
          return(list(msg = "[Regression] no formula.", tests = NULL))
        
        # 1) formula 파싱
        f <- try(stats::as.formula(formula_str), silent = TRUE)
        if (inherits(f, "try-error"))
          return(list(msg = sprintf("[Regression] invalid formula: %s",
                                    conditionMessage(attr(f, "condition"))),
                      tests = NULL))
        vars <- all.vars(f)
        if (length(vars) < 2)
          return(list(msg = "[Regression] need outcome ~ predictor(s).", tests = NULL))
        
        y     <- vars[1]
        xvars <- vars[-1]
        
        # 2) 예측변수 전처리
        use_x      <- character(0)
        ref_notes  <- character(0)
        
        for (vx in xvars) {
          if (is.null(dat[[vx]]))
            return(list(
              msg = sprintf("[Regression] predictor '%s' not found. Add it to data/options.", vx),
              tests = NULL))
          
          v <- dat[[vx]]
          if (is.character(v)) v <- factor(v)
          if (is.factor(v)) {
            v <- droplevels(v)
            nl <- nlevels(v)
            if (nl <= 1) {
              ref_notes <- c(ref_notes, sprintf("%s: single level → dropped", vx))
              next
            } else if (nl == 2) {
              dat[[vx]] <- as.integer(v) - 1L
              use_x <- c(use_x, vx)
              ref_notes <- c(ref_notes, sprintf("%s: binary factor, ref='%s'→0/1", vx, levels(v)[1]))
            } else {
              tab <- table(v); ref <- names(tab)[which.max(tab)]
              for (lev in levels(v)) {
                if (lev == ref) next
                new_name <- paste0(vx, "_", make.names(lev))
                dat[[new_name]] <- as.integer(v == lev)
                use_x <- c(use_x, new_name)
              }
              ref_notes <- c(ref_notes,
                             sprintf("%s: %d-level factor → %d dummies (ref='%s')",
                                     vx, nl, nl - 1, ref))
            }
          } else {
            dat[[vx]] <- suppressWarnings(as.numeric(v))
            use_x <- c(use_x, vx)
          }
        }
        
        if (is.null(dat[[y]]))
          return(list(msg = sprintf("[Regression] outcome '%s' not found.", y), tests = NULL))
        if (is.character(dat[[y]]) || is.factor(dat[[y]]))
          dat[[y]] <- suppressWarnings(as.numeric(dat[[y]]))
        
        if (length(use_x) == 0)
          return(list(msg = "[Regression] no usable predictors after processing.", tests = NULL))
        
        needed_cols <- unique(c(y, use_x))
        cc <- stats::complete.cases(dat[, needed_cols, drop = FALSE])
        ncc <- sum(cc)
        if (ncc < 10)
          return(list(msg = sprintf("[Regression] insufficient complete cases (n=%d).", ncc),
                      tests = NULL))
        dat_cc <- dat[cc, , drop = FALSE]
        
        new_formula <- sprintf("%s ~ %s", y, paste(use_x, collapse = " + "))
        
        fit2 <- try(tidySEM::BCH(res_final, model = new_formula, data = dat_cc), silent = TRUE)
        if (inherits(fit2, "try-error")) {
          emsg <- conditionMessage(attr(fit2, "condition"))
          note <- if (length(ref_notes)) paste0(" (", paste(ref_notes, collapse="; "), ")") else ""
          return(list(msg = sprintf("[Regression] BCH(model=...) fit failed: %s%s", emsg, note),
                      tests = NULL))
        }
        
        lrA <- try(tidySEM::lr_test(fit2, compare = "A"), silent = TRUE)
        if (!inherits(lrA, "try-error") && is.data.frame(lrA) && nrow(lrA) > 0) {
          nml <- tolower(names(lrA))
          get <- function(cs) { i <- which(nml %in% cs)[1]; if (length(i)==0 || is.na(i)) NA else lrA[[i]] }
          dff  <- suppressWarnings(as.integer(get(c("df","dof"))[1]))
          stat <- suppressWarnings(as.numeric(get(c("lr","chisq","x2","lrchisq"))[1]))
          pv   <- suppressWarnings(as.numeric(get(c("p","p.value"))[1]))
          note <- if (length(ref_notes)) paste0(" (", paste(ref_notes, collapse="; "), ")") else ""
          msg  <- sprintf("[Regression: %s] LR_A(df=%s) = %.2f, p = %.3g%s",
                          new_formula, as.character(dff), stat, pv, note)
          tests <- data.frame(method="LR (compare='A')", df=dff, stat=stat, p=pv,
                              constraint="", stringsAsFactors=FALSE)
          return(list(msg = msg, tests = tests))
        }
        
        K <- length(self$options$nc); if (is.null(K) || K < 2) K <- 2L
        p <- length(use_x)
        cons_list <- character(0)
        for (j in seq_len(p)) {
          idx <- paste0("[1,", j + 1L, "]")
          eq  <- paste0("class1.A", idx, "=class2.A", idx)
          cons_list <- c(cons_list, eq)
        }
        cons <- paste(cons_list, collapse = "&")
        
        wd <- try(tidySEM::wald_test(fit2, cons), silent = TRUE)
        if (!inherits(wd, "try-error") && is.data.frame(wd) && nrow(wd) > 0) {
          nmt <- tolower(names(wd))
          get2 <- function(cs) { i <- which(nmt %in% cs)[1]; if (length(i)==0 || is.na(i)) NA else wd[[i]] }
          dff  <- suppressWarnings(as.integer(get2(c("df","dof"))[1])); if (is.na(dff) || !is.finite(dff)) dff <- (2 - 1L) * p
          stat <- suppressWarnings(as.numeric(get2(c("wald","w","statistic"))[1]))
          pv   <- suppressWarnings(as.numeric(get2(c("p","p.value"))[1])); if (is.na(stat) && is.finite(pv)) stat <- stats::qchisq(1 - pv, dff)
          note <- if (length(ref_notes)) paste0(" (", paste(ref_notes, collapse="; "), ")") else ""
          msg  <- sprintf("[Regression: %s] Wald(df=%s) = %.2f, p = %.3g%s",
                          new_formula, as.character(dff), stat, pv, note)
          tests <- data.frame(method="Wald equality of slopes", df=dff, stat=stat, p=pv,
                              constraint=cons, stringsAsFactors=FALSE)
          return(list(msg = msg, tests = tests))
        }
        
        list(msg = "[Regression] LR/Wald both failed.", tests = NULL)
      }
      
    )
  )

# Progress Bar HTML  (R/progressBarH.R)
progressBarH <- function(progress=0, total=100, message='') {
  percentage <- round(progress/total*100)
  width      <- 400 * percentage/100
  html <- paste0(
    '<div style="text-align:center;padding:20px;">',
    '<div style="width:400px;height:20px;border:1px solid #ccc;',
    'background-color:#f8f9fa;margin:0 auto;border-radius:4px;">',
    '<div style="width:', width, 'px;height:18px;',
    'background-color:#999999;border-radius:3px;',
    'transition:width 0.3s ease;"></div>',
    '</div>',
    '<div style="margin-top:8px;font-size:12px;color:#666;">',
    message, ' (', percentage, '%)',
    '</div>',
    '</div>'
  )
  return(html)
}
