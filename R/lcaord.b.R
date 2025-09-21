
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
            res <- private$.results_cache$res
            
            ## (A) Distal 결과 텍스트
            out_lines <- c("=== 3-STEP auxiliary (tidySEM) ===")
            vname <- self$options$auxVar
            if (!is.null(vname) && nzchar(vname)) {
              dres <- private$.run3stepDistal(res, dat, vname)
              out_lines <- c(out_lines, dres$msg)
            } else {
              out_lines <- c(out_lines, "[Distal] no variable selected.")
            }
            
            ## (B) Class-wise 회귀 텍스트: 옵션 reg가 켜진 경우에만 별도 출력
            reg_lines <- NULL
            if (isTRUE(self$options$reg)) {
              fstr <- self$options$auxFormula
              reg_lines <- c("=== Class-wise regression (3-STEP BCH) ===")
              if (!is.null(fstr) && nzchar(fstr)) {
                rres <- private$.run3stepReg(res, dat, fstr)
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
      
      # ---- 3-STEP helpers (BCH/DCAT) : SIMPLIFIED ROBUST VERSION --------------
      .run3stepDistal = function(res_final, dat, vname) {
        if (is.null(vname) || !nzchar(vname) || is.null(dat[[vname]]))
          return(list(msg = sprintf("[Distal] variable '%s' not found.", vname),
                      omni = NULL))
        
        # 데이터 전처리
        orig_var <- dat[[vname]]
        
        # 완전한 케이스만 선택 (LCA 결과와 보조변수 모두)
        lca_data <- self$data[self$options$vars]  # LCA에 사용된 원본 데이터
        complete_lca <- complete.cases(lca_data)
        complete_aux <- !is.na(orig_var)
        complete_both <- complete_lca & complete_aux
        
        if (sum(complete_both) < 10) {
          return(list(msg = sprintf("[Distal: %s] Insufficient complete cases (n=%d).", 
                                    vname, sum(complete_both)), omni = NULL))
        }
        
        # 완전한 케이스만으로 데이터 구성
        dat_clean <- dat[complete_both, , drop = FALSE]
        
        # 변수 타입 결정 및 전처리
        var_clean <- dat_clean[[vname]]
        
        if (is.character(var_clean) || is.factor(var_clean)) {
          # 범주형 변수 처리
          if (is.character(var_clean)) {
            var_clean <- factor(var_clean, exclude = NULL)
          } else {
            var_clean <- droplevels(var_clean)
          }
          
          n_levels <- nlevels(var_clean)
          if (n_levels < 2) {
            return(list(msg = sprintf("[Distal: %s] Less than 2 levels after cleaning.", vname),
                        omni = NULL))
          }
          
          dat_clean[[vname]] <- var_clean
          is_categorical <- TRUE
          
        } else {
          # 연속형 변수 처리
          var_clean <- as.numeric(var_clean)
          if (all(is.na(var_clean))) {
            return(list(msg = sprintf("[Distal: %s] All values are NA after numeric conversion.", vname),
                        omni = NULL))
          }
          dat_clean[[vname]] <- var_clean
          is_categorical <- FALSE
        }
        
        # 포뮬러 생성
        formula_str <- sprintf("%s ~ 1", vname)
        
        # BCH 적합 시도 (단순한 방법부터)
        fit <- NULL
        method_used <- "Unknown"
        error_messages <- character(0)
        
        # 방법 1: 가장 기본적인 BCH
        fit_try1 <- try({
          tidySEM::BCH(res_final, model = formula_str, data = dat_clean)
        }, silent = TRUE)
        
        if (!inherits(fit_try1, "try-error")) {
          fit <- fit_try1
          method_used <- "BCH_basic"
        } else {
          error_messages <- c(error_messages, paste("Basic BCH:", conditionMessage(attr(fit_try1, "condition"))))
        }
        
        # 방법 2: 연속형을 명시적으로 처리
        if (is.null(fit) && !is_categorical) {
          fit_try2 <- try({
            # 연속형 변수를 표준화해서 시도
            dat_std <- dat_clean
            dat_std[[vname]] <- scale(dat_std[[vname]])[,1]
            tidySEM::BCH(res_final, model = formula_str, data = dat_std)
          }, silent = TRUE)
          
          if (!inherits(fit_try2, "try-error")) {
            fit <- fit_try2
            method_used <- "BCH_standardized"
          } else {
            error_messages <- c(error_messages, paste("Standardized BCH:", conditionMessage(attr(fit_try2, "condition"))))
          }
        }
        
        # 방법 3: 범주형을 이진 더미변수로 변환
        if (is.null(fit) && is_categorical && n_levels <= 5) {
          fit_try3 <- try({
            # 가장 빈번한 카테고리를 참조범주로 하여 더미변수 생성
            freq_table <- table(dat_clean[[vname]])
            ref_level <- names(freq_table)[which.max(freq_table)]
            
            # 다른 레벨들에 대한 더미변수 생성
            for (level in levels(dat_clean[[vname]])) {
              if (level != ref_level) {
                dummy_name <- paste0(vname, "_", level)
                dat_clean[[dummy_name]] <- as.numeric(dat_clean[[vname]] == level)
              }
            }
            
            # 첫 번째 더미변수로 모델 적합
            dummy_names <- paste0(vname, "_", setdiff(levels(dat_clean[[vname]]), ref_level))
            if (length(dummy_names) > 0) {
              dummy_formula <- sprintf("%s ~ 1", dummy_names[1])
              tidySEM::BCH(res_final, model = dummy_formula, data = dat_clean)
            } else {
              stop("No dummy variables created")
            }
          }, silent = TRUE)
          
          if (!inherits(fit_try3, "try-error")) {
            fit <- fit_try3
            method_used <- "BCH_dummy"
          } else {
            error_messages <- c(error_messages, paste("Dummy BCH:", conditionMessage(attr(fit_try3, "condition"))))
          }
        }
        
        if (is.null(fit)) {
          error_summary <- paste(error_messages, collapse = "; ")
          return(list(msg = sprintf("[Distal: %s] All fitting methods failed: %s", vname, error_summary),
                      omni = NULL))
        }
        
        # 통계적 검정 수행
        test_result <- NULL
        test_name <- "None"
        
        # LR 테스트 시도
        lr_options <- c("default", "M", "means")
        for (opt in lr_options) {
          lr_test <- try({
            if (opt == "default") {
              tidySEM::lr_test(fit)
            } else {
              tidySEM::lr_test(fit, compare = opt)
            }
          }, silent = TRUE)
          
          if (!inherits(lr_test, "try-error") && is.data.frame(lr_test) && nrow(lr_test) > 0) {
            test_result <- lr_test
            test_name <- paste("LR", opt, sep = "_")
            break
          }
        }
        
        # Wald 테스트 시도 (LR이 실패한 경우)
        if (is.null(test_result)) {
          wald_test <- try({
            # 단순한 평균 동등성 검정
            constraint <- "class1.M[1,1]=class2.M[1,1]"
            tidySEM::wald_test(fit, constraint)
          }, silent = TRUE)
          
          if (!inherits(wald_test, "try-error") && is.data.frame(wald_test) && nrow(wald_test) > 0) {
            test_result <- wald_test
            test_name <- "Wald_means"
          }
        }
        
        # 결과 정리
        if (is.null(test_result)) {
          return(list(msg = sprintf("[Distal: %s, %s] Model fitted but no statistical test available.", 
                                    vname, method_used), omni = NULL))
        }
        
        # 통계량 추출
        stat_value <- NA
        df_value <- NA
        p_value <- NA
        
        # 컬럼명을 소문자로 변환해서 찾기
        col_names <- tolower(names(test_result))
        
        # 통계량 찾기
        stat_cols <- c("lr", "chisq", "wald", "w", "x2", "statistic")
        for (col in stat_cols) {
          if (col %in% col_names) {
            stat_value <- as.numeric(test_result[[which(col_names == col)[1]]])
            break
          }
        }
        
        # 자유도 찾기
        df_cols <- c("df", "dof")
        for (col in df_cols) {
          if (col %in% col_names) {
            df_value <- as.integer(test_result[[which(col_names == col)[1]]])
            break
          }
        }
        
        # p값 찾기
        p_cols <- c("p", "p.value", "pvalue")
        for (col in p_cols) {
          if (col %in% col_names) {
            p_value <- as.numeric(test_result[[which(col_names == col)[1]]])
            break
          }
        }
        
        # 결과 메시지 생성
        msg <- sprintf("[Distal: %s, %s] %s(df=%s) = %.3f, p = %.4f",
                       vname, method_used, test_name,
                       ifelse(is.na(df_value), "?", as.character(df_value)),
                       ifelse(is.na(stat_value), 0, stat_value),
                       ifelse(is.na(p_value), 1, p_value))
        
        omni_result <- data.frame(
          var = vname,
          df = df_value,
          stat = stat_value,
          p = p_value,
          stringsAsFactors = FALSE
        )
        
        return(list(msg = msg, omni = omni_result))
      },
      
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
        
        # 2) 예측변수 전처리: 숫자/이진 → 그대로, 3레벨 이상 factor/문자 → (k-1) 더미 생성
        use_x      <- character(0)    # 최종 포뮬러에 들어갈 예측변수 이름들(더미 포함)
        ref_notes  <- character(0)    # 참조범주 기록
        
        for (vx in xvars) {
          if (is.null(dat[[vx]]))
            return(list(
              msg = sprintf("[Regression] predictor '%s' not found. Add it to data/options.", vx),
              tests = NULL))
          
          v <- dat[[vx]]
          # 문자 → factor
          if (is.character(v)) v <- factor(v)
          if (is.factor(v)) {
            v <- droplevels(v)
            nl <- nlevels(v)
            
            if (nl == 0) {
              next
            } else if (nl == 1) {
              # 변동이 없으면 스킵
              ref_notes <- c(ref_notes, sprintf("%s: single level (%s) → dropped", vx, levels(v)[1]))
              next
            } else if (nl == 2) {
              # 이진 → 0/1
              dat[[vx]] <- as.integer(v) - 1L
              use_x <- c(use_x, vx)
              ref_notes <- c(ref_notes, sprintf("%s: binary factor, ref='%s'→0/1", vx, levels(v)[1]))
            } else {
              # k>2 → 가장 빈도 높은 레벨을 참조로 (안정적)
              tab <- table(v)
              ref <- names(tab)[which.max(tab)]
              lv  <- levels(v)
              
              for (lev in lv) {
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
            # 숫자형
            dat[[vx]] <- suppressWarnings(as.numeric(v))
            use_x <- c(use_x, vx)
          }
        }
        
        # Y를 숫자형으로(문자/범주면 코어스)
        if (is.null(dat[[y]]))
          return(list(msg = sprintf("[Regression] outcome '%s' not found.", y), tests = NULL))
        if (is.character(dat[[y]]) || is.factor(dat[[y]]))
          dat[[y]] <- suppressWarnings(as.numeric(dat[[y]]))
        
        # 예측자가 하나도 남지 않으면 종료
        if (length(use_x) == 0)
          return(list(msg = "[Regression] no usable predictors after processing.", tests = NULL))
        
        # 3) 완전케이스
        needed_cols <- unique(c(y, use_x))
        cc <- stats::complete.cases(dat[, needed_cols, drop = FALSE])
        ncc <- sum(cc)
        if (ncc < 10)
          return(list(msg = sprintf("[Regression] insufficient complete cases (n=%d).", ncc),
                      tests = NULL))
        dat_cc <- dat[cc, , drop = FALSE]
        
        # 4) 더미까지 반영한 새 포뮬러 생성
        new_formula <- sprintf("%s ~ %s", y, paste(use_x, collapse = " + "))
        
        # 5) BCH 회귀 적합
        fit2 <- try(tidySEM::BCH(res_final, model = new_formula, data = dat_cc), silent = TRUE)
        if (inherits(fit2, "try-error")) {
          emsg <- conditionMessage(attr(fit2, "condition"))
          note <- if (length(ref_notes)) paste0(" (", paste(ref_notes, collapse="; "), ")") else ""
          return(list(msg = sprintf("[Regression] BCH(model=...) fit failed: %s%s", emsg, note),
                      tests = NULL))
        }
        
        # 6) Omnibus: A 행렬(기울기) 동일성 LR 테스트
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
        
        # 7) LR 실패 시 Wald: 모든 기울기 동일 제약
        K <- length(res_final$classes); if (is.null(K) || K < 2) K <- 2L
        p <- length(use_x)  # 더미 확장 후 예측자 개수 기준
        cons_list <- character(0)
        for (j in seq_len(p)) {
          idx <- paste0("[1,", j + 1L, "]")
          eq  <- paste0("class1.A", idx, "=class2.A", idx)
          if (K > 2) for (k in 3:K) eq <- paste0(eq, "&class1.A", idx, "=class", k, ".A", idx)
          cons_list <- c(cons_list, eq)
        }
        cons <- paste(cons_list, collapse = "&")
        
        wd <- try(tidySEM::wald_test(fit2, cons), silent = TRUE)
        if (!inherits(wd, "try-error") && is.data.frame(wd) && nrow(wd) > 0) {
          nmt <- tolower(names(wd))
          get2 <- function(cs) { i <- which(nmt %in% cs)[1]; if (length(i)==0 || is.na(i)) NA else wd[[i]] }
          dff  <- suppressWarnings(as.integer(get2(c("df","dof"))[1])); if (is.na(dff) || !is.finite(dff)) dff <- (K - 1L) * p
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
