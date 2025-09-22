
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
      
      # ---------- 3-STEP 계산 ----------
      # ---------- 3-STEP 계산 (안전장치 포함) ----------
      .computeThreeStep = function() {
        auxName <- self$options$auxVar
        if (is.null(auxName) || !nzchar(auxName))
          return(NULL)
        
        dat <- as.data.frame(self$data)
        if (!(auxName %in% names(dat)))
          return(list(msg = sprintf("[3-step] '%s' not found in data.", auxName)))
        
        # posterior 가져오기
        ind <- self$classProbabilities$individual
        
        # 견고한 posterior 열 탐색기
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
        
        # 최후 보정: 숫자열 중 (predicted/id 제외) 행합≈1 이면 posterior로 간주
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
        
        # ---- 보조변수 안전 판별/형변환 ----
        yraw <- dat[[auxName]]
        n <- NROW(yraw)
        if (n != NROW(ind))
          return(list(msg = "[3-step] data/posterior row mismatch."))
        
        notes <- character(0)
        
        is_cat_like <- function(x) {
          # 숫자라도 고유값이 적고 정수이면 범주형으로 간주(예: 0/1/2)
          if (!is.numeric(x)) return(FALSE)
          ux <- sort(unique(x[is.finite(x)]))
          if (length(ux) <= 6 && all(abs(ux - round(ux)) < 1e-8)) TRUE else FALSE
        }
        
        # 타입 자동결정 규칙:
        #  - factor/ordered/character/logical  -> DCAT
        #  - numeric & "cat-like" (고유값<=6 & 모두 정수) -> DCAT로 전환(노트 기록)
        #  - 그 외 numeric -> BCH
        if (is.factor(yraw) || is.ordered(yraw) || is.character(yraw) || is.logical(yraw)) {
          ytype <- "categorical"
          yfac  <- as.factor(yraw)
        } else if (is_cat_like(yraw)) {
          ytype <- "categorical"
          yfac  <- factor(yraw)   # 순서형으로 보고 싶으면 ordered(yraw)로 바꿔도 됨
          notes <- c(notes, sprintf("[note] '%s' has few integer levels; treated as categorical (DCAT).", auxName))
        } else if (is.numeric(yraw)) {
          ytype <- "numeric"
        } else {
          return(list(msg = sprintf("[3-step] Unsupported type for '%s'.", auxName)))
        }
        
        # ---- 결측 안전 처리 ----
        # posterior는 그대로 두고, y 결측만 가중치=0로 무해화
        # (listwise 옵션이 이미 적용되어 있더라도 추가 안전)
        if (ytype == "numeric") {
          y <- yraw
          wmask <- is.finite(y)
        } else {
          y <- yfac
          wmask <- !is.na(y)
        }
        # 가중치 마스크: 결측이면 모든 class weight 0
        for (pc in pcols) ind[[pc]][!wmask] <- 0
        
        # --------------- 분석 ---------------
        if (ytype == "numeric") {
          # BCH_basic (가중 ANOVA 근사)
          long <- lapply(seq_len(K), function(k) {
            data.frame(class = factor(k, levels = cls_levels),
                       y = y, w = ind[[pcols[k]]])
          })
          long <- do.call(rbind, long)
          
          # 분산 0 또는 유효 샘플 과소 경고
          eff_nk <- tapply(long$w, long$class, sum)
          if (any(eff_nk < 1e-6))
            notes <- c(notes, "[warn] One or more classes have (near-)zero effective weight for the distal variable.")
          if (stats::var(long$y[long$w > 0], na.rm = TRUE) < 1e-10)
            notes <- c(notes, "[warn] Near-zero variance in distal variable (BCH may be unreliable).")
          
          wmean <- tapply(long$y * long$w, long$class, sum) /
            pmax(eff_nk, .Machine$double.eps)
          wbar <- sum(long$y * long$w) / sum(long$w)
          ssb <- sum(eff_nk * (wmean - wbar)^2)
          long$gmean <- wmean[as.character(long$class)]
          ssw <- sum(long$w * (long$y - long$gmean)^2)
          
          dfb <- K - 1
          dfw <- max(1, sum(eff_nk) - K)
          msb <- ssb / dfb
          msw <- ssw / dfw
          Fst <- if (msw > 0) msb / msw else Inf
          pval <- tryCatch(stats::pf(Fst, dfb, dfw, lower.tail = FALSE), error = function(e) NA_real_)
          
          txt <- c(
            "3-step analysis (BCH/DCAT)",
            "=== 3-STEP auxiliary (tidySEM, BCH_basic) ===",
            sprintf("[Distal: %s, numeric] K=%d", auxName, K),
            sprintf("Class-weighted means: %s",
                    paste(paste0("C", names(wmean), "=", round(wmean, 3)), collapse = ", ")),
            sprintf("Wald/ANOVA approx: F(%d,%d)=%.3f, p=%.4f", dfb, dfw, Fst, pval)
          )
          if (length(notes)) txt <- c(txt, notes)
          return(list(text = paste(txt, collapse = "\n")))
        }
        
        # DCAT (가중 카이제곱)
        cats <- levels(y)
        W <- matrix(0, nrow = K, ncol = length(cats),
                    dimnames = list(paste0("C", cls_levels), cats))
        for (k in seq_len(K)) {
          for (j in seq_along(cats)) {
            W[k, j] <- sum( (y == cats[j]) * ind[[pcols[k]]] )
          }
        }
        
        # 희소성 진단 (가중 기대도수 근사)
        E <- outer(rowSums(W), colSums(W)) / sum(W)
        if (any(E < 1))
          notes <- c(notes, "[warn] Some expected weighted counts < 1 (chi-square may be inaccurate).")
        if (mean(E < 5) > 0.2)
          notes <- c(notes, "[note] >20% of expected weighted counts < 5 (use caution).")
        
        chis <- suppressWarnings(stats::chisq.test(W, correct = FALSE))
        txt <- c(
          "3-step analysis (BCH/DCAT)",
          "=== 3-STEP auxiliary (tidySEM, DCAT) ===",
          sprintf("[Distal: %s, categorical] K=%d, J=%d", auxName, K, ncol(W)),
          sprintf("Wald/Chi-square: X^2(%d)=%.3f, p=%.4f",
                  chis$parameter, chis$statistic, chis$p.value)
        )
        if (length(notes)) txt <- c(txt, notes)
        return(list(text = paste(txt, collapse = "\n")))
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
          
          if (is.null(res3)) {
            self$results$threeStep$setContent("[3-step] No auxiliary variable specified.")
          } else if (!is.null(res3$text)) {
            self$results$threeStep$setContent(res3$text)
          } else if (!is.null(res3$msg)) {
            self$results$threeStep$setContent(res3$msg)
          } else {
            self$results$threeStep$setContent("[3-step] Completed.")
          }
        }
        
        
        html <- progressBarH(100,100, 'Analysis complete!')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
        self$results$progressBarHTML$setVisible(FALSE)
        
        
        # html <- progressBarH(100,100, 'Analysis complete!')
        # self$results$progressBarHTML$setContent(html)
        # private$.checkpoint()
        # self$results$progressBarHTML$setVisible(FALSE)
        # 
        # 
        # html <- progressBarH(100,100, 'Analysis complete!')
        # self$results$progressBarHTML$setContent(html)
        # private$.checkpoint()
        # self$results$progressBarHTML$setVisible(FALSE)
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
        # 1) 원본과 지표 목록
        df   <- as.data.frame(self$data)
        vars <- self$options$vars              # 사용자가 지정한 지표(성장모형에 들어가는 y들)
        
        if (is.null(vars) || length(vars) < 1)
          return()
        
        # 2) 연속형( numeric ) 지표만 사용 (factor/ordered 제외)
        isNum <- vapply(df[vars], is.numeric, TRUE)
        xvars <- vars[isNum]
        if (length(xvars) < 1)
          return()
        
        # 3) id 보장
        if (!("id" %in% names(df)))
          df$id <- seq_len(nrow(df))
        
        # 4) long 형태로 변환 (지표들만)
        long <- reshape(df[, c("id", xvars), drop = FALSE],
                        direction = "long",
                        varying   = xvars,
                        v.names   = "value",
                        idvar     = "id",
                        timevar   = "time")
        
        # time 라벨을 지표명으로 (y1,y2,...) 표시하고 싶지 않으면 as.factor(seq_along(xvars))
        long$time <- factor(long$time, labels = xvars)
        
        # 5) 상태 저장
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
