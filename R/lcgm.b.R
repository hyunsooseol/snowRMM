
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
          
          # Person membership 데이터에서 클래스 할당 정보 추출
          individual_data <- data.frame(cp1$individual)
          predicted_classes <- individual_data$predicted
          
          # 빈도 계산
          class_counts <- table(predicted_classes)
          total_n <- sum(class_counts)
          
          # Class Size 테이블용 요약 데이터
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
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) || is.null(self$options$vars))
          self$results$instructions$setVisible(TRUE)
        
        # 안내문 갱신: 모형 비교 제거, AIC/BIC 수동 비교 안내
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
        
        # Show and initialize progress bar
        self$results$progressBarHTML$setVisible(TRUE)
        html <- progressBarH(5,  100, 'Starting analysis...')
        self$results$progressBarHTML$setContent(html)
        private$.checkpoint()
        
        set.seed(1234)
        
        # Descriptives
        if (isTRUE(self$options$desc)) {
          html <- progressBarH(15, 100, 'Computing descriptives...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.populateDescTable()
        }
        
        # Fit statistics
        if (isTRUE(self$options$fit)) {
          html <- progressBarH(30, 100, 'Calculating fit statistics...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.populateFitTable()
        }
        
        # Parameter estimates
        if (isTRUE(self$options$est)) {
          html <- progressBarH(45, 100, 'Extracting parameter estimates...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.populateEST()
        }
        
        # Class probabilities
        if (isTRUE(self$options$cp)) {
          html <- progressBarH(60, 100, 'Computing class probabilities...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.populateClassSizeTable()
        }
        
        # Class members
        if (isTRUE(self$options$mem)) {
          html <- progressBarH(70, 100, 'Listing class members...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.populateClassMemberTable()
        }
        
        # Class size plot
        if (isTRUE(self$options$plot2)) {
          html <- progressBarH(80, 100, 'Creating class size plot...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.setPlot2()
        }
        
        # Density plot
        if (isTRUE(self$options$plot1)) {
          html <- progressBarH(88, 100, 'Preparing density plot...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.setPlot1()
        }
        
        # Growth plot
        if (isTRUE(self$options$plot)) {
          html <- progressBarH(96, 100, 'Generating growth plot...')
          self$results$progressBarHTML$setContent(html)
          private$.checkpoint()
          private$.setPlot()
        }
        
        # Complete
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
        
        # Person membership 데이터를 직접 활용
        individual_data <- self$classProbabilities$individual
        predicted_classes <- individual_data$predicted
        
        # 빈도 계산
        class_counts <- table(predicted_classes)
        total_n <- sum(class_counts)
        
        # 각 클래스별로 테이블에 행 추가
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
        # Person membership 데이터를 직접 활용
        individual_data <- self$classProbabilities$individual
        predicted_classes <- individual_data$predicted
        
        # 빈도 계산
        class_counts <- table(predicted_classes)
        total_n <- sum(class_counts)
        
        # 플롯용 데이터프레임 생성
        plot_data <- data.frame(
          Class = factor(names(class_counts), levels = sort(as.numeric(names(class_counts)))),
          Count = as.numeric(class_counts),
          Proportion = round(as.numeric(class_counts) / total_n, 3),
          Percentage = round(as.numeric(class_counts) / total_n * 100, 1)
        )
        
        self$results$plot2$setState(plot_data)
      },
      
      .setPlot1 = function() {
        long <- reshape(self$data, direction="long",
                        varying=list(names(self$data)),
                        v.names="value",
                        idvar="id", timevar="time")
        self$results$plot1$setState(long)
      },
      
      .setPlot = function() {
        self$results$plot$setState(self$res)
      },
      
      .plot2 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) return(FALSE)
        
        plot_data <- image$state
        
        # Bar Chart
        p <- ggplot(plot_data, aes(x = Class, y = Count, fill = Class)) +
          geom_bar(stat = "identity", alpha = 0.8, color = "white", size = 0.5) +
          geom_text(aes(label = paste0("n = ", Count, "\n(", Percentage, "%)")), 
                    vjust = -0.5, size = 3.5, fontface = "bold", color = "black") +
          scale_fill_brewer(type = "qual", palette = "Set3") +
          labs(
            title = "",
            x = "Latent Class",
            y = "Number of Participants"
          ) +
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
        
        print(p + ggtheme)
        TRUE
      },
      
      .plot1 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) return(FALSE)
        long <- image$state
        p <- ggplot(long, aes(x=value)) +
          geom_density() + facet_wrap(~time) + theme_bw()
        print(p + ggtheme)
        TRUE
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state)) return(FALSE)
        p <- tidySEM::plot_growth(image$state,
                                  rawdata=self$options$raw,
                                  alpha_range=c(0,0.05))
        print(p + ggtheme)
        TRUE
      }
    )
  )


# Progress Bar HTML (R/progressBarH.R) — 그대로 사용
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
