
#' @importFrom tidyLPA get_data

lpaClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "lpaClass",
    inherit = lpaBase,
    private = list(
      .allCache = NULL,
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
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
        ))
        
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
        
        if (isTRUE(self$options$plot3)) {
          width <- self$options$width2
          height <- self$options$height2
          
          self$results$plot3$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot2)) {
          width <- self$options$width3
          height <- self$options$height3
          
          self$results$plot2$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot4)) {
          width <- self$options$width4
          height <- self$options$height4
          
          self$results$plot4$setSize(width, height)
        }
      },
      
      .run = function() {
        if (length(self$options$vars) < 2)
          return()
        
        if (is.null(private$.allCache)) {
          private$.allCache <- private$.computeRES()
        }
        
        all <- private$.allCache
        
        #Best model fit table---
        if (isTRUE(self$options$overall)) {
          table <- self$results$overall
          f <- all$bestfit
          
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
        # Fit measres----------
        if (isTRUE(self$options$fit)) {
          table <- self$results$fit
          df <- as.data.frame(all$res[[1]]$fit)
          
          lapply(rownames(df), function(name) {
            row <- list(value = df[name, 1])
            table$addRow(rowKey = name, values = row)
          })
        }
        
        #Estimates---
        if (isTRUE(self$options$est)) {
          table <- self$results$est
          
          # get estimates--------------
          set.seed(1234)
          e <- tidyLPA::get_estimates(all$res)
          e <- as.data.frame(e)
          
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
          
          pc <- tidyLPA::get_data(all$res)
          pc <- as.factor(pc$Class)
          #pc <- as.factor(pc)
          if (self$options$pc
              && self$results$pc$isNotFilled()) {
            self$results$pc$setValues(pc)
            self$results$pc$setRowNums(rownames(data))
          }
          image <- self$results$plot
          image$setState(pc)
        }
        
        # Posterior probabilities---
        if (isTRUE(self$options$post)) {
          post <- tidyLPA::get_data(all$res, "posterior_probabilities")
          post_name <- paste0("CPROB", 1:self$options$nc)
          post <- post[, post_name, drop = FALSE]
          post <- data.frame(post)
          
          if (self$options$post
              && self$results$post$isNotFilled()) {
            keys <- 1:self$options$nc
            measureTypes <- rep("continuous", self$options$nc)
            
            titles <- paste("Class", keys)
            descriptions <- paste("Class", keys)
            
            self$results$post$set(
              keys = keys,
              titles = titles,
              descriptions = descriptions,
              measureTypes = measureTypes
            )
            self$results$pc$setRowNums(rownames(data))
            for (i in 1:self$options$nc) {
              scores <- as.numeric(post[, i])
              self$results$post$setValues(index = i, scores)
            }
          }
        }
        
        # #https://github.com/data-edu/tidyLPA/issues/198
        # # Not resolved yet.
        # # correlation plot----------
        # if(isTRUE(self$options$plot)){
        #
        # image <- self$results$plot
        # image$setState(all$res)
        # }
        
        # Latent profile plot(Box plot)----------
        
        if (isTRUE(self$options$plot1)) {
          image1 <- self$results$plot1
          image1$setState(all$res)
        }
        
        # Latent profile plot(Line plot)----------
        
        if (isTRUE(self$options$plot4)) {
          image4 <- self$results$plot4
          image4$setState(all$res)
        }
        
        # elbow plot----------
        
        if (isTRUE(self$options$plot2)) {
          vars <- self$options$vars
          nc <- self$options$nc
          variances <- self$options$variances
          covariances <- self$options$covariances
          
          data <- self$data
          data <- jmvcore::naOmit(data)
          
          out <- NULL
          
          for (i in 1:self$options$nc) {
            set.seed(1234)
            res <- tidyLPA::estimate_profiles(
              data,
              n_profiles = i,
              variances = variances,
              covariances = covariances
            )
            
            res <- res[[1]]
            df <- data.frame(res$fit)
            df <- t(df)
            model <- df[1]
            class <- df[2]
            log <- df[3]
            aic <- df[4]
            awe <- df[5]
            bic <- df[6]
            caic <- df[7]
            clc <- df[8]
            kic <- df[9]
            sabic <- df[10]
            icl <- df[11]
            entropy <- df[12]
            df <- data.frame(model,
                             log,
                             aic,
                             awe,
                             bic,
                             caic,
                             clc,
                             kic,
                             sabic,
                             icl,
                             entropy,
                             class)
            if (is.null(out)) {
              out <- df
            } else {
              out <- rbind(out, df)
            }
          }
          out <- out
          # Elbow plot---
          out1 <- out[, c(3:10, 12), ]
          colnames(out1) <- c('AIC',
                              'AWE',
                              'BIC',
                              'CAIC',
                              'CLC',
                              'KIC',
                              'SABIC',
                              'ICL',
                              'Class')
          elbow <- reshape2::melt(
            out1,
            id.vars = 'Class',
            variable.name = "Fit",
            value.name = 'Value'
          )
          image <- self$results$plot2
          image$setState(elbow)
        }
      },
      
      # pLOT---
      # Percentage of class
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
        vars <- self$options$vars
        nc <- self$options$nc
        variances <- self$options$variances
        covariances <- self$options$covariances
        data <- self$data
        data <- jmvcore::naOmit(data)
        
        set.seed(1234)
        res1 <- tidyLPA::estimate_profiles(data,
                                           1:self$options$nc,
                                           variances = variances,
                                           covariances = covariances)
        
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
          aes(linetype = 'solid', linewidth = 1.3) +
          scale_linewidth_identity() +
          scale_linetype_identity()
        
        if (self$options$angle > 0) {
          plot4 <- plot4 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        
        print(plot4)
        TRUE
        
      },
      
      .computeRES = function() {
        vars <- self$options$vars
        nc <- self$options$nc
        variances <- self$options$variances
        covariances <- self$options$covariances
        
        data <- self$data
        data <- jmvcore::naOmit(data)
        
        # Best Model fit---------
        # res<- iris %>%
        #   select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
        #   estimate_profiles(n_profiles = 2:4, models = c(1, 2, 3, 6)) %>%
        #   get_fit() %>%
        #   as.data.frame()
        # res
        
        #Estimates profile------------------------
        set.seed(1234)
        #private$.checkpoint()
        
        res <- tidyLPA::estimate_profiles(data, nc, variances = variances, covariances = covariances)
        #------------------------------------------
        
        best <-  tidyLPA::estimate_profiles(data, n_profiles = 2:nc, models = c(1, 2, 3, 6))
        
        # Compare solution
        sol <- tidyLPA::compare_solutions(best)
        self$results$text$setContent(sol)
        
        allfit <- tidyLPA::get_fit(best)
        bestfit <- as.data.frame(allfit)
        
        retlist <- list(res = res, bestfit = bestfit)
        return(retlist)
      }
    )
  )

# if (isTRUE(self$options$overall)) {
#   table <- self$results$overall
#   f <- all$bestfit
#
#   names <- dimnames(f)[[1]]
#
#
#   for (name in names) {
#     # # if not rowKey, add rowkey---
#     # if (!name %in% table$rowKeys) {
#     #   table$addRow(rowKey = name)
#     # }
#     #
#     # if (table$getCell(rowKey=name,'model')$isEmpty) {
#     #
#     #   table$setStatus('running')
#     #   #private$.checkpoint()
#
#     row <- list()
#
#     row[['model']] <- f[name, 1]
#     row[['classes']] <- f[name, 2]
#     row[['log']] <- f[name, 3]
#     row[['aic']] <- f[name, 4]
#     row[['awe']] <- f[name, 5]
#     row[['bic']] <- f[name, 6]
#     row[['caic']] <- f[name, 7]
#     row[['clc']] <- f[name, 8]
#     row[['kic']] <- f[name, 9]
#     row[['sabic']] <- f[name, 10]
#     row[['icl']] <- f[name, 11]
#     row[['entropy']] <- f[name, 12]
#
#     # change setrow---
#     table$addRow(rowKey = name, values = row)
#     #table$setStatus('complete')
#   }
# }
#

#
# if (isTRUE(self$options$est)) {
#   table <- self$results$est
#
#   # get estimates--------------
#   set.seed(1234)
#   e <- tidyLPA::get_estimates(all$res)
#   #################################
#   e <- data.frame(e)
#   names <- dimnames(e)[[1]]
#
#   for (name in names) {
#     # # if not rowKey, add rowkey---
#     # if (!name %in% table$rowKeys) {
#     #   table$addRow(rowKey = name)
#     # }
#     #
#     # if (table$getCell(rowKey=name,'cat')$isEmpty) {
#     #
#     #   table$setStatus('running')
#
#     row <- list()
#     row[['cat']] <- e[name, 1]
#     row[['par']] <- e[name, 2]
#     row[['est']] <- e[name, 3]
#     row[['se']] <- e[name, 4]
#     row[['p']] <- e[name, 5]
#     row[['cl']] <- e[name, 6]
#     row[['model']] <- e[name, 7]
#     row[['cla']] <- e[name, 8]
#
#     table$addRow(rowKey = name, values = row)
#     # table$setStatus('complete')
#     # }
#   }
# }
