
#' Mixture Rasch Analysis
mixtureClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "mixtureClass",
    inherit = mixtureBase,
    private = list(
      .allCache = NULL,
      .htmlwidget = NULL,
      
      #####################
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        private$.allCache <- NULL
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Specify the number of <b>Class, Step, and Type</b> in the Analysis option.</li>',
            '<li>Highlight the variables and click the arrow to move it across into the <b>Variables</b> box.</li>',
            '<li><b>Person membership</b> table will be displayed in the datasheet.</li>',
            '<li>Rasch mixture model is estimated by <b>mixRasch</b> R package.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
          
        ))
        
        if (isTRUE(self$options$iplot)) {
          width <- self$options$width
          height <- self$options$height
          
          self$results$iplot$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot2)) {
          width <- self$options$width1
          height <- self$options$height1
          
          self$results$plot2$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot3)) {
          width <- self$options$width2
          height <- self$options$height2
          
          self$results$plot3$setSize(width, height)
        }
        
      },
      
      
      .run = function() {
        # Ready--------
        
        ready <- TRUE
        
        if (is.null(self$options$vars) |
            length(self$options$vars) < 2)
          
          ready <- FALSE
        
        if (ready) {
          if (is.null(private$.allCache)) {
            data <- private$.cleanData()
            private$.allCache <- private$.compute(data)
          }
          results <- private$.allCache
          
          #  populate Model information table-----
          
          private$.populateModelTable(results)
          
          
          # populate Item Statistics table-----
          
          private$.populateImeasureTable(results)
          private$.populateIseTable(results)
          private$.populateImeanTable(results)
          private$.populateInfitTable(results)
          private$.populateOutfitTable(results)
          private$.populatePbisTable(results)
          
          # populate Average theta table----
          private$.populateAverageTable(results)
          
        }
      },
      
      # compute results---
      
      .compute = function(data) {
        vars <- self$options$vars
        nc <- self$options$nc
        step <- self$options$step
        type <- self$options$type
        
        # computing mixRasch-----------
        
        set.seed(1234)
        #private$.checkpoint()
        
        res1 <-
          mixRasch::mixRasch(
            data = data,
            steps = step,
            model = type,
            n.c = nc
          )
        
        # item statistics--------
        
        imeasure <- sapply(res1$LatentClass, function(x)
          x$item.par$delta.i)
        imeasure <- as.data.frame(imeasure)
        
        ise <- sapply(res1$LatentClass, function(x)
          x$item.par$SE.delta.i)
        ise <- as.data.frame(ise)
        
        infit <- sapply(res1$LatentClass, function(x)
          x$item.par$in.out[, 1])
        infit <- as.data.frame(infit)
        outfit <- sapply(res1$LatentClass, function(x)
          x$item.par$in.out[, 3])
        outfit <- as.data.frame(outfit)
        # desc <- sapply(res1$LatentClass, function(x) x$item.par$itemDescriptives)
        imean <- sapply(res1$LatentClass, function(x)
          x$item.par$itemDescriptives[, 1])
        imean <- as.data.frame(imean)
        
        pbis <- sapply(res1$LatentClass, function(x)
          x$item.par$itemDescriptives[, 2])
        pbis <- as.data.frame(pbis)
        
        ######### Person analysis####################
        
        # person measure--------------
        pmeasure <- sapply(res1$LatentClass, function(x)
          x$person.par$theta)
        pmeasure <- as.data.frame(pmeasure)
        # person error-------------------
        pse <- sapply(res1$LatentClass, function(x)
          x$person.par$SE.theta)
        pse <- as.data.frame(pmeasure)
        # person fit--------------
        pinfit <- sapply(res1$LatentClass, function(x)
          x$person.par$infit)
        pinfit <- as.data.frame(pinfit)
        
        poutfit <- sapply(res1$LatentClass, function(x)
          x$person.par$outfit)
        poutfit <- as.data.frame(poutfit)
        # Person Outputs-------------------------
        #pmeasure--------------------------------
        
        if (self$options$pmeasure == TRUE) {
          keys <- 1:self$options$nc
          measureTypes <- rep("continuous", self$options$nc)
          
          titles <- paste("Measure_Class", keys)
          descriptions <- paste("Measure_Class", keys)
          
          self$results$pmeasure$set(
            keys = keys,
            titles = titles,
            descriptions = descriptions,
            measureTypes = measureTypes
          )
          
          self$results$pmeasure$setRowNums(rownames(data))
          
          for (i in 1:self$options$nc) {
            scores <- as.numeric(pmeasure[, i])
            self$results$pmeasure$setValues(index = i, scores)
          }
        }
        #pse---------------------------------------
        
        if (self$options$pse == TRUE) {
          keys <- 1:self$options$nc
          measureTypes <- rep("continuous", self$options$nc)
          
          titles <- paste("SE_Class", keys)
          descriptions <- paste("SE_Class", keys)
          
          self$results$pse$set(
            keys = keys,
            titles = titles,
            descriptions = descriptions,
            measureTypes = measureTypes
          )
          
          self$results$pse$setRowNums(rownames(data))
          
          for (i in 1:self$options$nc) {
            scores <- as.numeric(pse[, i])
            self$results$pse$setValues(index = i, scores)
          }
        }
        
        #pinfit-------------------------------
        if (self$options$pinfit == TRUE) {
          keys <- 1:self$options$nc
          measureTypes <- rep("continuous", self$options$nc)
          
          titles <- paste("Infit_Class", keys)
          descriptions <- paste("Infit_Class", keys)
          
          self$results$pinfit$set(
            keys = keys,
            titles = titles,
            descriptions = descriptions,
            measureTypes = measureTypes
          )
          
          self$results$pinfit$setRowNums(rownames(data))
          
          for (i in 1:self$options$nc) {
            scores <- as.numeric(pinfit[, i])
            self$results$pinfit$setValues(index = i, scores)
          }
        }
        
        #poutfit----------------------------------------
        
        if (self$options$poutfit == TRUE) {
          keys <- 1:self$options$nc
          measureTypes <- rep("continuous", self$options$nc)
          
          titles <- paste("Outfit_Class", keys)
          descriptions <- paste("Outfit_Class", keys)
          
          self$results$poutfit$set(
            keys = keys,
            titles = titles,
            descriptions = descriptions,
            measureTypes = measureTypes
          )
          
          self$results$poutfit$setRowNums(rownames(data))
          
          for (i in 1:self$options$nc) {
            scores <- as.numeric(poutfit[, i])
            self$results$poutfit$setValues(index = i, scores)
          }
        }
        # model fit information------
        
        out <- NULL
        
        for (i in 1:nc) {
          set.seed(1234)
          res1 <-
            mixRasch::mixRasch(
              data = data,
              steps = step,
              model = type,
              n.c = i
            )
          
          model <- res1$info.fit
          df <- data.frame(model)
          if (is.null(out)) {
            out <- df
          } else {
            out <- rbind(out, df)
          }
        }
        out <- out
        # elbow plot----------
        out1 <- out[, c(1:3)]
        colnames(out1) <- c('AIC', 'BIC', 'CAIC')
        
        df <-  as.data.frame(out1)
        df$Class <- seq.int(nrow(df))
        elbow <- reshape2::melt(
          df,
          id.vars = 'Class',
          variable.name = "Fit",
          value.name = 'Value'
        )
        image <- self$results$plot2
        image$setState(elbow)
        # number of class
        set.seed(1234)
        res0 <- mixRasch::getEstDetails(res1)
        class <- res0$nC
        
        # Average Theta Values
        set.seed(1234)
        average <- mixRaschTools::avg.theta(res1)
        # class membership-------------
        pclass <- res1$class
        mem <- as.numeric(apply(pclass, 1, which.max))
        if (self$options$pmember == TRUE) {
          mem <- as.factor(mem)
          self$results$pmember$setRowNums(rownames(data))
          self$results$pmember$setValues(mem)
        }
        # Person density across class-------------
        colnames(pmeasure) <- c(1:self$options$nc)
        dat <- reshape2::melt(pmeasure, variable.name = "Class", value.name =
                                'Measure')
        image <- self$results$plot3
        image$setState(dat)
        
        results <-
          list(
            'out' = out,
            'imeasure' = imeasure,
            'ise' = ise,
            'infit' = infit,
            'outfit' = outfit,
            'imean' = imean,
            'pbis' = pbis,
            'class' = class,
            'average' = average,
            'mem' = mem,
            'pmeasure' = pmeasure,
            'pse' = pse,
            'pinfit' = pinfit,
            'poutfit' = poutfit
          )
      },
      
      # populate Model information table-----
      .populateModelTable = function(results) {
        table <- self$results$item$fit
        fit <- as.data.frame(results$out)
        
        # 열 이름을 명시적으로 설정 (필요 시)
        colnames(fit) <- c("aic", "bic", "caic", "loglik", "parm", "person")
        
        # lapply를 사용하여 각 행(row)을 처리
        lapply(rownames(fit), function(name) {
          row <- as.list(fit[name, ])
          table$addRow(rowKey = name, values = row)
        })
      },
      # populate Item Statistics table-----
      .populateImeasureTable = function(results) {
        table <- self$results$item$imeasure
        nc <- self$options$nc
        vars <- self$options$vars
        imeasure <- results$imeasure
        nclass <- results$class
        if (nclass > 1) {
          for (i in 2:nclass)
            table$addColumn(
              name = paste0("pc", i),
              title = as.character(i),
              type = 'number',
              superTitle = 'Class'
            )
        }
        for (i in seq_along(vars)) {
          row <- list()
          for (j in 1:nclass) {
            row[[paste0("pc", j)]] <- imeasure[i, j]
          }
          table$addRow(rowKey = i, values = row)
        }
        
        # Prepare Data For Item Plot -------
        
        image <- self$results$iplot
        
        imeasure$item <- seq.int(nrow(imeasure))
        
        n <- paste(1:self$options$nc, sep = '')
        colnames(imeasure) <- c(n, 'item')
        
        
        data <- tidyr::gather(data = imeasure, class, measure, -item)
        image$setState(data)
        
        
      },
      
      .populateIseTable = function(results) {
        table <- self$results$item$ise
        nc <- self$options$nc
        vars <- self$options$vars
        ise <- results$ise
        nclass <- results$class
        
        if (nclass > 1) {
          for (i in 2:nclass)
            table$addColumn(
              name = paste0("pc", i),
              title = as.character(i),
              type = 'number',
              superTitle = 'Class'
            )
        }
        for (i in seq_along(vars)) {
          row <- list()
          for (j in 1:nclass) {
            row[[paste0("pc", j)]] <- ise[i, j]
          }
          table$addRow(rowKey = i, values = row)
        }
      },
      
      #---
      .populateImeanTable = function(results) {
        table <- self$results$item$imean
        nc <- self$options$nc
        vars <- self$options$vars
        imean <- results$imean
        nclass <- results$class
        if (nclass > 1) {
          for (i in 2:nclass)
            table$addColumn(
              name = paste0("pc", i),
              title = as.character(i),
              type = 'number',
              superTitle = 'Class'
            )
        }
        for (i in seq_along(vars)) {
          row <- list()
          for (j in 1:nclass) {
            row[[paste0("pc", j)]] <- imean[i, j]
          }
          table$addRow(rowKey = i, values = row)
        }
      },
      
      
      .populateInfitTable = function(results) {
        table <- self$results$item$infit
        nc <- self$options$nc
        vars <- self$options$vars
        infit <- results$infit
        nclass <- results$class
        
        if (nclass > 1) {
          for (i in 2:nclass)
            table$addColumn(
              name = paste0("pc", i),
              title = as.character(i),
              type = 'number',
              superTitle = 'Class'
            )
        }
        for (i in seq_along(vars)) {
          row <- list()
          for (j in 1:nclass) {
            row[[paste0("pc", j)]] <- infit[i, j]
          }
          table$addRow(rowKey = i, values = row)
        }
      },
      
      .populateOutfitTable = function(results) {
        table <- self$results$item$outfit
        nc <- self$options$nc
        vars <- self$options$vars
        outfit <- results$outfit
        nclass <- results$class
        if (nclass > 1) {
          for (i in 2:nclass)
            table$addColumn(
              name = paste0("pc", i),
              title = as.character(i),
              type = 'number',
              superTitle = 'Class'
            )
        }
        for (i in seq_along(vars)) {
          row <- list()
          for (j in 1:nclass) {
            row[[paste0("pc", j)]] <- outfit[i, j]
          }
          table$addRow(rowKey = i, values = row)
        }
      },
      
      .populatePbisTable = function(results) {
        table <- self$results$item$pbis
        nc <- self$options$nc
        vars <- self$options$vars
        pbis <- results$pbis
        nclass <- results$class
        if (nclass > 1) {
          for (i in 2:nclass)
            table$addColumn(
              name = paste0("pc", i),
              title = as.character(i),
              type = 'number',
              superTitle = 'Class'
            )
        }
        for (i in seq_along(vars)) {
          row <- list()
          for (j in 1:nclass) {
            row[[paste0("pc", j)]] <- pbis[i, j]
          }
          table$addRow(rowKey = i, values = row)
        }
      },
      
      # populate Average theta table-----
      .populateAverageTable = function(results) {
        nc <- self$options$nc
        table <- self$results$person$average
        value <- results$average
        value <- as.matrix(value)
        for (i in seq_len(nc)) {
          row <- list()
          row[["value"]] <- value[i, 1]
          table$setRow(rowNo = i, values = row)
        }
      },
      
      # plot--------------
      
      .itemPlot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        itemplot <- self$options$iplot
        plotData <- image$state
        plot <- ggplot2::ggplot(plotData, aes(
          x = as.factor(item),
          y = measure,
          group = class
        )) +
          geom_line(aes(color = class), size = 1.1) +
          geom_point(aes(color = class), size = 3) +
          labs(
            title = "Item parameters by class",
            x = "Item number",
            y = "Measure",
            color = 'Class'
          ) +
          ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        print(plot)
        TRUE
      },
      
      .plot3 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        dat <- image$state
        plot3 <- ggplot(dat, aes(x = Measure, color = Class)) +
          geom_density()  +
          coord_cartesian(xlim = c(-5, 5))
        plot3 <- plot3 + ggtheme
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
      #--------------------------
      
      .cleanData = function() {
        items <- self$options$vars
        data <- list()
        for (item in items)
          data[[item]] <-
          jmvcore::toNumeric(self$data[[item]])
        attr(data, 'row.names') <- seq_len(length(data[[1]]))
        attr(data, 'class') <- 'data.frame'
        data <- jmvcore::naOmit(data)
        return(data)
      }
    )
  )
