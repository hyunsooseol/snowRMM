
# This file is a generated template, your changes will not be overwritten
# Differential Item Functioning by eRm package

difClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "difClass",
    inherit = difBase,
    private = list(
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
            '<li>Performs DIF analysis using <b>eRm</b> R package.</li>',
            '<li>For partial credit model, the grouping variable should be coded as <b>1 and 2</b>.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowRMM/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
          
        ))
        
        
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
        
        if (isTRUE(self$options$plot3)) {
          width <- self$options$width3
          height <- self$options$height3
          self$results$plot3$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot4)) {
          width <- self$options$width4
          height <- self$options$height4
          self$results$plot4$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot5)) {
          width <- self$options$width5
          height <- self$options$height5
          self$results$plot5$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot6)) {
          width <- self$options$width6
          height <- self$options$height6
          self$results$plot6$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot7)) {
          width <- self$options$width7
          height <- self$options$height7
          self$results$plot7$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot8)) {
          width <- self$options$width8
          height <- self$options$height8
          self$results$plot8$setSize(width, height)
        }
        
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
        
      },
      
      
      ###############################################################
      .run = function() {
        #   vars <- self$options$vars
        #   facs <- self$options$facs
        #   #get the data--------
        #   data <- self$data
        #   data <- jmvcore::naOmit(data)
        #   # convert to appropriate data types
        #   for (i in seq_along(vars))
        #     data[[i]] <- jmvcore::toNumeric(data[[i]])
        #
        #   for (fac in facs)
        #     data[[fac]] <- as.factor(data[[fac]])
        # # data is now all of the appropriate type we can begin!
        #   data <- na.omit(data)
        #   data <- jmvcore::select(data, self$options$vars)
        
        
        data <- self$data
        groupVarName <- self$options$facs
        vars <- self$options$vars
        varNames <- c(groupVarName, vars)
        
        if (is.null(groupVarName))
          return()
        
        data <- jmvcore::select(self$data, varNames)
        for (var in vars)
          data[[var]] <- jmvcore::toNumeric(data[[var]])
        # exclude rows with missings in the grouping variable
        data <- data[!is.na(data[[groupVarName]]), ]
        
        # Example----------------------------------------------
        #https://bookdown.org/chua/new_rasch_demo2/DIF.html
        # dichot_model <- RM(raschdat1)
        # # Create subgroup classifications:
        # subgroups <- sample(1:2, 100, replace = TRUE)
        # # Calculate subgroup-specific item difficulty values:
        # subgroup_diffs <- Waldtest(dichot_model, splitcr = subgroups)
        #
        # # Create objects for subgroup-specific item difficulties:
        # subgroup_1_diffs <- subgroup_diffs$betapar1
        # subgroup_2_diffs <- subgroup_diffs$betapar2
        #
        # #store results from item comparisons in an object called "comparisons"
        # comparisons <- as.data.frame(subgroup_diffs$coef.table)
        #
        
        
        if (self$options$model == 'dicho') {
          dicho <- eRm::RM(data[, -1])
          subgroup_diffs <- eRm::Waldtest(dicho, splitcr = data[[groupVarName]])
          
          
          # Z statistic table--------------
          table <- self$results$z
          items <- self$options$vars
          
          # get result---
          z <- as.vector(subgroup_diffs$coef.table)
          p <- as.vector(subgroup_diffs$coef.table[, 2])
          
          
          for (i in seq_along(items)) {
            row <- list()
            
            row[["zstat"]] <- z[i]
            
            row[["p"]] <- p[i]
            
            
            table$setRow(rowKey = items[i], values = row)
          }
          
          
          # z plot1---------
          
          
          comparison <- as.data.frame(subgroup_diffs$coef.table)
          
          image1 <- self$results$plot1
          image1$setState(comparison)
          
          # Plot2(item parameters by Group)----------
          
          # Overall difficulty--------
          over <- as.vector(dicho$betapar)
          se <- as.vector(dicho$se.beta)
          
          # Create objects for subgroup-specific item difficulties:
          subgroup_1_diffs <- subgroup_diffs$betapar1
          subgroup_2_diffs <- subgroup_diffs$betapar2
          
          se1 <- subgroup_diffs$se.beta1
          se2 <- subgroup_diffs$se.beta2
          
          comp <- data.frame(over, se, subgroup_1_diffs, se1, subgroup_2_diffs, se2)
          
          # Name the columns of the results
          #  names(comp) <- c("group1", "group2")
          
          # Comparison table---------
          
          table <- self$results$comp
          items <- self$options$vars
          
          # get result---
          
          over <- comp[, 1]
          se <- comp[, 2]
          g1 <- comp[, 3]
          se1 <- comp[, 4]
          g2 <- comp[, 5]
          se2 <- comp[, 6]
          
          for (i in seq_along(items)) {
            row <- list()
            
            
            row[["over"]] <- over[i]
            row[["se"]] <- se[i]
            row[["g1"]] <- g1[i]
            row[["se1"]] <- se1[i]
            row[["g2"]] <- g2[i]
            row[["se2"]] <- se2[i]
            
            table$setRow(rowKey = items[i], values = row)
          }
          
          
          # Melting for line plot--------------------------
          
          comp1 <- data.frame(self$options$vars,
                              over,
                              subgroup_1_diffs,
                              subgroup_2_diffs)
          
          # Name the columns of the results
          names(comp1) <- c("item", "Overall", "group1", "group2")
          
          p <- reshape2::melt(comp1, id.vars = c('item'))
          colnames(p) <- c("Item", "Group", "Value")
          
          # self$results$text$setContent(comp)
          
          # Line plot---------
          image2   <-  self$results$plot2
          image2$setState(p)
          
          # Scatterplot of item difference-----------
          # Create objects for subgroup-specific item difficulties:
          
          subgroup_1_diffs <- subgroup_diffs$betapar1
          subgroup_2_diffs <- subgroup_diffs$betapar2
          
          se1 <- subgroup_diffs$se.beta1
          se2 <- subgroup_diffs$se.beta2
          
          state <- list(subgroup_1_diffs, subgroup_2_diffs, se1, se2)
          image3 <- self$results$plot3
          image3$setState(state)
          
          # Bar plot of item difference-----------
          
          # First, calculate difference in difficulty between subgroups
          # Note that I multiplied by -1 to reflect item difficulty rather than easiness (eRm quirk):
          item_dif <- (subgroup_1_diffs * -1) - (subgroup_2_diffs *
                                                   -1)
          
          
          # Bar plot code:
          item_dif <- as.vector(item_dif)
          subgroup_1_diffs <- subgroup_diffs$betapar1
          
          image4 <- self$results$plot4
          
          state <- list(item_dif, subgroup_1_diffs)
          image4$setState(state)
        }
        ###########################################################
        
        if (self$options$model == 'partial') {
          PC_model <- eRm::PCM(data[, -1])
          
          
          #- First, get overall item difficulties specific to each subgroup:
          group1_item.diffs.overall <- NULL
          group2_item.diffs.overall <- NULL
          group_item.diffs.overall <- NULL
          
          
          responses <- data[, -1]
          responses.g <- cbind.data.frame(data[[groupVarName]], responses)
          
          #------------------------------
          responses.g1 <- subset(responses.g, data[[groupVarName]] == 1)
          responses.g2 <- subset(responses.g, data[[groupVarName]] == 2)
          
          ## Compare thresholds between groups----------------
          subgroup_diffs <- Waldtest(PC_model, splitcr = data[[groupVarName]])
          
          
          for (item.number in 1:ncol(responses)) {
            n.thresholds.g1 <-  length(table(responses.g1[, item.number + 1])) - 1
            
            group1_item.diffs.overall[item.number] <- mean(subgroup_diffs$betapar1[((item.number *
                                                                                       (n.thresholds.g1)) - (n.thresholds.g1 - 1)):(item.number * (n.thresholds.g1))]) *
              -1
            
            n.thresholds.g2 <-  length(table(responses.g2[, item.number +
                                                            1])) - 1
            
            group2_item.diffs.overall[item.number] <- mean(subgroup_diffs$betapar2[((item.number *
                                                                                       (n.thresholds.g2)) - (n.thresholds.g2 - 1)):(item.number * (n.thresholds.g2))]) *
              -1
          }
          
          # Overal difficulties-----------
          
          for (item.number in 1:ncol(responses)) {
            n.thresholds.g <-  length(table(responses.g[, item.number + 1])) - 1
            
            group_item.diffs.overall[item.number] <- mean(PC_model$betapar[((item.number *
                                                                               (n.thresholds.g)) - (n.thresholds.g - 1)):(item.number * (n.thresholds.g))]) *
              -1
          }
          
          # self$results$text$setContent(group_item.diffs.overall)
          
          ## Get overall item SE values:
          
          #- First, get overall SEs specific to each subgroup:
          
          group1_item.se.overall <- NULL
          group2_item.se.overall <- NULL
          
          responses <- data[, -1]
          responses.g <- cbind.data.frame(data[[groupVarName]], responses)
          
          responses.g1 <- subset(responses.g, data[[groupVarName]] == 1)
          responses.g2 <- subset(responses.g, data[[groupVarName]] == 2)
          
          ## Compare thresholds between groups:
          subgroup_diffs <- Waldtest(PC_model, splitcr = data[[groupVarName]])
          
          
          for (item.number in 1:ncol(responses)) {
            n.thresholds.g1 <-  length(table(responses.g1[, item.number + 1])) - 1
            
            group1_item.se.overall[item.number] <- mean(subgroup_diffs$se.beta1[((item.number *
                                                                                    (n.thresholds.g1)) - (n.thresholds.g1 - 1)):(item.number * (n.thresholds.g1))])
            
            n.thresholds.g2 <-  length(table(responses.g2[, item.number +
                                                            1])) - 1
            
            group2_item.se.overall[item.number] <- mean(subgroup_diffs$se.beta2[((item.number *
                                                                                    (n.thresholds.g2)) - (n.thresholds.g2 - 1)):(item.number * (n.thresholds.g2))])
          }
          
          
          # Calculate test statistics for item comparisons:
          z1 <- (group1_item.diffs.overall - group2_item.diffs.overall) /
            sqrt(group1_item.se.overall ^ 2 + group2_item.se.overall ^
                   2)
          
          p <- 2 * pnorm(-abs(z1))
          
          # z table------------
          table <- self$results$z1
          items <- self$options$vars
          
          
          for (i in seq_along(items)) {
            row <- list()
            
            row[["zstat"]] <- z1[i]
            row[["p"]] <- p[i]
            
            table$setRow(rowKey = items[i], values = row)
          }
          
          
          # plot5-------------
          
          image5 <- self$results$plot5
          image5$setState(z1)
          
          ### Item difficulty table--------
          
          comp1 <- data.frame(
            group_item.diffs.overall,
            group1_item.diffs.overall,
            group2_item.diffs.overall
          )
          
          # Name the columns of the results
          # names(comp1) <- c("group1", "group2")
          
          
          # Comparison table---------
          table <- self$results$comp1
          items <- self$options$vars
          
          # get result---
          over <- comp1[, 1]
          g1 <- comp1[, 2]
          g2 <- comp1[, 3]
          
          
          for (i in seq_along(items)) {
            row <- list()
            
            row[["over"]] <- over[i]
            row[["g1"]] <- g1[i]
            row[["g2"]] <- g2[i]
            
            
            table$setRow(rowKey = items[i], values = row)
          }
          
          # Melting for plot--------------------------
          
          comp2 <- data.frame(
            self$options$vars,
            group_item.diffs.overall,
            group1_item.diffs.overall,
            group2_item.diffs.overall
          )
          
          # Name the columns of the results
          names(comp2) <- c("item", "overall", "group1", "group2")
          
          par <- reshape2::melt(comp2, id.vars = c('item'))
          colnames(par) <- c("Item", "Group", "Value")
          
          # Line plot---------
          image6  <-  self$results$plot6
          image6$setState(par)
          
          
          # Scatter plot for partial credit model------------------
          
          total <- cbind.data.frame(
            c(1:length(group1_item.diffs.overall)),
            group1_item.diffs.overall,
            group1_item.se.overall,
            group2_item.diffs.overall,
            group2_item.se.overall
          )
          
          
          group1_item.diffs.overall <- total$group1_item.diffs.overall
          group2_item.diffs.overall <- total$group2_item.diffs.overall
          
          se1 <-  group1_item.se.overall
          se2 <- group2_item.se.overall
          
          state <- list(group1_item.diffs.overall,
                        group2_item.diffs.overall,
                        se1,
                        se2)
          image7 <- self$results$plot7
          image7$setState(state)
          
          
          # Bar plot of item difference-----------
          
          item_dif <- group1_item.diffs.overall - group2_item.diffs.overall
          
          item_dif <- as.vector(item_dif)
          
          
          image8 <- self$results$plot8
          state <- list(item_dif, group1_item.diffs.overall)
          image8$setState(state)
          
          
          
        }
        
      },
      
      .plot1 = function(image1, ggtheme, theme, ...) {
        if (is.null(image1$state))
          return(FALSE)
        
        comparisons <- image1$state
        
        
        min.y <- ifelse(ceiling(min(comparisons$`z-statistic`)) > -3, -3, ceiling(min(comparisons$`z-statistic`)))
        
        max.y <- ifelse(ceiling(max(comparisons$`z-statistic`)) < 3, 3, ceiling(max(comparisons$`z-statistic`)))
        
        plot1 <- plot(
          comparisons$`z-statistic`,
          ylim = c(min.y, max.y),
          ylab = "Z",
          xlab = "Item",
          main = "",
          pch = 19
        )
        abline(h = 2, col = "red", lty = 2)
        abline(h = -2,
               col = "red",
               lty = 2)
        
        legend(
          "topright",
          c("Z Statistic", "Boundaries for Significant Difference"),
          pch = c(19, NA),
          lty = c(NA, 2),
          col = c("black", "red"),
          cex = .7
        )
        
        
        print(plot1)
        TRUE
        
      },
      
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (is.null(image2$state))
          return(FALSE)
        
        p <- image2$state
        
        plot2 <- ggplot2::ggplot(p, ggplot2::aes(x = Item, y = Value, group =
                                                   Group)) +
          ggplot2::geom_line(size = 1.2, ggplot2::aes(color = factor(Group))) +
          ggplot2::geom_point(size = 4, ggplot2::aes(color = factor(Group))) +
          ggplot2::xlab("Item") +
          ggplot2::ylab("Value") +
          ggplot2::labs(color = "Group") +
          ggtheme
        
        
        if (self$options$angle > 0) {
          plot2 <- plot2 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        
        print(plot2)
        TRUE
        
      },
      
      .plot3 = function(image3, ggtheme, theme, ...) {
        if (is.null(image3$state))
          return(FALSE)
        
        subgroup_1_diffs <- image3$state[[1]]
        subgroup_2_diffs <- image3$state[[2]]
        se1 <- image3$state[[3]]
        se2 <- image3$state[[4]]
        
        ## First, calculate values for constructing the confidence bands:
        
        # cf <- self$options$ci
        
        mean.1.2 <- ((subgroup_1_diffs - mean(subgroup_1_diffs)) / 2 * sd(subgroup_1_diffs) +
                       (subgroup_2_diffs - mean(subgroup_2_diffs)) / 2 *
                       sd(subgroup_2_diffs)
        )
        
        joint.se <- sqrt((se1 ^ 2 / sd(subgroup_1_diffs)) +
                           (se2 ^ 2 / sd(subgroup_2_diffs)))
        
        
        upper.group.1 <- mean(subgroup_1_diffs) + ((mean.1.2 - joint.se) *
                                                     sd(subgroup_1_diffs))
        upper.group.2 <- mean(subgroup_2_diffs) + ((mean.1.2 + joint.se) *
                                                     sd(subgroup_2_diffs))
        
        lower.group.1 <- mean(subgroup_1_diffs) + ((mean.1.2 + joint.se) *
                                                     sd(subgroup_1_diffs))
        lower.group.2 <- mean(subgroup_2_diffs) + ((mean.1.2 - joint.se) *
                                                     sd(subgroup_2_diffs))
        
        
        upper <- cbind.data.frame(upper.group.1, upper.group.2)
        upper <- upper[order(upper$upper.group.1, decreasing = FALSE), ]
        
        
        lower <- cbind.data.frame(lower.group.1, lower.group.2)
        lower <- lower[order(lower$lower.group.1, decreasing = FALSE), ]
        
        ## make the scatterplot:
        
        plot3 <-  plot(
          subgroup_1_diffs,
          subgroup_2_diffs,
          xlim = c(-2, 2),
          ylim = c(-2, 2),
          xlab = "Group 1",
          ylab = "Group 2",
          main = "",
          pch = 19
        )
        abline(a = 0,
               b = 1,
               col = "purple")
        
        par(new = T)
        
        lines(upper$upper.group.1,
              upper$upper.group.2,
              lty = 2,
              col = "red")
        
        lines(lower$lower.group.1,
              lower$lower.group.2,
              lty = 2,
              col = "red")
        
        legend(
          "bottomright",
          c("Item Location", "Identity Line", "95% Confidence Band"),
          pch = c(19, NA, NA),
          lty = c(NA, 1, 2),
          col = c("black", "purple", "red")
        )
        
        
        print(plot3)
        TRUE
        
      },
      
      .plot4 = function(image4, ggtheme, theme, ...) {
        if (is.null(image4$state))
          return(FALSE)
        
        item_dif <- image4$state[[1]]
        subgroup_1_diffs <- image4$state[[2]]
        
        
        nvars <- length(self$options$vars)
        # Code to use different colors to highlight items with differences >= .5 logits:
        colors <- NULL
        
        for (item.number in 1:nvars) {
          colors[item.number] <- ifelse(abs(item_dif[item.number]) > .5, "dark blue", "light green")
          
        }
        
        plot4 <- barplot(
          item_dif,
          horiz = TRUE,
          xlim = c(-2, 2),
          col = colors,
          ylim = c(1, 40),
          xlab = "Logit Difference"
        )
        
        # code to add labels to the plot:
        
        dif_labs <- NULL
        
        for (i in 1:length(subgroup_1_diffs)) {
          dif_labs[i] <- ifelse(item_dif[i] < 0, item_dif[i] - .2, item_dif[i] + .2)
        }
        
        plot4 <- text(
          dif_labs,
          plot4,
          labels = c(1:length(subgroup_1_diffs)),
          xlim = c(-1.5, 1.5),
          cex = .8
        )
        
        # add vertical lines to highlight .5 logit differences:
        abline(v = .5, lty = 3)
        abline(v = -.5, lty = 3)
        
        # add additional text to help with interpretation:
        
        text(-1, 40, "Easier to Endorse for Group 1", cex = .8)
        text(1, 40, "Easier to Endorse for Group 2", cex = .8)
        
        legend(
          "bottomright",
          c("Diff >= .5 logits", "Diff < .5 logits"),
          pch = 15,
          col = c("dark blue", "light green"),
          cex = .7
        )
        
        
        print(plot4)
        TRUE
        
      },
      
      .plot5 = function(image5, ggtheme, theme, ...) {
        if (is.null(image5$state))
          return(FALSE)
        
        z <- image5$state
        
        if (is.null(image5$state))
          return(FALSE)
        
        # Plot the test statistics:
        min.y <- ifelse(ceiling(min(z)) > -3, -3, ceiling(min(z)))
        
        max.y <- ifelse(ceiling(max(z)) < 3, 3, ceiling(max(z)))
        
        plot5 <- plot(
          z,
          ylim = c(min.y, max.y),
          ylab = "Z",
          xlab = "Item",
          main = " ",
          pch = 19
        )
        
        #  # axis(1, at = c(1, 2, 3, 4), labels = c(1, 2, 3, 4))
        # axis(1)
        # axis(2)
        
        abline(h = 2, col = "red", lty = 2)
        abline(h = -2,
               col = "red",
               lty = 2)
        
        legend(
          "topright",
          c("Z Statistic", "Boundaries for Significant Difference"),
          pch = c(19, NA),
          lty = c(NA, 2),
          col = c("black", "red"),
          cex = 0.8
        )
        
        print(plot5)
        TRUE
        
      },
      
      .plot6 = function(image6, ggtheme, theme, ...) {
        if (is.null(image6$state))
          return(FALSE)
        
        p <- image6$state
        
        plot6 <- ggplot2::ggplot(p, ggplot2::aes(x = Item, y = Value, group =
                                                   Group)) +
          ggplot2::geom_line(size = 1.2, ggplot2::aes(color = factor(Group))) +
          ggplot2::geom_point(size = 4, ggplot2::aes(color = factor(Group))) +
          ggplot2::xlab("Item") +
          ggplot2::ylab("Value") +
          ggplot2::labs(color = "Group") +
          ggtheme
        
        
        if (self$options$angle1 > 0) {
          plot6 <- plot6 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle1, hjust = 1))
        }
        
        print(plot6)
        TRUE
        
      },
      
      .plot7 = function(image7, ggtheme, theme, ...) {
        if (is.null(image7$state))
          return(FALSE)
        
        group1_item.diffs.overall <- image7$state[[1]]
        group2_item.diffs.overall <- image7$state[[2]]
        group1_item.se.overall <- image7$state[[3]]
        group2_item.se.overall <- image7$state[[4]]
        
        mean.1.2 <- ((
          group1_item.diffs.overall - mean(group1_item.diffs.overall)
        ) / 2 * sd(group1_item.diffs.overall) +
          (
            group2_item.diffs.overall - mean(group2_item.diffs.overall)
          ) / 2 * sd(group2_item.diffs.overall)
        )
        
        joint.se <- sqrt((group1_item.se.overall ^ 2 / sd(group1_item.diffs.overall)) +
                           (group2_item.se.overall ^ 2 / sd(group2_item.diffs.overall)))
        
        
        upper.group.1 <- mean(group1_item.diffs.overall) + ((mean.1.2 - joint.se) *
                                                              sd(group1_item.diffs.overall))
        upper.group.2 <- mean(group2_item.diffs.overall) + ((mean.1.2 + joint.se) *
                                                              sd(group2_item.diffs.overall))
        
        lower.group.1 <- mean(group1_item.diffs.overall) + ((mean.1.2 + joint.se) *
                                                              sd(group1_item.diffs.overall))
        lower.group.2 <- mean(group2_item.diffs.overall) + ((mean.1.2 - joint.se) *
                                                              sd(group1_item.diffs.overall))
        
        
        upper <- cbind.data.frame(upper.group.1, upper.group.2)
        upper <- upper[order(upper$upper.group.1, decreasing = FALSE), ]
        
        
        lower <- cbind.data.frame(lower.group.1, lower.group.2)
        lower <- lower[order(lower$lower.group.1, decreasing = FALSE), ]
        
        
        ## make the scatterplot:
        
        plot7 <- plot(
          group1_item.diffs.overall,
          group2_item.diffs.overall,
          xlim = c(-3, 3),
          ylim = c(-3, 3),
          xlab = "Group 1",
          ylab = "Group 2",
          main = "",
          pch = 19
        )
        abline(a = 0,
               b = 1,
               col = "purple")
        
        par(new = T)
        
        lines(upper$upper.group.1,
              upper$upper.group.2,
              lty = 2,
              col = "red")
        
        lines(lower$lower.group.1,
              lower$lower.group.2,
              lty = 2,
              col = "red")
        
        legend(
          "bottomright",
          c("Item Location", "Identity Line", "95% Confidence Band"),
          pch = c(19, NA, NA),
          lty = c(NA, 1, 2),
          col = c("black", "purple", "red")
        )
        
        
        print(plot7)
        TRUE
        
      },
      
      .plot8 = function(image8, ggtheme, theme, ...) {
        if (is.null(image8$state))
          return(FALSE)
        
        item_dif <- image8$state[[1]]
        group1_item.diffs.overall <- image8$state[[2]]
        
        
        nvars <- length(self$options$vars)
        # Code to use different colors to highlight items with differences >= .5 logits:
        colors <- NULL
        
        for (item.number in 1:nvars) {
          colors[item.number] <- ifelse(abs(item_dif[item.number]) > .5, "dark blue", "light green")
        }
        
        plot8 <- barplot(
          item_dif,
          horiz = TRUE,
          xlim = c(-2, 2),
          col = colors,
          #=ylim = c(1,4),
          xlab = "Logit Difference"
        )
        
        # code to add labels to the plot:
        
        dif_labs <- NULL
        
        for (i in 1:length(group1_item.diffs.overall)) {
          dif_labs[i] <- ifelse(item_dif[i] < 0, item_dif[i] - .2, item_dif[i] + .2)
        }
        
        text(
          dif_labs,
          plot8,
          labels = c(1:length(group1_item.diffs.overall)),
          xlim = c(-1.5, 1.5),
          cex = .8
        )
        
        # add vertical lines to highlight .5 logit differences:
        abline(v = .5, lty = 3)
        abline(v = -.5, lty = 3)
        
        # add additional text to help with interpretation:
        
        text(-1, 4.5, "Easier to Endorse for Group 1", cex = .8)
        text(1, 4.5, "Easier to Endorse for Group 2", cex = .8)
        
        legend(
          "bottomright",
          c("Diff >= .5 logits", "Diff < .5 logits"),
          pch = 15,
          col = c("dark blue", "light green"),
          cex = .7
        )
        
        print(plot8)
        TRUE
        
      }
    )
  )
