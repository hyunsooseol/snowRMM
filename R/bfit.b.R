
# This file is a generated template, your changes will not be overwritten

#' Mixture Rasch Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import boot
#' @import stats
#' @import mixRasch
#' @importFrom mixRasch mixRasch
#' @importFrom boot boot
#' @importFrom boot boot.ci
#' @importFrom stats quantile
#' @export


bfitClass <- if (requireNamespace('jmvcore'))
   R6::R6Class(
      "bfitClass",
      inherit = bfitBase,
      private = list(
         
         ###### .init function--------
         
         .init = function() {
            if (is.null(self$data) | is.null(self$options$vars)) {
               self$results$instructions$setVisible(visible = TRUE)
               
            }
            
            self$results$instructions$setContent(
               "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>

            <p> The traditional Rasch model is performed by mixRasch R package using Jonint Maximum Liklihood(JML).</p>

            <p> Specify <b>'Step'(number of category-1) and 'Bootstrap N'</b> in the 'Analysis option'.</p>

            <p> Please, be patient. The bootstrapped confidence interval is <b>quite time-consuming !</b></p>

            <p> Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowRMM/'  target = '_blank'>GitHub</a></p>

            </div>
            </body>
            </html>"
            )
            
         },
         
         .run = function() {
            
            ### Caution ####
            
            # When the estimates  do not converged(for example, all 0 or 1)
            # The error message will be shown (number of items to replace is not a multiple of replacement length)
            
            # get variables-------
            
            data <- self$data
            
            vars <- self$options$vars
            
            
            # Ready--------
            
            ready <- TRUE
            
            if (is.null(self$options$vars) |
                length(self$options$vars) < 2)
               
               ready <- FALSE
            
            if (ready) {
               data <- private$.cleanData()
               
               results <- private$.compute(data)
               
               
               # populate Boot Fit table-------------
               
               private$.populateInTable(results)
               
               private$.populateOutTable(results)
               
            }
         },
         
         .compute = function(data) {
            
            # get variables--------
            
           # data <- self$data
            
            vars <- self$options$vars
            
            step <- self$options$step
            
            bn <- self$options$bn
            
           
            
            # Computing boot infit-------------
            
            boot.infit <- function(data, indices) {
               d = data[indices,]
               
               # estimate Rasch model--------
               res1 <-
                  mixRasch::mixRasch(
                     data = d,
                     steps = step,
                     model = "RSM",
                     n.c = 1
                  )
               
               
               # item infit--------
               infit <- res1$item.par$in.out[, 1]
               
               return(infit)
               
            }
            
            boot.in <-
               boot::boot(data = data,
                          statistic = boot.infit,
                          R = bn)
            
            infit <- boot.in$t
            
            infitlow = NA
            
            for (i in 1:ncol(data)) {
               infitlow[i] <- stats::quantile(infit[, i], .025)
               
               
            }
            
            #infit lower--
            
            infitlow <- infitlow
            
            
            #infit high-
            
            infithigh = NA
            
            for (i in 1:ncol(data)) {
               infithigh[i] <- stats::quantile(infit[, i], .975)
               
               
            }
            
            infithigh <- infithigh
            
           
            ############ computing boot outfit------
            
            boot.outfit <- function(data, indices) {
               d = data[indices,]
               
               # estimate Rasch model
               res1 <-
                  mixRasch::mixRasch(
                     data = d,
                     steps = step,
                     model = "RSM",
                     n.c = 1
                  )
               
               
               # item outfit-------
               
               outfit <- res1$item.par$in.out[, 3]
               
               return(outfit)
            }
            
            boot.out <-
               boot::boot(data = data,
                          statistic = boot.outfit,
                          R = bn)
            
            outfit <- boot.out$t
            
            outfitlow = NA
            
            for (i in 1:ncol(data)) {
               outfitlow[i] <- stats::quantile(outfit[, i], .025)
               
               
            }
            
            # outfit low------
            
            outfitlow <- outfitlow
            
            
            #outfit high------------
            
            outfithigh = NA
            
            for (i in 1:ncol(data)) {
               outfithigh[i] <- stats::quantile(outfit[, i], .975)
               
               
            }
            
            outfithigh <- outfithigh
            
            
            
            results <-
               list(
                  'infitlow' = infitlow,
                  'infithigh' = infithigh,
                  'outfitlow' = outfitlow,
                  'outfithigh' = outfithigh
               )
            
            
            
         },
         
     
         # Populate boot table------------
         
         .populateInTable = function(results) {
            table <- self$results$item$binfit
            
            vars <- self$options$vars
            
            # results------
            
            infitlow <- results$infitlow
            infithigh <- results$infithigh
            
            
            for (i in seq_along(vars)) {
               row <- list()
               
               
               row[["infitlow"]] <- infitlow[i]
               row[["infithigh"]] <- infithigh[i]
               
               
               table$setRow(rowKey = vars[i], values = row)
               
            }
         },
         
         
         
         .populateOutTable = function(results) {
            table <- self$results$item$boutfit
            
            vars <- self$options$vars
            
            outfitlow <- results$outfitlow
            outfithigh <- results$outfithigh
            
            
            for (i in seq_along(vars)) {
               row <- list()
               
               row[['outfitlow']] <- outfitlow[i]
               row[['outfithigh']] <- outfithigh[i]
               
               
               table$setRow(rowKey = vars[i], values = row)
               
            }
         },
         
         
         #### Helper functions =================================
         
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
