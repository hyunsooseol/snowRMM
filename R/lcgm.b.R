
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import tidySEM
#' @import blavaan
#' @import OpenMx
#' @importFrom tidySEM mx_growth_mixture
#' @importFrom tidySEM mx_mixture
#' @importFrom tidySEM table_fit
#' @import ggplot2
#' @export

lcgmClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lcgmClass",
    inherit = lcgmBase,
    private = list(
        .run = function() {

          
          if (is.null(self$options$vars) ||
              length(self$options$vars) < 3) return()
           
          vars <- self$options$vars
          model <- self$options$model
          nc <- self$options$nc
          type <- self$options$type
          variance <- self$options$variance  
          
          data <- self$data
          data <- na.omit(data)
          data <- as.data.frame(data)
          
          library(tidySEM)
          res <- tidySEM::mx_growth_mixture(model = model,
                                     classes = nc,
                                     data = data)

          # Get fit table fit
          fit <- tidySEM::table_fit(res)
          
          self$results$text$setContent(fit)

        })
)
