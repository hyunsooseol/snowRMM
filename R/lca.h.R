
# This file is automatically generated, you probably don't want to edit this

lcaOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "lcaOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            vars = NULL,
            nc = 2,
            fit = TRUE,
            plot = FALSE, ...) {

            super$initialize(
                package="snowRMM",
                name="lca",
                requiresData=TRUE,
                ...)

            private$..vars <- jmvcore::OptionVariables$new(
                "vars",
                vars,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor"))
            private$..nc <- jmvcore::OptionInteger$new(
                "nc",
                nc,
                min=2,
                default=2)
            private$..fit <- jmvcore::OptionBool$new(
                "fit",
                fit,
                default=TRUE)
            private$..cm <- jmvcore::OptionOutput$new(
                "cm")
            private$..plot <- jmvcore::OptionBool$new(
                "plot",
                plot,
                default=FALSE)

            self$.addOption(private$..vars)
            self$.addOption(private$..nc)
            self$.addOption(private$..fit)
            self$.addOption(private$..cm)
            self$.addOption(private$..plot)
        }),
    active = list(
        vars = function() private$..vars$value,
        nc = function() private$..nc$value,
        fit = function() private$..fit$value,
        cm = function() private$..cm$value,
        plot = function() private$..plot$value),
    private = list(
        ..vars = NA,
        ..nc = NA,
        ..fit = NA,
        ..cm = NA,
        ..plot = NA)
)

lcaResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "lcaResults",
    inherit = jmvcore::Group,
    active = list(
        instructions = function() private$.items[["instructions"]],
        fit = function() private$.items[["fit"]],
        cm = function() private$.items[["cm"]],
        plot = function() private$.items[["plot"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Latent Class Analysis",
                refs="snowRMM")
            self$add(jmvcore::Html$new(
                options=options,
                name="instructions",
                title="Instructions",
                visible=TRUE))
            self$add(jmvcore::Table$new(
                options=options,
                name="fit",
                title="Model fit",
                rows=1,
                clearWith=list(
                    "vars",
                    "nc"),
                refs="poLCA",
                columns=list(
                    list(
                        `name`="Class", 
                        `type`="number"),
                    list(
                        `name`="AIC", 
                        `type`="number"),
                    list(
                        `name`="BIC", 
                        `type`="number"),
                    list(
                        `name`="Entropy", 
                        `type`="number"),
                    list(
                        `name`="Gsq", 
                        `type`="number"),
                    list(
                        `name`="Chisq", 
                        `type`="number"))))
            self$add(jmvcore::Output$new(
                options=options,
                name="cm",
                title="Class membership",
                varTitle="Membership",
                measureType="continuous",
                clearWith=list(
                    "vars",
                    "nc")))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot",
                title="LCA Plot",
                visible="(plot)",
                width=600,
                height=450,
                renderFun=".plot",
                clearWith=list(
                    "vars",
                    "nc")))}))

lcaBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "lcaBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "snowRMM",
                name = "lca",
                version = c(1,0,0),
                options = options,
                results = lcaResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE)
        }))

#' Latent Class Analysis
#'
#' 
#' @param data The data as a data frame.
#' @param vars .
#' @param nc .
#' @param fit .
#' @param plot .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$instructions} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$fit} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$cm} \tab \tab \tab \tab \tab an output \cr
#'   \code{results$plot} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$fit$asDF}
#'
#' \code{as.data.frame(results$fit)}
#'
#' @export
lca <- function(
    data,
    vars,
    nc = 2,
    fit = TRUE,
    plot = FALSE) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("lca requires jmvcore to be installed (restart may be required)")

    if ( ! missing(vars)) vars <- jmvcore::resolveQuo(jmvcore::enquo(vars))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(vars), vars, NULL))

    for (v in vars) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- lcaOptions$new(
        vars = vars,
        nc = nc,
        fit = fit,
        plot = plot)

    analysis <- lcaClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}
