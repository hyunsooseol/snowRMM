
# This file is automatically generated, you probably don't want to edit this

lcgmOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "lcgmOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            vars = NULL,
            model = list(),
            nc = 2,
            type = "conti",
            variance = "equal",
            fit = TRUE,
            est = FALSE,
            plot = FALSE,
            raw = "FALSE",
            width = 500,
            height = 500, ...) {

            super$initialize(
                package="snowRMM",
                name="lcgm",
                requiresData=TRUE,
                ...)

            private$..vars <- jmvcore::OptionVariables$new(
                "vars",
                vars,
                suggested=list(
                    "ordinal",
                    "continuous"),
                permitted=list(
                    "factor",
                    "numeric"))
            private$..model <- jmvcore::OptionArray$new(
                "model",
                model,
                default=list(),
                template=jmvcore::OptionString$new(
                    "model",
                    NULL))
            private$..nc <- jmvcore::OptionInteger$new(
                "nc",
                nc,
                min=2,
                default=2)
            private$..type <- jmvcore::OptionList$new(
                "type",
                type,
                options=list(
                    "conti",
                    "ordi"),
                default="conti")
            private$..variance <- jmvcore::OptionList$new(
                "variance",
                variance,
                options=list(
                    "equal",
                    "varying"),
                default="equal")
            private$..fit <- jmvcore::OptionBool$new(
                "fit",
                fit,
                default=TRUE)
            private$..est <- jmvcore::OptionBool$new(
                "est",
                est,
                default=FALSE)
            private$..plot <- jmvcore::OptionBool$new(
                "plot",
                plot,
                default=FALSE)
            private$..raw <- jmvcore::OptionList$new(
                "raw",
                raw,
                options=list(
                    "FALSE",
                    "TRUE"),
                default="FALSE")
            private$..width <- jmvcore::OptionInteger$new(
                "width",
                width,
                default=500)
            private$..height <- jmvcore::OptionInteger$new(
                "height",
                height,
                default=500)

            self$.addOption(private$..vars)
            self$.addOption(private$..model)
            self$.addOption(private$..nc)
            self$.addOption(private$..type)
            self$.addOption(private$..variance)
            self$.addOption(private$..fit)
            self$.addOption(private$..est)
            self$.addOption(private$..plot)
            self$.addOption(private$..raw)
            self$.addOption(private$..width)
            self$.addOption(private$..height)
        }),
    active = list(
        vars = function() private$..vars$value,
        model = function() private$..model$value,
        nc = function() private$..nc$value,
        type = function() private$..type$value,
        variance = function() private$..variance$value,
        fit = function() private$..fit$value,
        est = function() private$..est$value,
        plot = function() private$..plot$value,
        raw = function() private$..raw$value,
        width = function() private$..width$value,
        height = function() private$..height$value),
    private = list(
        ..vars = NA,
        ..model = NA,
        ..nc = NA,
        ..type = NA,
        ..variance = NA,
        ..fit = NA,
        ..est = NA,
        ..plot = NA,
        ..raw = NA,
        ..width = NA,
        ..height = NA)
)

lcgmResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "lcgmResults",
    inherit = jmvcore::Group,
    active = list(
        instructions = function() private$.items[["instructions"]],
        text = function() private$.items[["text"]],
        fit = function() private$.items[["fit"]],
        est = function() private$.items[["est"]],
        plot = function() private$.items[["plot"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Latent Class Growth Modeling")
            self$add(jmvcore::Html$new(
                options=options,
                name="instructions",
                title="Instructions",
                visible=TRUE))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text",
                title=""))
            self$add(jmvcore::Table$new(
                options=options,
                name="fit",
                title="Model fit",
                visible="(fit)",
                clearWith=list(
                    "vars",
                    "model",
                    "nc",
                    "type",
                    "variance"),
                refs="tidySEM",
                columns=list(
                    list(
                        `name`="name", 
                        `title`="", 
                        `type`="text", 
                        `content`="($key)"),
                    list(
                        `name`="value", 
                        `title`="Values"))))
            self$add(jmvcore::Table$new(
                options=options,
                name="est",
                title="Estimated parameters",
                visible="(est)",
                clearWith=list(
                    "vars",
                    "model",
                    "nc",
                    "type",
                    "variance"),
                refs="tidySEM",
                columns=list(
                    list(
                        `name`="name", 
                        `title`="", 
                        `type`="text", 
                        `content`="($key)"),
                    list(
                        `name`="cat", 
                        `title`="Category", 
                        `type`="text"),
                    list(
                        `name`="lhs", 
                        `title`="lhs", 
                        `type`="text"),
                    list(
                        `name`="est", 
                        `title`="Estimate", 
                        `type`="number"),
                    list(
                        `name`="se", 
                        `title`="SE", 
                        `type`="number"),
                    list(
                        `name`="p", 
                        `title`="p", 
                        `format`="zto,pvalue"),
                    list(
                        `name`="ci", 
                        `title`="Confidence Interval", 
                        `type`="number"),
                    list(
                        `name`="na", 
                        `title`="name", 
                        `type`="text"))))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot",
                title="Trajectory plot",
                visible="(plot)",
                renderFun=".plot",
                clearWith=list(
                    "vars",
                    "model",
                    "nc",
                    "type",
                    "variance",
                    "raw",
                    "width",
                    "height")))}))

lcgmBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "lcgmBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "snowRMM",
                name = "lcgm",
                version = c(1,0,0),
                options = options,
                results = lcgmResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' Latent Class Growth Modeling
#'
#' 
#' @param data .
#' @param vars .
#' @param model .
#' @param nc .
#' @param type .
#' @param variance .
#' @param fit .
#' @param est .
#' @param plot .
#' @param raw .
#' @param width .
#' @param height .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$instructions} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$text} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$fit} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$est} \tab \tab \tab \tab \tab a table \cr
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
lcgm <- function(
    data,
    vars,
    model = list(),
    nc = 2,
    type = "conti",
    variance = "equal",
    fit = TRUE,
    est = FALSE,
    plot = FALSE,
    raw = "FALSE",
    width = 500,
    height = 500) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("lcgm requires jmvcore to be installed (restart may be required)")

    if ( ! missing(vars)) vars <- jmvcore::resolveQuo(jmvcore::enquo(vars))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(vars), vars, NULL))


    options <- lcgmOptions$new(
        vars = vars,
        model = model,
        nc = nc,
        type = type,
        variance = variance,
        fit = fit,
        est = est,
        plot = plot,
        raw = raw,
        width = width,
        height = height)

    analysis <- lcgmClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

