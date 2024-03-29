
# This file is automatically generated, you probably don't want to edit this

mixtureOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "mixtureOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            vars = NULL,
            nc = 2,
            step = 1,
            type = "RSM",
            fit = TRUE,
            imean = TRUE,
            imeasure = FALSE,
            ise = FALSE,
            infit = FALSE,
            outfit = FALSE,
            pbis = FALSE,
            average = FALSE,
            angle = 0,
            iplot = FALSE,
            plot3 = FALSE,
            plot2 = TRUE,
            width = 500,
            height = 500,
            width1 = 500,
            height1 = 500,
            width2 = 500,
            height2 = 500, ...) {

            super$initialize(
                package="snowRMM",
                name="mixture",
                requiresData=TRUE,
                ...)

            private$..vars <- jmvcore::OptionVariables$new(
                "vars",
                vars,
                suggested=list(
                    "nominal",
                    "continuous"),
                permitted=list(
                    "factor",
                    "numeric"))
            private$..nc <- jmvcore::OptionInteger$new(
                "nc",
                nc,
                min=2,
                default=2)
            private$..step <- jmvcore::OptionInteger$new(
                "step",
                step,
                min=1,
                default=1)
            private$..type <- jmvcore::OptionList$new(
                "type",
                type,
                options=list(
                    "RSM",
                    "PCM"),
                default="RSM")
            private$..fit <- jmvcore::OptionBool$new(
                "fit",
                fit,
                default=TRUE)
            private$..imean <- jmvcore::OptionBool$new(
                "imean",
                imean,
                default=TRUE)
            private$..imeasure <- jmvcore::OptionBool$new(
                "imeasure",
                imeasure,
                default=FALSE)
            private$..ise <- jmvcore::OptionBool$new(
                "ise",
                ise,
                default=FALSE)
            private$..infit <- jmvcore::OptionBool$new(
                "infit",
                infit,
                default=FALSE)
            private$..outfit <- jmvcore::OptionBool$new(
                "outfit",
                outfit,
                default=FALSE)
            private$..pbis <- jmvcore::OptionBool$new(
                "pbis",
                pbis,
                default=FALSE)
            private$..average <- jmvcore::OptionBool$new(
                "average",
                average,
                default=FALSE)
            private$..angle <- jmvcore::OptionNumber$new(
                "angle",
                angle,
                min=0,
                max=90,
                default=0)
            private$..iplot <- jmvcore::OptionBool$new(
                "iplot",
                iplot,
                default=FALSE)
            private$..plot3 <- jmvcore::OptionBool$new(
                "plot3",
                plot3,
                default=FALSE)
            private$..plot2 <- jmvcore::OptionBool$new(
                "plot2",
                plot2,
                default=TRUE)
            private$..pmember <- jmvcore::OptionOutput$new(
                "pmember")
            private$..pmeasure <- jmvcore::OptionOutput$new(
                "pmeasure")
            private$..pse <- jmvcore::OptionOutput$new(
                "pse")
            private$..pinfit <- jmvcore::OptionOutput$new(
                "pinfit")
            private$..poutfit <- jmvcore::OptionOutput$new(
                "poutfit")
            private$..width <- jmvcore::OptionInteger$new(
                "width",
                width,
                default=500)
            private$..height <- jmvcore::OptionInteger$new(
                "height",
                height,
                default=500)
            private$..width1 <- jmvcore::OptionInteger$new(
                "width1",
                width1,
                default=500)
            private$..height1 <- jmvcore::OptionInteger$new(
                "height1",
                height1,
                default=500)
            private$..width2 <- jmvcore::OptionInteger$new(
                "width2",
                width2,
                default=500)
            private$..height2 <- jmvcore::OptionInteger$new(
                "height2",
                height2,
                default=500)

            self$.addOption(private$..vars)
            self$.addOption(private$..nc)
            self$.addOption(private$..step)
            self$.addOption(private$..type)
            self$.addOption(private$..fit)
            self$.addOption(private$..imean)
            self$.addOption(private$..imeasure)
            self$.addOption(private$..ise)
            self$.addOption(private$..infit)
            self$.addOption(private$..outfit)
            self$.addOption(private$..pbis)
            self$.addOption(private$..average)
            self$.addOption(private$..angle)
            self$.addOption(private$..iplot)
            self$.addOption(private$..plot3)
            self$.addOption(private$..plot2)
            self$.addOption(private$..pmember)
            self$.addOption(private$..pmeasure)
            self$.addOption(private$..pse)
            self$.addOption(private$..pinfit)
            self$.addOption(private$..poutfit)
            self$.addOption(private$..width)
            self$.addOption(private$..height)
            self$.addOption(private$..width1)
            self$.addOption(private$..height1)
            self$.addOption(private$..width2)
            self$.addOption(private$..height2)
        }),
    active = list(
        vars = function() private$..vars$value,
        nc = function() private$..nc$value,
        step = function() private$..step$value,
        type = function() private$..type$value,
        fit = function() private$..fit$value,
        imean = function() private$..imean$value,
        imeasure = function() private$..imeasure$value,
        ise = function() private$..ise$value,
        infit = function() private$..infit$value,
        outfit = function() private$..outfit$value,
        pbis = function() private$..pbis$value,
        average = function() private$..average$value,
        angle = function() private$..angle$value,
        iplot = function() private$..iplot$value,
        plot3 = function() private$..plot3$value,
        plot2 = function() private$..plot2$value,
        pmember = function() private$..pmember$value,
        pmeasure = function() private$..pmeasure$value,
        pse = function() private$..pse$value,
        pinfit = function() private$..pinfit$value,
        poutfit = function() private$..poutfit$value,
        width = function() private$..width$value,
        height = function() private$..height$value,
        width1 = function() private$..width1$value,
        height1 = function() private$..height1$value,
        width2 = function() private$..width2$value,
        height2 = function() private$..height2$value),
    private = list(
        ..vars = NA,
        ..nc = NA,
        ..step = NA,
        ..type = NA,
        ..fit = NA,
        ..imean = NA,
        ..imeasure = NA,
        ..ise = NA,
        ..infit = NA,
        ..outfit = NA,
        ..pbis = NA,
        ..average = NA,
        ..angle = NA,
        ..iplot = NA,
        ..plot3 = NA,
        ..plot2 = NA,
        ..pmember = NA,
        ..pmeasure = NA,
        ..pse = NA,
        ..pinfit = NA,
        ..poutfit = NA,
        ..width = NA,
        ..height = NA,
        ..width1 = NA,
        ..height1 = NA,
        ..width2 = NA,
        ..height2 = NA)
)

mixtureResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "mixtureResults",
    inherit = jmvcore::Group,
    active = list(
        instructions = function() private$.items[["instructions"]],
        item = function() private$.items[["item"]],
        person = function() private$.items[["person"]],
        iplot = function() private$.items[["iplot"]],
        plot3 = function() private$.items[["plot3"]],
        plot2 = function() private$.items[["plot2"]],
        pmember = function() private$.items[["pmember"]],
        pmeasure = function() private$.items[["pmeasure"]],
        pse = function() private$.items[["pse"]],
        pinfit = function() private$.items[["pinfit"]],
        poutfit = function() private$.items[["poutfit"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Rasch Mixture Model",
                refs="snowRMM")
            self$add(jmvcore::Html$new(
                options=options,
                name="instructions",
                title="Instructions",
                visible=TRUE))
            self$add(R6::R6Class(
                inherit = jmvcore::Group,
                active = list(
                    fit = function() private$.items[["fit"]],
                    imean = function() private$.items[["imean"]],
                    imeasure = function() private$.items[["imeasure"]],
                    ise = function() private$.items[["ise"]],
                    infit = function() private$.items[["infit"]],
                    outfit = function() private$.items[["outfit"]],
                    pbis = function() private$.items[["pbis"]]),
                private = list(),
                public=list(
                    initialize=function(options) {
                        super$initialize(
                            options=options,
                            name="item",
                            title="Item Analysis")
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="fit",
                            title="Model FIt Information",
                            visible="(fit)",
                            clearWith=list(
                                "vars",
                                "nc",
                                "step",
                                "type"),
                            refs="mixRasch",
                            columns=list(
                                list(
                                    `name`="name", 
                                    `title`="Class", 
                                    `type`="text", 
                                    `content`="($key)"),
                                list(
                                    `name`="aic", 
                                    `title`="AIC", 
                                    `type`="number"),
                                list(
                                    `name`="bic", 
                                    `title`="BIC", 
                                    `type`="number"),
                                list(
                                    `name`="caic", 
                                    `title`="CAIC", 
                                    `type`="number"),
                                list(
                                    `name`="loglik", 
                                    `title`="Log-likelihood", 
                                    `type`="number"),
                                list(
                                    `name`="parm", 
                                    `title`="Parameters", 
                                    `type`="integer"),
                                list(
                                    `name`="person", 
                                    `title`="Persons", 
                                    `type`="integer"))))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="imean",
                            title="Item mean",
                            visible="(imean)",
                            clearWith=list(
                                "vars",
                                "nc",
                                "step",
                                "type"),
                            refs="mixRasch",
                            columns=list(
                                list(
                                    `name`="name", 
                                    `title`="Item", 
                                    `type`="text", 
                                    `content`="($key)"),
                                list(
                                    `name`="pc1", 
                                    `title`="1", 
                                    `type`="number", 
                                    `superTitle`="Class"))))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="imeasure",
                            title="Measure",
                            visible="(imeasure)",
                            clearWith=list(
                                "vars",
                                "nc",
                                "step",
                                "type"),
                            refs="mixRasch",
                            columns=list(
                                list(
                                    `name`="name", 
                                    `title`="Item", 
                                    `type`="text", 
                                    `content`="($key)"),
                                list(
                                    `name`="pc1", 
                                    `title`="1", 
                                    `type`="number", 
                                    `superTitle`="Class"))))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="ise",
                            title="S.E.Measure",
                            visible="(ise)",
                            clearWith=list(
                                "vars",
                                "nc",
                                "step",
                                "type"),
                            refs="mixRasch",
                            columns=list(
                                list(
                                    `name`="name", 
                                    `title`="Item", 
                                    `type`="text", 
                                    `content`="($key)"),
                                list(
                                    `name`="pc1", 
                                    `title`="1", 
                                    `type`="number", 
                                    `superTitle`="Class"))))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="infit",
                            title="Infit",
                            visible="(infit)",
                            clearWith=list(
                                "vars",
                                "nc",
                                "step",
                                "type"),
                            refs="mixRasch",
                            columns=list(
                                list(
                                    `name`="name", 
                                    `title`="Item", 
                                    `type`="text", 
                                    `content`="($key)"),
                                list(
                                    `name`="pc1", 
                                    `title`="1", 
                                    `type`="number", 
                                    `superTitle`="Class"))))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="outfit",
                            title="Outfit",
                            visible="(outfit)",
                            clearWith=list(
                                "vars",
                                "nc",
                                "step",
                                "type"),
                            refs="mixRasch",
                            columns=list(
                                list(
                                    `name`="name", 
                                    `title`="Item", 
                                    `type`="text", 
                                    `content`="($key)"),
                                list(
                                    `name`="pc1", 
                                    `title`="1", 
                                    `type`="number", 
                                    `superTitle`="Class"))))
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="pbis",
                            title="Point biserial",
                            visible="(pbis)",
                            clearWith=list(
                                "vars",
                                "nc",
                                "step",
                                "type"),
                            refs="mixRasch",
                            columns=list(
                                list(
                                    `name`="name", 
                                    `title`="Item", 
                                    `type`="text", 
                                    `content`="($key)"),
                                list(
                                    `name`="pc1", 
                                    `title`="1", 
                                    `type`="number", 
                                    `superTitle`="Class"))))}))$new(options=options))
            self$add(R6::R6Class(
                inherit = jmvcore::Group,
                active = list(
                    average = function() private$.items[["average"]]),
                private = list(),
                public=list(
                    initialize=function(options) {
                        super$initialize(
                            options=options,
                            name="person",
                            title="Person Analysis")
                        self$add(jmvcore::Table$new(
                            options=options,
                            name="average",
                            title="Mean ability level for each class",
                            visible="(average)",
                            rows="(nc)",
                            clearWith=list(
                                "vars",
                                "nc",
                                "step",
                                "type"),
                            columns=list(
                                list(
                                    `name`="class", 
                                    `title`="Class", 
                                    `content`="($key)"),
                                list(
                                    `name`="value", 
                                    `title`="Theta", 
                                    `type`="number"))))}))$new(options=options))
            self$add(jmvcore::Image$new(
                options=options,
                name="iplot",
                title="Item Plot",
                visible="(iplot)",
                refs="snowRMM",
                renderFun=".itemPlot",
                clearWith=list(
                    "vars",
                    "nc",
                    "step",
                    "type",
                    "angle",
                    "width",
                    "height")))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot3",
                title="Person distributions across class",
                visible="(plot3)",
                refs="snowRMM",
                renderFun=".plot3",
                clearWith=list(
                    "vars",
                    "nc",
                    "step",
                    "type",
                    "width2",
                    "height2")))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot2",
                title="Elbow plot",
                visible="(plot2)",
                width=500,
                height=400,
                refs="snowRMM",
                renderFun=".plot2",
                clearWith=list(
                    "vars",
                    "nc",
                    "step",
                    "type",
                    "width1",
                    "height1")))
            self$add(jmvcore::Output$new(
                options=options,
                name="pmember",
                title="Class membership",
                varTitle="Membership",
                measureType="nominal",
                clearWith=list(
                    "vars",
                    "nc",
                    "step",
                    "type")))
            self$add(jmvcore::Output$new(
                options=options,
                name="pmeasure",
                title="Measure",
                measureType="continuous",
                clearWith=list(
                    "vars",
                    "nc",
                    "step",
                    "type")))
            self$add(jmvcore::Output$new(
                options=options,
                name="pse",
                title="SE",
                measureType="continuous",
                clearWith=list(
                    "vars",
                    "nc",
                    "step",
                    "type")))
            self$add(jmvcore::Output$new(
                options=options,
                name="pinfit",
                title="Infit",
                measureType="continuous",
                clearWith=list(
                    "vars",
                    "nc",
                    "step",
                    "type")))
            self$add(jmvcore::Output$new(
                options=options,
                name="poutfit",
                title="Outfit",
                measureType="continuous",
                clearWith=list(
                    "vars",
                    "nc",
                    "step",
                    "type")))}))

mixtureBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "mixtureBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "snowRMM",
                name = "mixture",
                version = c(1,0,0),
                options = options,
                results = mixtureResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'none')
        }))

#' Rasch Mixture Model
#'
#' 
#' @param data The data as a data frame.
#' @param vars .
#' @param nc .
#' @param step .
#' @param type .
#' @param fit .
#' @param imean .
#' @param imeasure .
#' @param ise .
#' @param infit .
#' @param outfit .
#' @param pbis .
#' @param average .
#' @param angle a number from 0 to 45 defining the angle of the x-axis labels,
#'   where 0 degrees represents completely horizontal labels.
#' @param iplot .
#' @param plot3 .
#' @param plot2 .
#' @param width .
#' @param height .
#' @param width1 .
#' @param height1 .
#' @param width2 .
#' @param height2 .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$instructions} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$item$fit} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$item$imean} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$item$imeasure} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$item$ise} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$item$infit} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$item$outfit} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$item$pbis} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$person$average} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$iplot} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plot3} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plot2} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$pmember} \tab \tab \tab \tab \tab an output \cr
#'   \code{results$pmeasure} \tab \tab \tab \tab \tab an output \cr
#'   \code{results$pse} \tab \tab \tab \tab \tab an output \cr
#'   \code{results$pinfit} \tab \tab \tab \tab \tab an output \cr
#'   \code{results$poutfit} \tab \tab \tab \tab \tab an output \cr
#' }
#'
#' @export
mixture <- function(
    data,
    vars,
    nc = 2,
    step = 1,
    type = "RSM",
    fit = TRUE,
    imean = TRUE,
    imeasure = FALSE,
    ise = FALSE,
    infit = FALSE,
    outfit = FALSE,
    pbis = FALSE,
    average = FALSE,
    angle = 0,
    iplot = FALSE,
    plot3 = FALSE,
    plot2 = TRUE,
    width = 500,
    height = 500,
    width1 = 500,
    height1 = 500,
    width2 = 500,
    height2 = 500) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("mixture requires jmvcore to be installed (restart may be required)")

    if ( ! missing(vars)) vars <- jmvcore::resolveQuo(jmvcore::enquo(vars))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(vars), vars, NULL))


    options <- mixtureOptions$new(
        vars = vars,
        nc = nc,
        step = step,
        type = type,
        fit = fit,
        imean = imean,
        imeasure = imeasure,
        ise = ise,
        infit = infit,
        outfit = outfit,
        pbis = pbis,
        average = average,
        angle = angle,
        iplot = iplot,
        plot3 = plot3,
        plot2 = plot2,
        width = width,
        height = height,
        width1 = width1,
        height1 = height1,
        width2 = width2,
        height2 = height2)

    analysis <- mixtureClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

