% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mixture.h.R
\name{mixture}
\alias{mixture}
\title{Rasch Mixture Model}
\usage{
mixture(
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
  height2 = 500
)
}
\arguments{
\item{data}{The data as a data frame.}

\item{vars}{.}

\item{nc}{.}

\item{step}{.}

\item{type}{.}

\item{fit}{.}

\item{imean}{.}

\item{imeasure}{.}

\item{ise}{.}

\item{infit}{.}

\item{outfit}{.}

\item{pbis}{.}

\item{average}{.}

\item{angle}{a number from 0 to 45 defining the angle of the x-axis labels,
where 0 degrees represents completely horizontal labels.}

\item{iplot}{.}

\item{plot3}{.}

\item{plot2}{.}

\item{width}{.}

\item{height}{.}

\item{width1}{.}

\item{height1}{.}

\item{width2}{.}

\item{height2}{.}
}
\value{
A results object containing:
\tabular{llllll}{
  \code{results$instructions} \tab \tab \tab \tab \tab a html \cr
  \code{results$item$fit} \tab \tab \tab \tab \tab a table \cr
  \code{results$item$imean} \tab \tab \tab \tab \tab a table \cr
  \code{results$item$imeasure} \tab \tab \tab \tab \tab a table \cr
  \code{results$item$ise} \tab \tab \tab \tab \tab a table \cr
  \code{results$item$infit} \tab \tab \tab \tab \tab a table \cr
  \code{results$item$outfit} \tab \tab \tab \tab \tab a table \cr
  \code{results$item$pbis} \tab \tab \tab \tab \tab a table \cr
  \code{results$person$average} \tab \tab \tab \tab \tab a table \cr
  \code{results$iplot} \tab \tab \tab \tab \tab an image \cr
  \code{results$plot3} \tab \tab \tab \tab \tab an image \cr
  \code{results$plot2} \tab \tab \tab \tab \tab an image \cr
  \code{results$pmember} \tab \tab \tab \tab \tab an output \cr
  \code{results$pmeasure} \tab \tab \tab \tab \tab an output \cr
  \code{results$pse} \tab \tab \tab \tab \tab an output \cr
  \code{results$pinfit} \tab \tab \tab \tab \tab an output \cr
  \code{results$poutfit} \tab \tab \tab \tab \tab an output \cr
}
}
\description{
Rasch Mixture Model
}
