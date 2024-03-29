% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lltm.h.R
\name{lltm}
\alias{lltm}
\title{Linear Logisitic Test Model}
\usage{
lltm(
  data,
  vars,
  mat = "1,0,1,1,1,0,1,0,1,0,1,1,1,1,1,0,0,0,1,1,0,1,0,1,1,  -0.5, 1,  1, -1,0",
  col = 3,
  items = TRUE,
  lr = TRUE,
  ml = TRUE,
  wald = FALSE,
  eta = FALSE,
  beta = FALSE,
  plot = FALSE,
  comp = FALSE,
  width = 500,
  height = 500
)
}
\arguments{
\item{data}{The data as a data frame.}

\item{vars}{.}

\item{mat}{.}

\item{col}{.}

\item{items}{.}

\item{lr}{.}

\item{ml}{.}

\item{wald}{.}

\item{eta}{.}

\item{beta}{.}

\item{plot}{.}

\item{comp}{.}

\item{width}{.}

\item{height}{.}
}
\value{
A results object containing:
\tabular{llllll}{
  \code{results$instructions} \tab \tab \tab \tab \tab a html \cr
  \code{results$ra$items} \tab \tab \tab \tab \tab a table \cr
  \code{results$ra$lr} \tab \tab \tab \tab \tab a table \cr
  \code{results$ra$ml} \tab \tab \tab \tab \tab a table \cr
  \code{results$ra$wald} \tab \tab \tab \tab \tab a table \cr
  \code{results$ll$eta} \tab \tab \tab \tab \tab a table \cr
  \code{results$ll$beta} \tab \tab \tab \tab \tab a table \cr
  \code{results$ll$comp} \tab \tab \tab \tab \tab a table \cr
  \code{results$plot} \tab \tab \tab \tab \tab an image \cr
}
}
\description{
Linear Logisitic Test Model
}
