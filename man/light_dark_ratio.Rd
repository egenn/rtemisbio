% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xt.R
\name{light_dark_ratio}
\alias{light_dark_ratio}
\title{Calculate light/dark ratio for \code{xt} object}
\usage{
light_dark_ratio(
  x,
  groupname = "Lights",
  fn = mean,
  backend = getOption("rt.backend", "data.table"),
  ...
)
}
\arguments{
\item{x}{\code{xt} object.}

\item{fn}{Function: Function to apply to each group.}

\item{backend}{Character: "base", "data.table", or "dplyr"; backend to use for aggregation.}

\item{...}{Additional arguments passed to \code{fn}.}
}
\value{
data.frame with columns for group and summary statistic.
}
\description{
Calculates light/dark ratio for each \code{y} and \code{y2} timeseries in an \code{xt} object.
}
\author{
EDG
}
