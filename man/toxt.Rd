% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xt.R
\name{toxt}
\alias{toxt}
\title{Create an \code{xt} object}
\usage{
toxt(
  x,
  y,
  x2 = NULL,
  y2 = NULL,
  zt = NULL,
  shade = NULL,
  group = NULL,
  xunits = NULL,
  yunits = NULL,
  y2units = NULL,
  description = NULL,
  reference = NULL
)
}
\arguments{
\item{x}{Named list of datetime vectors.}

\item{y}{Named list of numeric vectors: When plotted, these will correspond to the
left y-axis.}

\item{x2}{Named list of datetime vectors: When plotted, these will correspond to the
right x-axis. If not provided, \code{x} will be used for both \code{y} and \code{y2}.}

\item{y2}{Named list of numeric vectors: When plotted, these will correspond to the
right y-axis.}

\item{zt}{Numeric vector: Zeitgeber time. If provided, this will be used to label x-axis ticks.
Assumes a single datetime vector in \code{x}. Elements in \code{zt} must correspond to elements in \code{x}.}

\item{shade}{Binary vector: \code{0} indicates no shading, \code{1} indicates shading. If provided,
this will be used to shade the plot.}

\item{group}{Named list of factors: Grouping variable(s).}

\item{xunits}{Character: Units for \code{x}.}

\item{yunits}{Character: Units for \code{y}.}

\item{y2units}{Character: Units for \code{y2}.}

\item{description}{Character: Description of the data / experiment.}

\item{reference}{Character: Link to reference (journal publication, preprint, etc.)}
}
\value{
\code{xt} object
}
\description{
Creates an \code{xt} object from time series data.
}
\author{
EDG
}
