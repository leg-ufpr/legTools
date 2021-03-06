% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/loessGui.R
\name{loessGui}
\alias{loessGui}
\title{Loess regression GUI}
\usage{
loessGui(x, y, data, er = 0.05)
}
\arguments{
\item{x,y}{independent and dependent (numeric) regression variables.}

\item{data}{an optional \code{data.frame}.}

\item{er}{stands for extend range. It is used to extend the plotting
    range by a fraction on both sides and directions. Default is
    0.05. See \link[grDevices]{extendrange}.}
}
\value{
None is returned by the function, only a GUI is opened.
}
\description{
This function opens an interface to control the settings
of a loess regression:
\itemize{
  \item degree choose the local polynomial degree with a radio
selector;
  \item span set the span value that controls the degree of
smoothing;
  \item center move the x value to be predicted;
}

The elements of the interface change a plot that shows the observed
values and the corresponding fitted curve superimposed with
confidence bands (for the fitted values). It assumes that
\code{gWidgets} and \code{gWidgetstcltk} packages are available.
}
\examples{
\donttest{

library(gWidgets)
library(gWidgetstcltk)

loessGui(x=area, y=peri, data=rock, er=0.3)
loessGui(x=speed, y=dist, data=cars, er=0.3)
loessGui(x=eruptions, y=waiting, data=faithful, er=0.3)

}
}
\author{
Walmes Zeviani, \email{walmes@ufpr.br}
}

