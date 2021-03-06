% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/panel.segplot.by.R
\name{panel.segplot.by}
\alias{panel.segplot.by}
\title{Lattice panel to non overlapping segments in \code{segplot()}}
\usage{
panel.segplot.by(x, y, z, data, centers, subscripts, groups, f = 0.05,
  rev = FALSE, ...)
}
\arguments{
\item{x,y,z,data,centers,subscripts,...}{see
\code{\link[latticeExtra]{segplot}}.}

\item{groups}{the grouping variable. Must be a factor.}

\item{f}{numeric, factor that is the vertical distance among
arrows. In general a value less than 1. Default is 0.05.}

\item{rev}{logical, use the reverse order of the factor levels to
    place the segments. Default is \code{FALSE}.}
}
\value{
None is returned.
}
\description{
This panel allows no overlapping of segments in
    \code{latticeExtra::segplot()} when using the argument groups.
}
\examples{
library(latticeExtra)

m0 <- lm(log(breaks)~wool*tension, data=warpbreaks)
anova(m0)
par(mfrow=c(2,2)); plot(m0); layout(1)

pred <- data.frame(wool=c("A", "B", "A", "B", "A", "B"),
                   tension=c("L", "L", "M", "M", "H", "H"))

X <- matrix(c(1, 1, 1, 1, 1, 1,
              0, 1, 0, 1, 0, 1,
              0, 0, 1, 1, 0, 0,
              0, 0, 0, 0, 1, 1,
              0, 0, 0, 1, 0, 0,
              0, 0, 0, 0, 0, 1), nrow=6, ncol=6)

## Estimate and standart error.
## X\%*\%coef(m0)
## sqrt(diag(X\%*\%vcov(m0)\%*\%t(X)))

U <- chol(vcov(m0))
pred$est <- X\%*\%coef(m0)
pred$se <- sqrt(apply(X\%*\%t(U), MARGIN=1, FUN=function(x) sum(x^2)))

tval <- qt(p=c(lwr=0.025, upr=0.975), df=df.residual(m0))
pred <- cbind(pred, sweep(x=outer(pred$se, tval, "*"),
                          MARGIN=1, STATS=pred$est, FUN="+"))

## Overlapping segments.
segplot(wool~lwr+upr, centers=est, data=pred, draw=FALSE)

## Prefer ordering always before using "pch=".
pred <- pred[with(pred, order(tension, wool)), ]

segplot(tension~lwr+upr, data=pred,
        centers=est, draw=FALSE,
        ylab="Tension level",
        xlab=expression("Estimate"\%+-\%"error margin for a 0.95 CI"),
        groups=wool, f=0.05, rev=TRUE,
        pch=as.integer(pred$wool),
        panel=panel.segplot.by,
        key=list(title="Type of wool", cex.title=1.1,
                 text=list(levels(pred$wool)),
                 lines=list(pch=1:2, lty=1),
                 divide=1, type="o"))
}
\author{
Walmes Zeviani, \email{walmes@ufpr.br}
}
\seealso{
\code{\link[latticeExtra]{segplot}}
}

