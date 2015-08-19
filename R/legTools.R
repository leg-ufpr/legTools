##' @title Convenience Functions, Small GUI to Teach Statistics and Some
##'     Datasets.
##'
##' @description legTools is a collection of R functions and datasets
##'
##' @docType package
##' @name legTools
NULL

#' @name wgpigs
#'
#' @title Feeding type in pig weight gain
#'
#' @description This is an artifial dataset corresponding a experiment
#' to study the effect of feeding type (factor with 4 categorical
#' nominal levels) in pig weight gain. The experiment was a randomized
#' complete design with five experimental units per treatment level. The
#' experimental unit was a pig. The response measured was weight gain
#' from the beggining to the end of the experiment.
#'
#' \itemize{
#'     \item ft feeding type, a categorical factor with 4 levels.
#'     \item wg weight gain (kg).
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(wgpigs)
#'
#' @format a \code{data.frame} with 20 records and 2 variables.
#'
#' @source Frederico, P. (2009). Curso de Estatística Experimental
#' (15th ed.). Piracicaba, São Paulo: FEALQ. (page 62)
#'
#' @examples
#'
#' require(lattice)
#' data(wgpigs)
#'
#' xyplot(wg~ft, data=wgpigs,
#'        ylab="Weight gain (kg)",
#'        xlab="Feeding type")
#'
NULL

#' @name potatoyield
#'
#' @title Potato variety competition experiment
#'
#' @description These data are from an experiment done by the engineer
#' Oscar A. Garay at Balcare, Argentina. The experiment was done in a
#' randomized complete block design with 4 blocks. Potato yield (t/ha)
#' was recorded in each experimental unit.
#'
#' \itemize{
#'     \item block a categorical unordered factor with 4 levels.
#'     \item variety a categorical unordered factor with 8 levels.
#'     \item yield potato yield (t/ha).
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(potatoyield)
#'
#' @format a \code{data.frame} with 32 records and 3 variables.
#'
#' @source Frederico, P. (2009). Curso de Estatística Experimental
#' (15th ed.). Piracicaba, São Paulo: FEALQ. (page 76)
#'
#' @examples
#' require(lattice)
#' data(potatoyield)
#'
#' plot(yield~variety, data=potatoyield,
#'      groups=block, type="o",
#'      ylab=expression(Yield~(t~ha^{-1})),
#'      xlab="Variety")
#'
NULL

#' @name plowing
#'
#' @title Plowing level on corn yield
#'
#' @description These data are from an experiment done by the engineer
#' Duvilio Ometto to study the effect of plowing level on corn yield. It
#' was used 2 levels of plowing: normal (or superficial) and deep. The
#' experiment was done in a randomized complete block design with 6
#' blocks. Corn yield (t/ha) was recorded in each experimental unit
#' but in this experiment there was 2 experimental units for each factor
#' level in each block.
#'
#' \itemize{
#'     \item block a categorical unordered factor with 6 levels.
#'     \item plow a categorical unordered factor with 2 levels.
#'     \item yield corn yield (kg in 200 m\eqn{^2} of area).
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(plowing)
#'
#' @format a \code{data.frame} with 24 records and 3 variables.
#'
#' @source Frederico, P. (2009). Curso de Estatística Experimental
#' (15th ed.). Piracicaba, São Paulo: FEALQ. (page 91)
#'
#' @examples
#' require(lattice)
#' data(potatoyield)
#'
#' xyplot(yield~plow|block, data=plowing, type=c("p", "a"),
#'        ylab=expression(Yield~(t~ha^{-1})),
#'        xlab="Plowing level")
#'
NULL
