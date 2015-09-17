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
#'     to study the effect of feeding type (factor with 4 categorical
#'     nominal levels) in pig weight gain. The experiment was a
#'     randomized complete design with five experimental units per
#'     treatment level. The experimental unit was a pig. The response
#'     measured was weight gain from the beggining to the end of the
#'     experiment.
#'
#' \itemize{
#'     \item \code{ft} feeding type, a categorical factor with 4
#'     levels.
#'     \item \code{wg} weight gain (kg).
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
#' @source Frederico, P. (2009). Curso de Estatística Experimental (15th
#'     ed.). Piracicaba, São Paulo: FEALQ. (page 62)
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
#'     Oscar A. Garay at Balcare, Argentina. The experiment was done in
#'     a randomized complete block design with 4 blocks. Potato yield
#'     (t/ha) was recorded in each experimental unit.
#'
#' \itemize{
#'     \item \code{block} a categorical unordered factor with 4 levels.
#'     \item \code{variety} a categorical unordered factor with 8
#'     levels.
#'     \item \code{yield} potato yield (t/ha).
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
#' @source Frederico, P. (2009). Curso de Estatística Experimental (15th
#'     ed.). Piracicaba, São Paulo: FEALQ. (page 76)
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
#'     Duvilio Ometto to study the effect of plowing level on corn
#'     yield. It was used 2 levels of plowing: normal (or superficial)
#'     and deep. The experiment was done in a randomized complete block
#'     design with 6 blocks. Corn yield (t/ha) was recorded in each
#'     experimental unit but in this experiment there was 2 experimental
#'     units for each factor level in each block.
#'
#' \itemize{
#'     \item \code{block} a categorical unordered factor with 6 levels.
#'     \item \code{plow} a categorical unordered factor with 2 levels.
#'     \item \code{yield} corn yield (kg in 200 m\eqn{^2} of area).
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
#' @source Frederico, P. (2009). Curso de Estatística Experimental (15th
#'     ed.). Piracicaba, São Paulo: FEALQ. (page 91)
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

#' @name defoliation
#'
#' @title Bolls in cotton as function of artifitial defoliation
#'
#' @description This dataset contais the result of a real experiment to
#'     evaluate the effect of artifitial defoliation in combination with
#'     phenological stage of occurence on the production of cotton
#'     represented by the number of bolls produced at the end of the
#'     crop cycle. The experiment is a \eqn{5\times 5} factorial with 5
#'     replications casualized at random to the experimental units (a
#'     randomized complete design). The experimental unit was a pot with
#'     2 plants. An interesting fact about this data is that the
#'     response is a count variable that shows underdispersion (sample
#'     variance less than the sample mean).
#'
#' \itemize{
#' \item \code{phenol} a categorical ordered factor with 5 levels
#'     that represent the phenological stages of the cotton plant in
#'     which defoliation was applied.
#' \item \code{defol} a numeric factor with 5 levels that represents the
#'     artifical level of defoliation (percent in leaf area removed with
#'     scissors) applied for all leaves in the plant.
#' \item \code{rept} index for each experimenal unit in each treatment cell.
#' \item \code{bolls} the number of bolls produced (count variable)
#'     evaluated at harvest.
#' }
#'
#' @details The experiment was done in a greenhouse at Universidade
#'     Federal da Grande Dourados. Visit
#' \itemize{
#' \item 1) \code{http://www.cabdirect.org/abstracts/20123299470.html}
#' \item 2) \code{http://leg.ufpr.br/doku.php/publications:papercompanions:zeviani-jas2014}
#' }
#' 1 for an article discussing the effect of defoliation on cotton yield and
#'     visit 2 for an article that evaluate a count regression model able to
#'     deal with the underdispersion. See the references section also.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(defoliation)
#'
#' @format a \code{data.frame} with 125 records and 4 variables.
#'
#' @references Silva, A. M., Degrande, P. E., Suekane, R., Fernandes,
#'     M. G., & Zeviani, W. M. (2012). Impacto de diferentes níveis de
#'     desfolha artificial nos estádios fenológicos do
#'     algodoeiro. Revista de Ciências Agrárias, 35(1), 163–172.
#'
#' Zeviani, W. M., Ribeiro, P. J., Bonat, W. H., Shimakura, S. E., &
#'     Muniz, J. A. (2014). The Gamma-count distribution in the analysis
#'     of experimental underdispersed data. Journal of Applied
#'     Statistics, 41(12),
#'     1–11. http://doi.org/10.1080/02664763.2014.922168
#'
#' @examples
#'
#' library(lattice)
#' library(latticeExtra)
#'
#' ## x11(width=7, height=2.8)
#' xyplot(bolls~defol|phenol, data=defoliation,
#'        layout=c(NA, 1), type=c("p", "smooth"),
#'        xlab="Artificial defoliation level",
#'        ylab="Number of bolls produced",
#'        xlim=extendrange(c(0:1), f=0.15), jitter.x=TRUE)
#'
#' ## Sample mean and variance in each treatment cell.
#' mv <- aggregate(bolls~phenol+defol, data=defoliation,
#'                 FUN=function(x) c(mean=mean(x), var=var(x)))
#' str(mv)
#'
#' xlim <- ylim <- extendrange(c(mv$bolls), f=0.05)
#'
#' ## Evidence in favor of the underdispersion.
#' xyplot(bolls[,"var"]~bolls[,"mean"], data=mv,
#'        aspect="iso", xlim=xlim, ylim=ylim,
#'        ylab="Sample variance", xlab="Sample mean")+
#'     layer(panel.abline(a=0, b=1, lty=2))
#'
NULL

#' @name cassavaYield
#'
#' @title Cassava variety competition experiment
#'
#' @description These data are from an experiment done by The Brazilian
#'     Agricultural Research Corporation in Cassava & Tropical Fruits
#'     (Centro Nacional de Pesquisa da Mandioca e Fruticultura da
#'     Embrapa) at Cruz das Almas, Bahia. The experiment was done in a
#'     randomized complete block design with 4 blocks. Cassava yield
#'     (t/ha) was recorded in each experimental unit.
#'
#' \itemize{
#'     \item \code{block} a categorical unordered factor with 4 levels.
#'     \item \code{variety} a categorical unordered factor with 6
#'     levels.
#'     \item \code{yield} cassava yield (t/ha).
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(cassavaYield)
#'
#' @format a \code{data.frame} with 24 records and 3 variables.
#'
#' @source Frederico, P. (2009). Curso de Estatística Experimental (15th
#'     ed.). Piracicaba, São Paulo: FEALQ. (page 93)
#'
#' @examples
#' 
#' require(lattice)
#' data(cassavaYield)
#'
#' plot(yield~variety, data=cassavaYield,
#'      groups=block, type="o",
#'      ylab=expression(Yield~(t~ha^{-1})),
#'      xlab="Variety")
#'
NULL
