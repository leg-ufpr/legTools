##' @title Convenience Functions, Small GUI to Teach Statistics and Some
##'     Datasets.
##'
##' @description legTools is a collection of R functions and datasets
##'
##' @docType package
##' @name legTools
NULL

#' @name wgPigs
#'
#' @title Feeding type in pig weight gain
#'
#' @description This is an artificial data set corresponding a
#'     experiment to study the effect of feeding type (factor with 4
#'     categorical nominal levels) in pig weight gain. The experiment
#'     was a randomized complete design with five experimental units per
#'     treatment level. The experimental unit was a pig. The response
#'     measured was weight gain from the beginning to the end of the
#'     experiment.
#'
#' \itemize{
#'
#' \item \code{ft} feeding type, a categorical factor with 4 levels.
#'
#' \item \code{wg} weight gain (kg).
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(wgPigs)
#'
#' @format a \code{data.frame} with 20 records and 2 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 62)
#'
#' @examples
#'
#' library(lattice)
#' data(wgPigs)
#'
#' xyplot(wg~ft, data=wgPigs,
#'        ylab="Weight gain (kg)",
#'        xlab="Feeding type")
#'
NULL

#' @name potatoYield
#'
#' @title Potato variety competition experiment
#'
#' @description These data are from an experiment done by the engineer
#'     Oscar A. Garay at Balcare, Argentina. The experiment was done in
#'     a randomized complete block design with 4 blocks. Potato yield
#'     (t/ha) was recorded in each experimental unit.
#'
#' \itemize{
#'
#' \item \code{block} a categorical unordered factor with 4 levels.
#'
#' \item \code{variety} a categorical unordered factor with 8 levels.
#'
#' \item \code{yield} potato yield (t/ha).
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(potatoYield)
#'
#' @format a \code{data.frame} with 32 records and 3 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 76)
#'
#' @examples
#'
#' library(lattice)
#' data(potatoYield)
#'
#' plot(yield~variety, data=potatoYield,
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
#'
#' \item \code{block} a categorical unordered factor with 6 levels.
#'
#' \item \code{plow} a categorical unordered factor with 2 levels.
#'
#' \item \code{yield} corn yield (kg in 200 m\eqn{^2} of area).
#'
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
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 91)
#'
#' @examples
#'
#' library(lattice)
#' data(plowing)
#'
#' xyplot(yield~plow|block, data=plowing, type=c("p", "a"),
#'        ylab=expression(Yield~(t~ha^{-1})),
#'        xlab="Plowing level")
#'
NULL

#' @name defoliation
#'
#' @title Bolls in cotton as function of artificial defoliation
#'
#' @description This dataset contais the result of a real experiment to
#'     evaluate the effect of artificial defoliation in combination with
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
#'
#' \item \code{phenol} a categorical ordered factor with 5 levels that
#'     represent the phenological stages of the cotton plant in which
#'     defoliation was applied.
#'
#' \item \code{defol} a numeric factor with 5 levels that represents the
#'     artifical level of defoliation (percent in leaf area removed with
#'     scissors) applied for all leaves in the plant.
#'
#' \item \code{rept} index for each experimenal unit in each treatment
#'     cell.
#'
#' \item \code{bolls} the number of bolls produced (count variable)
#'     evaluated at harvest.
#'
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
#'
#' \item \code{block} a categorical unordered factor with 4 levels.
#'
#' \item \code{variety} a categorical unordered factor with 6 levels.
#'
#' \item \code{yield} cassava yield (t/ha).
#'
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
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 93)
#'
#' @examples
#'
#' library(lattice)
#' data(cassavaYield)
#'
#' plot(yield~variety, data=cassavaYield,
#'      groups=block, type="o",
#'      ylab=expression(Yield~(t~ha^{-1})),
#'      xlab="Variety")
#'
NULL

#' @name sugarcaneYield
#'
#' @title Sugarcane variety experiment
#'
#' @description These data are from an experiment done by The West São
#'     Paulo State Sugar Mills Cooperative. The experiment was done in a
#'     randomized complete block design with 4 blocks. Sugarcane yield
#'     (kg/plot) was recorded in each experimental unit.
#'
#' \itemize{
#'
#' \item \code{block} a categorical unordered factor with 4 levels.
#'
#' \item \code{variety} a categorical unordered factor with 7 levels.
#'
#' \item \code{yield} sugarcane yield (kg/plot).
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(sugarcaneYield)
#'
#' @format a \code{data.frame} with 28 records and 3 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 92)
#'
#' @examples
#'
#' library(lattice)
#' data(sugarcaneYield)
#'
#' plot(yield~variety, data=sugarcaneYield,
#'      groups=block, type="o",
#'      ylab=expression(Yield~(kg~plot^{-1})),
#'      xlab="Variety")
#'
NULL

#' @name sugarcaneYield2
#'
#' @title Sugarcane variety competition experiment
#'
#' @description These data are from an experiment done in a latin square
#'     design of size 5. Sugarcane yield (kg/plot) was recorded in each
#'     experimental unit.
#'
#' \itemize{
#'
#' \item \code{row} the rows of the latin square that controls in one
#'     dimention. A categorical unordered factor with 5 levels.
#'
#' \item \code{col} the columns of the latin square that controls in one
#'     dimention perpendicular to the previus. A categorical unordered
#'     factor with 5 levels.
#'
#' \item \code{variety} a categorical unordered factor with 5 levels.
#'
#' \item \code{yield} sugarcane yield (kg/plot).
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(sugarcaneYield2)
#'
#' @format a \code{data.frame} with 28 records and 3 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 96)
#'
#' @examples
#'
#' library(lattice)
#' library(latticeExtra)
#'
#' xyplot(yield~variety|col,  groups=row, data=sugarcaneYield2,
#'        ylab=expression(Yield~(kg~plot^{-1})),
#'        xlab="Variety")
#'
#' ## display.brewer.all()
#'
#' levelplot(yield~row+col,
#'           data=sugarcaneYield2, aspect="iso",
#'           xlab="Row", ylab="Column",
#'           main=expression(Yield~(kg~plot^{-1})),
#'           col.regions=colorRampPalette(
#'               colors=brewer.pal(n=11, name="Spectral")))+
#'     layer(with(sugarcaneYield2,
#'                panel.text(x=row, y=col,
#'                           label=paste(variety, yield))))
#'
#' aggregate(yield~row, data=sugarcaneYield2, FUN=mean)
#' aggregate(yield~col, data=sugarcaneYield2, FUN=mean)
#' aggregate(yield~variety, data=sugarcaneYield2, FUN=mean)
#'
NULL

#' @name sugarcaneYield3
#'
#' @title Sugarcane yield as function of fertilization strategy
#'
#' @description These data are from an experiment done in a latin square
#'     design of size 5. Sugarcane yield (kg/plot) was recorded in each
#'     experimental unit.
#'
#' \itemize{
#'
#' \item \code{row} the rows of the latin square that controls in one
#'     dimention. A categorical unordered factor with 6 levels.
#'
#' \item \code{col} the columns of the latin square that controls in one
#'     dimention perpendicular to the previus. A categorical unordered
#'     factor with 6 levels.
#'
#' \item \code{fertil} a categorical unordered factor with 6 levels that
#'     is the fertilization strategy applied. These levels are a result
#'     of treatment cells in a three incomplete factorial
#'     arrangrment. See detais for more information.
#'
#' \item \code{yield} sugarcane yield (kg/plot).
#'
#' }
#'
#' @details The levels of fertilization are in fact a combination of a
#'     \eqn{3^2} factorial experiment but not all cells are present, so
#'     this is a (intentional) incomplete three factorial
#'     experiment. The factors used were limestone (A: present, a:
#'     absent), \emph{Crotalaria juncae} (B: present, b: absent) and
#'     fertilizer (C: present, c: absent). Therefore, the level ABC
#'     means that all three factors are present. To access the effect of
#'     each factor and interactions can be applied contrasts.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(sugarcaneYield3)
#'
#' @format a \code{data.frame} with 28 records and 3 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 99)
#'
#' @examples
#'
#' library(lattice)
#' library(latticeExtra)
#'
#' xyplot(yield~fertil|col,  groups=row, data=sugarcaneYield3,
#'        ylab=expression(Yield~(kg~plot^{-1})),
#'        xlab="Fertilization", scales=list(x=list(rot=90)))
#'
#' ## display.brewer.all()
#'
#' levelplot(yield~row+col,
#'           data=sugarcaneYield3, aspect="iso",
#'           xlab="Row", ylab="Column",
#'           main=expression(Yield~(kg~plot^{-1})),
#'           col.regions=colorRampPalette(
#'               colors=brewer.pal(n=11, name="Spectral")))+
#'     layer(with(sugarcaneYield3,
#'                panel.text(x=row, y=col,
#'                           label=sprintf("%s\n%0.2f",
#'                                         fertil, yield))))
#'
#' aggregate(yield~row, data=sugarcaneYield3, FUN=mean)
#' aggregate(yield~col, data=sugarcaneYield3, FUN=mean)
#' aggregate(yield~fertil, data=sugarcaneYield3, FUN=mean)
#'
#' ## The incomplete factorial structure.
#' X <- mapply(FUN=grepl, c("A", "B", "C"),
#'             MoreArgs=list(x=sugarcaneYield3$fertil))*1
#' sugarcaneYield3 <- cbind(sugarcaneYield3, as.data.frame(X))
#'
#' ftable(with(sugarcaneYield3, tapply(yield, list(B, A, C), FUN=mean)))
#' aggregate(yield~A+B+C, data=sugarcaneYield3, FUN=mean)
#'
NULL

#' @name wgPigs2
#'
#' @title Age of castration in pig weight gain
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
#'
#' \item \code{litter} a categorical factor with 4 levels that
#'     represents the rows of the lattin square design and control for
#'     the differences among litters.
#'
#' \item code{size} a categorical ordered variable that represents the
#'     columns of latin square desing and control for the weight of the
#'     animals at the beggining of the experiment.
#'
#' \item \code{age} age of the animal (days) when castration was
#'     done. \code{controls} are the animals without castration.
#'
#' \item \code{wg} weight gain (kg) after 252 days.
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(wgPigs2)
#'
#' @format a \code{data.frame} with 16 records and 4 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 110)
#'
#' @examples
#'
#' library(lattice)
#'
#' data(wgPigs2)
#' str(wgPigs2)
#'
#' xyplot(wg~age, data=wgPigs2, groups=litter,
#'        ylab="Weight gain (kg)",
#'        xlab="Age at castration (days)")
#'
NULL

#' @name cornYield
#'
#' @title Corn yield as function of fertilization with NPK
#'
#' @description These data are from an \eqn{2^3} factorial experiment
#'     studing the effect of Nitrogen (N), Phosporus (P) and Potassium
#'     (K) on corn yield in a randomized block design.
#'
#' \itemize{
#'
#' \item \code{block} a factor with 4 levels.
#'
#' \item \code{N} low (-1) and high (+1) levels of nitrogen.
#'
#' \item \code{P} low (-1) and high (+1) levels of phosporus.
#'
#' \item \code{K} low (-1) and high (+1) levels of potassium.
#'
#' \item \code{yield} corn yield (ton/ha).
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(cornYield)
#'
#' @format a \code{data.frame} with 32 records and 4 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 115)
#'
#' @examples
#'
#' library(lattice)
#' library(latticeExtra)
#'
#' data(cornYield)
#' str(cornYield)
#'
#' xyplot(yield~N|P, groups=K,
#'        data=cornYield, type=c("p", "a"),
#'        ylab=expression(Yield~(ton~ha^{-1})),
#'        xlab="Nutrient level")
#'
#' xyplot(yield~N, groups=interaction(P, K),
#'        data=cornYield, type=c("p", "a"),
#'        auto.key=list(columns=2),
#'        ylab=expression(Yield~(ton~ha^{-1})),
#'        xlab="Nutrient level")
#'
NULL

#' @name vinasseFert
#'
#' @title Fertilization with vinasse and mineral
#'
#' @description These data are from an \eqn{2^2} factorial experiment
#'     studing the effect of fertilizaton with vinasse (a residual from
#'     industrial processing of sugar cane) and complete mineral
#'     fertilization.
#'
#' \itemize{
#'
#' \item \code{block} a factor with 4 levels.
#'
#' \item \code{mineral} low (-1) and high (+1) levels of mineral
#'     fertilization.
#'
#' \item \code{vinasse} low (-1) and high (+1) levels of fetilization
#'    with vinasse.
#'
#' \item \code{y} some response variable. The text book doesn't give any
#'     information.
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(vinasseFert)
#'
#' @format a \code{data.frame} with 16 records and 4 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 119)
#'
#' @examples
#'
#' library(lattice)
#' library(latticeExtra)
#'
#' data(vinasseFert)
#' str(vinasseFert)
#'
#' xyplot(y~vinasse, groups=mineral,
#'        auto.key=list(title="Mineral", columns=2),
#'        data=vinasseFert, type=c("p", "a"),
#'        ylab="y",
#'        xlab="Vinasse level")
#'
NULL

#' @name filterCake
#'
#' @title Fertilization with filter cake and mineral
#'
#' @description These data are from an \eqn{2^2} factorial experiment
#'     studing the effect of fertilizaton with filter cake (a residual
#'     from industrial processing of sugar cane) and traditional mineral
#'     fertilization.
#'
#' \itemize{
#'
#' \item \code{block} a factor with 4 levels.
#'
#' \item \code{mineral} low (-1) and high (+1) levels of mineral
#'     fertilization.
#'
#' \item \code{cake} low (-1) and high (+1) levels of fetilization with
#'     filter cake.
#'
#' \item \code{y} some response variable. The text book doesn't give any
#'     information.
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(filterCake)
#'
#' @format a \code{data.frame} with 16 records and 4 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 120)
#'
#' @examples
#'
#' library(lattice)
#' library(latticeExtra)
#'
#' data(filterCake)
#' str(filterCake)
#'
#' xyplot(y~cake, groups=mineral,
#'        auto.key=list(title="Mineral", columns=2),
#'        data=filterCake, type=c("p", "a"),
#'        ylab="y",
#'        xlab="Filter cake level")
#'
#'
NULL

#' @name sugarcaneYield4
#'
#' @title Triple factorial NPK fertilization on sugar cane yield
#'
#' @description These data are from an \eqn{3^3} factorial experiment
#'     studing the effect of NPK on the yield of sugar cane.
#'
#' \itemize{
#'
#'   \item \code{block} a local control factor with 3 levels.
#'
#'   \item \code{rept} factor with 2 levels.
#'
#'   \item \code{N} integer coded nitrogen levels (0, 1, 2).
#'
#'   \item \code{P} integer coded phosphorus levels (0, 1, 2).
#'
#'   \item \code{K} integer coded potassium levels (0, 1, 2).
#'
#'   \item \code{yield} sugar cane yield (ton/ha).
#'
#' }
#'
#' @details There is a missprint in the book for the 9th entry, which
#'     has yield 59.0, that is coded as 202 istead of 220.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(sugarcaneYield4)
#'
#' @format a \code{data.frame} with 54 records and 6 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 126)
#'
#' Straus, E. (1951). Experimentos de adubação na zona canavieira de
#'     Pernambuco. In: Anais da III Reunião Brasileira de Ciência do
#'     Solo. 1:336-443.
#'
#' @examples
#'
#' library(lattice)
#' library(latticeExtra)
#'
#' data(sugarcaneYield4)
#' str(sugarcaneYield4)
#'
#' xyplot(yield~N|P, groups=K,
#'        auto.key=list(title="Potassim level", columns=3),
#'        strip=strip.custom(var.name="Phosphorus", strip.names=TRUE,
#'                           strip.levels=TRUE, sep=": "),
#'        data=sugarcaneYield4, type=c("p", "a"),
#'        ylab=expression(Yield~(ton~ha^{-1})),
#'        xlab="Nitrogen level level")
#'
NULL

#' @name mangoAcidity
#'
#' @title Acidity of mango fruits by varieties, years and months
#'
#' @description These data are from an observational study along 3 years
#'     where acidity in fruits of 6 varieties of mango was determined in
#'     Novermber, December and January.
#'
#' \itemize{
#'
#' \item \code{variety} a categorical variable with 6 levels that
#'     represents mango varieties studied.
#'
#' \item \code{year} the year of harvesting.
#'
#' \item \code{month} the month of harvesting.
#'
#' \item \code{acid} mean of the acidity determined in 3 fruits.
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(mangoAcidity)
#'
#' @format a \code{data.frame} with 54 records and 6 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 132)
#'
#' Simão, S. (1960). Estudo da planta e dos frutos da mangueira
#'     (\emph{Manginifera indica} L.). Piracicaba, 1960. Thesis.
#'
#' @examples
#'
#' library(lattice)
#'
#' data(mangoAcidity)
#' str(mangoAcidity)
#'
#' ## reshape::cast() can also be used.
#' with(mangoAcidity,
#'      ftable(tapply(acid,
#'                    list(variety, year, month),
#'                    FUN=identity)))
#'
#' xyplot(acid~month|variety, groups=year,
#'        data=mangoAcidity, type=c("p", "a"),
#'        auto.key=TRUE,
#'        ylab="Acidity",
#'        xlab="Month")
#'
NULL

#' @name cornYield2
#'
#' @title Axial factorial NPK experiment with added treatments
#'
#' @description These data are from an axial 3 factorial experiment
#'     studing NPK in the yield of corn. Tow controls were added, one is
#'     zer control (no NPK) and the other is central factorial point
#'     plus presence of limestone.
#'
#' \itemize{
#'
#' \item \code{N} content of nitrogen in the fertilizer.
#'
#' \item \code{P} content of phosphorus in the fertilizer.
#'
#' \item \code{K} content of potassium in the fertilizer.
#'
#' \item \code{limestone} presence (1) or absence of limestone (0).
#'
#' \item \code{acid} mean of corn yield in 16 locations (ton/ha).
#'
#' }
#'
#' @details The experiment was carried out in 16 different locations but
#'     only the mean by cell combinations were available in the text
#'     book.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(cornYield2)
#'
#' @format a \code{data.frame} with 9 records and 5 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 132)
#'
#' Simão, S. (1960). Estudo da planta e dos frutos da mangueira
#'     (\emph{Manginifera indica} L.). Piracicaba, 1960. Thesis.
#'
#' @examples
#'
#' library(lattice)
#' library(latticeExtra)
#'
#' data(cornYield2)
#' str(cornYield2)
#'
#' ## Axial triple factorial with 2 controls.
#' ftable(xtabs(~N+P+K, data=cornYield2))
#'
#' xyplot(yield~N+P+K,
#'        groups=as.integer(limestone==1 | (N+P+K)==0),
#'        data=cornYield2, type=c("p", "a"),
#'        auto.key=TRUE,
#'        ylab=expression(Yield~(ton~ha^{-1})),
#'        xlab="Nutrient content")
#'
NULL

#' @name coffeeFert
#'
#' @title Number of dry branches in coffee trees as function of NPK
#'
#' @description These data are from a \eqn{2^3} factorial experiment
#'     studing the effect of NPK fertilizaton on the number of dry
#'     branches in coffee trees.
#'
#' \itemize{
#'
#' \item \code{N} content of nitrogen in the fertilizer (low/high).
#'
#' \item \code{P} content of phosphorus in the fertilizer (low/high).
#'
#' \item \code{K} content of potassium in the fertilizer (low/high).
#'
#' \item \code{block} an unordered factor representing the blocks
#'     used.
#'
#' \item \code{branches} an integer variable, the number of dry
#'     branches in a coffee the.
#'
#' }
#'
#' @details The experiment was carried out in a randomized block design
#'     with 6 blocks. In the book, the data is presented at squared root
#'     scale.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(coffeeFert)
#'
#' @format a \code{data.frame} with 48 records and 5 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 137)
#'
#' Malavolta, E.; Pimentel Gomes, F.; Coury, T. (1958). Estudos sobre a
#'     alimentação mineral do cafeeiro (\emph{Coffea arabica} L.,
#'     variedade Bourbon Vermelho). Piracicaba.
#'
#' @examples
#'
#' library(lattice)
#' library(latticeExtra)
#'
#' data(coffeeFert)
#' str(coffeeFert)
#'
#' xyplot(branches~N|P, groups=K,
#'        data=coffeeFert, type=c("p", "a"),
#'        ylab=expression(Branches~(plant^{-1})),
#'        xlab="Nutrient level")
#'
NULL

#' @name cottonFert
#'
#' @title A set of experiments in different locations studing NK on
#'     cotton
#'
#' @description These data is a set of experiments carried out in
#'     different locations studing NK fertilization in cotton. All the 5
#'     experiments are a complete randomized design with 4 replications
#'     and 5 levels of fertilization based on N and K levels and a
#'     control.
#'
#' \itemize{
#'
#' \item \code{trt} unordered factor, treatment that consist of 4 cells
#'     from a 2^2 factorial design (\eqn{N\times K}) and a control.
#'
#' \item \code{rept} integer, indexes experimental units.
#'
#' \item \code{loc} an unordered factor representing the locations where
#'     the experiment was carried out.
#'
#' \item \code{y} numeric, the response variable of the experiment. The
#'     text book didn't give details.
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(cottonFert)
#'
#' @format a \code{data.frame} with 100 records and 4 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 142)
#'
#' @examples
#'
#' library(lattice)
#'
#' data(cottonFert)
#' str(cottonFert)
#'
#' xyplot(y~trt|loc,
#'        data=cottonFert, type=c("p", "a"),
#'        ylab="y", xlab="Treatment")
#'
#' xyplot(log(y)~trt|loc,
#'        data=cottonFert, type=c("p", "a"),
#'        ylab="y", xlab="Treatment")
#'
NULL

#' @name potatoYield2
#'
#' @title Potato variety competition experiments in several locations
#'
#' @description These data are from a set of experiments done by the
#'     engineer Oscar A. Garay at Balcare, Argentina. These experiments
#'     were done in a randomized complete block design with 4 blocks and
#'     at 7 locations on the potato production region at the Buenos
#'     Aires province.
#'
#' \itemize{
#'
#' \item \code{variety} a categorical unordered factor with 8 levels,
#'     varieties of potato.
#'
#' \item \code{loc} a categorical unordered factor with 7 levels, the
#'     locations that represent farms or experimental stations.
#'
#' \item \code{sumYield} is the sum of yield for a variety in each
#'     experiment. Then, this sum values across 4 blocks in each
#'     experiment. To get the mean yield you should divide by 4. Yield
#'     is t/ha.
#'
#' }
#'
#' @details The data in the book was not complete because doesn't report
#'     individual plot values but, instead, the sum for a variety in
#'     each experiment. To do a joint or global analysis, with all
#'     locations, varieties and blocks, its necessary all individual
#'     plot values. The book report the Mean Square Error estimates for
#'     each experiment as an attribute of the object,
#'     \code{attr(potatoYield2, "MSE")} and they comes from the ANOVA
#'     table in which the model is \code{~block+variety} for each
#'     location. The data set \link[legTools]{potatoYield} correspond
#'     the location 3. With these MSE is possible use them in a such a
#'     way that a partial ANOVA table can be obtained to test the effect
#'     of location, variety and its interaction.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(potatoYield2)
#'
#' @format a \code{data.frame} with 56 records and 3 variables. There is
#'     an attribute named \code{MSE}, a named vector containing the Mean
#'     Squares Errors estimates for each experiment.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 147)
#'
#' @examples
#'
#' require(lattice)
#'
#' data(potatoYield2)
#' str(potatoYield2)
#'
#' xyplot(sumYield/4~variety, data=potatoYield2,
#'        groups=loc, type="o",
#'        ylab=expression(Yield~(t~ha^{-1})),
#'        xlab="Variety")
#'
NULL

#' @name castorbeansYield
#'
#' @title Castor beans variety competition experiments in some locations
#'
#' @description These data are from a set of experiments evaluating
#'     varieties of castor beans in terms of yield (kg/ha) for some
#'     locations (counties).
#'
#' \itemize{
#'
#' \item \code{variety} a categorical unordered factor with 8 levels,
#'     varieties and lines of castor beans.
#'
#' \item \code{loc} a categorical unordered factor with 5 levels, the
#'     locations (counties) experimental stations.
#'
#' \item \code{meanYield} is the mean of yield for a variety in each
#'     location. So, this the mean across all plots of the same variety
#'     in each experiment.
#'
#' }
#'
#' @details The data in the book was not complete because doesn't report
#'     individual plot values but the mean for a variety in each single
#'     experiment. Neither mention which experimental design was used in
#'     each station. The book report the Mean Square Error estimates for
#'     each experiment. These values as provided as an attribute of the
#'     object, \code{attr(peanut, "MSE")} and they comes from the ANOVA
#'     table corresponding to an appropriate model for each
#'     location. With these MSE is possible use them in a such a way
#'     that a partial ANOVA table can be obtained to test the effect of
#'     location, variety and its interaction.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(castorbeansYield)
#'
#' @format a \code{data.frame} with 45 records and 3 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 149)
#'
#' Souza, O. Ferreira de.; Canecchio, F. V. (1952). Melhoramento de
#'     mamoeira, VII. Bragantia 12:301-307.
#'
#' @examples
#'
#' require(lattice)
#'
#' data(castorbeansYield)
#' str(castorbeansYield)
#'
#' xyplot(meanYield~variety, data=castorbeansYield,
#'        groups=loc, type="o",
#'        ylab=expression(Yield~(t~ha^{-1})),
#'        xlab="Variety")
#'
NULL

#' @name peanutYield
#'
#' @title Peanut variety competition experiments in some locations and
#'     years
#'
#' @description These data are from a set of experiments evaluating
#'     varieties of peanut in terms of yield (kg/ha) for some locations
#'     and years.
#'
#' \itemize{
#'
#' \item \code{variety} a categorical unordered factor with 4 levels,
#'     peanut varieties.
#'
#' \item \code{loc} a categorical unordered factor with 3 levels, the
#'     locations (counties) of the experimental stations.
#'
#' \item \code{year} a categorical factor, the crop year.
#'
#' \item \code{meanYield} is the adjusted mean of yield for a variety in
#'     each location and year.
#'
#' }
#'
#' @details The data in the book was not complete because doesn't report
#'     individual plot values but the adjusted mean for a variety in
#'     each single experiment. Neither mention which experimental design
#'     was used in each station. The book report the Mean Square Error
#'     estimates for each experiment. These values as provided as an
#'     attribute of the object, \code{attr(peanut, "MSE")} and they
#'     comes from the ANOVA table corresponding to an appropriate model
#'     for each location. With these MSE is possible use them in a such
#'     a way that a partial ANOVA table can be obtained to test the
#'     effect of location, variety and its interaction.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(peanutYield)
#'
#' @format a \code{data.frame} with 36 records and 4 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 150)
#'
#' Souza, O. Ferreira de.; Abramides, Eduardo. (1952). Ensaios de
#'     variedades de amendoim. Bragantia 12:349-358.
#'
#' @examples
#'
#' require(lattice)
#'
#' data(peanutYield)
#' str(peanutYield)
#'
#' xyplot(meanYield~variety|year, data=peanutYield,
#'        groups=loc, type="o",
#'        ylab=expression(Yield~(t~ha^{-1})),
#'        xlab="Variety")
#'
NULL

#' @name peanutYield2
#'
#' @title Peanut variety competition experiments in some locations
#'
#' @description These data are from a set of experiments evaluating
#'     varieties of peanut in terms of yield (kg/ha) for some locations
#'     in different years.
#'
#' \itemize{
#'
#' \item \code{variety} a categorical unordered factor with 4 levels,
#'     peanut varieties.
#'
#' \item \code{loc} a categorical unordered factor with 4 levels, the
#'     location:year of the experiment.
#'
#' \item \code{meanYield} is mean of yield for a variety in each
#'     location:year.
#'
#' }
#'
#' @details The data in the book was not complete because doesn't report
#'     individual plot values but the adjusted mean for a variety in
#'     each single experiment. Neither mention which experimental design
#'     was used in each station. The book report the Mean Square Error
#'     estimates for each experiment. These values as provided as an
#'     attribute of the object, \code{attr(peanut, "MSE")} and they
#'     comes from the ANOVA table corresponding to an appropriate model
#'     for each location. With these MSE is possible use them in a such
#'     a way that a partial ANOVA table can be obtained to test the
#'     effect of location, variety and its interaction.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(peanutYield2)
#'
#' @format a \code{data.frame} with 16 records and 3 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 150)
#'
#' Souza, O. Ferreira de.; Abramides, Eduardo. (1952). Ensaios de
#'     variedades de amendoim. Bragantia 12:349-358.
#'
#' @examples
#'
#' require(lattice)
#'
#' data(peanutYield2)
#' str(peanutYield2)
#'
#' xyplot(meanYield~variety, data=peanutYield2,
#'        groups=loc, type="o",
#'        ylab=expression(Yield~(t~ha^{-1})),
#'        xlab="Variety")
#'
NULL

#' @name bib3
#'
#' @title A balanced incomplete block design of type III
#'
#' @description This data is under a balanced complete block design
#'     named type III. There are 5 treatments and 10 blocks of size 3
#'     plots. Each treatment is repeated 6 times and they occour
#'     together (in pairs) 3 times.
#'
#' \itemize{
#'
#' \item \code{block} a categorical unordered factor with 10 levels.
#'
#' \item \code{treat} a categorical unordered factor with 5 levels, the
#'     treatments studied.
#'
#' \item \code{y} some response variable. The book doesn't gave details.
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(bib3)
#'
#' @format a \code{data.frame} with 30 records and 3 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 185)
#'
#' @examples
#'
#' require(lattice)
#'
#' data(bib3)
#' str(bib3)
#'
#' xyplot(y~treat|block, data=bib3,
#'        ylab="Y",
#'        xlab="Treatment")
#'
#' g <- nlevels(bib3$treat)
#' a <- seq(0, by=(2*pi)/(g), length.out=g)
#' y <- sin(a)
#' x <- cos(a)
#' plot(y~x, asp=1, xlim=c(-1,1), ylim=c(-1,1))
#'
#' for (b in levels(bib3$block)){
#'     cbn <- combn(x=as.integer(bib3$treat[bib3$block==b]),
#'                  m=2)
#'     segments(
#'         x0=x[cbn[1,]], y0=y[cbn[1,]],
#'         x1=x[cbn[2,]], y1=y[cbn[2,]], col=b)
#' }
#'
NULL

#' @name bib1
#'
#' @title A balanced incomplete block design of type I
#'
#' @description This data is under a balanced complete block design
#'     named type I. There are 7 replications that are groups of 4
#'     blocks of size 2 in a such a way that each treatment occurs once
#'     in each replication and 7 times at all. There are 8 treatments,
#'     28 blocks at all. Treatment occur in pairs once. These treatments
#'     are in fact cells of a \eqn{2^3} factorial design from combining
#'     nitrogen (P), phosphorus (P) and potassium (K) fertilizers at two
#'     levels each.
#'
#' \itemize{
#'
#' \item \code{rept} a categorical unordered factor with 7 levels. Each
#'     \code{rept} has 4 bloks of size 2.
#'
#' \item \code{N} content of nitrogen in the fertilizer (low/high).
#'
#' \item \code{P} content of phosphorus in the fertilizer (low/high).
#'
#' \item \code{K} content of potassium in the fertilizer (low/high).
#'
#' \item \code{block} a categorical unordered factor with 4 levels in
#'     each \code{rept}, so 28 at all.
#'
#' \item \code{treat} a categorical unordered factor with 8 levels, the
#'     treatments studied.
#'
#' \item \code{y} some response variable. The book doesn't gave details.
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(bib1)
#'
#' @format a \code{data.frame} with 56 records and 4 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 190)
#'
#' @examples
#'
#' require(lattice)
#'
#' data(bib1)
#' str(bib1)
#'
#' xyplot(y~treat|rept, groups=block, data=bib1, type="b",
#'        ylab="Y", xlab="Treatment")
#'
#' xyplot(y~treat, data=bib1, jitter.x=TRUE,
#'        ylab="Y", xlab="Treatment")
#'
#' xyplot(y~N|P+K, groups=rept, data=bib1, type="b",
#'        ylab="Y", xlab="Nitrogen")
#'
NULL

#' @name bib2
#'
#' @title A balanced incomplete block design of type II
#'
#' @description This data is under a balanced complete block design
#'     named type II. There are 3 replications that are groups of 7
#'     blocks of size 2 in a such a way that each treatment occurs twice
#'     in each replication and 6 times at all. There are 7 treatments
#'     and 21 blocks at all. Treatments occur in pairs once.
#'
#' \itemize{
#'
#' \item \code{rept} a categorical unordered factor with 3 levels. Each
#'     \code{rept} has 7 bloks of size 2.
#'
#' \item \code{block} a categorical unordered factor with 8 levels in
#'     each \code{rept}, so 21 at all.
#'
#' \item \code{treat} a categorical unordered factor with 7 levels, the
#'     treatments studied.
#'
#' \item \code{y} some response variable. The book doesn't gave details.
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(bib2)
#'
#' @format a \code{data.frame} with 42 records and 4 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 192)
#'
#' @examples
#'
#' require(lattice)
#'
#' data(bib2)
#' str(bib2)
#'
#' xyplot(y~treat|rept, groups=block, data=bib2, type="b",
#'        ylab="Y", xlab="Treatment")
#'
#' xyplot(y~treat, data=bib2, jitter.x=TRUE,
#'        ylab="Y", xlab="Treatment")
#'
NULL

#' @name bib3asin
#'
#' @title A balanced incomplete block design of type III
#'
#' @description This data is under a balanced complete block design
#'     named type III. There are 13 treatments and 13 blocks of size 4
#'     plots. Each treatment is repeated 6 times and they occour
#'     together (in pairs) only once.
#'
#' \itemize{
#'
#' \item \code{block} a categorical unordered factor with 13 levels.
#'
#' \item \code{treat} a categorical unordered factor with 13 levels, the
#'     treatments studied.
#'
#' \item \code{z} correpond a transformation of the original recorded
#'     variable, the observed percentual of healthy plants, \eqn{p}. So,
#'     \eqn{z = \arcsin{\sqrt{p/100}}}, in radians is applied to
#'     stabilize the variance to comply with the assumption of constant
#'     variance.
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(bib3asin)
#'
#' @format a \code{data.frame} with 52 records and 3 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 198)
#'
#' Fraga Jr., C. G.; Costa, A. S. (1950). Análise de um experimento para
#'     combate de vira-cabeça do tomateiro. Bragantia, 10:305-316.
#'
#' @examples
#'
#' require(lattice)
#'
#' data(bib3asin)
#' str(bib3asin)
#'
#' xyplot(z~treat|block, data=bib3asin,
#'        ylab="Arc sin of heathy plants fraction",
#'        xlab="Treatment")
#'
#' ## Why not consider a beta distribution for p?
#' bib3asin$p <- sin(bib3asin$z*pi/180)^2
#'
#' xyplot(p~treat|block, data=bib3asin,
#'        ylab="Fraction of healthy plants",
#'        xlab="Treatment")
#'
#' g <- nlevels(bib3asin$treat)
#' a <- seq(0, by=(2*pi)/(g), length.out=g)
#' y <- sin(a)
#' x <- cos(a)
#' plot(y~x, asp=1, xlim=c(-1,1), ylim=c(-1,1))
#'
#' for (b in levels(bib3asin$block)){
#'     cbn <- combn(x=as.integer(bib3asin$treat[bib3asin$block==b]),
#'                  m=2)
#'     segments(
#'         x0=x[cbn[1,]], y0=y[cbn[1,]],
#'         x1=x[cbn[2,]], y1=y[cbn[2,]], col=b)
#' }
#'
NULL

#' @name cornYield3
#'
#' @title An triple lattice design in corn yield
#'
#' @description This is a experiment evaluating the yield (kg/plot) for
#'     16 corn hybrids in a triple lattice design.
#'
#' \itemize{
#'
#' \item \code{rept} a categorical unordered factor with 3 levels.
#'
#' \item \code{block} a categorical unordered factor with 4 levels in
#'     each \code{rept}, so 12 at all. Each block has size 4.
#'
#' \item \code{hybrid} a unordered factor with 16 levels, the corn
#'     hybrids studied.
#'
#' \item \code{yield} the (continuous) response variable of the
#'     experiment in kg/plot.
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(cornYield3)
#'
#' @format a \code{data.frame} with 48 records and 4 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 215)
#'
#' @examples
#'
#' require(lattice)
#'
#' data(cornYield3)
#' str(cornYield3)
#'
#' xyplot(yield~hybrid|rept, groups=block,
#'        data=cornYield3, type="b",
#'        ylab=expression(Yield~(kg~plot^{-1})),
#'        xlab="Hybrid")
#'
#' xyplot(yield~hybrid, data=cornYield3,
#'        jitter.x=TRUE, type=c("p", "a"),
#'        ylab=expression(Yield~(kg~plot^{-1})),
#'        xlab="Hybrid")
#'
#' g <- nlevels(cornYield3$hybrid)
#' a <- seq(0, by=(2*pi)/(g), length.out=g)
#' y <- sin(a)
#' x <- cos(a)
#' plot(y~x, asp=1, xlim=c(-1,1), ylim=c(-1,1))
#'
#' for (b in levels(cornYield3$block)){
#'     cbn <- combn(x=as.integer(cornYield3$hybrid[cornYield3$block==b]),
#'                  m=2)
#'     segments(
#'         x0=x[cbn[1,]], y0=y[cbn[1,]],
#'         x1=x[cbn[2,]], y1=y[cbn[2,]], col=b)
#' }
#'
NULL

#' @name sugarcaneYield5
#'
#' @title A set of 38 essays on the effect of P in sugar cane yield
#'
#' @description Sugar cane yield evaluated in a set of 38
#'     experiments. In each one was a triple factorial of NPK
#'     dosage. These data correspond to the mean values by level of P
#'     and the mean squares of each complete analysis.
#'
#' \itemize{
#'
#' \item \code{essay} an integer that represent each single experiment.
#'
#' \item \code{sugarcane} indicates which harvest the yield corresponts
#'     to. \code{"plant"} represents the first harvest of the crop and
#'     \code{"ratoon"} represents the second harvest.
#'
#' \item \code{P} phosphorus levels in kg/ha of \eqn{\textrm{P}_2
#' \textrm{O}_5}.
#'
#' \item \code{yield} sugar cane yield (ton/ha).
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(sugarcaneYield5)
#'
#' @format a \code{data.frame} with 228 records and 4 variables. An
#'     attribute \code{MSE} is a \code{data.frame} (76 by 3) with mean
#'     square error estimate from the analysis of the complete data in
#'     each experiment.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 259)
#'
#' Straus, E. (1951). Experimentos de adubação na zona canavieira de
#'     Pernambuco. In: Anais da III Reunião Brasileira de Ciência do
#'     Solo. 1:336-443.
#'
#' @examples
#'
#' library(lattice)
#'
#' data(sugarcaneYield5)
#' str(sugarcaneYield5)
#'
#' xyplot(yield~P|sugarcane, groups=essay,
#'        data=sugarcaneYield5, type="o",
#'        ylab=expression(Yield~(ton~ha^{-1})),
#'        xlab=expression(P[2]*O[5]~(kg~ha^{-1})))
#'
#' xyplot(yield~P|essay, groups=sugarcane,
#'        data=sugarcaneYield5, auto.key=TRUE,
#'        type="o", strip=FALSE,
#'        ylab=expression(Yield~(ton~ha^{-1})),
#'        xlab=expression(P[2]*O[5]~(kg~ha^{-1})))
#'
NULL

#' @name wgChikens
#'
#' @title Weight gain in chickens as function of sorghum in the feed
#'
#' @description This experiment considered four levels of sorghum in the
#'     feed and chickens of both sex, so it is a \eqn{4 x 2} factorial
#'     design and was performed in a randomized complete design with two
#'     replications. Each experimental unit is a set of 13 animals in
#'     which the total weight is recorded (decagrams). At the end of the
#'     experiment, 4 week later, some animals died resulting in some
#'     unbalancing.
#'
#' \itemize{
#'
#' \item \code{gender} a factor with two levels represengin the gerder
#'     of the chickens.
#'
#' \item \code{sorghum} a numeric variable at 4 levels that is the
#'     concentration, in percentage, of sorghum in the feed.
#'
#' \item \code{n} the number of animals in each experital unit. All
#'     experimental units had initiated with 13 animals but some of them
#'     died remaning 12 animals for some experimental units.
#'
#' \item \code{tw} is the total wieght of each experimental unit in
#'     decagrams.
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(wgChickens)
#'
#' @format a \code{data.frame} with 16 records and 4 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 267)
#'
#' Torres A. P.; Pimentel Gomes, F. (1958). Substituição de subprodutos
#'     de trigo pelo sorgo moído na alimentação de pintos. Escola
#'     Superior de Agricultura "Luiz de Queiroz". Anais
#'     E.S.A. "Luiz de Queiroz" 16: 251-276.
#'
#' @examples
#'
#' library(lattice)
#' 
#' data(wgChickens)
#' str(wgChickens)
#' 
#' xyplot(tw/n~sorghum, groups=gender,
#'        data=wgChickens, type=c("p", "a"),
#'        auto.key=list(
#'            columns=2, corner=c(0.95, 0.95), title="Gender"),
#'        ylab="Mean weight gain (kg)",
#'        xlab="Sorghum concentration in the feed (%)")
#'
NULL

#' @name cowmilkYield
#'
#' @title Milk yield as function of casein for nutrition of dairy cattle
#'
#' @description Data from a randomized block design experiment to study
#'     the milk yield in dairy cattle according to levels of casein. The
#'     experimental units are cows and the milk yield is the sum of
#'     daily volume over 8 weeks. The blocks keeps together cows with
#'     similar yield, so block 1 have the most productive cows and the
#'     block 3 the less productive ones.
#'
#' \itemize{
#'
#' \item \code{casein} a numeric variable that is the content of casein
#'     per day consumed for each cow.
#'
#' \item \code{block} a factor with three levels.
#'
#' \item \code{yield} total milk yield over 8 weeks (kg).
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(cowmilkYield)
#'
#' @format a \code{data.frame} with 12 records and 3 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 269)
#'
#' @examples
#'
#' library(lattice)
#' 
#' data(cowmilkYield)
#' str(cowmilkYield)
#' 
#' xyplot(yield~casein, groups=block,
#'        data=cowmilkYield, type="o",
#'        ylab=expression(Milk~yield~(kg)),
#'        xlab=expression(Casein~(g~day^{-1})))
#'
NULL

#' @name cowmilkYield2
#'
#' @title Milk yield in a change over blocked latin square design
#'
#' @description These are data adapted from Cochran et al. (1941). It
#'     consists of a experiment to evaluate three levels of feed type in
#'     the yield dairy cattle. Twelve cows were grouped in 4
#'     blocks. Each block, then, had 3 cows that received each feed type
#'     in a different period (three periods). Each block is a 3 size
#'     latin square where cows are lines, periods are columns and feed
#'     type is the treatment.
#'
#' \itemize{
#'
#' \item \code{groups} a nominal categorical factor with 4 levels.
#'
#' \item \code{period} an ordered categorical factor with 3 levels.
#'
#' \item \code{cow} a nominal variable that represent each cow in each
#'     group.
#'
#' \item \code{feed} a nominal categorical factor that is the feed type
#'     consumed by the cows.
#'
#' \item \code{yield} the milk yield.
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(cowmilkYield2)
#'
#' @format a \code{data.frame} with 36 records and 5 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 272)
#'
#' @examples
#'
#' library(lattice)
#' 
#' data(cowmilkYield2)
#' str(cowmilkYield2)
#' 
#' xyplot(yield~feed|group, groups=period,
#'        data=cowmilkYield2, type=c("p", "a"),
#'        ylab=expression(Milk~yield~(kg)),
#'        xlab="Feed")
#' 
NULL

#' @name cowmilkYield3
#'
#' @title Milk yield in a switch back experiment
#'
#' @description Data adapted from Lucas (1956). Were studied the effect
#'     of three treatments on the milk yield of dairy cattle. Twelve
#'     cows were the experimental units grouped in three blocks of
#'     different size. Each cow received two treatments, one in the
#'     first and third periods and the second at the secont period. This
#'     is a switch back design.
#'
#' \itemize{
#'
#' \item \code{period} an ordered categorical factor with 3 levels.
#'
#' \item \code{block} an nominal factor with 3 levels.
#'
#' \item \code{cow} a nominal variable that represent each cow of the
#'     experiment.
#'
#' \item \code{treat} a nominal categorical factor with 3 levels.
#'
#' \item \code{yield} the milk yield.
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(cowmilkYield3)
#'
#' @format a \code{data.frame} with 36 records and 5 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 278)
#'
#' Lucas, H. L. (1956). Switch-back trials for more than two
#'     treatments. Journal of Dairy Sciences, 39:146-154.
#'
#' @examples
#'
#' library(lattice)
#' 
#' data(cowmilkYield3)
#' str(cowmilkYield3)
#' 
#' xyplot(yield~period|cow, groups=treat,
#'        data=cowmilkYield3, type="o",
#'        ylab="Milk yield",
#'        xlab="Period")
#' 
NULL

#' @name wgCattle
#'
#' @title Grazing intensity and \emph{Brachiaria} species in the weight
#'     o cattle.
#'
#' @description Data adapted from Pimentel Gomes (1988). The experiment
#'     is a 2 grazing intensity x 3 species of \emph{Brachiaria}
#'     factorial. The experimental units were 48 animal grouped in 8
#'     groups of 6 animals. The 8 groups were splited in two blocks. The
#'     response variable recorded were the weight of the animals.
#'
#' \itemize{
#'
#' \item \code{block} an categorical factor with 2 levels.
#'
#' \item \code{group} an categorial factor with 4 levels into each
#'     block.
#'
#' \item \code{specie} a categorical factor that is species of
#'     \emph{Brachiaria} for pasture.
#'
#' \item \code{inten} a numeric factor with 2 levels that is the number
#'     of grazing animal per unit area.
#'
#' \item \code{weight} the recorded weight.
#'
#' }
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(wgCattle)
#'
#' @format a \code{data.frame} with 48 records and 5 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 278)
#'
#' Pimentel Gomes, F.; Nunes, S. G.; Gomes, S. de B.; Curvo,
#'     J. B. E. (1988). Modificação da análise de variância de ensaios
#'     de pastejo com bovinos, considerando os blocos de
#'     animais. Pesquisa Agrpecuária Brasileira 23(9):251-276.
#'
#' @examples
#'
#' library(lattice)
#' 
#' data(wgCattle)
#' str(wgCattle)
#' 
#' xyplot(weight~specie|group, groups=inten,
#'        data=wgCattle, type=c("p", "a"),
#'        auto.key=list(columns=2, title="Intensity (un/ha)"),
#'        scales=list(x=list(font=3)),
#'        ylab="Weight (kg)",
#'        xlab=expression(Species~of~italic("Brachiaria")))
#'  
NULL

