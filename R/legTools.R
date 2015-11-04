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
#'     Duvilio Ometto, to study the effect of plowing level on corn
#'     yield. It was used 2 levels of plowing: normal (or superficial)
#'     and deep. The experiment was done in a randomized complete block
#'     design with 6 blocks. Corn yield (t/ha) was recorded in each
#'     experimental unit, but in this experiment there was 2 
#'     experimental units for each factor level in each block.
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
#' @title Bolls in cotton in function of artificial defoliation
#'
#' @description This dataset contains the result of a real experiment to
#'     evaluate the effect of artificial defoliation in combination with
#'     phenological stage of occurrence on the production of cotton
#'     represented by the number of bolls produced at the end of the
#'     crop cycle. The experiment is a \eqn{5\times 5} factorial with 5
#'     replications casualized randomly to the experimental units (a
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
#' \item \code{rept} index for each experimenal unit in each treatment 
#' cell.
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
#' 1 for an article discussing the effect of defoliation on cotton yield 
#'     and visit 2 for an article that evaluate a count regression model 
#'     able to deal with the underdispersion. Also, see the references 
#'     section.
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
#'     desfolha artificial nos estúdios fenológicos do
#'     algodoeiro. Revista de Ciências Agrárias, 35(1), 163???172.
#'
#' Zeviani, W. M., Ribeiro, P. J., Bonat, W. H., Shimakura, S. E., &
#'     Muniz, J. A. (2014). The Gamma-count distribution in the analysis
#'     of experimental underdispersed data. Journal of Applied
#'     Statistics, 41(12),
#'     1???11. http://doi.org/10.1080/02664763.2014.922168
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
#'     \item \code{block} a categorical unordered factor with 4 levels.
#'     \item \code{variety} a categorical unordered factor with 7
#'     levels.
#'     \item \code{yield} sugarcane yield (kg/plot).
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
#'     \item \code{row} the rows of the latin square that controls in
#'     one dimension. A categorical unordered factor with 5 levels.
#'     \item \code{col} the columns of the latin square that controls in
#'     one dimension perpendicular to the previous. A categorical
#'     unordered factor with 5 levels.
#'     \item \code{variety} a categorical unordered factor with 5
#'     levels.
#'     \item \code{yield} sugarcane yield (kg/plot).
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
#' @title Sugarcane yield in function of fertilization strategy
#'
#' @description These data are from an experiment done in a latin square
#'     design of size 5. Sugarcane yield (kg/plot) was recorded in each
#'     experimental unit.
#'
#' \itemize{
#'   \item \code{row} the rows of the latin square that controls in
#'     one dimension. A categorical unordered factor with 6 levels.
#'   \item \code{col} the columns of the latin square that controls in
#'     one dimension perpendicular to the previous. A categorical
#'     unordered factor with 6 levels.
#'   \item \code{fertil} a categorical unordered factor with 6
#'     levels that is the fertilization strategy applied. These levels
#'     are a result of treatment cells in a three incomplete factorial
#'     arrangement. See details for more information.
#'   \item \code{yield} sugarcane yield (kg/plot).
#' }
#'
#' @details The levels of fertilization are in fact a combination of a
#'     \eqn{3^2} factorial experiment but not all cells are present, so
#'     this is an (intentional) incomplete three factorial
#'     experiment. The factors used were limestone (A: present, a:
#'     absent), \emph{Crotalaria juncae} (B: present, b: absent) and
#'     fertilizer (C: present, c: absent). Therefore, the level ABC
#'     means that all three factors are present. To access the effect of
#'     each factor and interactions, contrasts can be applied.
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
#'                           label=sprintf("%s\n%0.2f", fertil, yield))))
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
#' @description This is an artificial dataset which corresponds to a  
#'     experiment to study the effect of feeding type (factor with 4 
#'     categorical nominal levels) in pig weight gain. The experiment 
#'     was a randomized complete design with five experimental units per
#'     treatment level. The experimental unit was a pig. The response
#'     measured was weight gain from the beggining to the end of the
#'     experiment.
#'
#' \itemize{
#'   \item \code{litter} a categorical factor with 4 levels that
#'     represents the rows of the latin square design and control for
#'     the differences among litters.
#'   \item code{size} a categorical ordered variable that represents the
#'     columns of latin square desing and control for the weight of the
#'     animals at the beggining of the experiment.
#'   \item \code{age} age of the animal (days) when castration was
#'     done. \code{controls} are the animals without castration.
#'   \item \code{wg} weight gain (kg) after 252 days.
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
#'     studying the effect of Nitrogen (N), Phosporus (P) and Potassium
#'     (K) on corn yield in a randomized block design.
#'
#' \itemize{
#'   \item \code{block} a factor with 4 levels.
#'   \item \code{N} low (-1) and high (+1) levels of nitrogen.
#'   \item \code{P} low (-1) and high (+1) levels of phosporus.
#'   \item \code{K} low (-1) and high (+1) levels of potassium.
#'   \item \code{yield} corn yield (ton/ha).
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
#'     studying the effect of fertilizaton with vinasse (a residual from
#'     industrial processing of sugar cane) and complete mineral
#'     fertilization.
#'
#' \itemize{
#'   \item \code{block} a factor with 4 levels.
#'   \item \code{mineral} low (-1) and high (+1) levels of mineral
#'     fertilization.
#'   \item \code{vinasse} low (-1) and high (+1) levels of fertilization
#'     with vinasse.
#'   \item \code{y} some response variable. The text book doesn't give
#'     any information.
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
#'     studying the effect of fertilizaton with filter cake (a residual
#'     from industrial processing of sugar cane) and traditional mineral
#'     fertilization.
#'
#' \itemize{
#'   \item \code{block} a factor with 4 levels.
#'   \item \code{mineral} low (-1) and high (+1) levels of mineral
#'     fertilization.
#'   \item \code{cake} low (-1) and high (+1) levels of fetilization
#'     with filter cake.
#'   \item \code{y} some response variable. The text book doesn't give
#'     any information.
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
#'     studying the effect of NPK on the yield of sugar cane.
#'
#' \itemize{
#'   \item \code{block} a local control factor with 3 levels.
#'   \item \code{rept} factor with 2 levels.
#'   \item \code{N} integer coded nitrogen levels (0, 1, 2).
#'   \item \code{P} integer coded phosphorus levels (0, 1, 2).
#'   \item \code{K} integer coded potassium levels (0, 1, 2).
#'   \item \code{yield} sugar cane yield (ton/ha).
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
#'     November, December and January.
#'
#' \itemize{
#'   \item \code{variety} a categorical variable with 6 levels that
#'     represents the mango varieties studied.
#'   \item \code{year} the year of harvesting.
#'   \item \code{month} the month of harvesting.
#'   \item \code{acid} mean of the acidity determined in 3 fruits.
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
#' ## reshape::cast() also can be used.
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
#'     studing NPK in the yield of corn. Two controls were added, one is
#'     zero control (no NPK) and the other is central factorial point
#'     plus presence of limestone.
#'
#' \itemize{
#'   \item \code{N} content of nitrogen in the fertilizer.
#'   \item \code{P} content of phosphorus in the fertilizer.
#'   \item \code{K} content of potassium in the fertilizer.
#'   \item \code{limestone} presence (1) or absence of limestone (0).
#'   \item \code{acid} mean of corn yield in 16 locations (ton/ha).
#' }
#'
#' @details The experiment was conducted in 16 different locations but
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
#' @title Number of dry branches in coffee trees in function of NPK
#'
#' @description These data are from a \eqn{2^3} factorial experiment
#'     studying the effect of NPK fertilizaton on the number of dry
#'     branches in coffee trees.
#'
#' \itemize{
#'   \item \code{N} content of nitrogen in the fertilizer (low/high).
#'   \item \code{P} content of phosphorus in the fertilizer (low/high).
#'   \item \code{K} content of potassium in the fertilizer (low/high).
#'   \item \code{block} an unordered factor representing the blocks
#'     used.
#'   \item \code{branches} an integer variable, the number of dry
#'     branches in a coffee the.
#' }
#'
#' @details The experiment was conducted in a randomized block design
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

#' @name greenmanureYield
#'
#' @title block design experiment with added treatments
#'
#' @description These data are from a block design experiment
#'     studying the effect of four different blocks, with two 
#'     repetitions each (one per year), in the resulting green manure 
#'     in the yield of eight types of bean culture.  
#'     
#' \itemize{
#'   \item \code{year} the experiment was done in two years 
#'      (repetitions).
#'   \item \code{block} a categorical factor with 4 levels.
#'   \item \code{culture} a categorical factor with 8 levels.
#'   \item \code{greenmanure} the amount of resulting green manure.
#' }
#' 
#' @details The experiment was conducted in a randomized block design
#'     with 4 blocks, during 2 years.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @usage data(greenmanureYield)
#' 
#' @format a \code{data.frame} with 64 records and 4 variables.
#'
#' @source Pimentel Gomes, F. (2009). Curso de Estatística Experimental
#'     (15th ed.). Piracicaba, São Paulo: FEALQ. (page 166)
#'     
#' @examples
#' 
#' library(lattice)
#' library(latticeExtra)

#' data(greenmanureYield)
#' str(greenmanureYield)

#' xyplot(greenmanure~culture|year, data=greenmanureYield, type=c("p",
#' "a"), jitter.x=TRUE, groups=c(block, year), ylab = "Green Manure", 
#'       xlab = "Type of Culture")
#'       
#'       
NULL