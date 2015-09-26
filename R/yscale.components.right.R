#' @title y-axis annotations on the right side
#'
#' @name yscale.components.right
#'
#' @description This function if for place y axis annotation on the
#'     right side of the plot.
#'
#' @param ... arguments passed by the lattice function called. See
#'     \link[lattice]{yscale.components.default}.
#'
#' @import lattice latticeExtra
#'
#' @source When such feature was necessary, a search in the web was done
#'     and a post in the r-help mailing list inspired us
#'     \code{http://r.789695.n4.nabble.com/Spacing-between-lattice-panels-td855613.html}.
#'
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}
#' 
#' @export
#'
#' @examples
#'
#' library(lattice)
#' library(latticeExtra)
#' 
#' ## alternating=2 works when relation="same".
#' p1 <- xyplot(yield~K|N+P, data=npk,
#'              scales=list(y=list(alternating=2)))
#' useOuterStrips(p1)
#' 
#' ## y annotation don't is written on the right side.
#' p2 <- xyplot(yield~K|N+P, data=npk,
#'              scales=list(y=list(relation="free", alternating=2)))
#' useOuterStrips(p2)
#' 
#' ## The desired result.
#' p3 <- xyplot(yield~K|N+P, data=npk,
#'              scales=list(y=list(relation="free", alternating=2)),
#'              ylab=NULL, ylab.right="Yield",
#'              yscale.component=yscale.components.right,
#'              between=list(x=0.5, y=0.2),
#'              par.settings=list(
#'                  layout.widths=list(
#'                      right.padding=-2,
#'                      left.padding=-2,
#'                      ylab.right=5),
#'                  strip.background=list(col=c("gray50", "gray90"))),
#'              )
#' useOuterStrips(p3)
#'
yscale.components.right <- function(...){
    ans <- yscale.components.default(...)
    ans$right <- ans$left
    ans$left <- NULL
    ans
}
