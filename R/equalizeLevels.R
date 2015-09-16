#' @title Make that to data frames have the same factor levels
#'
#' @name equalizeLevels
#'
#' @description If two data frames have factor or character columns with
#'     the same name, those in the first will have the same level order
#'     as those in the second. So, in terms of factor columns, these
#'     data frames will have the same levels in the same
#'     order. Character columns in the first will be converted to factor
#'     if they aren't. This function is useful to assing to the data
#'     frame returned in the \code{grid} attribute returned by
#'     \code{doBy::LSmeans()} or \code{doBy::LSmatrix()} the same order
#'     to the levels present in the data frame used to fit the model and
#'     estimate the parameters.
#'
#' @param \code{target} the target data frame that will have factor
#'     levels reordered.
#' @param \code{ref} the reference data frame that contains the desired
#'     level order.
#'
#' @return the first data data frame with the levels in a new order.
#'
#' @seealso \link[doBy]{LSmeans}, \link[doBy]{LSmatrix}.
#'
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}
#'
#' @export
#'
#' @examples
#'
#' a <- data.frame(
#'     Species=as.character(sample(iris$Species, size=10, replace=TRUE)),
#'     stringsAsFactors=FALSE)
#' str(a)
#'
#' levels(a$Species)
#' levels(iris$Species)
#'
#' b <- equalizeLevels(target=a, ref=iris)
#' str(b)
#'
equalizeLevels <- function(target, ref){
    if(is.data.frame(target) & is.data.frame(ref)){
        com <- intersect(names(target), names(ref))
        for(i in com){
            if(!is.null(levels(ref[,i]))){
                target[,i] <- factor(target[,i], levels=levels(ref[,i]))
            }
        }
        target
    }
    else stop("`target` and `ref` must be a data.frame.")
}
