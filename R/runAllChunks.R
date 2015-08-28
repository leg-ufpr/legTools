#' @title Run all chunks in a Rmd file
#'
#' @name runAllChunks
#'
#' @description This function was developed to run all chunks in a knitr
#' Rmd (R markdown) file at once. Mainly for exploring and debugging
#' purposes.
#'
#' @param Rmd the name of the Rmd file.
#'
#' @param envir the environment in which the chunks will be
#' evaluated. By default it is the GlobalEnv.
#'
#' @references This function was based on this
#'     \href{http://stackoverflow.com/questions/24753969/knitr-run-all-chunks-in-an-rmarkdown-document}{SO
#'     thread}.
#'
#' @return Objects created in the chunks from the Rmd file.
#'
#' @import knitr
#'
#' @author Fernando Mayer, \email{fernando.mayer@@ufpr.br}
#'
#' @export
runAllChunks <- function(Rmd, envir = globalenv()){
    if (!requireNamespace("knitr", quietly = TRUE)){
        stop("`knitr` needed for this function to work. Please install it.",
             call. = FALSE)
    }
    tempR <- tempfile(tmpdir = ".", fileext = ".R")
    on.exit(unlink(tempR))
    purl(Rmd, output = tempR)
    sys.source(tempR, envir = envir)
}
