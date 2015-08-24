#' @title Combine two strips in one to save space
#'
#' @name twoStripCombined
#'
#' @description this function allow combine tow strips in one to save
#' space. It was designed to be used with two conditioning variables.
#'
#' @param textPos position of the strip annotation. The \code{"border"}
#' value indicates that the first conditioning variable is to be aligned
#' to left strip border and the second to the right strip border. The
#' \code{"center"} indicates tha the first is to be right aligned to the
#' strip center and the second to be left aligned to strip center.
#'
#' @param sep a strip, to separate variable names and levels when
#' concatenation is required.
#'
#' @param invert logical, \code{TRUE} means that the first is to be
#' placed on left strip side.
#'
#' @param which.given,which.panel,factor.levels,var.name,strip.names,bg,...
#' arguments of \link[lattice]{strip.default}.
#'
#' @import lattice
#'
#' @export
#'
#' @examples
#'
#' require(lattice)
#'
#' xyplot(yield~K|N+P, data=npk, type=c("p","a"))
#'
#' xyplot(yield~K|N+P, data=npk, type=c("p","a"),
#'        strip=twoStripCombined,
#'        par.strip.text=list(lines=0.6))
#'
#' xyplot(yield~K|N+P, data=npk, type=c("p","a"),
#'        strip=function(...){
#'            twoStripCombined(textPos="center", sep="=",
#'                             strip.names=c("Nitr", "Phos"),
#'                             invert=TRUE, ...)
#'        },
#'        par.strip.text=list(lines=0.5),
#'        par.settings=list(strip.background=list(col="gray75")))
#'
twoStripCombined <- function(textPos=c("border", "center"), sep=": ", invert=TRUE,
                             which.given, which.panel,
                             factor.levels, var.name,
                             strip.names=var.name, bg,
                             ...){
    strip.fun <- function(i, pos){
        if(is.na(strip.names[i])){
            lab <- factor.levels[which.panel[which.given]]
        } else {
            lab <- paste(sep=sep, strip.names[i],
                         factor.levels[which.panel[which.given]])
        }
        if(invert){
            switch(textPos[1],
                   "border"={
                       panel.text(x=0, y=0.5, pos=pos[1], lab=lab)
                   },
                   "center"={
                       panel.text(x=0.5, y=0.5, pos=pos[2], lab=lab)
                   },
                   stop("Only `\"border\"` and `\"center\"` values are allowed."))
        } else {
            switch(textPos[1],
                   "border"={
                       panel.text(x=1, y=0.5, pos=pos[2], lab=lab)
                   },
                   "center"={
                       panel.text(x=0.5, y=0.5, pos=pos[1], lab=lab)
                   },
                   stop("Only `\"border\"` and `\"center\"` values are allowed."))
        }
    }
    ##
    ##-------------------------------------------
    ##
    if(which.given==1L){
        panel.rect(0, 0, 1, 1, col=bg, border=1)
        strip.fun(i=1, pos=c(4,2))
    }
    if(which.given==2L){
        strip.fun(i=2, pos=c(2,4))
    }
}
