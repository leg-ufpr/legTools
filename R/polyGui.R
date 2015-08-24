#' @title Polynomial regression GUI
#'
#' @name polyGui
#'
#' @description This function opens a interface to control the
#' polynomial degree in linear regression. It shows the observed values
#' and the corresponding fitted curve superimposed with confidence bands
#' (for the fitted value) and show the residuals plot too. It assumes
#' that \code{gWidgets} and \code{gWidgetstcltk} packages are available.
#'
#' @param x,y independent and dependent (numeric) regression variables.
#'
#' @param data an optional \code{data.frame}.
#'
#' @param er stands for extend range. It is used to extend the plotting
#' range by a fraction on both sides and directions. Default is
#' 0.05. See \link[grDevices]{extendrange}.
#'
#' @return None is returned by the function.
#'
#' @import gWidgets gWidgetstcltk
#'
#' @export
#' @examples
#' \donttest{
#'
#' poly.gui(x=area, y=peri, data=rock, er=0.3)
#' poly.gui(x=speed, y=dist, data=cars, er=0.3)
#' poly.gui(x=eruptions, y=waiting, data=faithful, er=0.3)
#'
#' }
poly.gui <- function(x, y, data, er=0.05){
    ##
    ##-------------------------------------------
    ## Loading the required packages.
    ##
    if (!requireNamespace("gWidgets", quietly=TRUE)){
        stop("`gWidgets` needed for this function to work. Please install it.",
             call.=FALSE)
    }
    if (!requireNamespace("gWidgetstcltk", quietly=TRUE)){
        stop("`gWidgetstcltk` needed for this function to work. Please install it.",
             call.=FALSE)
    }
    ## stopifnot(require(gWidgets))
    ## stopifnot(require(gWidgetstcltk))
    options(guiToolkit="tcltk")
    ##
    ##-------------------------------------------
    ## Functions to annotate in the plot upper margin.
    ##
    annotations <- function(lm.obj){
        mtext(side=3, adj=0, line=1.5,
              text=sprintf("X rank: %i", lm.obj$rank))
        mtext(side=3, adj=0, line=0.5,
              text=sprintf("Residual DF: %i", lm.obj$df.residual))
        press <- sum((residuals(lm.obj)/(1-hatvalues(lm.obj)))^2)
        mtext(side=3, adj=1, line=0.5,
              text=sprintf("PRESS: %0.2f", press))
        sm <- summary(lm.obj)
        lastcoef <- sprintf("High term p-value: %0.5f",
                            sm$coeff[length(coef(lm.obj)), 4])
        mtext(side=3, adj=1, line=2.5, text=lastcoef)
        mtext(side=3, adj=1, line=1.5,
              text=sprintf("R^2 (adj. R^2): %0.2f (%0.2f)",
                  100*sm$r.squared, 100*sm$adj.r.squared))
    }
    ##
    ##-------------------------------------------
    ## Auxiliary variables not controled by the GUI.
    ##
    xlab <- deparse(substitute(x))
    ylab <- deparse(substitute(y))
    if(!missing(data)){
        da <- eval(data, envir=parent.frame())
        x <- da[, deparse(substitute(x))]
        y <- da[, deparse(substitute(y))]
    }
    maxd <- as.integer(length(unique(x))-1)
    xr <- extendrange(x, f=er)
    yr <- extendrange(y, f=er)
    newdata <- data.frame(x=seq(xr[1], xr[2], length.out=200))
    ##
    ##-------------------------------------------
    ## Function controled by the GUI.
    ##
    draw.poly <- function(h, ...){
        svalue(degree) <- min(c(
            max(c(1L, svalue(degree)+h$action$val)),
            maxd))
        m0 <- lm(y~poly(x, degree=svalue(degree)))
        switch(svalue(plottype),
               "Scatter plot"={
                   cb <- predict(m0, newdata=newdata, interval="confidence")
                   plot(y~x, xlim=xr, ylim=yr,
                        xlab=xlab, ylab=ylab)
                   matlines(newdata$x, cb, lty=c(1,2,2), col=1)
                   annotations(m0)
               },
               "Residuals"={
                   par(mfrow=c(2,2))
                   plot(m0)
                   layout(1)
               })
    }
    ##
    ##-------------------------------------------
    ## Building the GUI.
    ##
    w <- gwindow(title="Polynomial regression", visible=FALSE)
    g <- ggroup(container=w, horizontal=FALSE)
    gg_label <- ggroup(container=g)
    dlabel <- glabel(text="Polynomial degree:",
                     container=gg_label)
    degree <- gedit(text=1, width=2, coerce.with=as.integer,
                    handler=draw.poly, action=list(val=0L),
                    container=gg_label)
    gg_buttons <- ggroup(container=g)
    gminus <- gbutton(text="-",
                      handler=draw.poly,
                      action=list(val=-1L),
                      container=gg_buttons)
    gplus <- gbutton(text="+",
                     handler=draw.poly,
                     action=list(val=1L),
                     container=gg_buttons)
    gg_radio <- ggroup(container=g)
    plottype <- gradio(items=c("Scatter plot", "Residuals"),
                       horizontal=TRUE,
                       handler=draw.poly,
                       action=list(val=0L),
                       container=gg_radio)
    do.call(what=draw.poly, args=list(h=list(degree=1L)))
    visible(w) <- TRUE
    invisible()
}
