#' @title Loess regression GUI
#'
#' @name loessGui
#'
#' @description This function opens an interface to control the settings
#' of a loess regression:
#' \itemize{
#'   \item degree choose the local polynomial degree with a radio
#' selector;
#'   \item span set the span value that controls the degree of
#' smoothing;
#'   \item center move the x value to be predicted;
#' }
#'
#' The elements of the interface change a plot that shows the observed
#' values and the corresponding fitted curve superimposed with
#' confidence bands (for the fitted values). It assumes that
#' \code{gWidgets} and \code{gWidgetstcltk} packages are available.
#'
#' @param x,y independent and dependent (numeric) regression variables.
#' @param data an optional \code{data.frame}.
#' @param er stands for extend range. It is used to extend the plotting
#'     range by a fraction on both sides and directions. Default is
#'     0.05. See \link[grDevices]{extendrange}.
#'
#' @return None is returned by the function, only a GUI is opened.
#'
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}
#'
#' @export
#' @examples
#' \donttest{
#'
#' library(gWidgets)
#' library(gWidgetstcltk)
#' 
#' loessGui(x=area, y=peri, data=rock, er=0.3)
#' loessGui(x=speed, y=dist, data=cars, er=0.3)
#' loessGui(x=eruptions, y=waiting, data=faithful, er=0.3)
#'
#' }
loessGui <- function(x, y, data, er=0.05){
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
    options(guiToolkit="tcltk")
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
    xl <- range(x)
    nx <- length(x)
    erx <- extendrange(xl, f=er)
    ery <- extendrange(y, f=er)
    newdata <- data.frame(x=seq(erx[1], erx[2], length.out=200))
    ##
    ##-------------------------------------------
    ## Functions to annotate in the plot upper margin.
    ##
    annotations <- function(m0, y){
        mtext(side=3, adj=0, line=1.5,
              text=sprintf("H trace: %0.2f", m0$trace.hat))
        mtext(side=3, adj=0, line=0.5,
              text=sprintf("Equivalent num. param.: %0.2f", m0$enp))
        r2 <- 100*cor(fitted(m0), y)^2
        mtext(side=3, adj=1, line=1.5,
              text=sprintf("R^2: %0.2f", r2))
    }
    ##
    ##-------------------------------------------
    ## Functions to adjust 0 order, 1 and 2 local polynomial regression
    ## model.
    ##
    f0 <- function(w, xl){
        y.pred <- sum(w*y)/sum(w)
        segments(xl[1], y.pred, xl[2], y.pred, col=2)
    }
    f1 <- function(w, xl){
        m <- lm(y~poly(x, degree=1), weights=w)
        y.pred <- predict(m, newdata=list(x=xl))
        segments(xl[1], y.pred[1], xl[2], y.pred[2], col=2)
    }
    f2 <- function(w, xl){
        m <- lm(y~poly(x, degree=2), weights=w)
        x.pred <- seq(xl[1], xl[2], length.out=20)
        y.pred <- predict(m, newdata=list(x=x.pred))
        lines(x.pred, y.pred, col=2)
    }
    ##
    ##-------------------------------------------
    ## Reactive function.
    ##
    draw.loess <- function(...){
        ##
        ##-------------------------------------------
        ## Fit loess regression.
        ## 
        m0 <- loess(formula=y~x,
                    span=gWidgets::svalue(SPAN),
                    degree=as.integer(gWidgets::svalue(DEGREE)),
                    family="gaussian")
        ##
        ##-------------------------------------------
        ## Predicted values with confidence bands.
        ## 
        pred <- predict(m0, newdata=newdata, se=TRUE)
        pred$me <- pred$se.fit*1.96
        pred$ci <- sweep(x=cbind(fit=0, lwr=-pred$me, upr=pred$me),
                         MARGIN=1, STATS=pred$fit, FUN="+")
        ##
        ##-------------------------------------------
        ## Weights to be used in local polynomial.
        ## 
        x0 <- gWidgets::svalue(XCENTER)
        sp <- gWidgets::svalue(SPAN)
        a <- abs(x-x0)
        if (sp < 1){
            q <- as.integer(sp*nx)
            d <- sort(a)[q]
        } else {
            q <- nx
            d <- max(abs(a))*sqrt(sp)
        }
        s <- a <= d
        w <- rep(0, nx)
        w[s] <- (1-(a[s]/d)^3)^3
        ##
        ##-------------------------------------------
        ## Scatter plot, point size proportional to weight.
        ## 
        i <- as.integer(s)
        plot(x, y,
             pch=2*(!s)+1, cex=i*3*w+1,
             xlim=erx, ylim=ery)
        ##
        ##-------------------------------------------
        ## Local polynomial.
        ##
        xl[1] <- ifelse(x0 - d>xl[1], x0-d, xl[1])
        xl[2] <- ifelse(x0 + d<xl[2], x0+d, xl[2])
        matlines(x=newdata$x, y=pred$ci, lty=c(1,2,2), col=1)
        abline(v=c(x0, xl), lty=c(2,3,3))
        annotations(m0, y)
        mtext(side=3, adj=1, line=0.5,
              text=sprintf("Number of obs. used/total: %i/%i",
                  sum(s), nx))
        ## NOTE: usar action aqui!
        ## xl <- c(min(c(xl[1], x0)), max(c(x0, xl[2])))
        do.call(what=paste0("f", gWidgets::svalue(DEGREE)),
                args=list(w=w, xl=xl))
    }
    ##
    ##-------------------------------------------
    ## Building the GUI.
    ##
    WDW <- gWidgets::gwindow(title="LOESS regression", visible=FALSE)
    GG <- gWidgets::ggroup(container=WDW, expand=TRUE, horizontal=FALSE)
    GF_DG <- gWidgets::gframe(text="Local polynomial degree:", container=GG)
    DEGREE <- gWidgets::gradio(items=0:2, selected=2L, horizontal=TRUE,
                     handler=draw.loess,
                     container=GF_DG)
    GF_XC <- gWidgets::gframe(text="Predicted point:", expand=TRUE,
                              container=GG)
    XCENTER <- gWidgets::gslider(from=erx[1], to=erx[2],
                       value=mean(erx),
                       length.out=51,
                       handler=draw.loess,
                       expand=TRUE,
                       container=GF_XC)
    XCLABEL <- gWidgets::glabel(text=sprintf("%0.2f",
                                             gWidgets::svalue(XCENTER)),
                      container=GF_XC)
    gWidgets::addHandlerChanged(XCENTER,
                      action=XCLABEL,
                      handler=function(h, ...){
                          gWidgets::svalue(h$action) <-
                              sprintf("%0.2f", gWidgets::svalue(h$obj))
                      })
    GF_SP <- gWidgets::gframe(text="Span:", expand=TRUE,container=GG)
    SPAN <- gWidgets::gslider(from=0, to=1.5,
                    value=0.75, by=0.05,
                    handler=draw.loess,
                    expand=TRUE,
                    container=GF_SP)
    SPLABEL <- gWidgets::glabel(text=sprintf("%0.2f",
                                             gWidgets::svalue(SPAN)),
                      container=GF_SP)
    gWidgets::addHandlerChanged(SPAN,
                      action=SPLABEL,
                      handler=function(h, ...){
                          gWidgets::svalue(h$action) <-
                              sprintf("%0.2f", gWidgets::svalue(h$obj))
                      })
    ##-------------------------------------------
    ## Initializing.
    gWidgets::svalue(SPAN) <- 0.75
    gWidgets::svalue(DEGREE) <- 1L
    gWidgets::svalue(XCENTER) <- mean(erx)
    do.call(what=draw.loess, args=list(NA))
    gWidgets::visible(WDW) <- TRUE
}
