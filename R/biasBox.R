#' @title Box bias measaure for nonlinear regression models
#'
#' @name biasBox
#'
#' @description This function calculates the asymptotic Box bias measure for
#' nonlinear regression models defined by Box (1971). See the references section.
#'
#' @param nls.obj An object of class \code{nls} that has gradient and
#' hessian attributes. See \link[stats]{deriv3}.
#'
#' @return A list with three named elements:
#' \itemize{
#'     \item \code{Absolute_bias} is the absolute bias.
#'     \item \code{Relative_theta} is the bias in relation to the
#' pontual parameter estimates, in percentage.
#'     \item \code{Relative_std.error} is the bias in relation to the
#' precision of the potual parameter estimates, in this case, its
#' standard error, in percentage.
#' }
#'
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}
#'
#' @references Box, M. J. (1971). Bias in nonlinear estimation. Journal
#' of Royal Statistical Society. Serie B. Methodological, $33 (2), 171–201.
#'
#' @export
#' @examples
#'
#' library(lattice)
#'
#' data(Puromycin)
#'
#' xyplot(rate~conc, groups=state, data=Puromycin)
#'
#' da <- subset(Puromycin, state=="treated")
#'
#' ##-------------------------------------------
#' ## Model 1: Michaelis-Menten.
#'
#' model1 <- deriv3(expr=~A*conc/(B+conc),
#'                  namevec=c("A", "B"),
#'                  function.arg=function(conc, A, B){ NULL })
#'
#' m1 <- nls(rate~model1(conc, A, B), data=da,
#'           start=list(A=200, B=0.05))
#' coef(m1)
#' bb1 <- biasBox(m1)
#'
#' ##-------------------------------------------
#' ## Model 2: monomolecular.
#'
#' model2 <- deriv3(expr=~A*(1-exp(-log(2)*conc/B)),
#'                  namevec=c("A", "B"),
#'                  function.arg=function(conc, A, B){ NULL })
#'
#' m2 <- nls(rate~model2(conc, A, B), data=da,
#'           start=list(A=200, B=0.05))
#' coef(m2)
#' bb2 <- biasBox(m2)
#'
#' ##-------------------------------------------
#' ## Bias side by side.
#'
#' cbind(do.call(rbind, bb1),
#'       do.call(rbind, bb2))
#'
#'
biasBox <- function(nls.obj){
    smm.obj <- summary(nls.obj)
    theta <- smm.obj$coef[,1]
    sd.theta <- smm.obj$coef[,2]
    sig <- smm.obj$sigma
    F <- attr(nls.obj$m$fitted(), "gradient")
    H <- attr(nls.obj$m$fitted(), "hessian")
    if (is.null(F) | is.null(H)){
        stop("Models doesn't have a gradient/hessian attributes.")
    }
    n <- nrow(F)
    FtF <- crossprod(F)
    iFtF <- solve(FtF)
    d <- -(sig^2/2)*
        sapply(1:n,
               function(x){
                   sum(diag(iFtF%*%H[x, , ]))
               })
    bias <- as.vector(iFtF%*%t(F)%*%d)
    names(bias) <- names(coef(nls.obj))
    bias.sd <- 100*bias/sd.theta
    bias.th <- 100*bias/theta
    L <- list("Absolute_bias"=bias,
              "Relative_theta"=bias.th,
              "Relative_std.error"=bias.sd)
    class(L) <- "biasBox"
    return(L)
}
