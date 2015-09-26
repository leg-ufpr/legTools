##----------------------------------------------------------------------
## Data generation.

sugarcaneYield4 <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_npk2.txt",
                              header=TRUE, sep="\t")
names(sugarcaneYield4)[c(1,6)] <- c("block", "yield")
sugarcaneYield4 <- transform(sugarcaneYield4, rept=factor(rept))
str(sugarcaneYield4)

sugarcaneYield4 <- sugarcaneYield4[
    with(sugarcaneYield4, order(block, rept, N, P, K)),]

save(sugarcaneYield4, file="../data/sugarcaneYield4.RData")

##----------------------------------------------------------------------
## Examples.

library(lattice)
library(latticeExtra)
library(multcomp)

data(sugarcaneYield4)
str(sugarcaneYield4)

xyplot(yield~N|P, groups=K,
       auto.key=list(title="Potassim level", columns=3),
       strip=strip.custom(var.name="Phosphorus", strip.names=TRUE,
                          strip.levels=TRUE, sep=": "),
       data=sugarcaneYield4, type=c("p", "a"),
       ylab=expression(Yield~(ton~ha^{-1})),
       xlab="Nitrogen level level")

## Sums in each cell combination.
addmargins(with(sugarcaneYield4, tapply(yield, list(P, N), FUN=sum)))
addmargins(with(sugarcaneYield4, tapply(yield, list(K, N), FUN=sum)))
addmargins(with(sugarcaneYield4, tapply(yield, list(K, P), FUN=sum)))

sugarcaneYield4 <- transform(sugarcaneYield4,
                             blockr=interaction(block, rept),
                             nitro=factor(N),
                             phosp=factor(P),
                             potas=factor(K))
str(sugarcaneYield4)

m0 <- lm(yield~blockr+(nitro+phosp+potas)^3, data=sugarcaneYield4)
par(mfrow=c(2,2)); plot(m0); layout(1)
anova(m0)

m1 <- update(m0, .~blockr+(nitro+phosp)^2)
par(mfrow=c(2,2)); plot(m1); layout(1)

anova(m0, m1)
anova(m1)

m2 <- aov(yield~blockr+nitro/phosp, data=sugarcaneYield4)
anova(m2)

PinN <- sapply(paste0("nitro", levels(sugarcaneYield4$nitro)),
               FUN=grep, x=names(coef(m2))[m2$assign==3L],
               simplify=FALSE)

summary(m2, split=list("nitro:phosp"=PinN))

X <- model.matrix(m1)
X

aggregate(X~nitro+phosp, data=sugarcaneYield4, FUN=mean)

## It is better use multcomp::LSmatrix().
L <- aggregate(X~nitro+phosp, data=sugarcaneYield4, FUN=mean)
rownames(L) <- with(L, paste0("N", nitro, ":P", phosp))
L <- as.matrix(L[, colnames(X)])
str(L)

## Least squares means for N:P combinations.
L%*%coef(m1)

g1 <- glht(m1, linfct=L)

confint(g1, calpha=univariate_calpha())

rm(list=ls())
load("../data/sugarcaneYield4.RData")
ls()
str(sugarcaneYield4)
