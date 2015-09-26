##----------------------------------------------------------------------
## Data generation.

cornYield <- expand.grid(block=gl(4, 1), N=c(-1,1), P=c(-1,1),
                         K=c(-1,1), KEEP.OUT.ATTRS=FALSE)
cornYield$yield <- c(1.32, 2.12, 1.75, 2.35, 1.80, 2.20, 2.95, 2.96,
                     1.66, 2.66, 1.73, 2.58, 1.72, 3.85, 2.62, 3.00,
                     2.58, 3.56, 2.86, 2.75, 2.72, 3.20, 2.25, 2.75,
                     2.26, 2.08, 1.95, 2.70, 2.95, 3.28, 2.40, 3.35)
str(cornYield)

save(cornYield, file="../data/cornYield.RData")

##----------------------------------------------------------------------
## Examples.

library(lattice)
library(latticeExtra)

data(cornYield)
str(cornYield)

xyplot(yield~N|P, groups=K,
       data=cornYield, type=c("p", "a"),
       ylab=expression(Yield~(ton~ha^{-1})),
       xlab="Nutrient level")

xyplot(yield~N, groups=interaction(P, K),
       data=cornYield, type=c("p", "a"),
       auto.key=list(columns=2),
       ylab=expression(Yield~(ton~ha^{-1})),
       xlab="Nutrient level")

m0 <- lm(yield~block+(N+P+K)^3, data=cornYield)
par(mfrow=c(2,2)); plot(m0); layout(1)
anova(m0)

m1 <- update(m0, .~block+N+K)
par(mfrow=c(2,2)); plot(m1); layout(1)

anova(m0, m1)
anova(m1)

summary(m1)

pred <- expand.grid(block="1",
                    N=seq(-1, 1, by=0.1),
                    K=seq(-1, 1, by=0.1))
pred$mu <- predict(m1, newdata=pred)

wireframe(mu~N+K, data=pred,
          scales=list(arrows=FALSE),
          zlab=list(expression(Yield~(ton~ha^{-1})), rot=90),
          drape=TRUE, cuts=20,
          col.regions=colorRampPalette(
              color=brewer.pal(n=11, name="Spectral"))(21))

levelplot(mu~N+K, data=pred, aspect=1,
          main=expression(Yield~(ton~ha^{-1})),
          col.regions=colorRampPalette(
              color=brewer.pal(n=11, name="Spectral")))

rm(list=ls())
load("../data/cornYield.RData")
ls()
str(cornYield)
