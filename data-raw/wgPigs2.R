##----------------------------------------------------------------------
## Data generation.

wgPigs2 <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_castracao.txt",
                      header=TRUE, sep="\t")
names(wgPigs2) <- c("litter", "size", "age", "wg")
wgPigs2 <- transform(wgPigs2, litter=factor(litter), size=factor(size))

aggregate(wg~age, data=wgPigs2, FUN=mean)

wgPigs2$age <- factor(wgPigs2$age,
                      levels=levels(wgPigs2$age)[c(4,3,1,2)],
                      labels=c("control", "7", "21", "56"))
str(wgPigs2)

save(wgPigs2, file="../data/wgPigs2.RData")

##----------------------------------------------------------------------
## Examples.

library(lattice)

data(wgPigs2)
str(wgPigs2)

xyplot(wg~age, data=wgPigs2, groups=litter,
       ylab="Weight gain (kg)",
       xlab="Age at castration (days)")

m0 <- lm(wg~litter+size+age, data=wgPigs2)
par(mfrow=c(2,2)); plot(m0); layout(1)
anova(m0)

summary(m0)

library(multcomp)
summary(glht(m0, linfct=mcp(age="Dunnet")),
        test=adjusted(type="single-step"))

m1 <- glm(wg~litter+size+age, data=wgPigs2, family=Gamma)
m2 <- glm(wg~litter+size+age, data=wgPigs2,
          family=Gamma(link="log"))
m3 <- glm(wg~litter+size+age, data=wgPigs2,
          family=Gamma(link="identity"))

rbind(logLik(m0),
      logLik(m1),
      logLik(m2),
      logLik(m3))

par(mfrow=c(2,2)); plot(m1); layout(1)
anova(m1, test="F")
anova(m2, test="F")
anova(m3, test="F")

summary(glht(m3, linfct=mcp(age="Dunnet")),
        test=adjusted(type="single-step"))

rm(list=ls())
load("../data/wgPigs2.RData")
ls()
str(wgPigs2)

