##----------------------------------------------------------------------
## Data generation.

x <- read.table(file = 'clipboard', sep = "\t", header=TRUE)
greenmanureYield <- dput(x)
names(greenmanureYield) <- c("year", "block", "culture", "greenmanure")
greenmanureYield$culture <- as.factor(c("mucuna pruriens", "jack bean", 
      "crotalaria juncea", "pigeon pea", "tephrosia candida",
      "soy bean", "crotalaria grantiana", "corn"))
str(greenmanureYield)
save(greenmanureYield, file="../data/greenmanureYield.RData")

##----------------------------------------------------------------------
## Examples.
library(lattice)
library(latticeExtra)

data(greenmanureYield)
str(greenmanureYield)
load("../data/greenmanureYield.RData")

xyplot(greenmanure~culture|year, data=greenmanureYield, type=c("p","a"), 
       jitter.x=TRUE, groups=c(block, year), ylab = "Green Manure", 
       xlab = "Type of Culture")

## 
m0 <- lm(greenmanure~culture+block+year, data=greenmanureYield)
str(m0)
densityplot(m0$fitted.values)
## Departures from homecedasticity and normality.
par(mfrow=c(2,2)); plot(m0); layout(1)

rm(list=ls())
load("../data/greenmanureYield.RData")
ls()
str(greenmanureYield)



