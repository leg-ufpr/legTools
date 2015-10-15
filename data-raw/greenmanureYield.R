##----------------------------------------------------------------------
## Data generation.

x <- read.table(file = 'clipboard', sep = "\t", header=TRUE)
dput(x)
greenmanureYield <- structure(list(ano = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L), bloco = structure(c(1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 
3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 
4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L), .Label = c("i", 
"ii", "iii", "iv"), class = "factor"), cultura = structure(c(6L, 
3L, 2L, 4L, 8L, 7L, 1L, 5L, 6L, 3L, 2L, 4L, 8L, 7L, 1L, 5L, 6L, 
3L, 2L, 4L, 8L, 7L, 1L, 5L, 6L, 3L, 2L, 4L, 8L, 7L, 1L, 5L, 6L, 
3L, 2L, 4L, 8L, 7L, 1L, 5L, 6L, 3L, 2L, 4L, 8L, 7L, 1L, 5L, 6L, 
3L, 2L, 4L, 8L, 7L, 1L, 5L, 6L, 3L, 2L, 4L, 8L, 7L, 1L, 5L), 
.Label = c("Crotalaria grantiana", 
"Crotalaria juncea", "Feijao-de-porco", "Guandu", "Milho", "Mucuna preta", 
"Soja", "Tephrosia Candida"), class = "factor"), materiaverde = c(86.8, 
44, 102.4, 68.4, 34, 33, 25.8, 138.8, 90.2, 83.8, 120.2, 91, 
57.2, 33.6, 77, 110.2, 76.8, 56.6, 90.8, 55.2, 32.4, 34.8, 21.6, 
106.4, 94, 72.2, 104.6, 78.8, 54, 33.2, 62.4, 80, 88.6, 52.4, 
92, 49, 24.4, 32, 19.2, 108, 86.4, 88.6, 112, 83.4, 50.8, 33.4, 
63.6, 92, 81.6, 52.2, 84.8, 61.2, 30, 33.6, 21, 81.8, 82.2, 83.2, 
113.6, 91.2, 46.2, 42.6, 63.4, 90.6)), .Names = c("ano", "bloco", 
"cultura", "materiaverde"), class = "data.frame", row.names = c(NA, 
-64L))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
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



