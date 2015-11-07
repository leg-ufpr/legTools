##----------------------------------------------------------------------
## Data generation. Pimentel page 283.

## wgCattle <- read.table("clipboard", header=FALSE, sep="\t")
## wgCattle[,3] <- as.numeric(sub(x=wgCattle[,3], ",", "."))
## names(wgCattle) <- c("block", "group", "inten", "specie", "weight")
## str(wgCattle)
## wgCattle <- wgCattle[with(wgCattle,
##                           order(block, specie, inten, group)),]
## dput(wgCattle$weight)

wgCattle <- expand.grid(group=gl(4, 1),
                        inten=c(0.9, 1.5),
                        specie=c("B. decumbens", "B. humidicola",
                                 "B. ruziziensis"),
                        block=gl(2, 1),
                        KEEP.OUT.ATTRS=FALSE)
wgCattle$weight <- c(294, 360, 368, 400, 310, 288, 334, 335, 312, 350,
                     341, 380, 268, 355, 343, 333, 312, 345, 381, 340,
                     265, 345, 319, 407, 340, 363, 357, 390, 300, 347,
                     350, 338, 316, 325, 341, 364, 318, 247, 285, 295,
                     266, 370, 396, 377, 234, 341, 362, 349)
wgCattle <- wgCattle[, c(4,1,3,2,5)]
str(wgCattle)

save(wgCattle, file="../data/wgCattle.RData")

##----------------------------------------------------------------------
## Ckeck the data.

aggregate(weight~group+block, data=wgCattle, FUN=sum)
aggregate(weight~block+specie+inten, data=wgCattle, FUN=sum)

m0 <- lm(weight~block:group+factor(inten):specie, data=wgCattle)
anova(m0)

m0 <- lm(weight~block:group+specie*inten, data=wgCattle)
anova(m0)

library(doBy)
LSmeans(m0, at=list(inten=c(0.9)))
LSmeans(m0, at=list(inten=c(1.5)))

##----------------------------------------------------------------------
## Examples.

library(lattice)

data(wgCattle)
str(wgCattle)

xyplot(weight~specie|group, groups=inten,
       data=wgCattle, type=c("p", "a"),
       auto.key=list(columns=2, title="Intensity (un/ha)"),
       scales=list(x=list(font=3)),
       ylab="Weight (kg)",
       xlab=expression(Species~of~italic("Brachiaria")))

rm(list=ls())
load("../data/wgCattle.RData")
ls()
str(wgCattle)
