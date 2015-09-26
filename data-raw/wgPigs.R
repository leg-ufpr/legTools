##----------------------------------------------------------------------
## Data generation.

wgPigs <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_racoes.txt",
                     header=TRUE, sep="\t")
names(wgPigs) <- c("ft","wg")
str(wgPigs)

save(wgPigs, file="../data/wgPigs.RData")

##----------------------------------------------------------------------
## Examples.

require(lattice)

xyplot(wg~ft, data=wgPigs,
       ylab="Weight gain (kg)",
       xlab="Feeding type")

rm(list=ls())
load("../data/wgPigs.RData")
ls()
str(wgPigs)

##----------------------------------------------------------------------

write.table(x=wgPigs, file="wgPigs.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

## Port and IP.
u <- scan(n=2, what=character())
cmd <- paste0("scp -P ", u[1], " wgPigs.txt leg@", u[2],
              ":/home/leg/public_html/legTools/dataset")
system(cmd)

url <- "http://blog.leg.ufpr.br/~leg/legTools/dataset/wgPigs.txt"
browseURL(url)
wgPigs <- read.table(file=url, header=TRUE, sep="\t")
