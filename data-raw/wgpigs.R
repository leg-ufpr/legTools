##----------------------------------------------------------------------
## Data generation.

wgpigs <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_racoes.txt",
                     header=TRUE, sep="\t")
names(wgpigs) <- c("ft","wg")
str(wgpigs)

save(wgpigs, file="../data/wgpigs.RData")

##----------------------------------------------------------------------
## Examples.

require(lattice)

xyplot(wg~ft, data=wgpigs,
       ylab="Weight gain (kg)",
       xlab="Feeding type")

rm(list=ls())
load("../data/wgpigs.RData")
ls()
str(wgpigs)

##----------------------------------------------------------------------

write.table(x=wgpigs, file="wgpigs.txt",
            sep="\t", quote=FALSE, row.names=FALSE)

## Port and IP.
u <- scan(n=2, what=character())
cmd <- paste0("scp -P ", u[1], " wgpigs.txt leg@", u[2],
              ":/home/leg/public_html/legTools/dataset")
system(cmd)

url <- "http://blog.leg.ufpr.br/~leg/legTools/dataset/wgpigs.txt"
browseURL(url)
wgpigs <- read.table(file=url, header=TRUE, sep="\t")
