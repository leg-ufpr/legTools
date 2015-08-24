## Script to build and verify the package

## Set working directory
wd <- "~/GitLab/legTools"
setwd(wd)

if(!grepl(x=getwd(), pattern="/legTools$")){
    stop("Move to /legTools directory.")
}

## Packages
library(devtools)

## Create/update NAMESPACE, *.Rd files.
document()

## Check documentation.
check_doc()

## Check functions, datasets, run examples, etc. Using cleanup = FALSE
## and check_dir = "../" will create a directory named legTools.Rcheck
## with all the logs, manuals, figures from examples, etc.
check(cleanup = FALSE, manual = TRUE, vignettes = FALSE,
      check_dir = "../")

## Load the package.
load_all()

## Show all exported objects.
ls("package:legTools")
packageVersion("legTools")

## Build the package (it will be one directory up)
build(manual = TRUE, vignettes = FALSE)
## build_win()

## Test install with install.packages
pkg <- paste("../legTools_", packageVersion("legTools"),
             ".tar.gz", sep = "")
install.packages(pkg, repos = NULL)
