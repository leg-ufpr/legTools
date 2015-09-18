## Script to build and verify the package

## Set working directory
wd <- "~/GitLab/legTools"
setwd(wd)

if(!grepl(x=getwd(), pattern="/legTools$")){
    stop("Move to /legTools directory.")
}

## Packages
library(devtools)

## Load the package (to make functiona available)
load_all()

## Create/update NAMESPACE, *.Rd files.
document()

## Check documentation.
check_doc()

## Check functions, datasets, run examples, etc. Using cleanup = FALSE
## and check_dir = "../" will create a directory named legTools.Rcheck
## with all the logs, manuals, figures from examples, etc.
check(cleanup = FALSE, manual = TRUE, vignettes = FALSE,
      check_dir = "../")

## Examples
# Run examples from all functions of the package
# run_examples()
# Run examples from a specific function
# dev_example("yscale.components.right")

## Show all exported objects.
ls("package:legTools")
packageVersion("legTools")

## Build the package (it will be one directory up)
build(manual = TRUE, vignettes = FALSE)
# build the binary version for windows (not used)
# build_win()

## Test install with install.packages
pkg <- paste0("../legTools_", packageVersion("legTools"), ".tar.gz")
install.packages(pkg, repos = NULL)

##----------------------------------------------------------------------
## Package vignette.
## Based on: http://r-pkgs.had.co.nz/vignettes.html

use_vignette("PimentelGomes")


##======================================================================
## Sending package tarballs and manual to remote server to be
## downloadable
pkg.win <- paste0("../legTools_", packageVersion("legTools"), ".zip")
cmd.win <- paste("cd ../legTools.Rcheck && zip -r", pkg.win, "legTools")
system(cmd.win)
man <- "../legTools.Rcheck/legTools-manual.pdf"
cmd <- paste("scp -P $PATAXOP", pkg, man, pkg.win,
             "fernandomayer@$PATAXO:~/public_html/legTools")
system(cmd)
##======================================================================
