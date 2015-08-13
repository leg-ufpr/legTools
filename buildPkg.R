## require(roxygen2)
## setwd("/home/walmes/GitHub/")
## roxygenise(package.dir="wzRfun")

require(devtools)

if(!grepl(x=getwd(), pattern="/legTools$")){
    stop("Move to /legTools directory.")
}

## Create/update NAMESPACE, *.Rd files.
document()

## Check documentation.
check_doc()

## Check functions, datasets, run examples, etc.
check()

## Load the package.
load_all()

## Show all exported objects.
ls("package:legTools")
packageVersion("legTools")

## build()
## build_win()
