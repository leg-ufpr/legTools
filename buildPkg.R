## require(roxygen2)
## setwd("/home/walmes/GitHub/")
## roxygenise(package.dir="wzRfun")

require(devtools)

if(!grepl(x=getwd(), pattern="/legTools$")){
    stop("Move to /legTools directory.")
}

## Create/update NAMESPACE, *.Rd files.
document()
check_doc()

check()
## Updating legTools documentation
## Loading legTools
## Warning message:
## In setup_ns_exports(pkg, export_all) :
##   Objects listed as exports, but not present in namespace: wgpigs

## build()
## build_win()
