# legTools 0.1.1

An R package containing convenience functions, small GUI to teach
statistics and some datasets.

## Introduction

legTools is a collection of R functions and datasets, maintained by the
LEG group (Statistics and Geoinformation Laboratory, *Laboratório de
Estatística e Geoinformação*).

## Download and install

### Linux/Mac

Use the `devtools` package (available from
[CRAN](http://cran-r.c3sl.ufpr.br/web/packages/devtools/index.html)) to
install automatically from this GitLab repository:

```r
library(devtools)
install_git("http://git.leg.ufpr.br/leg/legTools.git")
```

Alternatively, download the package tarball: [legTools_0.1.1.tar.gz][]
and run from a UNIX terminal (make sure you are on the container file
directory):

```shell
R CMD INSTALL -l /path/to/your/R/library legTools_0.1.1.tar.gz
```

Or, inside an `R` session:

```r
install.packages("legTools_0.1.1.tar.gz", repos = NULL,
                 lib.loc = "/path/to/your/R/library"
                 dependencies = TRUE)
```

Note that `-l /path/to/your/R/library` in the former and `lib.loc =
"/path/to/your/R/library"` in the latter are optional. Only use it if you
want to install in a personal library, other than the standard R
library. 

### Windows

Download Windows binary version: [legTools_0.1.1.zip][] (**do not unzip
it under Windows**), put the file in your working directory, and from
inside `R`:

```r
install.packages("legTools_0.1.1.zip", repos = NULL,
                 dependencies = TRUE)
```

## Documentation

The reference manual in PDF can be found here: [legTools-manual.pdf][]

## Contributing

This R package is develop using [`roxygen2`][] for documentation and
[`devtools`] to check and build. Please, see the
[instructions for contributing](./contributing.md) to collaborate.

## License

This package is released under the
[GNU General Public License (GPL) v3.0][].

See [LICENSE](./LICENSE)


<!-- links -->

[GNU General Public License (GPL) v3.0]: http://www.gnu.org/licenses/gpl-3.0.html
[`roxygen2`]: https://github.com/klutometis/roxygen
[`devtools`]: https://github.com/hadley/devtools
[legTools_0.1.1.tar.gz]: http://www.leg.ufpr.br/~fernandomayer/legTools/legTools_0.1.1.tar.gz
[legTools_0.1.1.zip]: http://www.leg.ufpr.br/~fernandomayer/legTools/legTools_0.1.1.zip
[legTools-manual.pdf]: http://www.leg.ufpr.br/~fernandomayer/legTools/legTools-manual.pdf
