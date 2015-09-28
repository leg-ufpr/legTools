#' @title knit to wordpress that uses crayon sintax highlight
#'
#' @name knit2wpCrayon
#'
#' @description This function improves \code{RWordPress::knit2wp} to
#'     allow properly render code when using Crayon Sintax Highlighter
#'     in Wordpress.
#'
#' @param input a markdown or Rmarkdown file.
#' @param title title for the post.
#' @param ... other meta information of the post. See
#      \link[RWordPress]{knit2wp}.
#' @param action indicates a new post, a edition of the post or a new
#'     page.
#' @param postid number of the post.
#' @param encoding ht encoding of the input file.
#' @param upload logical, if the file is to be updated to the blog.
#' @param publish logical, if the post is to be published or stay in
#'     draft mode.
#' @param write logical, if the result of knit should be written to a
#'     html file. This is useful to copy from this file and paste inside
#'     the Wordpress editor (on text mode, not visual mode).
#'
#' @return Nothing is returned by the function.
#'
#' @author Walmes Zeviani, \email{walmes@@ufpr.br}
#'
#' @export
#' @examples
#' \donttest{
#'
#' library(knitr)
#' library(RWordPress)
#'
#' post <- "2015-08-24_polyGui.Rmd"
#' title <- "Interface para regressão polinomial"
#' categ <- c("gui", "rbloggers_pt")
#' keywd <- c("gWdigets", "legTools", "lm", "poly")
#'
#' pass <- scan(n=1, what=character())
#' options(WordpressLogin=c(walmes=pass),
#'         WordpressURL="http://blog.leg.ufpr.br/xmlrpc.php")
#'
#' knit2wpCrayon(post, title=title,
#'               action="editPost", postid=179,
#'               categories=categ, mt_keywords=keywd,
#'               ## write=TRUE, upload=FALSE,
#'               write=FALSE, upload=TRUE,
#'               publish=FALSE)
#'
#' }
knit2wpCrayon <- function(input, title="A post from knitr", ...,
                          action=c("newPost", "editPost", "newPage"),
                          postid, encoding=getOption("encoding"),
                          upload=FALSE, publish=FALSE, write=TRUE){
    ##
    ##-------------------------------------------
    ## Loading required packages.
    ##
    if (!requireNamespace("knitr", quietly=TRUE)){
        stop("`knitr` needed for this function to work. Please install it.",
             call.=FALSE)
    }
    if (!requireNamespace("markdown", quietly=TRUE)){
        stop("`markdown` needed for this function to work. Please install it.",
             call.=FALSE)
    }
    if (!requireNamespace("RWordPress", quietly=TRUE)){
        stop("`RWordPress` needed for this function to work. Please install it.",
             call.=FALSE)
    }
    ##
    ##-------------------------------------------
    ## knitr:::native_encode().
    ##
    native_encode <- function (x, to = ""){
        idx = Encoding(x) == "UTF-8"
        x2 = iconv(x, if (any(idx)) "UTF-8" else "", to)
        if (!any(is.na(x2)))
            return(x2)
        warning("some characters may not work under the current locale")
        x
    }
    ##
    ##-------------------------------------------
    ## Modifed body of the knit2wp().
    ##
    out <- knitr::knit(input, encoding=encoding)
    on.exit(unlink(out))
    con <- file(out, encoding=encoding)
    on.exit(close(con), add=TRUE)
    content <- native_encode(readLines(con, warn=FALSE))
    content <- paste(content, collapse="\n")
    content <- markdown::markdownToHTML(text=content, fragment.only=TRUE)
    content <- gsub(
        pattern="<pre><code class=\"([[:alpha:]]+)\">(.+?)</code></pre>",
        replacement="<pre class=\"lang:\\1 decode:true\">\\2</pre>",
        x=content)
    content=native_encode(content, "UTF-8")
    title=native_encode(title, "UTF-8")
    if (write){
        writeLines(text=content,
                   con=gsub(x=out, pattern="\\.md$", replacement=".html"))
    }
    if (upload){
        action=match.arg(action)
        WPargs=list(content=list(description=content, title=title,
                        ...), publish=publish)
        if (action=="editPost")
            WPargs=c(postid=postid, WPargs)
        do.call("library", list(package="RWordPress", character.only=TRUE))
        print(do.call(action, args=WPargs))
    }
}
