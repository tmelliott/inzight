#' inzight document constructor
#'
#' @param data the dataset contained within the document
#' @param name the name of the dataset as displayed to users
#'
#' @return an `inzdoc` object
#'
#' @export
#' @md
#' @examples
#' doc(iris, name = "Iris Data")
doc <- function(data, name = deparse(substitute(data))) {
    structure(
        list(
            data = data,
            name = name
        ),
        class = "inzdoc"
    )
}

#' @export
print.inzdoc <- function(x, ..., list_style = "- ") {
    cat(list_style, x$name, '\n', sep = "")
}

dispatch.inzdoc <- function(state, action) {

}
