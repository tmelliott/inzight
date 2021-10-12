#' inzight document constructor
#'
#' @param path the location where the `duckdb` data is stored on the server
#' @param name the name of the dataset as displayed to users
#'
#' @return an `inzdoc` object
#'
#' @export
#' @md
#' @examples
#' doc(iris, name = "Iris Data")
doc <- function(path, name = deparse(substitute(data))) {
    con <- duckdb::dbConnect(duckdb::duckdb(path))
    on.exit(duckdb::dbDisconnect(con, shutdown = TRUE))

    structure(
        list(
            path = path,
            name = name,
            colnames = duckdb::dbListFields(con, "Census%20at%20School-500")
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
