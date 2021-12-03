#' inzight document constructor
#'
#' @param path the location of the `SQLite` database on the server
#' @param name the name of the dataset as displayed to users
#'
#' @return an `inzdoc` object
#'
#' @export
#' @md
#' @examples
#' doc(iris, name = "Iris Data")
doc <- function(path, name = deparse(substitute(data))) {
    con <- RSQLite::dbConnect(RSQLite::SQLite(), path)
    on.exit(RSQLite::dbDisconnect(con))

    structure(
        list(
            path = path,
            name = name,
            colnames = RSQLite::dbListFields(con, "Census%20at%20School-500")
        ),
        class = "inzdoc"
    )
}

#' @export
print.inzdoc <- function(x, ..., list_style = "- ") {
    cat(list_style, x$name, '\n', sep = "")
}

#' @export
dispatch.inzdoc <- function(state, action) {

}
