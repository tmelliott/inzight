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
doc <- function(key, name = deparse(substitute(data))) {
    DB_USERNAME <- Sys.getenv('INZIGHT_MONOGODB_ADMINUSERNAME')
    DB_PASSWORD <- Sys.getenv('INZIGHT_MONOGODB_ADMINPASSWORD')
    DB_URL <- Sys.getenv('INZIGHT_MONGODB_URL')

    con <- mongolite::mongo(collection = key, url = DB_URL)
    on.exit(con$disconnect())

    structure(
        list(
            key = key,
            name = name,
            colnames = colnames(con$find(limit = 1L)),
            db_url = DB_URL
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
