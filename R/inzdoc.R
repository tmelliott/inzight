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
#' inzdoc(iris, name = "Iris Data")
inzdoc <- function(key, name = deparse(substitute(data))) {
    get_data <- function(n = 0L) {
        con <- mongolite::mongo(collection = key, url = Sys.getenv('INZIGHT_MONGODB_URL'))
        on.exit(con$disconnect())
        con$find(limit = n)
    }
    colnames <- function() colnames(doc$data(1))

    self <- environment()
    class(self) <- "inzdoc"
    self
}

#' @export
as_list.inzdoc <- function(x) {
    list(
        key = x$key,
        name = x$name,
        colnames = x$colnames
    )
}

#' @export
print.inzdoc <- function(x, ..., list_style = "- ") {
    cat(list_style, x$name, '\n', sep = "")
}

#' @export
dispatch.inzdoc <- function(state, action) {
    if (missing(state)) stop("Must supply a state")
    if (missing(action)) {
        warning("No action supplied")
        return(state)
    }
    if (!class(action) == "inzaction")
        stop("Must be an action created with inzaction()")


}

#' @export
viewDoc <- function(key, n = 10L, p = 1L) {
    DB_URL <- Sys.getenv('INZIGHT_MONGODB_URL')
    con <- mongolite::mongo(collection = key, url = DB_URL)
    on.exit(con$disconnect())
    con$find(limit = n)
}
