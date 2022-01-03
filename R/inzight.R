#' inzight state reducer
#'
#' Reduces a state based on a given action.
#'
#' @param state an `inzstate` object
#' @param action an `inzaction` object
#'
#' @return an `inzstate` object
#' @md
#' @export
#' @examples
#' app <- inzight()
#' tf <- tempfile("iris", fileext = ".csv")
#' write.csv(iris, tf, row.names = FALSE, quote = FALSE)
#' on.exit(unlink(tf))
#' action <- inzaction('LOAD_DATA', file = tf)
#' inzight(app, action)
inzight <- function(state = inzstate(), action) {
    if (missing(action)) return(state)
    inzstate(
        d = dispatch(state$docs, action),
        c = dispatch(state$controls, action)
    )
}

#' inzight state generator
#'
#' @param d an `inzdocs` object
#' @param c an `inzcontrols` object
#'
#' @return an `inzstate` object
#' @export
#' @md
inzstate <- function(d = docs(), c = controls()) {
    structure(
        list(
            docs = d,
            controls = c
        ),
        class = "inzstate"
    )
}

#' @export
print.inzstate <- function(x, ...) {
    print(x$docs)
    cat("\n")
    print(x$controls)
}

#' @export
dispatch <- function(state, action) UseMethod("dispatch", state)

#' @export
as_list <- function(x) UseMethod("as_list", x)

#' @export
as_list.default <- function(x) {
    if (!is.list(x)) return(x)
    unclass(
        lapply(x, as_list)
    )
}
