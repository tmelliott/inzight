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

    # switch(action$action,
    #     'LOAD_DATA'
    # )
    inzstate(
        d = dispatch(state$docs, action),
        c = dispatch(state$controls, action)
    )
}

#' inzight state generator
#'
#' @param docs an `inzdocs` object
#' @param active integer indicating the document currently being used
#' @param controls an `inzcontrols` object
#' @param settings an `inzsettings` object
#'
#' @return an `inzstate` object
#' @export
#' @md
inzstate <- function(docs = inzdocs(),
                     active = 0L,
                     controls = inzcontrols(),
                     settings = inzsettings()
                     ) {

    state <- environment()
    class(state) <- "inzstate"
    state
}

#' @export
as_list.inzstate <- function(x) {
    list(
        docs = as_list(x$docs),
        active = x$active,
        controls = as_list(x$controls),
        settings = as_list(x$settings)
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
