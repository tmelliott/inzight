#' inzight controls constructor
#'
#' @param v1 the name of the first variable
#' @param v2 the name of the second variable
#' @param g1 the name of the first subset variable
#' @param g2 the name of the second subset variable
#'
#' @return an `inzcontrols` object
#'
#' @export
#' @md
controls <- function(v1 = NULL, v2 = NULL, g1 = NULL, g2 = NULL) {
    structure(
        list(
            v1 = v1,
            v2 = v2,
            g1 = g1,
            g2 = g2
        ),
        class = "inzcontrols"
    )
}

#' @export
print.inzcontrols <- function(x, ...) {
    cat("inzight controls\n")
    cat("- variable 1:", ifelse(is.null(x$v1), "not set", x$v1), "\n")
    if (!is.null(x$v2)) cat("- variable 2:", x$v2, "\n")
    if (!is.null(x$g1)) cat("- subset variable 2:", x$g1, "\n")
    if (!is.null(x$g2)) cat("- subset variable 2:", x$g2, "\n")
    invisible()
}

#' @export
dispatch.inzcontrols <- function(state, action) {
    switch(action$action,
        'SET_V1' = {
            modifyList(state, list(v1 = action$payload$name))
        },
        state
    )
}
