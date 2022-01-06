#' inzight state management
#' @param state current application state (inzstate)
#' @param action action to apply (inzaction)
#' @return an inzstate
#' @export
inzight <- function(state, action) {
    if (missing(state)) return(inzstate())
    if (missing(action)) return(state)

    newstate <- dispatch(state, action)
    # do some checks ... does this need to be re-actioned?
    newstate
}

#' inzight action dispatch method
#' @param state appliation state (inzstate)
#' @param action action to apply (inzaction)
#' @return an inzstate
#' @export
dispatch <- function(state, action) UseMethod("dispatch", state)

#' @describeIn dispatch Default method
#' @export
dispatch.default <- function(state, action) state

#' inzight action constructor
#' @param action the name of the action
#' @param ... action payload
#' @return an inzaction object
#' @export
inzaction <- function(action, ...) {
    structure(
        list(action = action, payload = list(...)),
        class = "inzaction"
    )
}

#' @export
print.inzaction <- function(x, ...) {
    cli::cli_text("Action: {.strong {x$action}}")
    cli::cli_text("Payload contents:")
    cli::cli_ul()
    lapply(names(x$payload), \(y) {
        z <- x$payload[[y]]
        if (!is.character(z)) {
            if (is.numeric(z)) z <- as.character(z)
            else z <- class(z)
        }
        cli::cli_li("{y}: {.emph {z}}")
    })
}
