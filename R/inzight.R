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
#' @param payload action payload
#' @return an inzaction object
#' @export
inzaction <- function(action, payload) {
    structure(
        list(action = action, payload = payload),
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

#' Convert to a list
#' @param x an object to convert to list
#' @return a list
#' @export
as_list <- function(x) UseMethod("as_list", x)

#' @describeIn as_list Default method
#' @export
as_list.default <- function(x) {
    if (is.data.frame(x)) {
        return(lapply(seq_len(nrow(x)), \(i) unclass(x[i])))
    }
    if (!is.list(x)) return(x)
    unclass(
        lapply(x, as_list)
    )
}
