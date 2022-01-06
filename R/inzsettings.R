#' inzight settings state
#' @param settings a list of settings
#' @return an inzsettings object
#' @export
inzsettings <- function(settings = list()) {
    self <- list(settings = settings)
    class(self) <- "inzsettings"
    self
}

#' @describeIn inzsettings Dispatch method for inzsettings
#' @export
dispatch.inzsettings <- function(state, action) {
    cli::cli_h1("Dispatching action for inzsettings")
    cat("\n")
    print(action)

    state
}
