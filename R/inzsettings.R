#' inzight settings constructor
#'
#' @param ... settings to be specified
#'
#' @return an `inzsettings` object
#'
#' @export
#' @md
inzsettings <- function(...) {
    default_settings <- inzsettings_defaults()
    new_settings <- list(...)

    mismatch <- names(new_settings) %in% names(default_settings)
    if (any(mismatch)) {
        warning("Some settings aren't supported: ",
            paste(names(mismatch)[which(mismatch)], collapse = ", ")
        )
    }
    if (any(!mismatch))
        default_settings <- modifyList(defaut_settings, new_settings[!mismatch])

    settings <- default_settings
    rm(default_settings)
    rm(new_settings)
    rm(mismatch)

    self <- environment()
    class(self) <- "inzsettings"
    self
}

#' Default settings list
#'
#' @export
inzsettings_defaults <- function() {
    list(
        #' @param point_colour the colour of points
        point_colour = "black"
    )
}

#' @export
print.inzsettings <- function(x, ...) {
    cat("inzight settings\n")
    paste0("- ", names(x$settings), ": ", as.character(x$settings), "\n")
    invisible()
}

#' @export
dispatch.inzsettings <- function(state, action) {
    switch(action$action,
        state
    )
}
