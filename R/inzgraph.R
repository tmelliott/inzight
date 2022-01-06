#' inzight graph state
#' @param path location to store the file
#' @param dimensions width and height of the plot, in pixels
#' @return an inzgraph object
#' @export
inzgraph <- function(path = tempfile(fileext = ".png"),
                    dimensions = c(600, 400)
                    ) {
    self <- list(
        path = path,
        dimensions = dimensions
    )
    class(self) <- "inzgraph"
    self
}

#' @export
print.inzgraph <- function(x, ...) {
    cli::cli_dl()
    cli::cli_li(c(Path = cli::col_green(x$path)))
    cli::cli_li(c(Dimensions = cli::col_red(paste0(x$dimensions, "px", collapse = " x "))))
}

#' @describeIn inzgraph Dispatch method for inzgraph
#' @export
dispatch.inzgraph <- function(state, action) {
    cli::cli_h1("Dispatching action for inzgraph")
    cat("\n")
    print(action)

    switch(action$action,
        'UPDATE_DOC' = ,
        'CHANGE_DOC' = ,
        'UPDATE_SETTINGS' = {
            cli::cli_h2('UPDATING PLOT')
            state
        },
        state
    )
}
