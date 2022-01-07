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
#' @importFrom iNZightPlots inzplot iNZightPlot
dispatch.inzgraph <- function(state, action) {
    cli::cli_h1("Dispatching action for inzgraph")
    cat("\n")
    print(action)

    switch(action$action,
        'UPDATE_DOC' = ,
        'CHANGE_DOC' = ,
        'UPDATE_SETTINGS' = {
            cli::cli_h2('UPDATING PLOT')

            ctrls <- action$payload$data$controls$controls
            if (ctrls$v1$value == "") {
                cli::cli_alert_warning("Nothing to plot")
                return(state)
            }

            if (ctrls$v2$value == "") {
                fmla <- glue::glue("~ {ctrls$v1$value}")
            } else {
                fmla <- glue::glue("{ctrls$v1$value} ~ {ctrls$v2$value}")
            }
            fmla <- eval(parse(text = fmla))

            d <- action$payload$data$data$get()

            png(filename = state$path, width = state$dimensions[1], height = state$dimensions[2])
            on.exit(dev.off())

            iNZightPlots::inzplot(fmla, data = d)

            cli::cli_alert_info("Plot saved to {.strong {state$path}}")

            state
        },
        state
    )
}
