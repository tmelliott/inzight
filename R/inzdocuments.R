#' inzight documents list state
#' @param docs a list of documents (...inzdocument)
#' @param active integer identifying the currently active document
#' @return a inzdocuments object
#' @export
inzdocuments <- function(docs = list(),
                         active = length(docs)
                         ) {
    self <- list(
        docs = docs,
        active = active
    )
    class(self) <- "inzdocuments"
    self
}

#' @export
print.inzdocuments <- function(x, ...) {
    if (length(x$docs) == 0L) {
        cat(cli::style_italic("empty document list\n"))
        return()
    }
    cat("documents:\n")
    for (i in seq_along(x$docs)) {
        cat(sprintf("[%s] %s\n", ifelse(i == x$active, "*", " "), x$docs[[i]]$label))
    }
}

#' @describeIn inzdocuments Dispatch method for inzdocuments
#' @export
dispatch.inzdocuments <- function(state, action) {
    # will forward dispatch to *active* document (unless it's a delete/rename/etc action)
    cli::cli_h1("Dispatching action for inzdocuments")
    cat("\n")
    print(action)

    switch(action$action,
        'LOAD_DATA' = {
            cli::cli_h2('LOADING DATA')
            documents <- state$docs
            doc <- inzdocument(action$payload$data, action$payload$name)
            documents <- c(documents, list(doc))
            inzdocuments(documents)
        },
        'CHANGE_DOC' = {
            state
        },
        {
            documents <- lapply(state$docs, dispatch, action = action)
            inzdocuments(documents)
        }
    )
}

#' inzight document state
#' @param data a data.frame
#' @param name R object name
#' @param label human readable name (spaces etc OK)
#' @param settings document specific settings (inzsettings)
#' @param controls a list of global/user controls (inzcontrols)
inzdocument <- function(data,
                        name = deparse(substitute(data)),
                        label = name,
                        settings = inzsettings(),
                        controls = inzcontrols(variables = names(data))
                        ) {
    self <- list(
        data = data,
        name = name,
        label = label,
        settings = settings,
        controls = controls
    )
    class(self) <- "inzdocument"
    self
}

#' @export
print.inzdocument <- function(x, ...) {
    cli::cli_h2("{x$label} ({x$name})\n")

    print(head(x$data))

    cat("\n")
    cli::cli_h3("Controls")
    print(x$controls)

}

#' @describeIn inzdocument Dispatch method for inzdocument
#' @export
dispatch.inzdocument <- function(state, action) {
    cli::cli_h1("Dispatching action for inzdocument")
    cat("\n")
    print(action)

    switch(action$action,
        do.call(inzdocument, lapply(state, dispatch, action = action))
    )
}
