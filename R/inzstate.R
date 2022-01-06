#' inzight state object
#' @param documents a list of documents (inzdocuments)
#' @param settings a list of settings (inzsettings)
#' @param graph a graph object (inzgraph)
#' @return an inzstate object
#' @export
inzstate <- function(documents = inzdocuments(),
                     settings = inzsettings(),
                     graph = inzgraph()
                     ) {
    self <- list(
        documents = documents,
        settings = settings,
        graph = graph
    )
    class(self) <- "inzstate"
    self
}

#' @export
print.inzstate <- function(x, ...) {
    cli::cli_h1("inzight state")

    cli::cli_h2("Documents")
    print(x$documents)

    cat("\n")
    cli::cli_h2("Settings")
    print(x$settings)

    cat("\n")
    cli::cli_h2("Graph")
    print(x$graph)
}

#' @describeIn inzstate Dispatch method for inzstate
#' @export
dispatch.inzstate <- function(state, action) {
    cli::cli_h1("Dispatching action for inzstate")
    cat("\n")
    print(action)

    newstate <- do.call(inzstate, lapply(state, dispatch, action = action))

    # some checks
    cli::cli_h1("Checking for changes")
    if (!identical(state, newstate)) {
        cat("\n")
        check <- logical(length(state))
        names(check) <- names(state)
        for (i in seq_along(state)) {
            check[i] <- identical(state[[i]], newstate[[i]])
        }
        cat(paste0("[", ifelse(check, " ", "*"), "] ", names(state)), sep = "\n")

        if (!check["documents"]) {
            if (state$documents$active == newstate$documents$active) {
                # updating document
                action <- inzaction("UPDATE_DOC",
                    data = newstate$documents$docs[[newstate$documents$active]]
                )
            } else {
                # switching to a new document
                action <- inzaction("CHANGE_DOC",
                    data = newstate$documents$docs[[newstate$documents$active]]
                )
            }

            newstate <- do.call(inzstate, lapply(newstate, dispatch, action = action))
        }

        if (!check["settings"]) {
            action <- inzaction("UPDATE_SETTINGS",
                settings = list()
            )
            newstate <- do.call(inzstate, lapply(newstate, dispatch, action = action))
        }
    }

    newstate
}
