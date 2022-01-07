#' inzight documents list state
#' @param docs a list of documents (...inzdocument)
#' @param active integer identifying the currently active document
#' @return a inzdocuments object
#' @export
inzdocuments <- function(docs = list(),
                         active = length(docs)
                         ) {
    docs <- lapply(as.list(docs), \(x) {
        if (!inherits(x, "inzdocument")) {
            x$data <- x$data[[1]]
            x$name <- x$name[[1]]
            x$label <- x$label[[1]]
            do.call(inzdocument, x)
        } else x
    })
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

            args <- action$payload
            args <- args[!names(args) %in% c("name", "label")]
            data <- do.call(iNZightTools::smart_read, args)

            name <- ifelse(is.null(action$payload$name),
                basename(tools::file_path_sans_ext(action$payload$file)),
                action$payload$name
            )
            label <- ifelse(is.null(action$payload$label), name, action$payload$label)

            doc <- inzdocument(data = data, name = name, label = label)
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
#' @param data a data.frame or a data_store
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
    print(match.call())
    if (inherits(data, "data_store")) {
        store <- data
    } else if (is.data.frame(data)) {
        # store data somewhere
        file_path <- tempfile(pattern = name)
        store <- data_store(file_path, data)
    } else {
        store <- data_store(data)
    }

    if (!inherits(settings, "inzsettings")) settings <- do.call(inzsettings, settings)
    if (!inherits(controls, "inzcontrols")) controls <- do.call(inzcontrols, unclass(controls))

    self <- list(
        data = store,
        name = name,
        label = label,
        settings = settings,
        controls = controls
    )
    class(self) <- "inzdocument"
    self
}

#' @export
as_list.inzdocument <- function(x) {
    list(
        data = x$data$path,
        name = x$name,
        label = x$label,
        settings = as_list(x$settings),
        controls = as_list(x$controls)
    )
}

#' @export
print.inzdocument <- function(x, ...) {
    cli::cli_h2("{x$label} ({x$name})\n")

    print(head(x$data$get()))

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

#' Data Store class generator
#' @param x a data store constructor
#' @param data data to be stored (a data.frame)
#' @return a data_store object
#' @export
data_store <- function(x, data) UseMethod("data_store", x)

#' @describeIn data_store Default method for storing at a local path
#' @export
data_store.default <- function(x, data) {
    if (!missing(data)) {
        path <- paste(x, "rds", sep = ".")
        cli::cli_alert_info("Storing data in {.strong {path}}")
        saveRDS(data, file = path)
    } else {
        path <- x
    }

    self <- list(
        path = path,
        get = function() readRDS(path),
        close = function() {
            unlink(path)
            cli::cli_alert_info("Deleted {.strong {path}}")
        }
    )
    class(self) <- c("file_data_store", "data_store")
    self
}

#' @export
as_list.data_store <- function(x) list(data = x$path)
