#' inzight document collection constructor
#'
#' @param ... list of documents
#' @return an `inzdocs` object, which is a list of `inzdoc` objects
#'
#' @export
#' @md
docs <- function(...) {
    x <- list(...)
    if (length(x) == 0L)
        x <- structure(
            list(),
            class = "inzdocs",
            active = 0L
        )
    else {
        x <- do.call(c, x)
        x <- x[sapply(x, length) > 0L]
        attr(x, "active") <- length(x)
        attr(x, "class") <- "inzdocs"
    }
    x
}

#' @export
c.inzdocs <- function(...) c.inzdoc(...)

#' @export
c.inzdoc <- function(...) {
    x <- list(...)
    x <- lapply(x,
        function(z)
            if (class(z) == "inzdocs") unclass(z)
            else list(z)
    )
    structure(do.call(c, x), class = "inzdocs")
}

#' @export
print.inzdocs <- function(x, ...) {
    if (length(x) == 0L || length(x[[1]]) == 0L) {
        cat("empty inzight document list\n")
        return()
    }
    cat('inzight document list\n')
    active <- attr(x, "active")
    if (active == 0L) lapply(x, print)
    else
        lapply(seq_along(x),
            function(i) {
                print(x[[i]],
                    list_style = ifelse(i == active, "[*] ", "[ ] ")
                )
            }
        )
    invisible()
}

#' @export
dispatch.inzdocs <- function(state, action) {
    if (missing(state)) stop("Must supply a state")
    if (missing(action)) {
        warning("No action supplied")
        return(state)
    }
    if (!class(action) == "inzaction")
        stop("Must be an action created with inzaction()")

    switch(action$action,
        'LOAD_DATA' = {
            # load data into a database and return the DB path
            # and perhaps a key of some kind ...
            data <- do.call(
                iNZightTools::smart_read,
                action$payload
            )
            name <- tools::file_path_sans_ext(
                basename(action$payload$file)
            )
            key <- paste(LETTERS[sample(20, replace = TRUE)], collapse = "")
            dir <- file.path(tempdir(), key)
            db_file <- file.path(dir, "data.sqlite")
            con <- RSQLite::dbConnect(RSQLite::SQLite(), db_file)
            on.exit(RSQLite::dbDisconnect(con))
            RSQLite::dbWriteTable(con, name, data)

            doc <- doc(
                path = db_file,
                name = name
            )
            dispatch(
                state,
                inzaction(
                    'ADD_DOCUMENT',
                    doc = doc
                )
            )
        },
        'ADD_DOCUMENT' = {
            docs(state, action$payload$doc)
        },
        state
    )
}
