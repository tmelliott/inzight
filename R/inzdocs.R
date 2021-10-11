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
            list(NULL),
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
    if (length(x) == 1L && length(x[[1]]) == 0L) {
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
            doc <- doc(
                data = do.call(
                    iNZightTools::smart_read,
                    action$payload
                ),
                name = tools::file_path_sans_ext(
                    basename(action$payload$file)
                )
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
