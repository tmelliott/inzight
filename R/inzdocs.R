#' inzight document collection constructor
#'
#' @param ... list of documents
#' @return an `inzdocs` object, which is a list of `inzdoc` objects
#'
#' @export
#' @md
docs <- function(...) {
    docs <- new.env()
    x <- list(...)

    if (length(x) == 0L) {
        docs$.docs <- list()
        docs$.active <- 0L
    } else {
        x <- do.call(c, x)
        x <- x[sapply(x, length) > 0L]
        docs$.docs <- x
        docs$.active <- length(x)
    }

    docs$documents <- function() .docs
    docs$active <- function() .active
    docs$activeDoc <- function() .docs[[.active]]
    docs$setActive <- function(index) {
        "not sure if this works yet"
        if (index > length(.docs)) stop('index must be a valid document')
        if (index == 0L) stop('index must be > 0')
        .active <- index
    }
    docs$count <- function() length(.docs)

    attr(docs, "class") <- c("inzdocs", class(docs))
    docs
}

as_list.inzdocs <- function(x) {
    list(
        docs = lapply(x$documents(), as_list),
        active = x$active()
    )
}

#' @export
c.inzdocs <- function(...) c.inzdoc(...)

#' @export
c.inzdoc <- function(...) {
    x <- list(...)
    x <- lapply(x,
        function(z)
            if (any(class(z) == "inzdocs")) unclass(z$docs)
            else list(z)
    )
    docs <- docs(do.call(c, x))
}

#' @export
print.inzdocs <- function(x, ...) {
    if (x$count() == 0L || length(x$activeDoc()) == 0L) {
        cat("empty inzight document list\n")
        return()
    }
    cat('inzight document list\n')
    active <- x$active()
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

    # DB_USERNAME <- Sys.getenv('INZIGHT_MONOGODB_ADMINUSERNAME')
    # DB_PASSWORD <- Sys.getenv('INZIGHT_MONOGODB_ADMINPASSWORD')
    DB_URL <- Sys.getenv('INZIGHT_MONGODB_URL')

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
            con <- mongolite::mongo(collection = key, url = DB_URL)
            on.exit(con$disconnect())

            con$insert(data)

            doc <- doc(
                key = key,
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
