#' inzight document collection constructor
#'
#' @param ... list of documents
#' @return an `inzdocs` object, which is a list of `inzdoc` objects
#'
#' @export
#' @md
inzdocs <- function(...) {
    documents <- list(...)

    if (length(documents) == 0L) {
        documents <- list()
    } else {
        documents <- do.call(c, documents)
        #documents <- documents[sapply(documents, \(d) d$key) != ""]
    }

    self <- environment()
    class(self) <- "inzdocs"
    self
}

as_list.inzdocs <- function(x) {
    list(
        documents = lapply(x$documents, as_list)
    )
}

#' @export
c.inzdocs <- function(...) c.inzdoc(...)

#' @export
c.inzdoc <- function(...) {
    x <- list(...)
    x <- lapply(x,
        function(z) {
            if (any(class(z) == "inzdocs")) z$documents
            else list(z)
        }
    )
    x <- x[sapply(x, \(d) d$key) != ""]
    inzdocs(do.call(c, x))
}

#' @export
print.inzdocs <- function(x, active = 0L, ...) {
    n <- length(x$documents)
    if (n == 0L || x$documents[[n]]$key == "") {
        cat("empty inzight document list\n")
        return()
    }
    cat('inzight document list\n')
    if (active == 0L) lapply(x$documents, print)
    else
        lapply(seq_along(x$documents),
            function(i) {
                print(x$documents[[i]],
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

            doc <- inzdoc(
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
            inzdocs(state, action$payload$doc)
        },
        state
    )
}
