#' @export
inzaction <- function(action, ...) {
    structure(
        list(action = action, payload = list(...)),
        class = "inzaction"
    )
}
