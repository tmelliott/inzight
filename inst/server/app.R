library(inzight)
library(plumber)

#* @apiTitle inzight state API

#* Get session info
#* @get /info
#* @serializer unboxedJSON
function() {
    list(
        r_version = as.character(getRversion()),
        inzight_version = as.character(packageVersion('inzight'))
    )
}

#* Create a new inzight state instance
#* @get /new
function() {
    as_list(inzight())
}

#* dispatch
#* @param state the current application state
#* @param action an action to perform on the state
#* @post /dispatch
function(state, action, ...) {
    # print(match.call())
    # cat("============ STATE:\n")
    # print(state)
    # cat("============ ACTION:\n")
    # print(action)

    # state <- jsonlite::fromJSON(state)
    state <- inzstate(
        d = docs(state$docs),
        c = do.call(controls, state$controls)
    )

    # action <- jsonlite::fromJSON(action)
    class(action) <- "inzaction"

    as_list(inzight(state, action))
}

#* Get data view
#* @param key
#* @param page
#* @param pageSize
#* @post /view
function(key, page = 10, pageSize = 10L) {
    as_list(viewDoc(key, n = pageSize, p = page))
}
