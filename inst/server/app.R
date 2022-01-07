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
    state <- as_list(state)
    action <- as_list(action)
    state <- do.call(inzstate, state)
    action <- do.call(inzaction, action)

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
