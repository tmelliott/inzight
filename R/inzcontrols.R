#' inzight controls state
#' @param controls a list of controls
#' @param variables variable names in the dataset
#' @return a inzcontrols object
#' @export
inzcontrols <- function(controls, variables = character()) {
    if (missing(controls)) {
        controls <- list(
            v1 = inzcontrol("Variable 1", variables),
            v2 = inzcontrol("Variable 2", variables),
            g1 = inzcontrol("Subset Variable 1", variables),
            g2 = inzcontrol("Subset Variable 2", variables)
        )
    }

    controls <- lapply(controls, \(x) {
        if (!inherits(x, "inzcontrol")) do.call(inzcontrol, x) else x
    })

    self <- list(
        controls = controls,
        variables = variables
    )
    class(self) <- "inzcontrols"
    self
}

#' @export
print.inzcontrols <- function(x, ...) {
    cat("\n")
    lapply(x$controls, \(z) {
        print(z)
        cat("\n")
    })
}

#' @describeIn inzcontrols Dispatch method for inzcontrols
#' @export
dispatch.inzcontrols <- function(state, action) {
    # again will forward to each control
    cli::cli_h1("Dispatching action for inzcontrols")
    cat("\n")
    print(action)

    switch(action$action,
        'SET_V1' = ,
        'SET_V2' = ,
        'SET_G1' = ,
        'SET_G2' = {
            newstate <- state
            v <- tolower(gsub("SET_", "", action$action))
            action$action <- "SET_VARIABLE"
            newstate$controls[[v]] <- dispatch(newstate$controls[[v]], action)

            # change all the options and dispatch to all children
            used_vars <- sapply(newstate$controls, \(x) x$value)
            used_vars <- as.character(used_vars[used_vars != ""])
            available_vars <- newstate$variable[!newstate$variables %in% used_vars]
            action <- inzaction("UPDATE_OPTIONS", list(variables = available_vars))
            ctrls <- lapply(newstate$controls, dispatch, action = action)

            inzcontrols(ctrls, state$variables)
        },
        state
    )
}

#' inzight control state
#' @param name the name of the control (displayed to users)
#' @param options valid options for the control
#' @param value the chosen value
#' @param slider optional, a slider object associated with the control (inzslider)
inzcontrol <- function(name, options, value = "", slider = NULL) {
    self <- list(
        name = name,
        options = options,
        value = value,
        slider = slider
    )
    class(self) <- "inzcontrol"
    self
}

#' @export
print.inzcontrol <- function(x, ...) {
    cli::cli_text("{.strong {x$name}}")
    cat(paste0("[", ifelse(x$options == x$value, "*", " "), "] ", x$option), sep = "\n")
}

#' @describeIn inzcontrol Dispatch method for inzcontrol
#' @export
dispatch.inzcontrol <- function(state, action) {
    cli::cli_h1("Dispatching action for inzcontrol")
    cat("\n")
    print(action)

    switch(action$action,
        'SET_VARIABLE' = {
            cli::cli_h2("Setting {.emph {state$name}} to {.emph {action$payload$value}}")
            newstate <- state
            newstate$value <- action$payload$value
            newstate
        },
        'UPDATE_OPTIONS' = {
            cli::cli_h2("Updating options for {.emph {state$name}}")
            newstate <- state
            opts <- action$payload$variables
            if (newstate$value != "") opts <- c(newstate$value, opts)
            newstate$options <- opts
            newstate
        },
        state
    )
}
