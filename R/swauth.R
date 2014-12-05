.swauth <-
    function(curl, state=.state)
{
    if (!all(c("USER", "KEY", "AUTH") %in% ls(state)))
        stop("set credentials with 'swauth()'")

    resp <- GET(state[["AUTH"]],
                config(httpheader=c(
                         `X-Auth-User`=state[["USER"]],
                         `X-Auth-Key`=state[["KEY"]])),
                curl=curl)
    stop_for_status(resp)
    headers(resp)
}

swauth <-
    function(USER=Sys.getenv("ST_USER"), KEY=Sys.getenv("ST_KEY"),
             AUTH=Sys.getenv("ST_AUTH"))
{
    stopifnot(.isString(USER))
    stopifnot(.isString(KEY))
    stopifnot(.isString(AUTH))

    .state[["USER"]] <- USER
    .state[["KEY"]] <- KEY
    .state[["AUTH"]] <- AUTH
    invisible(as.list(.state)[c("USER", "KEY", "AUTH")])
}
