.swauth <-
    function(curl, state=.state)
{
    resp <- GET("https://tin.fhcrc.org/auth/v1.0",
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
    stopifnot(.isSingleString(USER))
    stopifnot(.isSingleString(KEY))
    stopifnot(.isSingleString(AUTH))

    .state[["USER"]] <- USER
    .state[["KEY"]] <- KEY
    .state[["AUTH"]] <- AUTH
    invisible(.state)
}
