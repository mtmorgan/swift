swread <-
    function(container, object, ..., as=NULL, type=NULL)
{
    stopifnot(.isString(container))
    if (missing(object))
        object <- swlist(container, ..., format="abbrv")
    stopifnot(.isString(object))

    curl <- RCurl::getCurlHandle()
    hdr <- .swauth(curl)
    url <- .RESTurl(hdr[["X-Storage-Url"]], container, object)
    .RESTread(curl, hdr, url, as, type)
}

swsave <-
    function(x, container, object, ..., mode=c("create", "replace"))
{
    stopifnot(.isString(container))
    stopifnot(.isString(object))
    mode <- match.arg(mode)
    .stop_for_writable(container, object, mode)

    curl <- RCurl::getCurlHandle()
    hdr <- .swauth(curl)
    url <- .RESTurl(hdr[["X-Storage-Url"]], container, object)
    .RESTsave(curl, hdr, url, serialize(x, NULL))
}

swload <-
    function(container, object, ...)
{
    stopifnot(.isString(container))
    if (missing(object))
        object <- swlist(container, ..., format="abbrv")
    stopifnot(.isString(object))

    curl <- RCurl::getCurlHandle()
    hdr <- .swauth(curl)
    url <- .RESTurl(hdr[["X-Storage-Url"]], container, object)
    unserialize(.RESTread(curl, hdr, url, "raw", NULL))
}
    
