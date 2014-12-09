swserialize <-
    function(x, container, object, ..., mode=c("create", "replace"))
{
    stopifnot(.isString(container))
    stopifnot(.isString(object))
    mode <- match.arg(mode)
    .stop_for_upload_size(object.size(x), "swserialize()")
    .stop_for_writable(container, object, mode, "swserialize()")

    curl <- RCurl::getCurlHandle()
    hdr <- .swauth(curl)
    url <- .RESTurl(hdr[["X-Storage-Url"]], container, object)
    .RESTsave(curl, hdr, url, serialize(x, NULL))
    object
}

swunserialize <-
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
    
