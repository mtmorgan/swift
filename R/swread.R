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
