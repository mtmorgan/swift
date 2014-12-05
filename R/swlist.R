swlist <-
    function(container)
{
    stopifnot(.isSingleString(container))

    curl <- RCurl::getCurlHandle()
    hdr <- .swauth(curl)

    url <- sprintf("%s/%s?format=json", hdr[["X-Storage-Url"]], container)
    auth <- sprintf("%s: %s", "X-Auth-Token", hdr[["X-Storage-Token"]])
    objects <- GET(url, config(httpheader=auth), curl=curl)
    stop_for_status(objects)

    content <- content(objects)
    data.frame(bytes=sapply(content, "[[", "bytes"),
               last_modified=sapply(content, "[[", "last_modified"),
               name=sapply(content, "[[", "name"))
}
