swlist <-
    function(container, format=c("short", "long"))
{
    stopifnot(.isSingleString(container))
    format <- match.arg(format)

    curl <- RCurl::getCurlHandle()
    hdr <- .swauth(curl)

    url <- sprintf("%s/%s?format=json", hdr[["X-Storage-Url"]], container)
    auth <- sprintf("%s: %s", "X-Auth-Token", hdr[["X-Storage-Token"]])
    objects <- GET(url, config(httpheader=auth), curl=curl)
    stop_for_status(objects)

    content <- content(objects)

    bytes <- sapply(content, "[[", "bytes")
    last_modified <- sapply(content, "[[", "last_modified")
    name <- sapply(content, "[[", "name")

    switch(format, short={
        FUN <- utils:::format.object_size
        size <- sapply(bytes, FUN, "auto")
        data.frame(size=size, last_modified=last_modified, name=name,
                   stringsAsFactors=FALSE)
    }, long={
        hash <- sapply(content, "[[", "hash")
        data.frame(bytes=bytes, last_modified=last_modified, hash=hash,
                   name=name, stringsAsFactors=FALSE)
    })
}
