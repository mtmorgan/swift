.swdownload_object <-
    function(curl, hdr, container, object, destination, overwrite)
{
    if (missing(destination))
        destination <- sprintf("%s_%s", tempfile(), object)
    if (!file.exists(dirname(destination)))
        dir.create(dirname(destination), recursive=TRUE)

    url <- sprintf("%s/%s/%s", hdr[["X-Storage-Url"]], container, object)
    auth <- sprintf("%s: %s", "X-Auth-Token", hdr[["X-Storage-Token"]])
    resp <- GET(url, config(httpheader=auth), progress(), 
                write_disk(destination, overwrite=overwrite),
                curl=curl)
    cat("\n")
    stop_for_status(resp)

    destination
}

.swdownload_container <-
    function(curl, hdr, container, destination, overwrite)
{
    if (missing(destination))
        destination <- tempfile()
    if (!file.exists(destination))
        dir.create(destination, recursive=TRUE)

    ## objects
    url <- sprintf("%s/%s?format=json", hdr[["X-Storage-Url"]], container)
    auth <- sprintf("%s: %s", "X-Auth-Token", hdr[["X-Storage-Token"]])
    objects <- GET(url, config(httpheader=auth), curl=curl)
    stop_for_status(objects)

    for (object in content(objects)) {
        ##  FIXME: what's this for?
        url <- sprintf("%s/%s?format=json&marker=%s", hdr[["X-Storage-Url"]],
                       container, object$name)
        json <- GET(url, config(httpheader=auth), curl=curl)
        stop_for_status(json)
        
        .swdownload_object(curl, hdr, container, object=object$name,
                           destination=file.path(destination, object$name),
                           overwrite)
    }

    destination
}

swdownload <-
    function(container, object, destination, overwrite=FALSE)
{
    stopifnot(.isSingleString(container))
    stopifnot(missing(object) || .isSingleString(object))
    stopifnot(missing(destination) || .isSingleString(destination))

    if (!overwrite && !missing(destination) && file.exists(destination))
        stop("'destination' exists, and overwrite is 'FALSE'",
             "\n  destination:", destination, call.=FALSE)

    curl <- RCurl::getCurlHandle()
    hdr <- .swauth(curl)
 
   if (!missing(object))
        .swdownload_object(curl, hdr, container, object, destination, overwrite)
    else
        .swdownload_container(curl, hdr, container, destination, overwrite)
}
