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
    function(curl, hdr, container, destination, overwrite, ...,
             prefix=NULL, delimiter=NULL, marker=NULL)
{
    stopifnot(is.null(prefix) || .isString(prefix))
    stopifnot(is.null(delimiter) || .isString(delimiter, nchar=1L))
    stopifnot(is.null(marker) || .isString(marker))
    if (missing(destination))
        destination <- tempfile()
    if (!file.exists(destination))
        dir.create(destination, recursive=TRUE)

    repeat {                            # Objects
        query <- .RESTquery(format="json", prefix=prefix,
            delimiter=delimiter, marker=marker, ...)
        path <- sprintf("/%s%s", container, query)
        contents <- .swcontent(curl, hdr, path)
        if (identical(attr(contents, "status"), "complete"))
            break
        marker <- attr(contents, "marker")

        for (object in contents)
            .swdownload_object(curl, hdr, container, object=object$name,
                destination=file.path(destination, object$name), overwrite)
    }
    
    destination
}

swdownload <-
    function(container, object, destination, overwrite=FALSE, ...)
{
    stopifnot(.isString(container))
    stopifnot(missing(object) || .isString(object))
    stopifnot(missing(destination) || .isString(destination))
    if (!overwrite && !missing(destination) && file.exists(destination))
        stop("'destination' exists, and overwrite is 'FALSE'",
             "\n  destination:", destination, call.=FALSE)

    curl <- RCurl::getCurlHandle()
    hdr <- .swauth(curl)
 
   if (!missing(object))
        .swdownload_object(curl, hdr, container, object, destination, overwrite)
    else
        .swdownload_container(curl, hdr, container, destination, overwrite, ...)
}
