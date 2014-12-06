swlist <-
    function(container=NULL, format=c("short", "long"), ...,
             prefix=NULL, delimiter=NULL)
{
    stopifnot(missing(container) || .isString(container))
    format <- match.arg(format)
    stopifnot(is.null(prefix) || .isString(prefix))
    stopifnot(is.null(delimiter) || .isString(delimiter, nchar=1L))
    marker <- NULL

    curl <- RCurl::getCurlHandle()
    hdr <- .swauth(curl)
    
    result <- new.env(parent=emptyenv())
    ith <- 0L
    repeat {
        url <- .RESTurl(hdr[["X-Storage-Url"]], container, format="json",
            prefix=prefix, delimiter=delimiter, marker=marker, ...)
        contents <- .RESTcontent(curl, hdr, url)
        if (identical(attr(contents, "status"), "complete"))
            break
        marker <- attr(contents, "marker")

        ith <- ith + 1L
        bytes <- sapply(contents, "[[", "bytes")
        last_modified <- .NULLas(sapply(contents, "[[", "last_modified"))
        name <- .NULLas(sapply(contents, "[[", "name"))
        result[[as.character(ith)]] <- switch(format, short={
            FUN <- utils:::format.object_size
            size <- sapply(bytes, FUN, "auto")
            data.frame(size=size, last_modified=last_modified, name=name,
                       stringsAsFactors=FALSE)
        }, long={
            hash <- .NULLas(sapply(contents, "[[", "hash"))
            data.frame(bytes=bytes, last_modified=last_modified, hash=hash,
                       name=name, stringsAsFactors=FALSE)
        })
    }

    do.call(rbind, as.list(result)[as.character(seq_along(result))])
}
