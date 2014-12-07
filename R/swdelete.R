.swdelete <-
    function(object, curl, hdr, container, verbose)
{
    if (verbose)
        message(sprintf("deleting %s", sQuote(object)))

    tryCatch({
        url <- .RESTurl(hdr[["X-Storage-Url"]], container, object)
        contents <- .RESTdelete(curl, hdr, url)
        TRUE
    }, error=function(e) {
        warning(sprintf("%s: %s", conditionMessage(e), sQuote(object)),
                call.=FALSE)
        FALSE
    })
}

swdelete <-
    function(container, objects, verbose=TRUE)
{
    stopifnot(.isString(container))
    stopifnot(is.character(objects) || !anyNA(objects))
    stopifnot(.isLogical(verbose))

    curl <- RCurl::getCurlHandle()
    hdr <- .swauth(curl)

    status <- sapply(objects, .swdelete, curl, hdr, container, verbose)
    invisible(sprintf("%s/%s", container, objects)[status])
}
