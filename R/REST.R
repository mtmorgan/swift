.RESTurl <-
    function(url, container=NULL, object=NULL, ...)
{
    args<- list(...)
    args <- args[!vapply(args, is.null, logical(1))]
    if (!(length(names(args)) == length(args) &&
          all(vapply(names(args), .isString, logical(1)))))
        stop("all .RESTquery '...' arguments must be named")
    if (!all(vapply(args, length, integer(1)) == 1L))
        stop("all .RESTquery '...' arguments must be length 1")

    query <- paste(RCurl::curlEscape(names(args)),
                   RCurl::curlEscape(unname(args)),
                   sep="=", collapse="&")
    query <- if (nzchar(query)) sprintf("?%s", query) else ""
    paste0(url, sprintf("/%s", container), sprintf("/%s", object), query)
}

.RESTupload <-
    function(curl, hdr, url, source)
{
    auth <- sprintf("%s: %s", "X-Auth-Token", hdr[["X-Storage-Token"]])
    resp <- PUT(url, config(httpheader=auth), body=upload_file(source))
    stop_for_status(resp)
    
    source
}

.RESTdownload <-
    function(curl, hdr, url, destination, overwrite)
{
    auth <- sprintf("%s: %s", "X-Auth-Token", hdr[["X-Storage-Token"]])
    resp <- GET(url, config(httpheader=auth), progress(), 
                write_disk(destination, overwrite=overwrite),
                curl=curl)
    cat("\n")
    stop_for_status(resp)

    destination
}

.RESTdelete <-
    function(curl, hdr, url)
{
    auth <- sprintf("%s: %s", "X-Auth-Token", hdr[["X-Storage-Token"]])
    resp <- DELETE(url, config(httpheader=auth))
    stop_for_status(resp)
}

.RESTcontent <-
    function(curl, hdr, url)
{
    auth <- sprintf("%s: %s", "X-Auth-Token", hdr[["X-Storage-Token"]])
    objects <- GET(url, config(httpheader=auth), curl=curl)
    stop_for_status(objects)

    status <- marker <- NULL
    contents <- content(objects)
    if (length(contents)) {
        last <- contents[[length(contents)]]
        marker <- if ("subdir" %in% names(last)) {
            last[["subdir"]]
        } else last[["name"]]
    } else {
        status <- "complete"
    }
    attr(contents, "marker") <- marker
    attr(contents, "status") <- status
    contents
}
