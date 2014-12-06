.RESTquery <-
    function(...)
{
    args<- list(...)
    args <- args[!vapply(args, is.null, logical(1))]
    if (!(length(names(args)) == length(args) &&
          all(vapply(names(args), .isString, logical(1)))))
        stop("all .RESTquery arguments must be named")
    if (!all(vapply(args, length, integer(1)) == 1L))
        stop("all .RESTquery arguments must be length 1")
    ans <- paste(names(args), unname(args), sep="=", collapse="&")
    if (nzchar(ans)) sprintf("?%s", ans) else ans
}

.swcontent <-
    function(curl, hdr, path)
{
    url <- sprintf("%s%s", hdr[["X-Storage-Url"]], path)
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
        contents <- contents[vapply(contents, length, integer(1)) == 5L]
    } else {
        status <- "complete"
    }
    attr(contents, "marker") <- marker
    attr(contents, "status") <- status
    contents
}
