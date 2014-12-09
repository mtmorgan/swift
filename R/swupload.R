.swupload_file <-
    function(curl, hdr, container, object, path, verbose)
{
    if (verbose) {
        msg <- sprintf("uploading %s to %s", sQuote(basename(path)),
                        sQuote(object))
        message(msg)
    }
    url <- .RESTurl(hdr[["X-Storage-Url"]], container, object)
    .RESTupload(curl, hdr, url, path)
    object
}

swupload <-
    function(container, path=".", ..., prefix, mode=c("create", "replace"),
             verbose=TRUE)
{
    stopifnot(.isString(container))
    stopifnot(.isString(path))
    if (!file.exists(path))
        stop("'source' does not exist:\n  ", sQuote(source))
    stopifnot(missing(prefix) || .isString(prefix))
    mode <- match.arg(mode)
    stopifnot(.isLogical(verbose))

    isdir <- file.info(path)$isdir
    istrailing <- substring(path, nchar(path)) == "/"
    path <- normalizePath(path)
    root <- if (!isdir || !istrailing) dirname(path) else path
    if (isdir) {
        paths <- dir(path, ..., full.names=TRUE)
        paths <- paths[file.info(paths)$isdir != TRUE]
    } else {
        paths <- path
    }
    toolarge <- file.info(paths)$size > .SW_UPLOAD_MAX_SIZE
    if (any(toolarge))
        stop(sum(toolarge), " files > 5 GB cannot be uploaded (yet):",
             "\n  ", paste(sQuote(paths[toolarge]), collapse="\n  "))

    if (missing(prefix)) {
        prefix <- ""
        pattern <- sprintf("^%s/", root)
    } else {
        pattern <- sprintf("^%s", root)
    }
    objects <- sub(pattern, prefix, paths)

    if ((!"replace" %in% mode) &&
        any(exist <- swexists(container, objects)))
    {
        idx <- head(which(exist))
        stop(sum(exist), " object(s) already exist and 'mode' is not 'replace'",
             "\n  mode: ", paste(sQuote(mode), collapse=", "),
             "\n  container: ", sQuote(container),
             "\n  path(s): ", paste(sQuote(paths)[idx], collapse=", "),
             "\n  object(s): ", paste(sQuote(objects)[idx], collapse=", "),
             call.=FALSE)
    }

    curl <- RCurl::getCurlHandle()
    hdr <- .swauth(curl)

    for (i in seq_along(paths))
        .swupload_file(curl, hdr, container, objects[[i]], paths[[i]], verbose)
    setNames(objects, paths)
}
