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
    function(container, path=".", ..., prefix, mode=c("create", "replace", "skip"),
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
    .stop_for_upload_size(file.info(paths)$size, paths)

    if (missing(prefix)) {
        prefix <- ""
        pattern <- sprintf("^%s/", root)
    } else {
        pattern <- sprintf("^%s", root)
    }
    objects <- sub(pattern, prefix, paths)
    exist <- swexists(container, objects)
    .stop_for_writable(exist, mode, paths)

    upload <- if ("skip" %in% mode) !exist else TRUE
    if (verbose && ("skip" %in mode) && any(exist))
        message(sum(exist), " existing object(s) skipped")

    curl <- RCurl::getCurlHandle()
    hdr <- .swauth(curl)

    for (i in seq_along(paths)[upload])
        .swupload_file(curl, hdr, container, objects[[i]], paths[[i]], verbose)
    setNames(objects, paths)
}
