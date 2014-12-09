.isString <-
    function(x, nchar, z.ok=FALSE, na.ok=FALSE)
{
    !missing(x) && is.character(x) && length(x) == 1L &&
        if (is.na(x)) na.ok else {
            (z.ok || nzchar(x)) && (missing(nchar) || nchar(x) == nchar)
        }
}

.isInteger <-
    function(x, min, max, as=FALSE, na.ok=FALSE)
{
    if (as)
        x <- as.integer(x)
    !missing(x) && is.integer(x) && length(x) == 1L &&
        if (is.na(x)) na.ok else  {
           (missing(min) || x >= min) && (missing(max) || x <= max)
       }
}

.isLogical <-
    function(x, na.ok=FALSE)
{
    !missing(x) && is.logical(x) && length(x) == 1L && (na.ok || !is.na(x))
}

.NULLas <- function(x, as=NA_character_) {
    if (is.list(x)) {
        mapply(function(x) if (is.null(x)) as else x, x)
    } else {
        x[is.null(x)] <- as
        x
    }
}

.stop_for_upload_size <-
    function(paths, sizes)
{
    toolarge <- sizes > .SW_UPLOAD_MAX_SIZE
    if (any(toolarge))
        stop(sum(toolarge), " file(s) > 5 GB cannot be uploaded (yet):",
             "\n  ", paste(sQuote(paths[toolarge]), collapse="\n  "))

}

.stop_for_writable <-
    function(container, objects, mode)
{
    exist <- swexists(container, objects)
    if ((!"replace" %in% mode) && any(exist)) {
        idx <- head(which(exist))
        stop(sum(exist), " object(s) already exist and 'mode' is not 'replace'",
             "\n  mode: ", paste(sQuote(mode), collapse=", "),
             "\n  container: ", sQuote(container),
             "\n  object(s): ", paste(sQuote(objects)[idx], collapse=", "),
             call.=FALSE)
    }
}
