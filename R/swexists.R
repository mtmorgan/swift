swexists <-
    function(container, objects)
{
    stopifnot(.isString(container))
    stopifnot(is.character(objects) && !anyNA(objects))
    ## FIXME: more efficient
    setNames(objects %in% swlist(container, "abbrv"), objects)
}
