.state <- new.env(parent=emptyenv())

.isSingleString <- function(x)
    is.character(x) && length(x) == 1L && !is.na(x)

.onLoad <-
    function(...)
{
    tryCatch(swauth())
}
