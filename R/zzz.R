.SW_UPLOAD_MAX_SIZE <- 5000000000

.state <- new.env(parent=emptyenv())

.onLoad <-
    function(...)
{
    tryCatch(swauth(), error=function(e) NULL)
}
