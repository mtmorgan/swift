.state <- new.env(parent=emptyenv())

.onLoad <-
    function(...)
{
    tryCatch(swauth(), error=function(e) NULL)
}
