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
