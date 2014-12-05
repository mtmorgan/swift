\name{swlist}
\alias{swlist}
\title{Content listings}
\description{

  This function lists the contents of swift containers, including the
  size and last modification date.

}
\usage{
swlist(container, format=c("short", "long"))
}

\arguments{

  \item{container}{A character(1) specifying the container whose
    contents is to be listed.}

  \item{format}{(Optional) format for return value; see \sQuote{value}
    section.}

}

\value{

  For \code{format="short"}, a data frame with as many rows as there are
  objects in the container, and columns \code{size} (object size, in
  human-readable form), \code{last_modified} (last modification date),
  and \code{name} (object name).

  For \code{format="long"}, a data frame with as many rows as there are
  objects in the container, and columns \code{bytes} (object size, in
  bytes), \code{last_modified} (last modification date), \code{hash}
  (object hash, for validation), and \code{name} (object name).

}

\author{
  Martin Morgan (\url{mailto:mtmorgan@fredhutch.org})
}

\seealso{\code{swauth}, \code{swdownload}}

\examples{
\dontrun{
swlist("mtm_swift")
}
}

\keyword{ manip }
