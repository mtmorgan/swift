\name{swlist}
\alias{swlist}
\alias{swexists}
\title{Container listing and other queries}
\description{

  List or test the existence of the contents of swift containers.


}
\usage{
swlist(container, format=c("short", "abbrv", "long"), ...,
    prefix=NULL, delimiter=NULL)
swexists(container, objects)
}

\arguments{

  \item{container}{(optional) A character(1) specifying the container
    whose contents is to be listed. When missing, list all containers.}

  \item{objects}{A character() specifying object paths.}

  \item{format}{(Optional) format for return value; see \sQuote{value}
    section.}

  \item{...}{Additional arguments, for internal use.}


  \item{prefix}{A character(1) value that restricts listings to objects
    whose paths begin with this value.}

  \item{delimiter}{A single-letter character(1). Restricts listings
    to objects that are nested in the container, from the end of
    the prefix (if any) to the first occurrence of this character.}

}

\value{

  \code{swexists} returns a named logical vector indicating whether the
  object exists in the container.

  \code{swlist} returns a vector or data frame, as follows:

  For \code{format="short"}, a data frame with as many rows as there are
  objects in the container, and columns \code{size} (object size, in
  human-readable form), \code{last_modified} (last modification date),
  and \code{name} (object name).

  For \code{format="abbrv"}, a character vector of object names.

  For \code{format="long"}, a data frame with as many rows as there are
  objects in the container, and columns \code{bytes} (object size, in
  bytes), \code{last_modified} (last modification date), \code{hash}
  (object hash, for validation), and \code{name} (object name).

}

\author{
  Martin Morgan (\url{mailto:mtmorgan@fredhutch.org})
}

\seealso{

  \code{\link{swauth}}, \code{\link{swdownload}}

  The OpenStack documentation,
  \url{http://docs.openstack.org/api/openstack-object-storage/1.0/content/ch_object-storage-dev-overview.html}

}

\examples{
\dontrun{
swlist("mtm_swift")
}
}

\keyword{ manip }
