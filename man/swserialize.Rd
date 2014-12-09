\name{swserialize}
\alias{swserialize}
\alias{swunserialize}
\title{Serialize (save in binary format) R objects}

\description{

  Use \code{swserialize} / \code{swunserialize} to save and load R objects
  directly to / from swift containers.

}

\usage{
swserialize(x, container, object, ..., mode=c("create", "replace"))
swunserialize(container, object, ...)
}

\arguments{

  \item{x}{An \R object.}

  \item{container}{A character(1) name of the container download.}

  \item{object}{A character(1) name of the object in the container. If
    missing, the entire container is downloaded.}

  \item{...}{For use with \code{swunserialize}. If \code{objects} is
      missing, use \code{swlist(container, ..., format="abbrv")} to
      identify the object to be , e.g., using \code{prefix=...}.}

  \item{mode}{(Optional) a character(1) controlling object creation; see
    \code{\link{swupload}}.}

}

\value{

  For \code{swserialize()}, the name of the stored object. For
  \code{swunserialize()}, the R object.

}

\author{Martin Morgan (\url{mailto:mtmorgan@fredhutch.org})}

\seealso{

  \code{\link{swauth}}.

  The OpenStack documentation,
  \url{http://docs.openstack.org/api/openstack-object-storage/1.0/content/ch_object-storage-dev-overview.html}

}

\examples{
\dontrun{
swauth()
data(mtcars)
swserialize(mtcars, "mtm_swift", "my/cars.Robj", mode="replace")
mycars <- swunserialize("mtm_swift", "my/cars.Robj")
identical(mycars, mtcars)
}
}

\keyword{ manip }
