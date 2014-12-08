\name{swdelete}
\alias{swdelete}
\title{Object deletion}
\description{

  Delete objects from containers.

}
\usage{
swdelete(container, objects, ..., verbose=TRUE)
}

\arguments{

  \item{container}{A character(1) specifying the container whose
    contents is to be listed. When missing, list all containers.}

  \item{objects}{A character() vector specifying one or more objects in
    the container.}

  \item{...}{If \code{objects} is missing, use \code{swlist(container,
      ..., format="abbrv")} to identify objects to be deleted.}

  \item{verbose}{A logical(1) specifying level of information provided
    during function evaluation.}

}

\value{

  \code{swdelete} returns a named logical vector indicating whether the
  object was successfully deleted. A warning summarizes attempts to
  delete non-existent objects.

}

\author{
  Martin Morgan (\url{mailto:mtmorgan@fredhutch.org})
}

\seealso{

  \code{\link{swauth}}, \code{\link{swlist}}.

  The OpenStack documentation,
  \url{http://docs.openstack.org/api/openstack-object-storage/1.0/content/ch_object-storage-dev-overview.html}

}

\examples{
\dontrun{
swauth()
swdelete("mtm_swift", "my/svn-counts.txt") # object "my/svn-counts.txt"
swdelete("mtm_swift", prefix="my/log")     # objects with prefix 'my/log'
}
}

\keyword{ manip }
