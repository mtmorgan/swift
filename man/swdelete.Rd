\name{swdelete}
\alias{swdelete}
\title{Object deletion}
\description{

  Delete objects from containers.

}
\usage{
swdelete(container, objects)
}

\arguments{

  \item{container}{A character(1) specifying the container whose
    contents is to be listed. When missing, list all containers.}

  \item{objects}{A character() vector specifying one or more objects in
  the container.}

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

  \code{swauth}

  The OpenStack documentation,
  \url{http://docs.openstack.org/api/openstack-object-storage/1.0/content/ch_object-storage-dev-overview.html}

}

\examples{
\dontrun{
swauth()
swlist("mtm_swift")
swdelete("mtm_swift", "svn-counts.txt")
}
}

\keyword{ manip }
