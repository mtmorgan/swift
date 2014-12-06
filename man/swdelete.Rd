\name{swdelete}
\alias{swdelete}
\title{Object deletion}
\description{

  This function removes one or more objects from a container.

}
\usage{
swdelete(container, objects)
}

\arguments{

  \item{container}{A character(1) specifying the container whose
    contents is to be listed. When missing, list all containers.}

  \item{objects}{A character() vector specifying one or more objects in
  the container to be deleted.}

}

\value{

  A character() vector of successfully deleted objects, invisibly.

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
