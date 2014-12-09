\name{swread}
\alias{swread}
\title{Input object directly into an R representation}

\description{

  \code{swread} retrieves an object in a container to it's local R
  representation, e.g., a text file is retrieved as a character()
  vector.

}

\usage{
swread(container, object, ..., as=NULL, type=NULL)
}

\arguments{

  \item{container}{A character(1) name of the container download.}

  \item{object}{A character(1) name of the object in the container. If
    missing, the entire container is downloaded.}

  \item{...}{If \code{objects} is missing, use \code{swlist(container,
      ..., format="abbrv")} to identify the object to be read.}

  \item{as, type}{A character(1) description of type of output; see
    \code{\link{content}}.}

}

\value{

  A character() (for text files) or raw() (for binary files) vector
  reprsenting the object.

  Some data types are parsed according to MIME type; see
  \code{\link{content}}.
  
}

\author{Martin Morgan (\url{mailto:mtmorgan@fredhutch.org})}

\seealso{

  \code{\link{swauth}}, \code{\link{swdownload}}.

  The OpenStack documentation,
  \url{http://docs.openstack.org/api/openstack-object-storage/1.0/content/ch_object-storage-dev-overview.html}

}

\examples{
\dontrun{
swauth()
data(mtcars)
fl <- file.path(tempfile, "mtcars.csv")
write.csv(mtcars, fl)
swupload("mtm_swift", fl)           # stored MIME type text/csv 
swread("mtm_swift", "mtcars.csv")   # parsed using read.csv
}
}

\keyword{ manip }
