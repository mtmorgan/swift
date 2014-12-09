\name{swread}
\alias{swread}
\alias{swsave}
\alias{swload}
\title{Input, save, and load R, text, and binary objects}

\description{

  \code{swread} retrieves an object in a container to it's local R
  representation, e.g., a text file is retrieved as a character()
  vector. Use \code{swsave} / \code{swload} to save and load R objects
  directly to / from swift containers.

}

\usage{
swread(container, object, ..., as=NULL, type=NULL)
swsave(x, container, object, ..., mode=c("create", "replace"))
swload(container, object, ...)
}

\arguments{

  \item{container}{A character(1) name of the container download.}

  \item{object}{A character(1) name of the object in the container. If
    missing, the entire container is downloaded.}

  \item{x}{An \R object.}

  \item{...}{If \code{objects} is missing, use \code{swlist(container,
      ..., format="abbrv")} to identify the object to be read, e.g.,
      using \code{prefix=...}.}

  \item{as, type}{A character(1) description of type of output; see
    \code{\link{content}}.}

  \item{mode}{A character(1) controlling object creation; see
    \code{\link{swupload}}.}

}

\value{

  For \code{swread()}, a character() (for text files) or raw() (for
  binary files) vector reprsenting the object.  Some data types are
  parsed according to MIME type; see \code{\link{content}}.

  For \code{swsave()}, the name of the stored object. For
  \code{swload()}, the R object.
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

## swread()
fl <- file.path(tempfile, "mtcars.csv")
write.csv(mtcars, fl)
swupload("mtm_swift", fl)           # stored MIME type text/csv 
swread("mtm_swift", "mtcars.csv")   # parsed using read.csv

## swsave() / swload()
swsave(mtcars, "mtm_swift", "mtcars.Robj", mode="replace")
mycars <- swload("mtm_swift", "mtcars.Robj")
swdelete("mtm_swift", "mtcars.Robj")
identical(mycars, mtcars)
}
}

\keyword{ manip }
