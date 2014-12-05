\name{swdownload}
\alias{swdownload}
\title{Download containers or objects}
\description{

  This function retrieves an object in a container, or all objects in a
  container, to a local destination file or directory. It is necessary
  that authentication occurs before downloading; see
  \code{\link{swauth}}.

}
\usage{
swdownload(container, object, destination, overwrite = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{container}{A character(1) name of the container download.}

  \item{object}{A character(1) name of the object in the container. If
    missing, the entire container is downloaded.}

  \item{destination}{A character(1) file (when \code{object} is provided
    or directory path to download the container and objects to.}

  \item{overwrite}{A logical(1) variable indicating whether an existing
    destination can be over-written during download.}

}

\value{

  A character(1) destination to which the container or object was
  downloaded.
  
}

\author{Martin Morgan (\url{mailto:mtmorgan@fredhutch.org})}

\seealso{\code{\link{swauth}}}

\examples{
\dontrun{
fl = swdownload("mtm_swift", "svn-stats.txt")
read.csv(fl)
}
}

\keyword{ manip }
