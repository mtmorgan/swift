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
swdownload(container, object, destination, overwrite = FALSE, ...)
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

  \item{...}{Additional arguments, available when \code{object} is
    missing, as follows:

    \describe{

      \item{prefix}{a character(1) value that restricts downloads to
        objects whose paths begin with this value.}

      \item{delimiter}{A single-letter character(1). Restrict downloads
        to objects that are nested in the container, from the end of
        the prefix (if any) to the first occurrence of this character.}

      \item{marker}{A character(1) file name. Restrict downloads to
        objects that are greater than this value.}

    }
  }

}

\value{

  A character(1) destination to which the container or object was
  downloaded.
  
}

\author{Martin Morgan (\url{mailto:mtmorgan@fredhutch.org})}

\seealso{

  \code{\link{swauth}}

  The OpenStack documentation,
  \url{http://docs.openstack.org/api/openstack-object-storage/1.0/content/ch_object-storage-dev-overview.html}

}

\examples{
\dontrun{
fl = swdownload("mtm_swift", "svn-stats.txt")
## All objects contained in 'Folder/'
fl = swdownload("mtm_swift", prefix="Folder/")
## All objects contained in 'Folder/', up to first occurrence of '/'
fl = swdownload("mtm_swift", prefix="Folder/", delimiter="/")
read.csv(fl)
}
}

\keyword{ manip }
