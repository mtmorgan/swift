\name{swupload}
\alias{swupload}
\title{Object upload}
\description{

  Upload objects to the container.

}
\usage{
swupload(container, path=".", ..., prefix,
    mode=c("create", "replace", "skip"), verbose=TRUE)
}

\arguments{

  \item{container}{A character(1) specifying the container to recieve
    objects}

  \item{path}{A character(1) path to a file or directory to be
    uploaded.}

  \item{...}{Additional arguments influencing how \code{path} is
    expanded to include additional files, as \code{dir(path, ...)}.}

  \item{prefix}{(Optional) A character(1) vector specifying the object
    prefix in the container.}

  \item{mode}{(Optional) A character(1) indicating whether uploaded
    objects must not already exist (\code{"create"}) or existing objects
    can be replaced (\code{"replace"}), or existing objects may be skipped (\code{"skip"}.}

  \item{verbose}{A logical(1) specifying level of information provided
    during function evaluation.}

}

\details{

  The \code{path} and \code{...} arguments determine which files will be
  updated. These are expanded to \code{paths = dir(path, ...,
  full=TRUE)}, so for instance all files ending with \code{".gz"} in the
  direcotry \code{"~/archive"} can be uploaded with
  \code{path="~/archive", pattern="*.gz"}.

  The \code{path} and \code{prefix} arguments determine the uploaded
  object name. If \code{path} is a plain file (e.g.,
  \code{"~/archive/file.gz"}) and \code{prefix} is not provided, the
  object name is simply \code{"file.gz"}. If the prefix is specified,
  e.g., \code{prefix="My/Test"}, the object name is the concatenation of
  the prefix and file name \code{"My/Test/file.gz"}.  If \code{path} is
  a directory specified with a trailing \code{"/"} (e.g.,
  \code{"~/archive/"}) the directory name \emph{is not} included in the
  object identifier. If the directory name is specified without a
  trailing \code{"/"}, e.g., \code{"~/archive"}, the directory
  (\code{"archive"}) \emph{is} included in the object identifier. Thus
  \code{path="~/archive/", prefix="My"} might result in an object
  \code{"My/file.gz"}, while \code{path="~/archive", prefix="My"}
  results in \code{"My/archive/file.gz"}.

}

\value{

  \code{swupload} returns a named characer vector, where names are the
  original paths and values are the corresponding objects.

}

\author{
  Martin Morgan (\url{mailto:mtmorgan@fredhutch.org})
}

\seealso{

  \code{\link{swauth}}

  The OpenStack documentation,
  \url{http://docs.openstack.org/api/openstack-object-storage/1.0/content/ch_object-storage-dev-overview.html}

}

\examples{
\dontrun{
swauth()
swlist("mtm_swift")
swupload("mtm_swift", "~/archive", pattern=".*.gz", prefix="My")
swlist("mtm_swift")

}
}

\keyword{ manip }
