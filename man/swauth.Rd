\name{swauth}
\alias{swauth}
\title{Authentication}
\description{

  This function provide authentication credentials for the duration of
  an R session. Credentials can be read from system environment
  variables. If environment variables are set at the time the package is
  loaded, then the credentials are recorded and the user does not have
  to call this function before using other functions in the package.

}
\usage{
swauth(USER = Sys.getenv("ST_USER"), KEY = Sys.getenv("ST_KEY"),
       AUTH = Sys.getenv("ST_AUTH"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{USER}{User name know to the swift system.}

  \item{KEY}{User authentication key known to the swift system.}

  \item{AUTH}{Url of the authentication server.}
}

\value{

  An list (invisibly) containing the user, key, and authentication key /
  value pairs.

}

\author{
  Martin Morgan (\url{mailto:mtmorgan@fredhutch.org})
}

\examples{
\dontrun{
swauth("Swift_user", "xzyabc", "https://tin.fhcrc.org/auth/v1.0")
}
}

\keyword{ manip }
