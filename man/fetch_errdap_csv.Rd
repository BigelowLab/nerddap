% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buoy.R
\name{fetch_errdap_csv}
\alias{fetch_errdap_csv}
\title{Fetch ERRDAP CSV file}
\usage{
fetch_errdap_csv(x, filename = tempfile(), ...)
}
\arguments{
\item{x}{char a url for download}

\item{filename}{char, the destination for the file}

\item{...}{other argument for \code{download.file}}
}
\value{
0 for success, non-zero for problems
}
\description{
Fetch ERRDAP CSV file
}
