\name{run_facets_app}
\alias{run_facets_app}
\title{Launch the facetsviz Shiny app}
\usage{
run_facets_app(launch = TRUE, host = "127.0.0.1", port = NULL)
}
\arguments{
  \item{launch}{If \code{TRUE}, run the app immediately. If \code{FALSE}, return the app object.}
  \item{host}{Host passed to \code{shiny::runApp()} when launching.}
  \item{port}{Port passed to \code{shiny::runApp()} when launching.}
}
\value{A shiny app object or the result of running it.}
\description{Launches a Shiny interface for uploading FACETS \code{.out} files and exploring the parsed diagnostics.}
