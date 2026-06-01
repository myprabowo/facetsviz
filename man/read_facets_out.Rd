\name{read_facets_out}
\alias{read_facets_out}
\title{Read a FACETS output file}
\usage{
read_facets_out(path, encoding = "UTF-8")
}
\arguments{
  \item{path}{Path to a FACETS \code{.out} file.}
  \item{encoding}{Text encoding used when reading the file.}
}
\value{
An object of class \code{facets_out} containing generic measurement reports,
legacy compatibility aliases, category statistics, and unexpected-response tables.
}
\description{
Reads a FACETS output file and extracts the main reports used for diagnostic
visualization and review.
}
