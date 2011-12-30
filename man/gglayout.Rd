\name{gglayout}
\alias{gglayout}
\title{Generate grid layout for ggplot2}
\usage{
  gglayout(...)
}
\arguments{
  \item{mat}{a matrix object specifying the location of
  each plot. see also \code{\link{layout}} in graphics
  package.}

  \item{row,col}{list of indecies of the location of each
  plot. see also \code{\link{grid.layout}} and
  \code{layout.pos.row}/\code{layout.pos.col} in
  \code{\link{viewport}}.}

  \item{widths}{relative widths of each column, or unit
  object.}

  \item{heights}{relative heights of each row, or unit
  object.}

  \item{respect}{not implemented. see ?layout.}

  \item{nrow}{number of rows of grid.}

  \item{ncol}{number of columns of grid.}
}
\description{
  At the moment, there are two ways of layout
  specification: via matrix and via list-of-col/row-index.
  matrix is analogue to \code{\link{layout}}. list-of-index
  is analogue to \code{\link{grid.layout}} and
  \code{\link{viewport}}.
}
\examples{
layout as matrix
m <- matrix(
  c(1, 1, 1,
    2, 2, 3,
    4, 5, 5,
    6, 5, 5), 4, byrow = T)
lay <- gglayout(m)
lay

m <- matrix(
  c(1, 1, 1, 1,
    2, 0, 0, 3,
    2, 4, 4, 3), 3, byrow = T)
lay <- gglayout(m)
lay

m <- matrix(
  c(1, 2, 3,
    4, 5, 6), 2, byrow = T)
lay <- gglayout(m, widths = c(1, 1.5, 2), heights = seq(1, 2))
lay

# layout by list of index
lay <- gglayout(row = list(1, 2, 2, 3, 3:4, 4), col = list(1:3, 1:2, 3, 1, 2:3, 1))
lay

lay <- gglayout(row = list(1, 2, 2, 3, 3:4, 4), col = list(1:3, 1:2, 3, 1, 2:3, 3))
lay
}
\seealso{
  \code{\link{layout}}, \code{\link{grid.layout}},
  \code{\link{viewport}}.
}

