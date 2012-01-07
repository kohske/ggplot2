\name{ggarrange}
\alias{ggarrange}
\title{Arrange multiple plots on grid layout}
\usage{
  ggarrange(..., plots = NULL, dim = NULL, nrow = dim[1],
    ncol = dim[2], byrow = TRUE, widths = NULL,
    heights = NULL, respect = FALSE, layout = NULL)
}
\arguments{
  \item{...}{plot to display}

  \item{plots}{list of plots. If specified, \code{...} will
  be ignored.}

  \item{dim}{2-length vector of c(nrow, ncol)}

  \item{nrow}{number of rows of grid.}

  \item{ncol}{number of columns of grid.}

  \item{byrow}{logical. If TRUE (the default) the grid is
  filled by rows, otherwise the grid is filled by columns.}

  \item{widths}{relative widths of each column, or unit
  object.}

  \item{heights}{relative heights of each row, or unit
  object.}

  \item{respect}{not implemented. see ?layout.}

  \item{layout}{gglayout object.}
}
\value{
  ggarrange object that inherits gtable.
}
\description{
  If \code{dim}, \code{nrow}, \code{ncol} and \code{layout}
  are missing, gglayout autimatically makes a layout as
  square as possible. If \code{dim}, \code{nrow} or
  \code{ncol} are specifies, ggplot makes a layout analogue
  to \code{matrix}. More flexible layout can be set through
  \code{gglayout}.
}
\details{
  TODO: compatibility with ggsave TODO: generate gTree and
  return grobs
}
\examples{
# list of plot
p <- lapply(1:10, function(i) ggplot(mtcars, aes(factor(cyl))) + geom_bar(fill = rainbow(10)[i]) + opts(title = paste(i)))

# automatic layout as square as possible
ggarrange(p[[1]], p[[2]], p[[3]])
ggarrange(plots = p[1:3])
ggarrange(plots = p[1:7] ,byrow = F)
ggarrange(plots = p[1:10])

# sepcify dimenstion
ggarrange(plots = p[1:5], dim = c(2, 3))
ggarrange(plots = p[1:5], dim = c(2, 3), byrow = F)
ggarrange(plots = p[1:5], nrow = 2)
ggarrange(plots = p[1:5], ncol = 2)

# layout as matrix
m <- matrix(
  c(1, 1, 1,
    2, 2, 3,
    4, 5, 5,
    6, 5, 5), 4, byrow = T)
lay <- gglayout(m)
ggarrange(plots = p[1:6], layout = lay)

# with blank space
m <- matrix(
  c(1, 1, 1, 1,
    2, 0, 0, 3,
    2, 4, 4, 3), 3, byrow = T)
lay <- gglayout(m)
ggarrange(plots = p[1:4], layout = lay)

# layout as list of col/row list
lay <- gglayout(row = list(1, 2, 2, 3, 3:4, 4), col = list(1:3, 1:2, 3, 1, 2:3, 1))
ggarrange(plots = p[1:6], layout = lay)

# enables inset layout
lay <- gglayout(row = list(1, 2, 2, 3, 3:4, 4), col = list(1:3, 1:2, 3, 1, 2:3, 3))
ggarrange(plots = p[1:6], layout = lay)

# width/height
ggarrange(plots = p[1:6], widths = c(1, 1.5, 2), heights = c(1, 2))

m <- matrix(
  c(1, 2, 3,
    4, 5, 6), 2, byrow = T)
lay <- gglayout(m, widths = c(1, 1.5, 2), heights = seq(1, 2))
ggarrange(plots = p[1:6], layout = lay)

# work with separate guide
# plot
p2 <- qplot(1:3, 1:3, colour = 1:3)
# without guide
p2p <- p2 + opts(legend.position = "none")
# only guide
p2g <- p2 + opts(keep = "guide")
# plot and guide in separate sell
ggarrange(plots = c(p[1:4], list(p2p, p2g)))
}
\seealso{
  \code{\link{gglayout}} for methods generating flexible
  layout. \code{\link{ggtable}} for size sensitive
  arrangement.
}

