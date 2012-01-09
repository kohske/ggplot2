\name{ggtable}
\alias{ggtable}
\title{Arrange multiple plot keeping plot size consistent.}
\usage{
  ggtable(..., plots = NULL, dim = NULL, nrow = dim[1],
    ncol = dim[2], byrow = TRUE, widths = NULL,
    heights = NULL, respect = FALSE, layout = NULL,
    main = NULL)
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

  \item{main}{main title of the plot.}
}
\value{
  ggtable object that inherits gtable class.
}
\description{
  Current limitation (i.e., TODO) - cannot handle different
  nrow/ncol plots (i.e., extra plot parts such as title or
  guides) - cannot handle flexible width/height - cannot
  handle flexible guide placement - etc.
}
\examples{
# list of plot
p <- lapply(1:10, function(i) ggplot(mtcars, aes(factor(cyl))) + geom_bar(fill = rainbow(10)[i]) + theme_grey(base_size = i*5))

# automatic layout as square as possible
ggtable(p[[1]], p[[2]], p[[3]])
ggtable(plots = p[1:3])
ggtable(plots = p[1:7] ,byrow = F)
ggtable(plots = p[1:10])

# simple cbind, rbind
ggtable(plots = p[c(1,10)], nrow = 1) # cbind
ggtable(plots = p[c(1,10)], nrow = 2) # rbind

# sepcify dimenstion
ggtable(plots = p[1:5], dim = c(2, 3))
ggtable(plots = p[1:5], dim = c(2, 3), byrow = F)
ggtable(plots = p[1:5], nrow = 2)
ggtable(plots = p[1:5], ncol = 2)

# layout as matrix
m <- matrix(
  c(1, 2, 3,
    4, 5, 6), 2, byrow = T)
lay <- gglayout(m)
ggtable(plots = p[1:6], layout = lay)

# with blank space
m <- matrix(
  c(1, 2, 3,
    4, 0, 5), 2, byrow = T)
lay <- gglayout(m)
ggtable(plots = p[1:5], layout = lay)

# layout as list of col/row list
lay <- gglayout(row = list(1, 2, 3, 1, 2, 3), col = list(1, 1, 1, 2, 2, 2))
ggtable(plots = p[1:6], layout = lay)

# combine scatter plot and density plot
df <- data.frame(x = rnorm(100), y = rnorm(100))
p1 <- ggplot(df, aes(x, y)) + geom_point()
p2 <- ggplot(df, aes(x)) + geom_density()
p3 <- ggplot(df, aes(y)) + geom_density() + coord_flip()

ggtable(p1, p2, p3, layout = gglayout(matrix(c(2, 1, 0, 3), 2)))

# combine ggtable and ggplot by ggarange
gt1 <- ggtable(p[[1]], p[[10]], nrow = 1)
gt2 <- ggtable(p[[3]], p[[5]], p[[7]], nrow = 1)
ggarrange(gt1, gt2, p[[4]], ncol = 1)
}
\seealso{
  \code{\link{gglayout}} for methods generating flexible
  layout. \code{\link{ggarrange}} for simple arrangement.
}

