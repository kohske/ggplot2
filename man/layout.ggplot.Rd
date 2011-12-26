\name{layout.ggplot}
\alias{layout.ggplot}
\title{Arrange multiple plots on grid layout}
\usage{
  \method{print}{ggtable} (..., dim = NULL, nrow = dim[1],
    ncol = dim[2], byrow = TRUE, layout = NULL,
    widths = NULL, heights = NULL, newpage = is.null(vp),
    vp = NULL)
}
\arguments{
  \item{...}{plot to display}

  \item{dim}{2-length vector of c(nrow, ncol)}

  \item{nrow}{number of rows of grid.}

  \item{ncol}{number of columns of grid.}

  \item{byrow}{logical. If TRUE (the default) the grid is
  filled by rows, otherwise the grid is filled by columns.}

  \item{layout}{list of list specifying layout grid.}

  \item{widths}{relative widths of each column, or unit
  object.}

  \item{heights}{relative heights of each row, or unit
  object.}

  \item{newpage}{draw new (empty) page first?}

  \item{vp}{viewport to draw plot in}
}
\description{
  Arrange multiple plots on grid layout
}
\examples{
# automatic layout as square as possible
p <- lapply(1:10, function(i) ggplot(mtcars, aes(factor(cyl))) + geom_bar(fill = rainbow(10)[i]) + opts(title = paste(i)))

layout.ggplot(p[[1]], p[[2]], p[[3]])
do.call("layout.ggplot", p[1:7])
do.call("layout.ggplot", p[1:10])

# sepcify dimenstion
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], dim = c(2, 3))
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], dim = c(2, 3), byrow = F)
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], nrow = 2)
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], ncol = 3)

# specify layout
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]],
  layout = list(row = list(1, 1, 1, 2, 2), col = list(1, 2, 3, 1, 2)))
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]],
  layout = list(row = list(1, 1, 1, 2, 2), col = list(1, 2, 3, 1, 2:3)))
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], nrow = 3, ncol = 4,
  layout = list(row = list(1, 1, 1, 2, 2), col = list(1, 2, 3, 1, 2:3)))

# width/height
layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]],
  layout = list(row = list(1, 1, 1, 2, 2), col = list(1, 2, 3, 1, 2:3)),
  widths = c(2, 1, 1), heights = c(2, 1))
layout.ggplot(p[[1]], p[[2]], p[[3]],
  nrow = 1, widths = c(1, 1.5, 2.5))
}

