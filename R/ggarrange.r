#' Arrange multiple plots on grid layout
#'
#' If \code{dim}, \code{nrow}, \code{ncol} and \code{layout} are missing,
#' gglayout autimatically makes a layout as square as possible.
#' If \code{dim}, \code{nrow} or \code{ncol} are specifies,
#' ggplot makes a layout analogue to \code{matrix}.
#' More flexible layout can be set through \code{gglayout}.
#' 
#' TODO: compatibility with ggsave
#' TODO: generate gTree and return grobs
#'
#' @param ... plot to display
#' @param plots list of plots. If specified, \code{...} will be ignored.
#' @param dim 2-length vector of c(nrow, ncol)
#' @param nrow number of rows of grid.
#' @param ncol number of columns of grid.
#' @param byrow logical. If TRUE (the default) the grid is filled by rows, otherwise the grid is filled by columns.
#' @param widths relative widths of each column, or unit object.
#' @param heights relative heights of each row, or unit object.
#' @param respect not implemented. see ?layout.
#' @param layout gglayout object.
#' @return ggarrange object that inherits gtable.
#' @seealso \code{\link{gglayout}} for methods generating flexible layout. \code{\link{ggtable}} for size sensitive arrangement.
#' @export
#' @examples
#' # list of plot
#' p <- lapply(1:10, function(i) ggplot(mtcars, aes(factor(cyl))) + geom_bar(fill = rainbow(10)[i]) + opts(title = paste(i)))
#' 
#' # automatic layout as square as possible
#' ggarrange(p[[1]], p[[2]], p[[3]])
#' ggarrange(plots = p[1:3])
#' ggarrange(plots = p[1:7] ,byrow = F)
#' ggarrange(plots = p[1:10])
#' 
#' # sepcify dimenstion
#' ggarrange(plots = p[1:5], dim = c(2, 3))
#' ggarrange(plots = p[1:5], dim = c(2, 3), byrow = F)
#' ggarrange(plots = p[1:5], nrow = 2)
#' ggarrange(plots = p[1:5], ncol = 2)
#' 
#' # layout as matrix
#' m <- matrix(
#'   c(1, 1, 1, 
#'     2, 2, 3, 
#'     4, 5, 5, 
#'     6, 5, 5), 4, byrow = T)
#' lay <- gglayout(m)
#' ggarrange(plots = p[1:6], layout = lay)
#' 
#' # with blank space
#' m <- matrix(
#'   c(1, 1, 1, 1,
#'     2, 0, 0, 3,
#'     2, 4, 4, 3), 3, byrow = T)
#' lay <- gglayout(m)
#' ggarrange(plots = p[1:4], layout = lay)
#' 
#' # layout as list of col/row list
#' lay <- gglayout(row = list(1, 2, 2, 3, 3:4, 4), col = list(1:3, 1:2, 3, 1, 2:3, 1))
#' ggarrange(plots = p[1:6], layout = lay)
#' 
#' # enables inset layout
#' lay <- gglayout(row = list(1, 2, 2, 3, 3:4, 4), col = list(1:3, 1:2, 3, 1, 2:3, 3))
#' ggarrange(plots = p[1:6], layout = lay)
#' 
#' # width/height
#' ggarrange(plots = p[1:6], widths = c(1, 1.5, 2), heights = c(1, 2))
#' 
#' m <- matrix(
#'   c(1, 2, 3, 
#'     4, 5, 6), 2, byrow = T)
#' lay <- gglayout(m, widths = c(1, 1.5, 2), heights = seq(1, 2))
#' ggarrange(plots = p[1:6], layout = lay)
#'
#' # work with separate guide
#' # plot
#' p2 <- qplot(1:3, 1:3, colour = 1:3)
#' # without guide
#' p2p <- p2 + opts(legend.position = "none")
#' # only guide
#' p2g <- p2 + opts(keep = "guide")
#' # plot and guide in separate sell
#' ggarrange(plots = c(p[1:4], list(p2p, p2g)))
ggarrange <- function(..., plots = NULL, dim = NULL, nrow = dim[1], ncol = dim[2], byrow = TRUE, widths = NULL, heights = NULL, respect = FALSE,
                      layout = NULL) {
  # detect plots
  if (is.null(plots)) plots <- list(...)
  plots <- Filter(function(x) any(inherits(x, c("ggtable", "ggplot", "grob"))), plots)
  np <- length(plots)
  if (np == 0) return

  if (is.null(layout)) {
    # if layout is NULL, the plots are automatically placed in that order.

    # dimenstion
    if (is.null(nrow) && is.null(ncol)) {
      ncol <- ceiling(sqrt(np))
      nrow <- ceiling(np / ncol)
    } else if (is.null(nrow)) {
      nrow <- ceiling(np / ncol)
    } else if (is.null(ncol)) {
      ncol <- ceiling(np / nrow)
    }

    # set layout 
    if (byrow == FALSE) {
      row <- as.list((seq(np) - 1) %% nrow + 1)
      col <- as.list(ceiling(seq(np) / nrow))
    } else {
      row <- as.list(ceiling(seq(np) / ncol))
      col <- as.list((seq(np) - 1) %% ncol + 1)
    }

    layout <- gglayout(row = row, col = col, nrow = nrow, ncol = ncol,
                       widths = widths %||% rep(1, ncol), heights = heights %||% rep(1, nrow), respect = respect)
  }
  
  # layout is not NULL
  if (!inherits(layout, "gglayout")) stop("Layout must be a class of gglayout. see ?gglayout")
  
  if (np > layout$np) {
    warning("Number of plots is larger than length of layout index. Some plots are not be shown.")
    plots <- plots[1:layout$np]
  } else if (np < layout$np) {
    warning("Number of plots is smaller than length of layout index. Some spaces will be blank.")
    layout$row <- layout$row[1:np]
    layout$col <- layout$col[1:np]
  }

  lay <- data.frame(t(rbind(sapply(layout$row, range), sapply(layout$col, range))))
  names(lay) <- c("t", "b", "l", "r")
  g <- gtable(widths = layout$widths, heights = layout$heights)
  grobs <- lapply(plots,
                  function(g)
                  if (inherits(g, "ggtable")) gtable_gTree(g)
                  else if (inherits(g, "ggplot")) ggplotGrob(g)
                  else if (inherits(g, "grob")) g)
  g <- gtable_add_grob(g, grobs = grobs, t = lay$t, l = lay$l, b = lay$b, r = lay$r)
  class(g) <- c("ggarrange", class(g))
  g
}

#' print ggarrange object
#'
#' @inheritParams geom_point
#' @param x ggarrange object to display
#' @S3method print ggarrange
#' @method print ggarrange
print.ggarrange <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  if (newpage) grid.newpage()
  if (is.null(vp)) {
    grid.draw(x)
  } else {
    if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(x)
    upViewport()
  }
  invisible()
}
