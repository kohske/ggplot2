#' Arrange multiple plot keeping plot size consistent.
#'
#' Current limitation (i.e., TODO)
#' - cannot handle different nrow/ncol plots (i.e., extra plot parts such as title or guides)
#' - cannot handle flexible width/height
#' - cannot handle flexible guide placement
#' - etc.
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
#' @param main main title of the plot.
#' @return ggtable object that inherits gtable class.
#' @seealso \code{\link{gglayout}} for methods generating flexible layout. \code{\link{ggarrange}} for simple arrangement.
#' @export
#' @examples
#' # list of plot
#' p <- lapply(1:10, function(i) ggplot(mtcars, aes(factor(cyl))) + geom_bar(fill = rainbow(10)[i]) + theme_grey(base_size = i*5))
#'
#' # automatic layout as square as possible
#' ggtable(p[[1]], p[[2]], p[[3]])
#' ggtable(plots = p[1:3])
#' ggtable(plots = p[1:7] ,byrow = F)
#' ggtable(plots = p[1:10])
#'
#' # simple cbind, rbind
#' ggtable(plots = p[c(1,10)], nrow = 1) # cbind
#' ggtable(plots = p[c(1,10)], nrow = 2) # rbind
#'
#' # sepcify dimenstion
#' ggtable(plots = p[1:5], dim = c(2, 3))
#' ggtable(plots = p[1:5], dim = c(2, 3), byrow = F)
#' ggtable(plots = p[1:5], nrow = 2)
#' ggtable(plots = p[1:5], ncol = 2)
#' 
#' # layout as matrix
#' m <- matrix(
#'   c(1, 2, 3, 
#'     4, 5, 6), 2, byrow = T)
#' lay <- gglayout(m)
#' ggtable(plots = p[1:6], layout = lay)
#' 
#' # with blank space
#' m <- matrix(
#'   c(1, 2, 3,
#'     4, 0, 5), 2, byrow = T)
#' lay <- gglayout(m)
#' ggtable(plots = p[1:5], layout = lay)
#' 
#' # layout as list of col/row list
#' lay <- gglayout(row = list(1, 2, 3, 1, 2, 3), col = list(1, 1, 1, 2, 2, 2))
#' ggtable(plots = p[1:6], layout = lay)
#' 
#' # combine scatter plot and density plot
#' df <- data.frame(x = rnorm(100), y = rnorm(100))
#' p1 <- ggplot(df, aes(x, y)) + geom_point()
#' p2 <- ggplot(df, aes(x)) + geom_density()
#' p3 <- ggplot(df, aes(y)) + geom_density() + coord_flip()
#' 
#' ggtable(p1, p2, p3, layout = gglayout(matrix(c(2, 1, 0, 3), 2)))
#'
#' # combine ggtable and ggplot by ggarange
#' gt1 <- ggtable(p[[1]], p[[10]], nrow = 1)
#' gt2 <- ggtable(p[[3]], p[[5]], p[[7]], nrow = 1)
#' ggarrange(gt1, gt2, p[[4]], ncol = 1)
ggtable <- function(..., plots = NULL, dim = NULL, nrow = dim[1], ncol = dim[2], byrow = TRUE, widths = NULL, heights = NULL, respect = FALSE,
                    layout = NULL, main = NULL) {
  
  # detect plots
  if (is.null(plots)) plots <- list(...)
  plots <- Filter(function(x) any(inherits(x, c("ggtable", "ggplot"))), plots)
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

  # transfrom ggplot into gtable
  plot_gtables <- lapply(plots,
    function(x) {
      if(inherits(x, "ggplot"))
        ggplot_gtable(ggplot_build(x))
      else
        x
    })

  # matrix of gtables
  mat <- array(list(), dim(layout$mat))
  for (i in seq(layout$np)) {
    mat[layout$row[[i]], layout$col[[i]]] <- list(plot_gtables[[i]])
  }

  # nrow and ncol of each cell
  mat.nr <- apply(mat, c(1, 2), function(x)length(x[[1]]$heights))
  mat.nc <- apply(mat, c(1, 2), function(x)length(x[[1]]$widths))

  # at the moment, only blank cell are filled with blank gtable
  #
  # TODO: handle inconsistent dimension
  unr <- unique(c(mat.nr))
  unc <- unique(c(mat.nc))
  if (length(unr[unr != 0]) != 1 || length(unc[unc != 0]) != 1)
    stop("ggtable cannot handle inconsistent dimensions.")
  
  # max nrow/ncol
  # all cell must have same dim with mat.nr.max/mat.nc.max
  mat.nr.max <- max(mat.nr, na.rm = T)
  mat.nc.max <- max(mat.nc, na.rm = T)

  # blank gtable with mat.nr.max/mat.nc.max
  blank_gtable <- gtable(widths = unit(rep(0, mat.nc.max), "null"), heights = unit(rep(0, mat.nr.max), "null"), name = "blank")
  mat[mat.nr == 0 & mat.nc == 0] <- list(blank_gtable)

  # r/c-bind matix of gtable
  ret_gtable <- Reduce(function(x, y) rbind.gtable(x, y, width = "max"), apply(mat, 1, function(m) Reduce(function(x, y) cbind.gtable(x, y, height = "max"), m)))

  # relative width/height of panel
  # TODO: not elegant...
  px <- unique(subset(ret_gtable$layout, name == "panel")$l)
  py <- unique(subset(ret_gtable$layout, name == "panel")$t)
  for (i in seq(px)) ret_gtable$widths[[px[i]]] <- layout$widths[i]
  for (i in seq(py)) ret_gtable$heights[[py[i]]] <- layout$heights[i]

  # add main title
  if (!is.null(main)) {
    txt <- textGrob(main)
    ret_gtable <- gtable_add_rows(ret_gtable, grobHeight(txt) * 2, pos = 0)
    ret_gtable <- gtable_add_grob(ret_gtable, txt, 1, 1, 1, length(ret_gtable$widths))
  }

  class(ret_gtable) <- c("ggtable", "gtable")
  ret_gtable
}

#' Print ggtable object
#' 
#' @param x plot to display
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
#' @param ... other arguments not used by this method
#' @S3method print ggtable
#' @seealso \code{\link{ggtable}}
print.ggtable <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  set_last_plot(x)
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

