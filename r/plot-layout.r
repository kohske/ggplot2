#' @S3method cbind ggplot
cbind.ggplot <- function(...) cbind_gg(...)

#' @S3method cbind ggtable
cbind.ggtable <- function(...) cbind_gg(...)

# rbind all objects and return ggtable object
cbind_gg <- function(...) {
  structure(Reduce(function(x, y) cbind.gtable(x, y, height = "max"), transform_gg(...)),
    class = c("ggtable", "gtable"))
}

#' @S3method rbind ggplot
rbind.ggplot <- function(...) rbind_gg(...)

#' @S3method rbind ggtable
rbind.ggtable <- function(...) rbind_gg(...)

# rbind all objects and return ggtable object
rbind_gg <- function(...) {
  structure(Reduce(function(x, y) rbind.gtable(x, y, width = "max"), transform_gg(...)),
    class = c("ggtable", "gtable"))
}

# apply ggplot_build and ggplot_gtable to each object if need.
transform_gg <- function(...) {
  lapply(list(...),
    function(x) {
      if(inherits(x, "ggplot"))
        ggplot_gtable(ggplot_build(x))
      else
        x
    })
}

#' Draw plot on current graphics device.
#'
#' @param x plot to display
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
#' @param ... other arguments not used by this method
#' @S3method print ggtable
#' @method print ggtable
#' @examples
#' p1 <- ggplot(economics, aes(date, pce)) + geom_point()
#' p2 <- ggplot(economics, aes(pce)) + geom_histogram()
#' p3 <- ggplot(economics, aes(pce, pop)) + stat_smooth() + opts(axis.text.x = theme_text(angle = 90))
#' p4 <- ggplot(economics, aes(uempmed)) + geom_histogram() + opts(axis.text.y = theme_blank())
#' 
#' cbind(p1, p2, p3)
#' rbind(cbind(p1, p2), cbind(p3, p4))
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

#' Arrange multiple plots on grid layout
#'
#' @param ... plot to display
#' @param dim 2-length vector of c(nrow, ncol)
#' @param nrow number of rows of grid.
#' @param ncol number of columns of grid.
#' @param byrow logical. If TRUE (the default) the grid is filled by rows, otherwise the grid is filled by columns.
#' @param layout list of list specifying layout grid.
#' @param widths relative widths of each column, or unit object.
#' @param heights relative heights of each row, or unit object.  
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
#' @S3method print ggtable
#' @method print ggtable
#' @examples
#' # automatic layout as square as possible
#' p <- lapply(1:10, function(i) ggplot(mtcars, aes(factor(cyl))) + geom_bar(fill = rainbow(10)[i]) + opts(title = paste(i)))
#' 
#' layout.ggplot(p[[1]], p[[2]], p[[3]])
#' do.call("layout.ggplot", p[1:7])
#' do.call("layout.ggplot", p[1:10])
#' 
#' # sepcify dimenstion
#' layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], dim = c(2, 3))
#' layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], dim = c(2, 3), byrow = F)
#' layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], nrow = 2)
#' layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], ncol = 3)
#' 
#' # specify layout
#' layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], 
#'   layout = list(row = list(1, 1, 1, 2, 2), col = list(1, 2, 3, 1, 2)))
#' layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], 
#'   layout = list(row = list(1, 1, 1, 2, 2), col = list(1, 2, 3, 1, 2:3)))
#' layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], nrow = 3, ncol = 4,
#'   layout = list(row = list(1, 1, 1, 2, 2), col = list(1, 2, 3, 1, 2:3)))
#' 
#' # width/height
#' layout.ggplot(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], 
#'   layout = list(row = list(1, 1, 1, 2, 2), col = list(1, 2, 3, 1, 2:3)),
#'   widths = c(2, 1, 1), heights = c(2, 1))
#' layout.ggplot(p[[1]], p[[2]], p[[3]],
#'   nrow = 1, widths = c(1, 1.5, 2.5))
layout.ggplot <- function(..., dim = NULL, nrow = dim[1], ncol = dim[2], byrow = TRUE, layout = NULL, widths = NULL, heights = NULL, newpage = is.null(vp), vp = NULL) {
  # detect layout
  np <- length(list(...))
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
      layout$row <- as.list((seq(np) - 1) %% nrow + 1)
      layout$col <- as.list(ceiling(seq(np) / nrow))
    } else {
      layout$row <- as.list(ceiling(seq(np) / ncol))
      layout$col <- as.list((seq(np) - 1) %% ncol + 1)
    }
  } else {
    # layout is not NULL

    # error check
    if (length(layout$row) != np || length(layout$col) != np)
      stop("length of layout is not equal to the number of plot")
    
    # set nrow and ncol as fit the layout
    if (is.null(nrow)) nrow <- max(unlist(layout$row))
    if (is.null(ncol)) ncol <- max(unlist(layout$col))
  }

  # widths/heights
  if (!is.null(widths)) {
    if (length(widths) != ncol) stop("length of width is not equal to the number of plot")
    if (!inherits(widths, "unit")) widths <- unit(widths / sum(widths), "null")
  } else {
    widths <- unit(rep(1, ncol), "null")
  }
  if (!is.null(heights)) {
    if (length(heights) != nrow) stop("length of height is not equal to the number of plot")
    if (!inherits(heights, "unit")) heights <- unit(heights / sum(heights), "null")
  } else {
    heights <- unit(rep(1, nrow), "null")
  }

  if (newpage) grid.newpage()

  draw <- function() {
    vp <- function(r, c) viewport(layout.pos.row = r, layout.pos.col = c)
    d <- function(p, r, c) print(p, vp = vp(r, c))
    
    pushViewport(viewport(layout = grid.layout(nrow, ncol, widths = widths, heights = heights)))
    mapply(d, list(...), layout$row, layout$col)
    upViewport()
  }
  
  if (is.null(vp)) {
    draw()
  } else {
    if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
    draw()
    upViewport()
  }
  invisible()
}
