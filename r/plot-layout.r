#' Generate grid layout for ggplot2
#'
#' At the moment, there are two ways of layout specification: via matrix and via list-of-col/row-index.
#' matrix is analogue to \code{\link{layout}}.
#' list-of-index is analogue to \code{\link{grid.layout}} and \code{\link{viewport}}.
#'
#' @param mat a matrix object specifying the location of each plot. see also \code{\link{layout}} in graphics package.
#' @param row,col list of indecies of the location of each plot. see also \code{\link{grid.layout}} and \code{layout.pos.row}/\code{layout.pos.col} in \code{\link{viewport}}.
#' @param widths relative widths of each column, or unit object.
#' @param heights relative heights of each row, or unit object.
#' @param respect not implemented. see ?layout.
#' @param grid logical specifing if \code{grid.layout} is generated.
#' @param nrow number of rows of grid.
#' @param ncol number of columns of grid.
#' @S3method gglayout matrix
#' @S3method gglayout default
#' @seealso \code{\link{layout}}, \code{\link{grid.layout}}, \code{\link{viewport}}.
#' @examples
#' layout as matrix
#' m <- matrix(
#'   c(1, 1, 1, 
#'     2, 2, 3, 
#'     4, 5, 5, 
#'     6, 5, 5), 4, byrow = T)
#' lay <- gglayout(m)
#' lay
#' 
#' m <- matrix(
#'   c(1, 1, 1, 1,
#'     2, 0, 0, 3,
#'     2, 4, 4, 3), 3, byrow = T)
#' lay <- gglayout(m)
#' lay
#' 
#' m <- matrix(
#'   c(1, 2, 3, 
#'     4, 5, 6), 2, byrow = T)
#' lay <- gglayout(m, widths = c(1, 1.5, 2), heights = seq(1, 2))
#' lay
#' 
#' # layout by list of index
#' lay <- gglayout(row = list(1, 2, 2, 3, 3:4, 4), col = list(1:3, 1:2, 3, 1, 2:3, 1))
#' lay
#' 
#' lay <- gglayout(row = list(1, 2, 2, 3, 3:4, 4), col = list(1:3, 1:2, 3, 1, 2:3, 3))
#' lay
gglayout <- function(...) UseMethod("gglayout")

#' @S3method gglayout matrix
gglayout.matrix <- function(mat, widths = rep(1, ncol(mat)), heights = rep(1, nrow(mat)), respect = FALSE, grid = TRUE) {
  if (nrow(mat) != length(heights) || ncol(mat) != length(widths))
    stop("size of layout matrix and widths/heights are inconsistent")
  
  inds <- sort(unique(c(mat)))
  inds <- inds[!is.na(inds) & 0 < inds]
  coo <- llply(inds, function(x) {
    ind <- which(mat == x, arr.ind = T)
    list(row = range(ind[, "row"]), col = range(ind[, "col"]))
    })
  row <- llply(coo, "[[", "row")
  col <- llply(coo, "[[", "col")
  np <- length(inds)
  if (!inherits(widths, "unit")) widths <- unit(widths / sum(widths), "null")
  if (!inherits(heights, "unit")) heights <- unit(heights / sum(heights), "null")
  if (grid) grid <- grid.layout(nrow = nrow(mat), ncol = ncol(mat), widths = widths, heights = heights)

  structure(list(mat = mat, widths = widths, heights = heights, respect = respect, row = row, col = col, np = length(inds), grid = grid), class = "gglayout")
}

gglayout.default <- function(row = NULL, col = NULL, nrow = max(unlist(row)), ncol = max(unlist(col)),
                             widths = rep(1, ncol), heights = rep(1, nrow), respect = FALSE, grid = TRUE) {
  if (length(row) != length(col) ||
      ncol != length(widths) ||
      nrow != length(heights))
    stop("Diminstion is not consistent.")

  np <- length(row)
  mat <- matrix("", nrow, ncol)
  for (i in seq(np)) {
    mat[row[[i]], col[[i]]] <- paste(mat[row[[i]], col[[i]]], i)
  }
  if (!inherits(widths, "unit")) widths <- unit(widths / sum(widths), "null")
  if (!inherits(heights, "unit")) heights <- unit(heights / sum(heights), "null")
  if (grid) grid <- grid.layout(nrow, ncol, widths = widths, heights = heights)
  
  structure(list(mat = mat, widths = widths, heights = heights, respect = respect, row = row, col = col, np = np, grid = grid), class = "gglayout")
}

print.gglayout <- function(lay) {
  print(lay$mat)
  cat("\n",
      "Widths:  ", lay$widths, "\n",
      "Heights: ", lay$heights, "\n",
      "Respect: ", lay$respect, "\n",
      "\n")
}

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
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
#' @S3method print ggtable
#' @seealso \code{\link{gglayout}} for methods generating flexible layout.
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
                      layout = NULL, newpage = is.null(vp), vp = NULL) {
  # detect plots
  if (is.null(plots)) plots <- list(...)
  np <- length(plots)
  if (np == 0) return
  plots <- Filter(function(x) inherits(x, "ggplot"), plots)

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

  if (newpage) grid.newpage()

  draw <- function() {
    vp <- function(r, c) viewport(layout.pos.row = r, layout.pos.col = c)
    d <- function(p, r, c) print(p, vp = vp(r, c))

    pushViewport(viewport(layout = layout$grid))
    mapply(d, plots, layout$row, layout$col)
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

