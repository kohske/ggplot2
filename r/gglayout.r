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
#' @param nrow number of rows of grid.
#' @param ncol number of columns of grid.
#' @S3method gglayout matrix
#' @S3method gglayout default
#' @seealso \code{\link{layout}}, \code{\link{grid.layout}}, \code{\link{viewport}}.
#' @export
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

gglayout.matrix <- function(mat, widths = rep(1, ncol(mat)), heights = rep(1, nrow(mat)), respect = FALSE) {
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

  structure(list(mat = mat, widths = widths, heights = heights, respect = respect, row = row, col = col, np = length(inds)), class = "gglayout")
}

gglayout.default <- function(row = NULL, col = NULL, nrow = max(unlist(row)), ncol = max(unlist(col)),
                             widths = rep(1, ncol), heights = rep(1, nrow), respect = FALSE) {
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
  
  structure(list(mat = mat, widths = widths, heights = heights, respect = respect, row = row, col = col, np = np), class = "gglayout")
}

print.gglayout <- function(lay) {
  print(lay$mat)
  cat("\n",
      "Widths:  ", lay$widths, "\n",
      "Heights: ", lay$heights, "\n",
      "Respect: ", lay$respect, "\n",
      "\n")
}

#' Generate grid.layout from gglayout
#'
#' @param layout gglayout object
#' @seealso \code\link{gglayout}
gglayout_grid <- function(layout) {
  grid.layout(nrow = nrow(layout$mat), ncol = ncol(layout$mat), widths = layout$widths, heights = layout$heights)
}
