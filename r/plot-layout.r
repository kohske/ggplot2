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
#' @keywords hplot
#' @S3method print ggtable
#' @method print ggtable
#' @example
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
