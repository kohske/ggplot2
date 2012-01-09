#' Convert object into grob
#'
#' @param x object
#' @return a grob
#' @export
#' @S3method as.grob ggplot
#' @S3method as.grob default
#' @S3method as.grob grob
#' @S3method as.grob gtable

as.grob <- function(x) UseMethod("as.grob")

as.grob.ggplot <- function(x) as.grob(ggplot_gtable(ggplot_build(x)))
as.grob.gtable <- function(x) gtable_gTree(x)
as.grob.grob <- function(x) x
as.grob.default <- function(x) stop("cannot convert type ", typeof(x), " to grob")
  
