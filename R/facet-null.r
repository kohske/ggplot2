#' Facet specification: a single panel.
#'
#' @inheritParams facet_grid
#' @export
#' @examples
#' # facet_null is the default facetting specification if you 
#' # don't override it with facet_grid or facet_wrap
#' ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' qplot(mpg, wt, data = mtcars)
facet_null <- function(shrink = TRUE) {
  facet(shrink = shrink, subclass = "null")
}

#' @S3method facet_train_layout null
facet_train_layout.null <- function(facet, data) {     
  data.frame(
    PANEL = 1L, ROW = 1L, COL = 1L, 
    SCALE_X = 1L, SCALE_Y = 1L)
}

#' @S3method facet_map_layout null
facet_map_layout.null <- function(facet, data, layout) {
  if (empty(data)) return(data.frame(PANEL = 1))
  data$PANEL <- 1L
  data
}

#' @S3method facet_render null
facet_render.null <- function(facet, panel, coord, theme, geom_grobs) {
  range <- panel$ranges[[1]]
  
  # Figure out aspect ratio
  aspect_ratio <- theme$aspect.ratio %||% coord_aspect(coord, range)
  if (is.null(aspect_ratio)) {
    aspect_ratio <- 1
    respect <- FALSE
  } else {
    respect <- TRUE
  }
  
  fg <- coord_render_fg(coord, range, theme)
  bg <- coord_render_bg(coord, range, theme)

  # Flatten layers - we know there's only one panel
  geom_grobs <- lapply(geom_grobs, "[[", 1)
  panel_grobs <- c(list(bg), geom_grobs, list(fg))
  
  panel_grob <- gTree(children = do.call("gList", panel_grobs))  

  # generate grobs of positional guide
  guides <- pguides_gengrob(c(panel$x_scales[1], panel$y_scales[1]), range, coord, theme)

  # calculate placement of each guide
  guides <- llply(guides,
                  function(guide) {
                    attr(guide, "place") <- switch(attr(guide, "position"),
                                                top = c(2, 1), bottom = c(2, 3), left = c(1, 2), right = c(3, 2), float = c(2, 2))
                    guide
                  })

  # maxz returns zero if no guides is drawn
  zero <- unit(0, "null")
  maxz <- function(x = list()) if (length(x) == 0) zero else do.call("max", x)

  # calculate widths/heights by guides size
  topHeight    <- maxz(llply(Filter(function(guide)attr(guide, "place")[2] == 1, guides), grobHeight))
  bottomHeight <- maxz(llply(Filter(function(guide)attr(guide, "place")[2] == 3, guides), grobHeight))
  leftWidth    <- maxz(llply(Filter(function(guide)attr(guide, "place")[1] == 1, guides), grobWidth))
  rightWidth   <- maxz(llply(Filter(function(guide)attr(guide, "place")[1] == 3, guides), grobWidth))

  # w/h of gtable
  widths <- unit.c(leftWidth, unit(1, "null"), rightWidth)
  heights <- unit.c(topHeight, unit(1, "null"), bottomHeight)

  # create gtable
  gt <- gtable(widths = widths, heights = heights, respect = respect)
  
  # panel
  gt <- gtable_add_grob(gt, list(panel_grob), 2, 2, 2, 2, clip = "on", name = "panel")
  for (guide in guides) {
    gt <- gtable_add_grob(gt, list(guide), attr(guide, "place")[2], attr(guide, "place")[1], clip = "off", name = guide$name)
  }
  
  gt
}
  
icon.facet_null <- function(.) {
  gTree(children = gList(
    rectGrob(0, 1, width=0.95, height=0.05, hjust=0, vjust=1, gp=gpar(fill="grey60", col=NA)),
    rectGrob(0.95, 0.95, width=0.05, height=0.95, hjust=0, vjust=1, gp=gpar(fill="grey60", col=NA)),
    segmentsGrob(c(0, 0.475), c(0.475, 0), c(1, 0.475), c(0.475, 1))
  ))
}  

#' @S3method facet_vars null
facet_vars.null <- function(facet) ""
