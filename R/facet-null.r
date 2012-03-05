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
  
  table <- gtable(widths = rep(1, 3), heights = rep(1, 3), respect = respect)
  table <- facet_panels(facet, table, panel, coord, theme, geom_grobs)
  table <- facet_axes(facet, table, panel, coord, theme)

  # maxz returns zero if no guides is drawn
  zero <- unit(0, "mm")
  maxz <- function(x = list(), fun) if (length(x) == 0) zero else do.call("max", llply(x, fun))
  
  table$widths <- unit.c(maxz(table$grob[table$layout$l == 1], grobWidth),
                         unit(1, "null"),
                         maxz(table$grob[table$layout$l == 3], grobWidth))
  table$heights <- unit.c(maxz(table$grob[table$layout$t == 1], grobHeight),
                          unit(aspect_ratio, "null"),
                          maxz(table$grob[table$layout$t == 3], grobHeight))
  table
}

facet_panels.null <- function(facet, table, panel, coord, theme, geom_grobs) {
  fg <- coord_render_fg(coord, panel$ranges[[1]], theme)
  bg <- coord_render_bg(coord, panel$ranges[[1]], theme)

  # Flatten layers - we know there's only one panel
  geom_grobs <- lapply(geom_grobs, "[[", 1)
  panel_grobs <- c(list(bg), geom_grobs, list(fg))
  
  panel_grob <- gTree(children = do.call("gList", panel_grobs))
  gtable_add_grob(table, list(panel_grob), 2, 2, clip = "on", name = paste("panel", panel_grob$name, sep = ""))
}

facet_axes.null <- function(facet, table, panel, coord, theme) {
  guides <- pguides_gengrob(c(panel$x_scales[1], panel$y_scales[1]), panel$ranges[[1]], coord, theme)
  for (guide in guides) {
    pos <- switch(attr(guide, "position"), top = c(1, 2), bottom = c(3, 2), left = c(2, 1), right = c(2, 3), float = c(2, 2))
    table <- gtable_add_grob(table, list(guide), pos[1], pos[2], clip = "off", name = guide$name)
  }
  table
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
