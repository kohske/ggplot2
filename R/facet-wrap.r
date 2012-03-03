#' Wrap a 1d ribbon of panels into 2d.
#' 
#' @param nrow number of rows
#' @param ncol number of columns
#' @param facets formula specifying variables to facet by
#' @param scales should scales be fixed (\code{"fixed"}, the default), 
#'   free (\code{"free"}), or free in one dimension  (\code{"free_x"},
#'   \code{"free_y"})
#' @param strip position of the strip (tblr) or NULL for no strip.
#' @inheritParams facet_grid
#' @export
#' @examples
#' \donttest{
#' d <- ggplot(diamonds, aes(carat, price, fill = ..density..)) + 
#'   xlim(0, 2) + stat_binhex(na.rm = TRUE) + opts(aspect.ratio = 1)
#' d + facet_wrap(~ color)
#' d + facet_wrap(~ color, ncol = 1)
#' d + facet_wrap(~ color, ncol = 4)
#' d + facet_wrap(~ color, nrow = 1)
#' d + facet_wrap(~ color, nrow = 3)
#' 
#' # Using multiple variables continues to wrap the long ribbon of 
#' # plots into 2d - the ribbon just gets longer
#' # d + facet_wrap(~ color + cut)
#' 
#' # To change plot order of facet wrap, 
#' # change the order of varible levels with factor()
#' diamonds$color <- factor(diamonds$color, levels = c("G", "J", "D", "E", "I", "F", "H"))
#' # Repeat first example with new order
#' d <- ggplot(diamonds, aes(carat, price, fill = ..density..)) + 
#' xlim(0, 2) + stat_binhex(na.rm = TRUE) + opts(aspect.ratio = 1)
#' d + facet_wrap(~ color)
#'
#' # You can choose to keep the scales constant across all panels
#' # or vary the x scale, the y scale or both:
#' p <- qplot(price, data = diamonds, geom = "histogram", binwidth = 1000)
#' p + facet_wrap(~ color)
#' p + facet_wrap(~ color, scales = "free_y")
#' 
#' p <- qplot(displ, hwy, data = mpg)
#' p + facet_wrap(~ cyl)
#' p + facet_wrap(~ cyl, scales = "free")
#'
#' # Use as.table to to control direction of horizontal facets, TRUE by default
#' p + facet_wrap(~ cyl, as.table = FALSE)
#'
#' # Add data that does not contain all levels of the faceting variables
#' cyl6 <- subset(mpg, cyl == 6)
#' p + geom_point(data = cyl6, colour = "red", size = 1) + 
#'   facet_wrap(~ cyl)
#' p + geom_point(data = transform(cyl6, cyl = 7), colour = "red") + 
#'   facet_wrap(~ cyl)
#' p + geom_point(data = transform(cyl6, cyl = NULL), colour = "red") + 
#'   facet_wrap(~ cyl)
#' }
facet_wrap <- function(facets, nrow = NULL, ncol = NULL, scales = "fixed", shrink = TRUE, as.table = TRUE, drop = TRUE, strip = "top") {
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

  if (!is.null(strip)) strip <- match.arg(strip, c("top", "right", "bottom", "left"))
    
  facet(
    facets = as.quoted(facets), free = free, shrink = shrink,
    as.table = as.table, drop = drop,
    ncol = ncol, nrow = nrow, strip = strip,
    subclass = "wrap"
  )
}

#' @S3method facet_train_layout wrap
facet_train_layout.wrap <- function(facet, data) { 
  panels <- layout_wrap(data, facet$facets, facet$nrow, facet$ncol,
     facet$as.table, facet$drop)
  
  n <- nrow(panels)
  nrow <- max(panels$ROW)
  
  # Add scale identification
  panels$SCALE_X <- if (facet$free$x) seq_len(n) else 1L
  panels$SCALE_Y <- if (facet$free$y) seq_len(n) else 1L

  # Figure out where axes should go
  panels$AXIS_X <- if (facet$free$x) TRUE else panels$ROW == nrow
  panels$AXIS_Y <- if (facet$free$y) TRUE else panels$COL == 1
  
  panels
}

#' @S3method facet_map_layout wrap
facet_map_layout.wrap <- function(facet, data, layout) {
  locate_wrap(data, layout, facet$facets)
}

# How to think about facet wrap:
#  * vector of panels
#  * every panel has strips (strip_pos) and axes (axis_pos)
#  * if scales fixed, most axes empty
#  * combine panels, strips and axes, then wrap into 2d
#  * finally: add title, labels and legend
#
#' @S3method facet_render wrap
facet_render.wrap <- function(facet, panel, coord, theme, geom_grobs) {
  
  # If user hasn't set aspect ratio, and we have fixed scales, then
  # ask the coordinate system if it wants to specify one

  # TODO
  # now not work with coord_polar when free_x or free_y
  aspect_ratio <- theme$aspect.ratio
  if (is.null(aspect_ratio) && !facet$free$x && !facet$free$y) {
    aspect_ratio <- coord_aspect(coord, panel$ranges[[1]])
  }
  
  if (is.null(aspect_ratio)) {
    aspect_ratio <- 1
    respect <- FALSE
  } else {
    respect <- TRUE
  }
  
  layout <- panel$layout
  ncol <- max(layout$COL)
  nrow <- max(layout$ROW)
  n <- nrow(layout)

  table <- gtable(widths = rep(1, 6 * ncol), heights = rep(1, 6 * nrow))
  table <- facet_axes(facet, table, panel, coord, theme)
  table <- facet_strips(facet, table, panel, theme)
  table <- facet_panels(facet, table, panel, coord, theme, geom_grobs)

  # width/height of gtable
  zero <- unit(0, "mm")
  heights <- llply(seq_len(nrow(table)), function(i)
                   do.call("max", c(list(zero), llply(table$grobs[which(table$layout$t==i)], grobHeight))))
  widths <- llply(seq_len(ncol(table)), function(i)
                  do.call("max", c(list(zero), llply(table$grobs[which(table$layout$l==i)], grobWidth))))

  # each panel is 1 null
  ppos <- ldply(seq_len(n), function(p) facet_position_grob_wrap(NULL, panel$layout, "panel", p))
  heights[unique(ppos$V1)] <- list(unit(1, "null"))
  widths[unique(ppos$V2)] <- list(unit(1, "null"))
  
  # margin b/w panel
  vspace <- unit(height_cm(theme$panel.margin), "cm")
  hspace <- unit(width_cm(theme$panel.margin), "cm")
  widths[6 * seq_len(ncol-1)] <- list(hspace)
  heights[6 * seq_len(nrow-1)] <- list(vspace)

  table$widths <- do.call("unit.c", widths)
  table$heights <- do.call("unit.c", heights)
  table
}

#' @S3method facet_panels wrap
facet_panels.wrap <- function(facet, table, panel, coord, theme, geom_grobs) {
  for (p in as.numeric(panel$layout$PANEL)) {
    fg <- coord_render_fg(coord, panel$range[[p]], theme)
    bg <- coord_render_bg(coord, panel$range[[p]], theme)
    grobs <- lapply(geom_grobs, "[[", p)
    panel_grobs <- c(list(bg), grobs, list(fg))
    grob <- ggname(paste("panel", p, sep = "-"), 
                   gTree(children = do.call("gList", panel_grobs)))
    loc <- facet_position_grob_wrap(NULL, panel$layout, "panel", p)
    table <- gtable_add_grob(table, list(grob), loc[1], loc[2], clip = "off", name = grob$name)
  }
  table
}

#' @S3method facet_strips wrap
facet_strips.wrap <- function(facet, table, panel, theme) {
  pl <- panel$layout
  pl$label <- mdply(subset(pl, select = c(names(facet$facets))),
                    function(...) c(label = paste(llply(list(...), format, justify = "none"),
                                      collapse = ", ")))$label
  grobs <- llply(pl$label, ggstrip, theme = theme)
  for (p in seq_len(nrow(pl))) {
    loc <- facet_position_grob_wrap(NULL, pl, "strip", pl[p, ]$PANEL, facet$strip)
    table <- gtable_add_grob(table, list(grobs[[p]]), loc[1], loc[2], clip = "off", name = grobs[[p]]$name)
  }
  table
}

#' @S3method facet_axes wrap
facet_axes.wrap <- function(facet, table, panel, coord, theme) {
  place <- list(h = switch(facet$strip, top = "bottom", bottom = "top", NULL),
                v = switch(facet$strip, left = "right", right = "left", NULL))
  guides <- list(x = llply(unique(panel$layout$SCALE_X),
                   function(i) llply(panel$x_scales[[i]]$guide, pguide_gengrob, panel$ranges[[i]], panel$x_scales[[i]], coord, theme, place$h)),
                 y = llply(unique(panel$layout$SCALE_Y),
                   function(i) llply(panel$y_scales[[i]]$guide, pguide_gengrob, panel$ranges[[i]], panel$y_scales[[i]], coord, theme, place$v)))
  guides$x <- llply(guides$x, Filter, f = Negate(is.null))
  guides$y <- llply(guides$y, Filter, f = Negate(is.null))
  guides

  pl <- panel$layout

  # positions of guides
  axes <- replicate(nrow(pl), list(top = list(), left = list(), bottom = list(), right = list(), float = list()), simplify = FALSE)
  for (pi in seq_along(guides$x)) {
    for (gi in seq_along(guides$x[[pi]])) {
      pos <- attr(guides$x[[pi]][[gi]], "position")
      if (facet$free$x) {
        ps <- pi
      } else {
        ps <- switch(pos,
                     top = which(pl$ROW == 1),
                     bottom = which(pl$ROW == max(pl$ROW)),
                     float = which(pl$PANEL == 1))
      }
      for (p in ps) {
        loc <- facet_position_grob_wrap(NULL, pl, "guide", p, pos)
        table <- gtable_add_grob(table, list(guides$x[[pi]][[gi]]), loc[1], loc[2], clip = "off", name = guides$x[[pi]][[gi]]$name)
      }
    }
  }
  for (pi in seq_along(guides$y)) {
    for (gi in seq_along(guides$y[[pi]])) {
      pos <- attr(guides$y[[pi]][[gi]], "position")
      if (facet$free$y) {
        ps <- pi
      } else {
        ps <- switch(attr(guides$y[[pi]][[gi]], "position"),
                     left = which(pl$COL == 1),
                     right = which(pl$COL == max(pl$COL)),
                     float = which(pl$PANEL == 1))
      }
      for (p in ps) {
        loc <- facet_position_grob_wrap(NULL, pl, "guide", p, pos)
        table <- gtable_add_grob(table, list(guides$y[[pi]][[gi]]), loc[1], loc[2], clip = "off", name = guides$y[[pi]][[gi]]$name)
      }
    }
  }
  table  
}

#' @keywords internal
facet_wrap_place_guides <- function(guides, panel, facet) {
}

#' @S3method facet_vars wrap
facet_vars.wrap <- function(facet) {
  paste(lapply(facet$facets, paste, collapse = ", "), collapse = " ~ ")
}

facet_position_grob_wrap <- function(gtinfo, layout, type, p, pos = NULL) {
  loc <- switch(type,
                panel = c(0, 0),
                guide = switch(pos, top = c(-2, 0), left = c(0, -2), bottom = c(2, 0), right = c(0, 2), float = c(0, 0)),
                strip = switch(pos, top = c(-1, 0), left = c(0, -1), bottom = c(1, 0), right = c(0, 1)))
  pr <- subset(layout, PANEL == p)$ROW
  pc <- subset(layout, PANEL == p)$COL
  t <- 6 * (pr - 1) + loc[1] + 3
  l <- 6 * (pc - 1) + loc[2] + 3
  c(t, l)
}

