#' Wrap a 1d ribbon of panels into 2d.
#' 
#' @param nrow number of rows
#' @param ncol number of columns
#' @param facets formula specifying variables to facet by
#' @param scales should scales be fixed (\code{"fixed"}, the default), 
#'   free (\code{"free"}), or free in one dimension  (\code{"free_x"},
#'   \code{"free_y"})
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
facet_wrap <- function(facets, nrow = NULL, ncol = NULL, scales = "fixed", shrink = TRUE, as.table = TRUE, drop = TRUE) {
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )
  
  facet(
    facets = as.quoted(facets), free = free, shrink = shrink,
    as.table = as.table, drop = drop,
    ncol = ncol, nrow = nrow, 
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

  panels <- facet_panels(facet, panel, coord, theme, geom_grobs)
  guides <- facet_axes(facet, panel, coord, theme)
  strips <- facet_strips(facet, panel, theme)


  axes <- replicate(n, list(top = list(), left = list(), bottom = list(), right = list(), float = list()), simplify = FALSE)
  for (pi in seq_along(guides$x)) {
    for (gi in seq_along(guides$x[[pi]])) {
      pos <- attr(guides$x[[pi]][[gi]], "position")
      if (facet$free$x) {
        ps <- pi
      } else {
        ps <- switch(pos,
                     top = which(layout$ROW == 1),
                     bottom = which(layout$ROW == nrow),
                     float = which(layout$PANEL == 1))
      }
      for (p in ps) {
        axes[[p]][[pos]] <- c(axes[[p]][[pos]], list(guides$x[[pi]][[gi]]))
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
                     left = which(layout$COL == 1),
                     right = which(layout$COL == ncol),
                     float = which(layout$PANEL == 1))
      }
      for (p in ps) {
        axes[[p]][[pos]] <- c(axes[[p]][[pos]], list(guides$y[[pi]][[gi]]))
      }
    }
  }

  gt_dim <- c(6 * nrow, 6 * ncol)
  size <- c(6, 6)
  find_pos <- function(p, loc = "panel") {
    loc <- switch(loc, panel =, float = c(0, 0),
                  top = c(0, -2), left = c(-2, 0), bottom = c(0, 2), right = c(2, 0),
                  N = c(0, -1), W = c(-1, 0), S = c(0, 1), E = c(1, 0))
    pr <- subset(layout, PANEL == p)$ROW
    pc <- subset(layout, PANEL == p)$COL
    l <- size[1] * (pc - 1) + loc[1] + 3
    t <- size[2] * (pr - 1) + loc[2] + 3
    c(l, t)
  }
  
  # create gtable
  gt <- gtable(widths = rep(unit(1/gt_dim[2], "null"), gt_dim[2]), heights = rep(unit(1/gt_dim[1], "null"), gt_dim[1]))
  for (p in seq_along(panels)) {
    loc <- find_pos(p, "panel")
    gt <- gtable_add_grob(gt, list(panels[[p]]), loc[2], loc[1], clip = "on", name = panels[[p]]$name)

    loc <- find_pos(p, "N")
    gt <- gtable_add_grob(gt, list(strips$t[[p]]), loc[2], loc[1], clip = "on", name = panels[[p]]$name)
    
    ax <- axes[[p]]
    for (pos in names(ax)) {
      for (a in ax[[pos]]) {
        loc <- find_pos(p, pos)
        gt <- gtable_add_grob(gt, a, loc[2], loc[1], clip = "on", name = a$name)
      }
    }
  }

  vspace <- unit(height_cm(theme$panel.margin), "cm")
  hspace <- unit(width_cm(theme$panel.margin), "cm")

  zero <- unit(0, "mm")
  gt$heights <- do.call("unit.c",
                        llply(seq(nrow(gt)), function(i)
                              do.call("max", c(list(zero), llply(gt$grobs[which(gt$layout$t==i)], grobHeight)))))
  gt$widths <- do.call("unit.c",
                       llply(seq(ncol(gt)), function(i)
                             do.call("max", c(list(zero), llply(gt$grobs[which(gt$layout$l==i)], grobWidth)))))

  ppos <- ldply(seq(n), find_pos)
  gt$widths[unique(ppos$V1)] <- list(unit(1/ncol, "null"))
  gt$heights[unique(ppos$V2)] <- list(unit(1/nrow, "null"))

  gt$widths[6 * seq(ncol-1)] <- list(hspace)
  gt$heights[6 * seq(nrow-1)] <- list(vspace)

  gt
}

#' @S3method facet_panels wrap
facet_panels.wrap <- function(facet, panel, coord, theme, geom_grobs) {
  panels <- panel$layout$PANEL
  lapply(panels, function(i) {
    fg <- coord_render_fg(coord, panel$range[[i]], theme)
    bg <- coord_render_bg(coord, panel$range[[i]], theme)
    
    geom_grobs <- lapply(geom_grobs, "[[", i)
    panel_grobs <- c(list(bg), geom_grobs, list(fg))
    
    ggname(paste("panel", i, sep = "-"), 
      gTree(children = do.call("gList", panel_grobs)))
  })
}

#' @S3method facet_strips wrap
facet_strips.wrap <- function(facet, panel, theme) {
  labels_df <- panel$layout[names(facet$facets)]
  labels_df[] <- llply(labels_df, format, justify = "none")
  
  labels <- apply(labels_df, 1, paste, collapse=", ")

  list(t = llply(labels, ggstrip, theme = theme))
}

#' @S3method facet_axes wrap
facet_axes.wrap <- function(facet, panel, coord, theme) {

  panels <- panel$layout$PANEL
  guides <- list(x = llply(unique(panel$layout$SCALE_X),
                   function(i) llply(panel$x_scales[[i]]$guide, pguide_gengrob, panel$ranges[[i]], panel$x_scales[[i]], coord, theme)),
                 y = llply(unique(panel$layout$SCALE_Y),
                   function(i) llply(panel$y_scales[[i]]$guide, pguide_gengrob, panel$ranges[[i]], panel$y_scales[[i]], coord, theme)))

  guides
}

#' @S3method facet_vars wrap
facet_vars.wrap <- function(facet) {
  paste(lapply(facet$facets, paste, collapse = ", "), collapse = " ~ ")
}
