# TODO: float range doesn't need outer padding and the line, not
#       the center, should be aligned

#' Range guide
#'
#' @export
#' @param title NYI
#' @param label
#' @param label_pos one of "trbl" or waiver. position of label
#' @param length 
#' @param unit character string
#' @param position tlbr or float
#' @param location numeric vector of x and y
#' @param space "panel" or "data"
guide_range <- function(
  title = NULL,
  label = NULL,
  label_pos = waiver(),                        
  length = 1,
  unit = NULL,
  position = NULL,
  location = NULL,
  space = "panel" # not yet implemented
  ) {
  structure(list(
    title = title,
    label = label,
    label_pos = label_pos,
    length = length,
    unit = unit,
    position = position,
    location = location,                 
    space = space
                 ),
    class=c("pguide", "range"))
}

#' @S3method pguide_gengrob.range cartesian
pguide_gengrob.range.cartesian <- function(pguide, ginfo, scale, coord, theme, place = NULL) {

  # aes of this axis
  if ("x" %in% scale$aesthetics)     type <- "h"
  else if("y" %in% scale$aesthetics) type <- "v"
  else stop()

  # range in original space of this axis
  ranges <- list(x = ginfo$x.range, y = ginfo$y.range)
  range <- switch(type, h = ranges$x, v = ranges$y)

  # position of this guide
  if (is.null(pguide$position)) {
    position <- switch(type,
           h = "bottom",
           v = "left")
  } else {
    position <- pguide$position
  }

  # location
  if (is.null(pguide$location)) {
    px <- 0.5
    py <- 0.5
  } else {
    px <- pguide$location[1]
    py <- pguide$location[2]
    
    if (pguide$space == "data") {
      # need to convert panel space to data space
      px <- rescale(px, from = ranges$x)
      py <- rescale(py, from = ranges$y)
    }
  }

  # label of range
  if (!is.null(pguide$label)) {
    label <- pguide$label
  } else if (!is.null(pguide$unit)) {
    label <- paste(pguide$length, pguide$unit)
  } else {
    label <- paste(pguide$length)
  }

  length <- pguide$length / abs(diff(range))

  # rendering
  
  padding <- unit(1.5, "mm")
  zero <- unit(0, "mm")
  one <- unit(1, "npc")
  
  if (!is.null(label)) {
    if (type == "h") {
      label_grob <- theme_render(theme, "axis.text.x", label)
    } else if (type == "v") {
      label_grob <- theme_render(theme, "axis.text.y", label)
    }

    if (is.waive(pguide$label_pos)) {
      if (type == "h")
        label_pos <- switch(position, top =, left =, right =, float = "t", bottom = "b")
      else if (type == "v")
        label_pos <- switch(position, top =, left =, bottom =, float = "l", right = "r")
    } else {
      label_pos <- pguide$label_pos
    }
  } else {
    label_grob <- NULL
  }

  if (type == "h") {
    # hirozontal
    if (!is.null(label)) {
      dim <- c(1, 5)
      if (label_pos == "t") {
        height <- unit.c(padding, grobHeight(label_grob), padding, zero, padding)
      } else if (label_pos == "b") {
        height <- unit.c(padding, zero, padding, grobHeight(label_grob), padding)
      }
    } else {
      dim <- c(1, 2)
      height <- unit.c(padding, zero, padding)
    }
    
    line <- segmentsGrob(0.5 - length/2, 0.5, 0.5 + length/2, 0.5)
    width <- one
    
    if      (label_pos == "t") { grobs <- gList(line, label_grob) }
    else if (label_pos == "b") { grobs <- gList(label_grob, line) }
    
  } else if (type == "v") {
    # vertical
    if (!is.null(label)) {
      dim <- c(5, 1)
      if (label_pos == "l") {
        width <- unit.c(padding, grobWidth(label_grob), padding, zero, padding)
      } else if (label_pos == "r") {
        width <- unit.c(padding, padding, grobWidth(label_grob), zero, padding)
      }
    } else {
      width <- unit.c(padding, zero, padding)
    }
    
    line <- segmentsGrob(0.5, 0.5 - length/2, 0.5, 0.5 + length/2)
    height <- one

    if      (label_pos == "l") grobs <- gList(label_grob, line)
    else if (label_pos == "r") grobs <- gList(line, label_grob)
  }

  if (position == "float") {
    vp <- viewport(x = px, y = py, width = sum(width), height = sum(height))
  } else if (position %in% c("top", "bottom")) {
    vp <- viewport(x = px, width = sum(width))
  } else if (position %in% c("left", "right")) {
    vp <- viewport(y = py, height = sum(height))
  }

  fg <- frameGrob(layout = grid.layout(nrow = length(height), ncol = length(width),
                    widths = width, heights = height), vp = vp)

  if (!is.null(label)) {
    fg <- placeGrob(fg, label_grob,
                    row = switch(label_pos, l =, r = 1, t = 2, b = 4),
                    col = switch(label_pos, t =, b = 1, l = 2, r = 4))
    fg <- placeGrob(fg, line,
                    row = switch(label_pos, l =, r = 1, t = 4, b = 2),
                    col = switch(label_pos, t =, b = 1, l = 4, r = 2))
  } else {
    fg <- placeGrob(fg, line,
                    row = switch(label_pos, l =, r = 1, t =, b = 2),
                    col = switch(label_pos, t =, b = 1, l =, r = 2))
  }

  structure(
    absoluteGrob(
                 gList(fg),
                 width = grobWidth(fg),
                 height = grobHeight(fg)),
            position = position
            )
}
