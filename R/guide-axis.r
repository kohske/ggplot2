# TODO: finally cooad_train doesn't need to calculate breaks
#       since the breaks only used for some gudies
# TODO: error check for arguments
# TODO: style (margin, padding, etc)
# TODO: reuse codes in cartesian and flip
#       make scale_type(scale, coord) that returns the aes of the scale
#       if cartesian, x -> x, y -> y
#       if flip, x -> y, y -> x
#       if polar default, x -> theta, y -> r
# TODO: coord_polar will enable to set the position of axis (upper or lower)
# TODO: input param `ginfo` should be reduced to limits
# TODO: scale limits should be substituted by coord limits before doing something?
# TODO: render should use gtable
# TODO: test for linearity in making inverse transformation
# TODO: palce axis inside the panel

# Flow of guide axis
#
# 1. facet -> pguides_gengrob -> pguide_gengrob (S3 by guide)
#     -> pguide_gengrob.axis (S3 by coord)
#     -> pguide_gengrob.axis.*
#
# 2. pguide_gengrob.axis.*
#     <- pguide, ginfo, scale, coord, theme
#     |- axis_linear_train: calculate breaks
#     |- axis_linear_render: generate grob
#     -> return grob

#' Axis guide
#'
#' @export
#' @param title a character of the title of this axis.
#' @param ticks.major waiver, theme or NULL (NYI)
#' @param ticks.minor waiver, theme or NULL (NYI)
#' @param label label of this axis. A function, vector, waiver, or NULL.
#' @param breaks breaks of this axis. A function, vector, waiver, or NULL.
#' @param minor_breaks A function, vector, waiver, or NULL.
#' @param transform transformation function for this axis.
#' @param inverse_transform inverse function of the transformation function.
#' @param position position of this axis
#' @return grob of axis guide
guide_axis <- function(
  title = NULL,                       
  ticks.major = waiver(),
  ticks.minor = NULL,
  label = waiver(),
  breaks = waiver(),
  minor_breaks = waiver(),
  transform = NULL,
  inverse_transform = NULL,
  position = NULL
  ) {
  
  structure(list(
    title = title,
    ticks.major = ticks.major,
    ticks.minor = ticks.minor,
    label = label,
    breaks = breaks,
    minor_breaks = minor_breaks,
    transform = transform,
    inverse_transform = inverse_transform,
    position = position
                 ),
    class=c("pguide", "axis"))
}

#' @S3method pguide_gengrob.axis cartesian
pguide_gengrob.axis.cartesian <- function(pguide, ginfo, scale, coord, theme) {
  
  # aes of this axis
  if ("x" %in% scale$aesthetics)     type <- "h"
  else if("y" %in% scale$aesthetics) type <- "v"
  else stop()

  # range in original space
  range <- switch(type, h = ginfo$x.range, v = ginfo$y.range)
  
  # calculate break position  
  def <- axis_linear_train(pguide, range, scale)

  # position of this guide
  if (is.null(pguide$position)) {
    position <- switch(type,
           h = "bottom",
           v = "left")
  } else {
    position <- pguide$position
  }

  # Quick fix for conflicts #297 and #118
  # Previously, at = NA if there is no breaks (breaks = NA).
  # Fix for oob bug changed so that at = numeric(0) if there is no breaks.
  # Temporally, at is set as NA if there is no breaks.
  # see also SHA: f332070fca77399a84ea7a116e8c63f6990abaf6, SHA: 2ae13ad0a856c24cab6a69b523da0936ef7a94d8
  if (is.null(def$at) || length(def$at) == 0) return(zeroGrob())
  
  axis_linear_render(pguide$title, def, position, theme)  
}

#' @S3method pguide_gengrob.axis flip
pguide_gengrob.axis.flip <- function(pguide, ginfo, scale, coord, theme) {

  # aes of this axis
  if ("x" %in% scale$aesthetics)     type <- "v"
  else if("y" %in% scale$aesthetics) type <- "h"
  else stop()

  # range in original space
  range <- switch(type, h = ginfo$x.range, v = ginfo$y.range)

  # calculate break position
  def <- axis_linear_train(pguide, range, scale)

  # Quick fix for conflicts #297 and #118
  # Previously, at = NA if there is no breaks (breaks = NA).
  # Fix for oob bug changed so that at = numeric(0) if there is no breaks.
  # Temporally, at is set as NA if there is no breaks.
  # see also SHA: f332070fca77399a84ea7a116e8c63f6990abaf6, SHA: 2ae13ad0a856c24cab6a69b523da0936ef7a94d8
  if (is.null(def$at) || length(def$at) == 0) return(zeroGrob())
  
  # position of this guide
  if (is.null(pguide$position)) {
    position <- switch(type, h = "bottom", v = "left")
  } else {
    position <- pguide$position
  }

  axis_linear_render(pguide$title, def, position, theme)  
}

#' @S3method pguide_gengrob.axis polar
pguide_gengrob.axis.polar <- function(pguide, ginfo, scale, coord, theme) {
  
  # aes of this axis
  if(coord$theta %in% scale$aesthetics) {
    type <- "theta"
  } else if(coord$r %in% scale$aesthetics) {
    type <- "r"
  } else {
    stop()
  }

  if (type == "r") {

    # position of breaks
    def <- axis_linear_train(pguide, ginfo$r.range, scale)

    # position conversion for polar coords
    def$at <- rescale(def$at, c(0, 0.4)) + 0.5
    def$minor <- rescale(def$minor, c(0, 0.4)) + 0.5
    
    if (is.null(def$at) || length(def$at) == 0) return(zeroGrob())
    
    # position of this guide
    if (is.null(pguide$position)) {
      position <- "left"
    } else {
      position <- pguide$position
    }
    
    guide <- axis_linear_render(pguide$title, def, position, theme)  
  } else {
    guide <- NULL
  }
  guide
}

#' @keywords internal
#' calculate breaks and labels
axis_linear_train <- function(pguide, range, scale) {

  # guide has 3 spaces
  # 1. original space
  # 2. transformed space  
  # 3. coordinate space (normalized 0-1)

  # trasnformation functions
  #  
  # tr_fun: transformation function
  # inv_fun: inverse transformation function
  if (is.function(pguide$transform)) {
    tr_fun <- pguide$transform
    
    # if pguide$transform is not NULL and pguide$inverse_transform is NULL,
    # the transform is assumed to be linear.
    if (is.function(pguide$inverse_transform)) {
      inv_fun <- pguide$inverse_transform
    } else {
      x0 <- tr_fun(0); x1 <- tr_fun(1)
      pguide$inverse_transform <- function(x) (x - x0) / ( x1 - x0)
      warning(paste("Inverse of the transform function:", paste(deparse(pguide$trans), collapse = ""), ", is not defined.",
                    "Assuming linear transformation."))
      inv_fun <- pguide$inverse_transform
    }
  } else {
    # if no transformation, transformation = I
    tr_fun <- I
    inv_fun <- I
  }

  # limits
  or_limits <- range # original space
  tr_limits <- tr_fun(or_limits) # trans space
  # co_limits is [0, 1]
  
  # breaks
  #
  # or_breaks: breaks in original space
  # tr_breaks: breaks in transformed space  
  # co_breaks: breaks in coordinate space (normalized 0-1)
  
  # if waiver(), pretty breaks or scales'defaut is applied.
  # if function, the function is applied.
  # if NULL, no breaks,
  # otherwise, as-is.
  if (is.waive(pguide$breaks)) {
    if (is.function(pguide$transform)) {
      # if trans = TRUE, use pretty breaks
      tr_breaks <- pretty(tr_limits)
    } else {
      # otherwise, use scale's default
      tr_breaks <- scale_break_positions(scale)
    }
  } else if (is.function(pguide$breaks)) {
    tr_breaks <- pguide$breaks(tr_limits)
  } else if (is.null(pguide$breaks)) {
    tr_breaks <- NULL
  } else {
    tr_breaks <- pguide$breaks
  }

  # breaks in other space
  or_breaks <- inv_fun(tr_breaks)
  co_breaks <- censor(rescale(or_breaks, from = or_limits))

  # minor breaks
  #
  # or_minor: minor breaks in original space
  # tr_minor: minor breaks in transformed space  
  # co_minor: minor breaks in coordinate space (normalized 0-1)

  if (is.waive(pguide$minor_breaks)) {
    if (is.function(pguide$transform)) {
      # if trans = TRUE, use pretty breaks
      tr_minor <- minor_breaks()(tr_breaks, tr_limits)
    } else {
      # otherwise, use scale's default
      tr_minor <- scale_breaks_minor_positions(scale)
    }
  } else if (is.function(pguide$minor_breaks)) {
    tr_minor <- pguide$minor_breaks(tr_breaks, tr_limits)
  } else if (is.null(pguide$minor_breaks)) {
    tr_minor <- NULL
  } else {
    tr_minor <- pguide$minor_breaks
  }

  # minor breaks in other space
  or_minor <- inv_fun(tr_minor)
  co_minor <- censor(rescale(or_minor, from = or_limits))

  # label of major breaks
  if (is.function(pguide$label)) {
    labels <- pguide$labels(tr_breaks)
  } else if (is.waive(pguide$label)) {
    labels <- as.character(tr_breaks)
  } else if (is.character(pguide$label)) {
    labels <- pguide$label
  } else if (is.null(pguide$label)) {
    labels <- NULL
  } else {
    stop()
  }

  list(at = co_breaks, minor = co_minor, labels = labels)
}

#' @keywords internal
axis_linear_render <- function(title, def, position, theme) {
  # TODO: move to gtalbe
  
  at <- unit(def$at, "native")
  labels <- def$labels
  
  length <- theme$axis.ticks.length
  label_pos <- length + theme$axis.ticks.margin

  one <- unit(1, "npc")
  
  label_render <- switch(position,
    top = , bottom = "axis.text.x",
    left = , right = "axis.text.y"
  )

  label_x <- switch(position,
    top = , 
    bottom = at,
    right = label_pos,
    left = one - label_pos
  )
  label_y <- switch(position,
    top = label_pos, 
    bottom = one - label_pos,
    right = ,
    left = at,
  )

  if (is.list(labels)) {
    if (any(sapply(labels, is.language))) {
      labels <- do.call(expression, labels)
    } else {
      labels <- unlist(labels)    
    }
  }

  labels <- switch(position,
                   top = ,
                   bottom = theme_render(theme, label_render, labels, x = label_x),
                   right = ,
                   left =  theme_render(theme, label_render, labels, y = label_y))
  
  line <- switch(position,
    top =    theme_render(theme, "axis.line", 0, 0, 1, 0),
    bottom = theme_render(theme, "axis.line", 0, 1, 1, 1),
    right =  theme_render(theme, "axis.line", 0, 1, 0, 1),
    left =   theme_render(theme, "axis.line", 1, 0, 1, 1)
  )
  
  ticks <- switch(position,
    top =    theme_render(theme, "axis.ticks", at, 0, at, length),
    bottom = theme_render(theme, "axis.ticks", at, one - length, at, 1),
    right =  theme_render(theme, "axis.ticks", 0, at, length, at),
    left =   theme_render(theme, "axis.ticks", one - length, at, 1, at)
  )

  just <- switch(position,
    top =    "bottom",
    bottom = "top",
    right =  "left",
    left =   "right"
  )

  if (!is.null(title)) {
    title <- switch(position,
                    top =, bottom = theme_render(theme, "axis.title.x", title),
                    left =, right = theme_render(theme, "axis.title.y", title))
  } else {
    title <- zeroGrob()
  }
  
  fg <- ggname("axis", switch(position,
                              top = frameGrob(layout = grid.layout(nrow = 3, ncol = 1,
                                                   widths = one, heights = unit.c(grobHeight(title), grobHeight(labels), label_pos), just = just)),
                              bottom = frameGrob(layout = grid.layout(nrow = 3, ncol = 1,
                                                   widths = one, heights = unit.c(label_pos, grobHeight(labels), grobHeight(title)), just = just)),
                              right = frameGrob(layout = grid.layout(nrow = 1, ncol = 3,
                                                  widths = unit.c(label_pos, grobWidth(labels), grobWidth(title)), heights = one, just = just)),
                              left = frameGrob(layout = grid.layout(nrow = 1, ncol = 3,
                                                 widths = unit.c(grobWidth(title), grobWidth(labels), label_pos), heights = one, just = just))))
  

  if (!is.zero(labels)) {
    fg <- switch(position,
                 top = placeGrob(fg, labels, row = 2, col = 1),
                 bottom = placeGrob(fg, labels, row = 2, col = 1),
                 right = placeGrob(fg, labels, row = 1, col = 2),
                 left = placeGrob(fg, labels, row = 1, col = 2))
  }

  if (!is.zero(ticks)) {
    fg <- switch(position,
                 top = placeGrob(fg, ticks, row = 3, col = 1),
                 bottom = placeGrob(fg, ticks, row = 1, col = 1),
                 right = placeGrob(fg, ticks, row = 1, col = 1),
                 left = placeGrob(fg, ticks, row = 1, col = 3))
  }

  if (!is.null(title)) {
    fg <- switch(position,
                 top = placeGrob(fg, title, row = 1, col = 1),
                 bottom = placeGrob(fg, title, row = 3, col = 1),
                 right = placeGrob(fg, title, row = 1, col = 3),
                 left = placeGrob(fg, title, row = 1, col = 1))
  }

  structure(
    absoluteGrob(
                 gList(line, fg),
                 width = grobWidth(fg),
                 height = grobHeight(fg)),
            position = position
            )
}

minor_breaks <- function(n = 1) {
  
  function(breaks, limits) {    
    breaks <- breaks[!is.na(breaks)]
    if (length(breaks) < 2) return()

    # Ensure minor breaks cover entire range of data
    bd <- diff(breaks)[1]
    if (min(limits) < min(breaks)) {
      breaks <- c(breaks[1] - bd, breaks)
    }
    if (max(limits) > max(breaks)) {
      breaks <- c(breaks, breaks[length(breaks)] + bd)
    }

    b <- breaks
    unique(unlist(mapply(seq, b[-length(b)], b[-1], length = n + 2,
      SIMPLIFY = FALSE)))
  }
}


