#' Lay out panels in a grid.
#'
#' @param facets a formula with the rows (of the tabular display) on the LHS
#'   and the columns (of the tabular display) on the RHS; the dot in the
#'   formula is used to indicate there should be no faceting on this dimension
#'   (either row or column). The formula can also be provided as a string
#'   instead of a classical formula object
#' @param margins logical value, should marginal rows and columns be displayed
#' @param scales Are scales shared across all facets (the default,
#'   \code{"fixed"}), or do they vary across rows (\code{"free_x"}),
#'   columns (\code{"free_y"}), or both rows and columns (\code{"free"})
#' @param space If \code{"fixed"}, the default, all panels have the same size.
#'   If \code{"free_y"} their height will be proportional to the length of the
#'   y scale; if \code{"free_x"} their width will be proportional to the 
#'  length of the x scale; or if \code{"free"} both height and width will 
#'  vary.  This setting has no effect unless the appropriate scales also vary.
#' @param labeller A function that takes two arguments (\code{variable} and 
#'   \code{value}) and returns a string suitable for display in the facet
#'   strip. See \code{\link{label_value}} for more details and pointers
#'   to other options.
#' @param as.table If \code{TRUE}, the default, the facets are laid out like
#'   a table with highest values at the bottom-right. If \code{FALSE}, the 
#'   facets are laid out like a plot with the highest value at the top-right.
#' @param shrink If \code{TRUE}, will shrink scales to fit output of
#'   statistics, not raw data. If \code{FALSE}, will be range of raw data
#'   before statistical summary.
#' @param drop If \code{TRUE}, the default, all factor levels not used in the
#'   data will automatically be dropped. If \code{FALSE}, all factor levels
#'   will be shown, regardless of whether or not they appear in the data.
#' @param strip position of the strip (tblr) or NULL for no strip.
#' @export
#' @examples 
#' \donttest{
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' # With one variable
#' p + facet_grid(. ~ cyl)
#' p + facet_grid(cyl ~ .)
#' 
#' # With two variables
#' p + facet_grid(vs ~ am)
#' p + facet_grid(am ~ vs)
#' p + facet_grid(vs ~ am, margins=TRUE)
#' 
#' # To change plot order of facet grid,
#' # change the order of variable levels with factor()
#'
#' set.seed(6809)
#' diamonds <- diamonds[sample(nrow(diamonds), 1000), ]
#' diamonds$cut <- factor(diamonds$cut, 
#'          levels = c("Ideal", "Very Good", "Fair", "Good", "Premium"))
#'
#' # Repeat first example with new order
#' p <- ggplot(diamonds, aes(carat, ..density..)) +
#'         geom_histogram(binwidth = 1)
#' p + facet_grid(. ~ cut)
#' 
#' qplot(mpg, wt, data=mtcars, facets = . ~ vs + am)
#' qplot(mpg, wt, data=mtcars, facets = vs + am ~ . )
#' 
#' # You can also use strings, which makes it a little easier
#' # when writing functions that generate faceting specifications
#' # p + facet_grid("cut ~ .")
#' 
#' # see also ?plotmatrix for the scatterplot matrix
#' 
#' # If there isn't any data for a given combination, that panel
#' # will be empty
#' qplot(mpg, wt, data=mtcars) + facet_grid(cyl ~ vs)
#' 
# If you combine a facetted dataset with a dataset that lacks those
# facetting variables, the data will be repeated across the missing
# combinations:
#' p <- qplot(mpg, wt, data=mtcars, facets = vs ~ cyl)
#' 
#' df <- data.frame(mpg = 22, wt = 3)
#' p + geom_point(data = df, colour="red", size = 2)
#' 
#' df2 <- data.frame(mpg = c(19, 22), wt = c(2,4), vs = c(0, 1))
#' p + geom_point(data = df2, colour="red", size = 2)
#' 
#' df3 <- data.frame(mpg = c(19, 22), wt = c(2,4), vs = c(1, 1))
#' p + geom_point(data = df3, colour="red", size = 2)
#' 
#' 
#' # You can also choose whether the scales should be constant
#' # across all panels (the default), or whether they should be allowed
#' # to vary
#' mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
#' 
#' mt + facet_grid(. ~ cyl, scales = "free")
#' # If scales and space are free, then the mapping between position
#' # and values in the data will be the same across all panels
#' mt + facet_grid(. ~ cyl, scales = "free", space = "free")
#' 
#' mt + facet_grid(vs ~ am, scales = "free")
#' mt + facet_grid(vs ~ am, scales = "free_x")
#' mt + facet_grid(vs ~ am, scales = "free_y")
#' mt + facet_grid(vs ~ am, scales = "free", space="free")
#' mt + facet_grid(vs ~ am, scales = "free", space="free_x")
#' mt + facet_grid(vs ~ am, scales = "free", space="free_y")
#' 
#' # You may need to set your own breaks for consistent display:
#' mt + facet_grid(. ~ cyl, scales = "free_x", space="free") +
#'   scale_x_continuous(breaks = seq(10, 36, by = 2))
#' # Adding scale limits override free scales:
#' last_plot() + xlim(10, 15)
#' 
#' # Free scales are particularly useful for categorical variables
#' qplot(cty, model, data=mpg) +
#'   facet_grid(manufacturer ~ ., scales = "free", space = "free")
#' # particularly when you reorder factor levels
#' mpg <- within(mpg, {
#'   model <- reorder(model, cty)
#'   manufacturer <- reorder(manufacturer, cty)
#' })
#' last_plot() %+% mpg + opts(strip.text.y = theme_text())
#' 
#' # Use as.table to to control direction of horizontal facets, TRUE by default
#' h <- ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point()
#' h + facet_grid(cyl ~ vs)
#' h + facet_grid(cyl ~ vs, as.table = FALSE)
#' 
#' # Use labeller to control facet labels, label_value is default
#' h + facet_grid(cyl ~ vs, labeller = label_both)
#' # Using label_parsed, see ?plotmath for more options
#' mtcars$cyl2 <- factor(mtcars$cyl, labels = c("alpha", "beta", "sqrt(x, y)"))
#' k <- qplot(wt, mpg, data = mtcars)
#' k + facet_grid(. ~ cyl2)
#' k + facet_grid(. ~ cyl2, labeller = label_parsed)
#' # For label_bquote the label value is x.
#' p <- qplot(wt, mpg, data = mtcars)
#' p + facet_grid(. ~ vs, labeller = label_bquote(alpha ^ .(x)))
#' p + facet_grid(. ~ vs, labeller = label_bquote(.(x) ^ .(x))) 
#' }
facet_grid <- function(facets, margins = FALSE, scales = "fixed", space = "fixed", shrink = TRUE, labeller = "label_value", as.table = TRUE, drop = TRUE, strip = "tr") {
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )
  
  space <- match.arg(space, c("fixed", "free_x", "free_y", "free"))
  space_free <- list(
      x = any(space %in% c("free_x", "free")),
      y = any(space %in% c("free_y", "free"))
  )
  
  # Facets can either be a formula, a string, or a list of things to be
  # convert to quoted
  if (is.character(facets)) {
    facets <- as.formula(facets)
  }
  if (is.formula(facets)) {
    lhs <- function(x) if(length(x) == 2) NULL else x[-3]
    rhs <- function(x) if(length(x) == 2) x else x[-2]
    
    rows <- as.quoted(lhs(facets))
    rows <- rows[!sapply(rows, identical, as.name("."))]
    cols <- as.quoted(rhs(facets))
    cols <- cols[!sapply(cols, identical, as.name("."))]
  }
  if (is.list(facets)) {
    rows <- as.quoted(facets[[1]])
    cols <- as.quoted(facets[[2]])
  }
  if (length(rows) + length(cols) == 0) {
    stop("Must specify at least one variable to facet by", call. = FALSE)
  }

  # position of strip
  strip_h <- str_extract_all(strip, "[tb]")[[1]]
  if (length(strip_h) == 0) {
    strip_h <- "null"
  } else if (length(strip_h) > 1) {
    stop("Invalid strip:", strip, "in facet_grid.")
  } else {
    strip_h <- switch(strip_h, t = "top", b = "bottom", "top")
  }
  
  strip_v <- str_extract_all(strip, "[lr]")[[1]]
  if (length(strip_v) == 0) {
    strip_v <- "null"
  } else if (length(strip_v) > 1) {
    stop("Invalid strip:", strip, "in facet_grid.")
  } else {
    strip_v <- switch(strip_v, l = "left", r = "right", "right")
  }
  
  strip <- list(h = strip_h, v = strip_v)
  
  facet(
    rows = rows, cols = cols, margins = margins, shrink = shrink,
    free = free, space_free = space_free, 
    labeller = labeller, as.table = as.table, drop = drop,
    strip = strip,
    subclass = "grid"
  )
}

#' @S3method facet_train_layout grid
facet_train_layout.grid <- function(facet, data) { 
  layout <- layout_grid(data, facet$rows, facet$cols, facet$margins,
    facet$drop)
    
  # Relax constraints, if necessary
  layout$SCALE_X <- if (facet$free$x) layout$COL else 1L
  layout$SCALE_Y <- if (facet$free$y) layout$ROW else 1L
  
  layout
}


#' @S3method facet_map_layout grid
facet_map_layout.grid <- function(facet, data, layout) {
  locate_grid(data, layout, facet$rows, facet$cols, facet$margins)
}

#' @S3method facet_render grid
facet_render.grid <- function(facet, panel, coord, theme, geom_grobs) {
  nrow <- max(panel$layout$ROW)
  ncol <- max(panel$layout$COL)
  n <- nrow(panel$layout$COL)

  # information for building panel table
  gtinfo <- within(list(), {
    
    # number of strips
    nstripH <- length(facet$cols)
    nstripV <- length(facet$rows)
    
    # dimension of gtable
    dim <- c(nrow * 2 + 2 + nstripH, ncol * 2 + 2 + nstripV)

    # offset of panel region
    poffset_row <- 1 + switch(facet$strip$h, top = nstripH, bottom = 0, 0) + 1
    poffset_col <- 1 + switch(facet$strip$v, left = nstripV, top = 0, 0) + 1

    # row/col indices of panels, margins, guides, and strips
    panels <- list(r = (seq(nrow) - 1) * 2 + poffset_row,
                   c = (seq(ncol) - 1) * 2 + poffset_col)
    margins <- list(r = (seq(nrow - 1) - 1) * 2 + poffset_row + 1,
                    c = (seq(ncol - 1) - 1) * 2 + poffset_col + 1)
    guides <- list(r = c(1, dim[1]), c = c(1, dim[2]))
    strips <- list(r = switch(facet$strip$h, null = NULL, top = 1 + seq(nstripH), bottom = dim[1] - seq(nstripH)),
                   c = switch(facet$strip$v, null = NULL, left = 1 + seq(nstripV), right = dim[2] - seq(nstripV)))
  })

  table <- gtable(widths = seq_len(gtinfo$dim[2]), heights = seq_len(gtinfo$dim[1]))
  attr(table, "gtinfo") <- gtinfo

  table <- facet_axes(facet, table, panel, coord, theme)
  table <- facet_strips(facet, table, panel, theme)
  table <- facet_panels(facet, table, panel, coord, theme, geom_grobs)
  
  # sizes
  
  heights <- rep(list(unit(0, "mm")), gtinfo$dim[1])
  widths <- rep(list(unit(0, "mm")), gtinfo$dim[2])

  # panel
  
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

  size <- function(x) unit(diff(scale_dimension(x)), "null")
  
  if (facet$space_free$x) {
    x_scales <- panel$layout$SCALE_X[panel$layout$ROW == 1]
    panel_widths <- llply(panel$x_scales, size)[x_scales]
  } else {
    panel_widths <- rep(list(unit(1, "null")), ncol)
  }
  if (facet$space_free$y) {
    y_scales <- panel$layout$SCALE_Y[panel$layout$COL == 1]
    panel_heights <- llply(panel$y_scales, size)[y_scales]
  } else {
    panel_heights <- rep(list(unit(1 * aspect_ratio, "null")), nrow)
  }

  heights[gtinfo$panels$r] <- panel_heights
  widths[gtinfo$panels$c] <- panel_widths
  
  # margin b/w panel
  heights[gtinfo$margins$r] <- list(theme$panel.margin)
  widths[gtinfo$margins$c] <- list(theme$panel.margin)

  zmax <- function(x) do.call("max", c(list(unit(0, "mm")), x))
  
  # strips
  widths[gtinfo$strips$c] <- llply(gtinfo$strips$c, function(i) zmax(llply(table$grobs[which(table$layout$l == i)], grobWidth)))
  heights[gtinfo$strips$r] <- llply(gtinfo$strips$r, function(i) zmax(llply(table$grobs[which(table$layout$t == i)], grobHeight)))

  # guides
  widths[gtinfo$guides$c] <-
    llply(gtinfo$guides$c,
          function(i) zmax(llply(table$grobs[which(table$layout$l == i)], grobWidth)))
  heights[gtinfo$guides$r] <-
    llply(gtinfo$guides$r,
          function(i) zmax(llply(table$grobs[which(table$layout$t == i)], grobHeight)))

  table$widths <- do.call("unit.c", widths)
  table$heights <- do.call("unit.c", heights)
  
  table$respect <- respect
  table$name <- "layout"

  attr(table, "gtinfo") <- NULL
  table
}

#' @S3method facet_strips grid
facet_strips.grid <- function(facet, table, panel, theme) {
  pl <- panel$layout
  labeller <- match.fun(facet$labeller)
  
  if (facet$strip$h != "null") {
    colvars <- subset(panel$layout,
                      ROW == switch (facet$strip$h, top = 1, bottom = max(pl$ROW)),
                      select = c(names(facet$cols), "PANEL"))
    for (i in seq_along(facet$col)) {
      for (j in seq_len(nrow(colvars))) {
        label <- labeller(names(colvars)[i], colvars[j, i])
        grob <- ggstrip(label, TRUE, theme)
        loc <- facet_position_grob_grid(attr(table, "gtinfo"), pl, "strip", as.numeric(colvars[j, ]$PANEL), facet$strip$h, i)
        table <- gtable_add_grob(table, list(grob), loc[1], loc[2], clip = "off", name = grob$name)      
      }
    }
  }

  if (facet$strip$v != "null") {
    rowvars <- subset(panel$layout,
                      COL == switch (facet$strip$v, left = 1, right = max(pl$COL)),
                      select = c(names(facet$rows), "PANEL"))
    for (i in seq_along(facet$row)) {
      for (j in seq_len(nrow(rowvars))) {
        label <- labeller(names(rowvars)[i], rowvars[j, i])
        grob <- ggstrip(label, TRUE, theme)
        loc <- facet_position_grob_grid(attr(table, "gtinfo"), pl, "strip", as.numeric(rowvars[j, ]$PANEL), facet$strip$v, i)
        table <- gtable_add_grob(table, list(grob), loc[1], loc[2], clip = "off", name = grob$name)      
      }
    }
  }
  
  table
}

#' @S3method facet_axes grid
facet_axes.grid <- function(facet, table, panel, coord, theme) {
  place <- list(h = switch(facet$strip$h, top = "bottom", bottom = "top", NULL),
                v = switch(facet$strip$v, left = "right", right = "left", NULL))
  guides <- list(x = llply(unique(panel$layout$SCALE_X),
                   function(i) llply(panel$x_scales[[i]]$guide, pguide_gengrob,
                                     panel$ranges[[subset(panel$layout, SCALE_X == i)$PANEL[1]]],
                                     panel$x_scales[[i]], coord, theme, place$h)),
                 y = llply(unique(panel$layout$SCALE_Y),
                   function(i) llply(panel$y_scales[[i]]$guide, pguide_gengrob,
                                     panel$ranges[[subset(panel$layout, SCALE_Y == i)$PANEL[1]]],
                                     panel$y_scales[[i]], coord, theme, place$v)))

  guides$x <- llply(guides$x, Filter, f = Negate(is.null))
  guides$y <- llply(guides$y, Filter, f = Negate(is.null))

  pl <- panel$layout

  for (pi in seq_along(guides$x)) {
    for (gi in seq_along(guides$x[[pi]])) {
      pos <- attr(guides$x[[pi]][[gi]], "position")
      plp <- subset(pl, SCALE_X == pi)
      ps <- switch(pos,
                   top = subset(plp, ROW == 1)$PANEL,
                   bottom = subset(plp, ROW == max(ROW))$PANEL,
                   float = 1)
      for (p in as.numeric(ps)) {
        loc <- facet_position_grob_grid(attr(table, "gtinfo"), pl, "guide", p, pos)
        table <- gtable_add_grob(table, list(guides$x[[pi]][[gi]]), loc[1], loc[2], clip = "off", name = guides$x[[pi]][[gi]]$name)
      }
    }
  }
  
  for (pi in seq_along(guides$y)) {
    for (gi in seq_along(guides$y[[pi]])) {
      pos <- attr(guides$y[[pi]][[gi]], "position")
      plp <- subset(pl, SCALE_Y == pi)
      ps <- switch(pos,
                   left = subset(plp, COL == 1)$PANEL,
                   right = subset(plp, COL == max(ROW))$PANEL,
                   float = 1)
      for (p in as.numeric(ps)) {
        loc <- facet_position_grob_grid(attr(table, "gtinfo"), pl, "guide", p, pos)
        table <- gtable_add_grob(table, list(guides$y[[pi]][[gi]]), loc[1], loc[2], clip = "off", name = guides$y[[pi]][[gi]]$name)
      }
    }
  }
  table
}

#' @S3method facet_panels grid
facet_panels.grid <- function(facet, table, panel, coord, theme, geom_grobs) {
  
  # Add background and foreground to panels
  panels <- panel$layout$PANEL    
  ncol <- max(panel$layout$COL)
  nrow <- max(panel$layout$ROW)

  for (i in as.numeric(panels)) {
    fg <- coord_render_fg(coord, panel$range[[i]], theme)
    bg <- coord_render_bg(coord, panel$range[[i]], theme)
    
    geoms <- lapply(geom_grobs, "[[", i)
    panel_grobs <- c(list(bg), geoms, list(fg))
    grob <- gTree(children = do.call("gList", panel_grobs))  
    loc <- facet_position_grob_grid(attr(table, "gtinfo"), panel$layout, "panel", i)
    table <- gtable_add_grob(table, list(grob), loc[1], loc[2], clip = "on", name = paste("panel", grob$name, sep = ""))
  }
  table
}

icon.grid <- function(.) {
  gTree(children = gList(
    rectGrob(0, 1, width=0.95, height=0.05, hjust=0, vjust=1, gp=gpar(fill="grey60", col=NA)),
    rectGrob(0.95, 0.95, width=0.05, height=0.95, hjust=0, vjust=1, gp=gpar(fill="grey60", col=NA)),
    segmentsGrob(c(0, 0.475), c(0.475, 0), c(1, 0.475), c(0.475, 1))
  ))
}  

#' @S3method facet_vars grid
facet_vars.grid <- function(facet) {
  paste(lapply(list(facet$rows, facet$cols), paste, collapse = ", "), 
    collapse = " ~ ")
}

#' @keywords internal
#
# type: panel, float, strip, or guide.
# p: index of panel
# pos: position of the grob
# offset: used for multi-row/column strip
facet_position_grob_grid <- function(gtinfo, layout, type, p, pos = NULL, offset = 1) {

  panr <- subset(layout, PANEL == p)$ROW
  panc <- subset(layout, PANEL == p)$COL

  # location of panel in gtable
  pr <- (panr - 1) * 2 + gtinfo$poffset_row
  pc <- (panc - 1) * 2 + gtinfo$poffset_col

  switch (type,
          panel =, float = c(pr, pc),
          strip = switch(pos,
            top = c(2 + (offset - 1), pc),
            left = c(pr, 2 + (offset - 1)),
            bottom = c(gtinfo$dim[1] - 1 - (offset - 1), pc),
            right = c(pr, gtinfo$dim[2] - 1 - (offset - 1))),
          guide = switch(pos,
            top = c(1, pc),
            left = c(pr, 1),
            bottom = c(gtinfo$dim[1], pc),
            right = c(pr, gtinfo$dim[2])))
}

