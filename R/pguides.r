## TODO
## coord_train

#' @keywords internal
pguides_gengrob <- function(scales, range, coord, theme, place = NULL) {
  # apply pguide_gengrob for each guide
  guides <- llply(scales, function(sc) llply(sc$guide, pguide_gengrob, range, sc, coord, theme, place))
  # merge all grob
  guides <- unlist(guides, recursive = FALSE)
  # drop NULL
  guides <- Filter(Negate(is.null), guides)
}

#' @keywords internal
pguide_gengrob <- function(pguide, ginfo, scale, coord, theme, place = NULL) {
  UseMethod("pguide_gengrob")
}

#' @keywords internal
pguide_gengrob_not_found <- function(pguide, ginfo, scale, coord, theme, place = NULL) {
  warning(paste("Positional guide [", class(pguide)[2], "] for Coordinate [", class(coord)[1], "] is not implemented.", sep = ""), call. = FALSE)
  NULL
}

#' @S3method pguide_gengrob axis
pguide_gengrob.axis <- function(pguide, ginfo, scale, coord, theme, place = NULL) UseMethod("pguide_gengrob.axis", coord)

#' @S3method pguide_gengrob.axis default
pguide_gengrob.axis.default <- pguide_gengrob_not_found

#' @S3method pguide_gengrob range
pguide_gengrob.range <- function(pguide, ginfo, scale, coord, theme, place = NULL) UseMethod("pguide_gengrob.range", coord)

#' @S3method pguide_gengrob.range default
pguide_gengrob.range.default <- pguide_gengrob_not_found

