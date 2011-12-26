\name{print.ggtable}
\alias{print.ggtable}
\title{Draw plot on current graphics device.}
\usage{
  \method{print}{ggtable} (x, newpage = is.null(vp),
    vp = NULL, ...)
}
\arguments{
  \item{x}{plot to display}

  \item{newpage}{draw new (empty) page first?}

  \item{vp}{viewport to draw plot in}

  \item{...}{other arguments not used by this method}
}
\description{
  Draw plot on current graphics device.
}
\examples{
p1 <- ggplot(economics, aes(date, pce)) + geom_point()
p2 <- ggplot(economics, aes(pce)) + geom_histogram()
p3 <- ggplot(economics, aes(pce, pop)) + stat_smooth() + opts(axis.text.x = theme_text(angle = 90))
p4 <- ggplot(economics, aes(uempmed)) + geom_histogram() + opts(axis.text.y = theme_blank())

cbind(p1, p2, p3)
rbind(cbind(p1, p2), cbind(p3, p4))
}

