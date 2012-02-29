\name{guide_axis}
\alias{guide_axis}
\title{Axis guide}
\usage{
  guide_axis(title = NULL, ticks.major = waiver(),
    ticks.minor = NULL, label = waiver(),
    breaks = waiver(), minor_breaks = waiver(),
    transform = NULL, inverse_transform = NULL,
    position = NULL)
}
\arguments{
  \item{title}{a character of the title of this axis.}

  \item{ticks.major}{waiver, theme or NULL (NYI)}

  \item{ticks.minor}{waiver, theme or NULL (NYI)}

  \item{label}{label of this axis. A function, vector,
  waiver, or NULL.}

  \item{breaks}{breaks of this axis. A function, vector,
  waiver, or NULL.}

  \item{minor_breaks}{A function, vector, waiver, or NULL.}

  \item{transform}{transformation function for this axis.}

  \item{inverse_transform}{inverse function of the
  transformation function.}

  \item{position}{position of this axis}
}
\value{
  grob of axis guide
}
\description{
  Axis guide
}

