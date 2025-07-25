% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_cases_5er.R
\name{scale_y_cases_5er}
\alias{scale_y_cases_5er}
\alias{scale_x_cases_5er}
\title{Continuous x-axis and y-axis scale for (case) counts}
\usage{
scale_y_cases_5er(
  name = waiver(),
  n = 8,
  min.n = 5,
  u5.bias = 4,
  expand = NULL,
  limits = c(0, NA),
  labels = waiver(),
  oob = scales::censor,
  na.value = NA_real_,
  transform = "identity",
  position = "left",
  sec.axis = waiver(),
  guide = waiver(),
  ...
)

scale_x_cases_5er(
  name = waiver(),
  n = 8,
  min.n = 5,
  u5.bias = 4,
  expand = NULL,
  limits = c(0, NA),
  labels = waiver(),
  oob = scales::censor,
  na.value = NA_real_,
  transform = "identity",
  position = "bottom",
  sec.axis = waiver(),
  guide = waiver(),
  ...
)
}
\arguments{
\item{name}{The name of the scale. Used as the axis or legend title. If
\code{waiver()}, the default, the name of the scale is taken from the first
mapping used for that aesthetic. If \code{NULL}, the legend title will be
omitted.}

\item{n}{Target number of breaks passed to \code{\link[base:pretty]{base::pretty()}}. Defaults to 8.}

\item{min.n}{Minimum number of breaks passed to \code{\link[base:pretty]{base::pretty()}}. Defaults to 5.}

\item{u5.bias}{The "5-bias" parameter passed to \code{\link[base:pretty]{base::pretty()}}; higher values
push the breaks more strongly toward multiples of 5. Defaults to 4.}

\item{expand}{Uses own expansion logic. Use \code{expand = waiver()} to restore ggplot defaults
or \code{\link[ggplot2:expansion]{ggplot2::expansion()}} to modify}

\item{limits}{The lower limit defaults to 0 and the upper limits is chosen based on the data.
This is the recommended approach for visualizing case numbers and incidences,
i.e. the scale starts at 0 and is only positive.
To use the default \code{ggplot2} limits use \code{limits = NULL}.}

\item{labels}{One of:
\itemize{
\item \code{NULL} for no labels
\item \code{waiver()} for the default labels computed by the
transformation object
\item A character vector giving labels (must be same length as \code{breaks})
\item An expression vector (must be the same length as breaks). See ?plotmath for details.
\item A function that takes the breaks as input and returns labels
as output. Also accepts rlang \link[rlang:as_function]{lambda} function
notation.
}}

\item{oob}{One of:
\itemize{
\item Function that handles limits outside of the scale limits
(out of bounds). Also accepts rlang \link[rlang:as_function]{lambda}
function notation.
\item The default (\code{\link[scales:oob]{scales::censor()}}) replaces out of
bounds values with \code{NA}.
\item \code{\link[scales:oob]{scales::squish()}} for squishing out of bounds values into range.
\item \code{\link[scales:oob]{scales::squish_infinite()}} for squishing infinite values into range.
}}

\item{na.value}{Missing values will be replaced with this value.}

\item{transform}{For continuous scales, the name of a transformation object
or the object itself. Built-in transformations include "asn", "atanh",
"boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2",
"logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal",
"reverse", "sqrt" and "time".

A transformation object bundles together a transform, its inverse,
and methods for generating breaks and labels. Transformation objects
are defined in the scales package, and are called \verb{transform_<name>}. If
transformations require arguments, you can call them from the scales
package, e.g. \code{\link[scales:transform_boxcox]{scales::transform_boxcox(p = 2)}}.
You can create your own transformation with \code{\link[scales:new_transform]{scales::new_transform()}}.}

\item{position}{For position scales, The position of the axis.
\code{left} or \code{right} for y axes, \code{top} or \code{bottom} for x axes.}

\item{sec.axis}{\code{\link[ggplot2:sec_axis]{sec_axis()}} is used to specify a secondary axis.}

\item{guide}{A function used to create a guide or its name. See
\code{\link[ggplot2:guides]{guides()}} for more information.}

\item{...}{Additional arguments passed on to \code{\link[base:pretty]{base::pretty()}}.}
}
\value{
A \code{ggplot2} scale object that can be added to a plot.
}
\description{
A continuous ggplot scale for count data with sane defaults for breaks.
It uses \code{\link[base:pretty]{base::pretty()}} to increase the default number of breaks and prefers 5er breaks.
Additionally, the first tick (i.e. zero) is aligned to the lower left corner.
}
\examples{
library(ggplot2)

data <- data.frame(date = as.Date("2024-01-01") + 0:30)
ggplot(data, aes(x = date)) +
  geom_epicurve(date_resolution = "week") +
  scale_y_cases_5er() +
  theme_mod_remove_minor_grid_y()
}
\seealso{
\code{\link[=geom_epicurve]{geom_epicurve()}}, \code{\link[ggplot2:scale_continuous]{ggplot2::scale_y_continuous()}}, \code{\link[base:pretty]{base::pretty()}},
\code{\link[=theme_mod_remove_minor_grid_y]{theme_mod_remove_minor_grid_y()}}
}
