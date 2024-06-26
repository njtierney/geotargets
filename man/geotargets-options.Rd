% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geotargets-option.R
\name{geotargets_option_set}
\alias{geotargets_option_set}
\alias{geotargets_option_get}
\title{Get or Set geotargets Options}
\usage{
geotargets_option_set(
  gdal_raster_driver = NULL,
  gdal_raster_creation_options = NULL,
  gdal_vector_driver = NULL,
  gdal_vector_creation_options = NULL
)

geotargets_option_get(name)
}
\arguments{
\item{gdal_raster_driver}{character, length 1; set the driver used for raster
data in target store (default: \code{"GTiff"}). Options for driver names can be
found here: \url{https://gdal.org/drivers/raster/index.html}}

\item{gdal_raster_creation_options}{character; set the GDAL creation options
used when writing raster files to target store (default: \code{""}). You may
specify multiple values e.g. \code{c("COMPRESS=DEFLATE", "TFW=YES")}. Each GDAL
driver supports a unique set of creation options. For example, with the
default \code{"GTiff"} driver:
\url{https://gdal.org/drivers/raster/gtiff.html#creation-options}}

\item{gdal_vector_driver}{character, length 1; set the file type used for
vector data in target store (default: \code{"GeoJSON"}).}

\item{gdal_vector_creation_options}{character; set the GDAL layer creation
options used when writing vector files to target store (default:
\code{"ENCODING=UTF-8"}). You may specify multiple values e.g.
\code{c("WRITE_BBOX=YES", "COORDINATE_PRECISION=10")}. Each GDAL driver supports
a unique set of creation options. For example, with the default \code{"GeoJSON"}
driver:
\url{https://gdal.org/drivers/vector/geojson.html#layer-creation-options}}

\item{name}{character; option name to get.}
}
\value{
Specific options, such as "gdal.raster.driver". See "Details" for
more information.
}
\description{
Get or set behavior for geospatial data target stores using
geotargets-specific global options.
}
\details{
These options can also be set using \code{options()}.  For example,
\code{geotargets_options_set(gdal_raster_driver = "GTiff")} is equivalent to
\code{options("geotargets.gdal.raster.driver" = "GTiff")}.
}
\examples{
if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
 targets::tar_dir({ # tar_dir() runs code from a temporary directory.
   library(geotargets)
  op <- getOption("geotargets.gdal.raster.driver")
  withr::defer(options("geotargets.gdal.raster.driver" = op))
  geotargets_option_set(gdal_raster_driver = "COG")
   targets::tar_script({
     list(
       geotargets::tar_terra_rast(
         terra_rast_example,
         system.file("ex/elev.tif", package = "terra") |> terra::rast()
       )
     )
   })
   targets::tar_make()
   x <- targets::tar_read(terra_rast_example)
 })
}

geotargets_option_get("gdal.raster.driver")
geotargets_option_get("gdal.raster.creation.options")
}
