# Registry of patterns this package provides; add new ones here.
heartfill_patterns <- function() {
  list(hearts = create_pattern_hearts,
       stars  = create_pattern_stars,
       flowers = create_pattern_flowers,
       butterflies = create_pattern_butterflies,
       cats = create_pattern_cats,
       dogs = create_pattern_dogs,
       unicorns = create_pattern_unicorns,
       sugarbeets = create_pattern_sugarbeets,
       computers = create_pattern_computers,
       viris = create_pattern_viris)
}

.onLoad <- function(libname, pkgname) {
  current <- getOption("ggpattern_geometry_funcs", list())
  new <- heartfill_patterns()
  current[names(new)] <- new
  options(ggpattern_geometry_funcs = current)
  invisible()
}
