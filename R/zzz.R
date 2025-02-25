.onLoad <- function(libname, pkgname) {
  # Install custom units if they aren't already defined
  if (!"ac" %in% units::valid_udunits()) {
    units::install_unit("ac", "4046.8564224 m^2")
  }
  if (!"ha" %in% units::valid_udunits()) {
    units::install_unit("ha", "10000 m^2")
  }
  if (!"AUM" %in% units::valid_udunits()) {
    units::install_unit("AUM", "700 lb")
  }
}
