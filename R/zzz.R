.onLoad <- function(libname, pkgname) {
  # Install base units
  units::install_unit("ac", "4046.8564224 m^2")
  units::install_unit("ha", "10000 m^2")

  # Install AUM definitions
  units::install_unit("AUM_SRM", "790 lb", "AUM")
  units::install_unit("AUM_NRCS", "915 lb")
  # For default value down the road will likely need a table to activly manipulate
}
