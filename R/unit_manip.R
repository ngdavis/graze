units::install_unit("ac", "4046.8564224 m^2")
units::install_unit("ha", "10000 m^2")

convert_lookup <- dplyr::tibble(
  unit_type = c("weight", "area", "frg_dmnd", "st_rt", "in_st_rt"),
  imperial = c("lb", "ac", "lb/ac", "AUM/ac", "ac/AUM"),
  metric = c("kg", "ha", "kg/ha", "AUM/ha", "ha/AUM")
)

#' @importFrom dplyr %>%

apply_units <- function(value, unit_type, system = "imperial") {
  # Validate system
  if (!system %in% c("imperial", "metric")) {
    stop("Invalid unit system. Choose 'imperial' or 'metric'.")
  }

  # Get expected unit from lookup table
  expected_unit <- dplyr::filter(convert_lookup, unit_type == !!unit_type) %>%
    dplyr::pull(!!system)

  # Handle missing unit_type
  if (length(expected_unit) == 0) {
    stop("Invalid unit_type: ", unit_type)
  }

  # Assign/convert units
  if (inherits(value, "units")) {
    current_unit <- units::deparse_unit(value)
    if (current_unit != expected_unit) {
      value <- units::set_units(value, expected_unit, mode = "standard")
    }
  } else {
    value <- units::set_units(value, expected_unit)
  }

  return(value)
}

metric_to_imperial <- function(x) {
  unit <- as.character(units(x))
  if (unit %in% "kg") {
    units::set_units(x, "lb")
  } else if (unit %in% "ha") {
    units::set_units(x, "ac")
  } else {
    simpleError("Inappropriate input units; must be 'kg' or 'ha'")
  }
}

imperial_to_metric <- function(x) {
  unit <- as.character(units(x))
  if (unit %in% "lb") {
    units::set_units(x, "kg")
  } else if (unit %in% "ac") {
    units::set_units(x, "ha")
  } else {
    simpleError("Inappropriate input units; must be 'lb' or 'ac'")
  }
}
