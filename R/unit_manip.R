


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
    value <- units::set_units(value, expected_unit, mode = "standard")
  }

  return(value)
}

forage_intake_units <- function(x) {
  if (x > 4 | x < 1) {
    warning("Forage intake is expressed on a percentage BW basis and is generally expected to be between 1-4%. Check if values were entered correctly.")
  }
  value <- units::set_units(x, "%*1/d")
  return(value)
}

unit_system_message <- function(x) {
  # Print message if using default imperial input units
  if (missing(x)) {
    message("Assuming imperial units: weight in lb, area in acres.")
  }
}

valid_units <- c("lb/ac", "kg/ha", "AUM/ac", "AUM/ha", "ac/AUM", "ha/AUM")

set_result_units <- function(value, output_unit) {
  if (is.null(output_unit)) {
    return(value)
  }
  # Validate output_unit against allowed options
  if (!output_unit %in% valid_units) {
    stop("Invalid output_unit. Choose from: ",
         paste(valid_units, collapse = ", "))
  }

  # Convert to desired unit
  units::set_units(value, output_unit, mode = "standard")
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
