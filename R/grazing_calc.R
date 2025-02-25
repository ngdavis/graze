#' @export
frgd_ <- function(head, wt, days, acres, prct_int = 2.5, unit_system = "imperial") {
  # Apply units using helper function
  wt <- apply_units(wt, "weight", unit_system)
  days <- apply_units(days, "intake", unit_system)
  acres <- apply_units(acres, "area", unit_system)
  prct_int <- apply_units(prct_int, "intake", unit_system)

  # Perform calculation
  result <- head * wt * prct_int * days / acres

  # Return result in correct units
  if (unit_system == "metric") {
    result <- units::set_units(result, "kg/ha")
  } else {
    result <- units::set_units(result, "lb/ac")
  }

  return(result)
}
