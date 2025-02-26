#' Calculate forage demand
#'
#' Calculate pasture forage demand expressed in "lb/ac" or "kg/ha"
#' @param head Number of head
#' @param wt Mean animal weight
#' @param days Number of days in the pasture
#' @param area Area of the pasture
#' @param prct_int Estimated daily dry matter intake expressed on a percent body weight basis. Common values range between 1.5 and 3 percent. The default is 2 percent.
#' @param input_system Unit system for \code{wt} and \code{area}; either \code{"imperial"} or \code{"metric"}. If \code{"imperial"}, assumes \code{"lb"} and \code{"ac"}. If \code{"metric"}, assumes \code{"kg"} and \code{"ha"}. Default is \code{"imperial"}. Respects inherited units.
#' @param output_unit Optional. Desired output units. Typically either \code{"lb/ac"} or \code{"kg/ha"}. Default matches the input unit system.
#' @examples
#' frgd_(head = 136, wt = 1350, days = 45, area = 931, prct_int = 2.5, input_system = "imperial")
#'
#' frgd_(55, 480, 30, 60, input_system = "metric")
#'
#' @export
frgd_ <- function(head, wt, days, area, prct_int = 2,
                  input_system = "imperial",
                  output_unit = NULL) {  # Make output_unit optional

  # Check Units
  unit_system_message(input_system)

  # Apply units using helper function
  wt <- apply_units(wt, "weight", input_system)
  days <- units::set_units(days, "d")
  acres <- apply_units(area, "area", input_system)
  prct_int <- forage_intake_units(prct_int)

  # Perform calculation
  result <- head * wt * prct_int * days / acres

  # Set default output_unit if not specified
  if (is.null(output_unit)) {
    output_unit <- ifelse(input_system == "imperial", "lb/ac", "kg/ha")
  }

  # Convert to desired output unit
  result <- set_result_units(result, output_unit)

  return(result)
}
