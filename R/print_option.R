#' Print ExtremeClass objects
#'
#' This function prints the Black-Scholes options price, delta, and gamma of an ExtremeClass object.
#'
#' @param x Object of class "ExtremeClass"
#' @param ... Additional arguments to be passed to print function (not used in this function)
#'
#' @return None (prints output directly)
#' @export
#'
#' @examples
#' x <- BS_option(100, 100, 0.02, 1, 0.2, "C")
#'
#' print(x)
print.ExtremeClass <- function(x, ...) {
  # Create a data frame to display the results
  df <- data.frame(
    "Black-Scholes options price" = x$price,
    "Black-Scholes delta" = x$delta,
    "Black-Scholes gamma" = x$gamma
  )
  # Print the data frame
  print(df)
}
