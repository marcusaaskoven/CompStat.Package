#' Summarize an ExtremeClass object
#'
#' This function computes and presents a summary of an ExtremeClass object,
#' including the parameters used to calculate the Black-Scholes option price, delta, and gamma.
#'
#' @param object An object of class ExtremeClass
#' @param ... Additional arguments to be passed to print function (not used in this function)
#'
#' @return A list containing summary information about the ExtremeClass object
#' @export
#'
#' @examples
#' # Create an example ExtremeClass object
#' x <- BS_option(100, 100, 0.02, 1, 0.2, 'C')
#'
#' # Summarize the ExtremeClass object
#' summary(x)
#'
summary.ExtremeClass <- function(object, ...) {
  df_input <- data.frame(
    `Parameter` = c("Initial price", "Strike price", "Interest rate",
                    "Time until maturity", "Implied volatility", "Call(C)/Put(P)"),
    `Value` = c(object$s, object$K, object$r, object$tau, object$sigma, object$CP)
  )

  df_output <- data.frame(
    `Output` = c("Price", "Delta", "Gamma"),
    `Value` = round(c(object$price, object$delta, object$gamma),3)
  )

  cat("Input parameters:\n")
  print(df_input)

  cat("\n")

  cat("Black-Scholes output:\n")
  print(df_output)
}
