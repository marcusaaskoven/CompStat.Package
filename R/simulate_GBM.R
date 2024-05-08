#' Simulate Geometric Brownian Motion
#'
#' @param S0 an initial value of the GBM
#' @param r the risk free interest rate
#' @param mu the drift component
#' @param sigma the volatility component
#' @param num_simulations amount of simulations to create
#' @param num_steps amount of steps to simulate
#'
#' @return an object of class "LessExtremeClass"
#' @export
#'
#' @examples
#' simulate_GBM(100, 0.02, 0.02, 0.2, 1000, 52)
simulate_GBM <- function(S0, r, mu, sigma, num_simulations, num_steps) {
  # Simulate GBM Paths
  dt <- 1 / num_steps
  W <- matrix(stats::rnorm(num_steps * num_simulations, mean = 0, sd = sqrt(dt)), nrow = num_steps)
  St <- exp((mu - sigma^2 / 2) * dt + sigma * W)
  St <- rbind(rep(S0, num_simulations), St)
  St <- apply(St, 2, cumprod)

  #For plot
  n_paths = min(100, num_simulations)
  return(
    structure(
      list(St = St,
           n_steps = num_steps,
           n_sim = num_simulations,
           n_paths = n_paths),
      class = "LessExtremeClass")
    )
}
