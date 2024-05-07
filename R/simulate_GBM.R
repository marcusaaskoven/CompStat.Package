#' Simulate Geometric Brownian Motion
#'
#' @param S0 an initial value of the GBM
#' @param r the risk free interest rate
#' @param mu the drift component
#' @param sigma the volatility component
#' @param num_simulations amount of simulations to create
#' @param num_steps amount of steps to simulate
#' @param num_paths_to_plot amount of simulations to plot
#'
#' @return a matrix of the selected amount of simulated GBMs, as well as a time vector showing time to maturity at every step
#' @export
#'
#' @examples
#' simulate_GBM(100, 0.02, 0.02, 0.2, 1000, 52, 100)
simulate_GBM <- function(S0, r, mu, sigma, num_simulations, num_steps, num_paths_to_plot = num_simulations/100) {
  # Calculate number of steps
  n <- num_steps

  # Simulate GBM Paths
  dt <- 1 / n
  W <- matrix(stats::rnorm(n * num_simulations, mean = 0, sd = sqrt(dt)), nrow = n)
  St <- exp((mu - sigma^2 / 2) * dt + sigma * W)
  St <- rbind(rep(S0, num_simulations), St)
  St <- apply(St, 2, cumprod)

  # Define time interval
  time <- seq(0, 1, length.out = n + 1)

  # Subset paths to plot
  indices_to_plot <- sample(1:num_simulations, num_paths_to_plot)
  St_to_plot <- St[, indices_to_plot]

  # Plotting Results
  plot_colors <- grDevices::rainbow(num_paths_to_plot)
  graphics::matplot(time * num_steps, St_to_plot,
          type = "l", xlab = "Weeks", ylab = "Stock Price",
          main = bquote("Simulations of Geometric Brownian Motion"),
          col = plot_colors, lty = 1
  )
  return(list(St = St,time = time))
}
