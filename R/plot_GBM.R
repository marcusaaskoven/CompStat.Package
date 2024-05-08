#' Plot method for class LessExtremeClass
#'
#' @param x An object of class LessExtremeClass
#' @param ... Additional arguments to be passed to print function (not used in this function)
#'
#' @return a plot
#' @export
#'
#' @examples
#' GBM <- simulate_GBM(100, 0.02, 0.02, 0.2, 1000, 52)
#'
#' plot(GBM)
#'
plot.LessExtremeClass <- function(x, ...) {
  n_steps <- x$n_steps
  n_paths <- x$n_paths
  n_sim <- x$n_sim
  St <- x$St
  time <- seq(0, 1, length.out = n_steps + 1)
  indices_to_plot <- sample(1:n_sim, n_paths)
  y <- St[, indices_to_plot]
  time <- time * n_steps

  plot_colors <- grDevices::rainbow(n_paths)
  graphics::matplot(time, y,
    type = "l", xlab = "Steps", ylab = "GBM",
    main = bquote("Simulations of Geometric Brownian Motion"),
    col = plot_colors, lty = 1
  )
}
