#' Black-Scholes option price, delta and gamma
#'
#' Generates option price, delta and gamma using standard Black-Scholes framework
#'
#' @param s double containing price of the underlying asset
#' @param K double containing the strike price of the option
#' @param r double containing the risk free interest rate
#' @param tau double containing the time until maturity (in years)
#' @param sigma double containing the implied volatility
#' @param CP string to determine whether option is european call, 'C', or put, 'P'
#'
#' @return An object of class "ExtremeClass"
#' @export
#'
#' @examples
#' BS_option(100, 100, 0.02, 1, 0.2, 'C')
BS_option <- function(s,K,r,tau,sigma,CP){
  if (length(s)!=1){
    stop("Argument 's' must have length 1")
  }
  if (length(K)!=1){
    stop("Argument 'K' must have length 1")
  }
  if (length(r)!=1){
    stop("Argument 'r' must have length 1")
  }
  if (length(tau)!=1){
    stop("Argument 'tau' must have length 1")
  }
  if (length(sigma)!=1){
    stop("Argument 's' must have length 1")
  }
  d1 <- (1/(sigma*sqrt(tau)))*(log(s/K)+(r+(sigma^2)/2)*(tau))
  d2 <- d1-sigma*sqrt(tau)
  Nd1 <- stats::pnorm(d1,mean=0,sd=1)
  Nd2 <- stats::pnorm(d2,mean=0,sd=1)
  formula <- s*Nd1-exp(-r*(tau))*K*Nd2
  delta <- Nd1
  gamma <- (1/(s*sigma*sqrt(2*pi*(tau))))*exp((-d1^2)/2)
  if(CP=='C'){
    price = formula
    delta = delta
  }
  else if(CP=='P'){
    price = formula-s+K*(-exp(-r*(tau)))
    delta = delta-1
  }
  else{
    stop("CP must be 'C' or 'P', which refers to a European call or put option.")
  }
  return(
    structure(
      list(price = price,
           delta = delta,
           gamma = gamma,
           s = s,
           K = K,
           r = r,
           tau = tau,
           sigma = sigma,
           CP = CP),
      class = "ExtremeClass")
  )
}
