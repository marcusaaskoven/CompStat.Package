#' Black-Scholes option price, delta and gamma
#'
#' Generates option price, delta and gamma using standard Black-Scholes framework
#'
#' @param s the price of the underlying asset
#' @param K the strike price of the option
#' @param r the risk free interest rate
#' @param tau the time until maturity
#' @param sigma the implied volatility
#' @param CP a string to determine whether option is european call, 'C', or put, 'P'
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
  if(CP=='C')
    return(
      structure(
        list(price = formula,
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
  else if(CP=='P')
    return(
      structure(
        list(price = formula-s+K*(-exp(-r*(tau))),
             delta = delta-1,
             gamma = gamma,
             s = s,
             K = K,
             r = r,
             tau = tau,
             sigma = sigma,
             CP = CP),
        class = "ExtremeClass")
    )
  else
    return("CP must be 'C' or 'P', which refers to a European call or put option.")
}
