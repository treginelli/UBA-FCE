
## https://www.r-bloggers.com/2020/12/pricing-of-european-options-with-monte-carlo/

## VALUACIÓN DE OPCIONES POR BLACK-SCHOLES (& MERTON)

K = 100
r = 0.02
sigma = 0.2
T = 0.5
S0 = 102

# CALL OPTION

d1 <- (log(S0/K) + (r + sigma^2/2) * T)/(sigma * sqrt(T))
d2 <- d1 - sigma * sqrt(T)
phid1 <- pnorm(d1)
call_price <- S0 * phid1 - K * exp(-r * T) * pnorm(d2)

# PUT OPTION

d1 <- (log(S0/K) + (r + sigma^2/2) * T)/(sigma * sqrt(T))
d2 <- d1 - sigma * sqrt(T)
phimd1 <- pnorm(-d1)
put_price <- -S0 * phimd1 + K * exp(-r * T) * pnorm(-d2)

price <- c(call_price, put_price)
names(price) <- c("Call Price - BSM","Put Price - BSM")
price

## MONTE CARLO

# CALL PUT OPTION MONTE CARLO
call_put_mc <- function(nSim = 1000000, tau, r, sigma, S0, K){
  
  Z <- rnorm(nSim, mean=0, sd=1)
  WT <- sqrt(tau) * Z
  ST = S0 * exp((r - 0.5*sigma^2) * tau + sigma * WT)
  
  # PRICE AND STANDARD ERROR OF CALL OPTION
  
  simulated_call_payoffs <- exp(-r*tau)*pmax(ST-K,0)
  price_call <- mean(simulated_call_payoffs)
  sterr_call <- sd(simulated_call_payoffs)/sqrt(nSim)
  
  # PRICE AND STANDARD ERROR OF PUT OPTION
  
  simulated_put_payoffs <- exp(-r*tau)*pmax(K-ST,0)
  price_put <- mean(simulated_put_payoffs)
  sterr_put <- sd(simulated_put_payoffs)/sqrt(nSim)
  
  output <- list(price_call = price_call, 
                 sterr_call = sterr_call, 
                 price_put = price_put,
                 sterr_put = sterr_put)
  return(output)
}

set.seed(1)
results <- call_put_mc(n = 1000000,
                       tau = 0.5, 
                       r = 0.02,
                       sigma = 0.2,
                       S0 = 102,
                       K = 100)
results

price_mc <- c(results$price_call,results$price_put)
names(price_mc) <- c("Call Price - MC","Put Price - MC")
price_mc

table <- cbind(price,price_mc)
rownames(table) <- c("Call","Put")
colnames(table) <- c("BSM","Monte Carlo")
table

