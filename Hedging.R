#hedging
##static/dynamic delta hedging
set.seed(2014)
library(fOptions)
GBSOption(TypeFlag = 'c', S=100, X=100, Time=1/2 , r=0.05, b = 0.05, sigma = 0.3)


Price_simulation = function(S0, mu, sigma, rf, K, Time,  dt, plots = FALSE ){
  
  t <- seq(0, Time, by = dt)
  N <- length(t)
  
  W <- c(0,cumsum(rnorm(N-1)))
  S <- S0*exp((mu-sigma^2/2)*t + sigma*sqrt(dt)*W)
  
  delta <- rep(0, N-1)
  call_ <- rep(0, N-1)
  
  for(i in 1:(N-1) ){
    delta[i] <- GBSGreeks("Delta", "c", S[i], K, Time-t[i], rf, rf, sigma)
    call_[i] <- GBSOption("c", S[i], K, Time-t[i], rf, rf, sigma)@price}
  
  if(plots){
    dev.new(width=30, height=10)
    par(mfrow = c(1,3))
    plot(t, S, type = "l", main = "Price of underlying")
    plot(t[-length(t)], delta, type = "l", main = "Delta", xlab = "t")
    plot(t[-length(t)], call_ , type = "l", main = "Price of option", xlab = "t")
  }
}

Price_simulation(100, 0.2, 0.3, 0.05, 100, 0.5, 1/250, plots = TRUE)

cost_simulation = function(S0, mu, sigma, rf, K, Time,  dt){
  t <- seq(0, Time, by = dt)
  N <- length(t)
  W <- c(0,cumsum(rnorm(N-1)))
  S <- S0*exp((mu-sigma^2/2)*t + sigma*sqrt(dt)*W)
  
  delta <- rep(0, N-1)
  call_ <- rep(0, N-1)
  
  for(i in 1:(N-1) ){
    delta[i] <- GBSGreeks("Delta", "c", S[i], K, Time-t[i], rf, rf, sigma)
    call_[i] <- GBSOption("c", S[i], K, Time-t[i], rf, rf, sigma)@price}
  
  share_cost <- rep(0,N-1)
  interest_cost <- rep(0,N-1)
  total_cost <- rep(0, N-1)
  
  share_cost[1] <- S[1]*delta[1]
  interest_cost[1] <- (exp(rf*dt)-1) * share_cost[1]
  total_cost[1] <- share_cost[1] + interest_cost[1]
  
  for(i in 2:(N-1)){
    share_cost[i] <- ( delta[i] - delta[i-1] ) * S[i]
    interest_cost[i] <- ( total_cost[i-1] + share_cost[i] ) * (exp(rf*dt)-1)
    total_cost[i] <- total_cost[i-1] + interest_cost[i] + share_cost[i]
  }
  
  c = max( S[N] - K , 0)
  cost = c - delta[N-1]*S[N] + total_cost[N-1]  		 
  return(cost*exp(-Time*rf))
}

call_price = GBSOption("c", 100, 100, 0.5, 0.05, 0.05, 0.3)@price
A = rep(0, 1000)
for (i in 1:1000){A[i] = cost_simulation(100, .20, .30,.05, 100, 0.5, 1/52)}
B = rep(0, 1000)
for (i in 1:1000){B[i] = cost_simulation(100, .20, .30,.05, 100, 0.5, 1/250)}

dev.new(width=20, height=10)

par(mfrow=c(1,2))
hist(A, freq = F, main = paste("E = ",round(mean(A), 4) ,"  sd = ",round(sd(A), 4)), xlim = c(6,14), ylim = c(0,0.7))
curve(dnorm(x, mean=mean(A), sd=sd(A)), col="darkblue", lwd=2, add=TRUE, yaxt="n")
hist(B, freq = F, main = paste("E = ",round(mean(B), 4) ,"  sd = ",round(sd(B), 4)), xlim = c(6,14), ylim = c(0,0.7))
curve(dnorm(x, mean=mean(B), sd=sd(B)), col="darkblue", lwd=2, add=TRUE, yaxt="n")
## compare weekly hedge strategy & daily hedge strategy with the original BSW price.
## different dt and different hedging results
cost_simulation = function(S0, mu, sigma, rf, K, Time, dt, periods){
  
  t <- seq(0, Time, by = dt)
  N <- length(t)
  W = c(0,cumsum(rnorm(N-1)))
  S <- S0*exp((mu-sigma^2/2)*t + sigma*sqrt(dt)*W)
  SN = S[N]
  
  delta <- rep(0, N-1)
  call_ <- rep(0, N-1)
  
  for(i in 1:(N-1) ){
    delta[i] <- GBSGreeks("Delta", "c", S[i], K, Time-t[i], rf, rf, sigma)
    call_[i] <- GBSOption("c", S[i], K, Time-t[i], rf, rf, sigma)@price
  }
  
  S = S[seq(1, N-1, by = periods)]
  delta = delta[seq(1, N-1, by = periods)]
  
  m = length(S)
  
  share_cost <- rep(0,m)
  interest_cost <- rep(0,m)
  total_cost <- rep(0, m)
  
  share_cost[1] <- S[1]*delta[1]
  interest_cost[1] <- (exp(rf*dt*periods)-1) * share_cost[1]
  total_cost[1] <- share_cost[1] + interest_cost[1]
  
  
  for(i in 2:(m)){
    share_cost[i] <- ( delta[i] - delta[i-1] ) * S[i]
    interest_cost[i] <- ( total_cost[i-1] + share_cost[i] ) * (exp(rf*dt*periods)-1)
    total_cost[i] <- total_cost[i-1] + interest_cost[i] + share_cost[i]
  }
  
  c = max( SN - K , 0)
  
  cost = c - delta[m]*SN + total_cost[m]                         
  
  return(cost*exp(-Time*rf))
  
}
dev.new(width=30,height=20)
par(mfrow = c(2,3))
i = 0
per = c(2,4,8,20,40,80) 
call_price = GBSOption("c", 100, 100, 0.5, 0.05, 0.05, 0.3)@price
results = matrix(0, 6, 5)
rownames(results) = c("1/2 days", "1 day", "2 days", "1 week", "2 weeks", "4 weeks")
colnames(results) = c("E", "lower", "upper", "v", "ratio")
for (j in per){
  i = i+1
  A = rep(0, 1000)
  set.seed(10125987)
  for (h in 1:1000){A[h] = cost_simulation(100, .20, .30,.05, 100, 0.5, 1/1000,j)}
  
  E = mean(A)
  v = sd(A)
  results[i, 1] = E
  results[i, 2] = E-1.96*v/sqrt(1000)
  results[i, 3] = E+1.96*v/sqrt(1000)
  results[i, 4] = v
  results[i, 5] = v/call_price
  hist(A, freq = F, main = "", xlab = "", xlim = c(4,16), ylim = c(0,0.8))
  title(main = rownames(results)[i], sub = paste("E = ",round(E, 4) ,"  sd = ",round(v, 4)))
  curve(dnorm(x, mean=mean(A), sd=sd(A)), col="darkblue", lwd=2, add=TRUE, yaxt="n")
}
print(results)
dev.new()
curve(dnorm(x,results[1,1], results[1,4]), 6,14, ylab = "", xlab = "cost")
for (l in 2:6) curve(dnorm(x, results[l,1], results[l,4]), add = TRUE, xlim = c(4,16), ylim = c(0,0.8), lty=l)
legend(legend=rownames(results), "topright", lty = 1:6)

## heding Optimazation
n_sim <- 20
threshold <- 12
cost_Sim <- function(cost = 0.01, n = n_sim, per = 1){a <- replicate(n, cost_simulation(100, .20, .30,.05, 100, 0.5, 1/1000,per,cost)); 
l <- list(mean(a), sd(a), quantile(a,0.95))}
A <- sapply(seq(1,80) ,function(per) {print(per); set.seed(2019759); cost_Sim(per = per)})
e <- unlist(A[1,])
s <- unlist(A[2,])
q <- unlist(A[3,])
u <- e + s^2
A <- cbind(t(A), u)
z1 <- which.min(e)
z2 <- which.min(s)
z3 <- which.min(u)
(paste("E min =", z1, "cost of hedge = ",e[z1]," sd = ", s[z1]))
(paste("s min =", z2, "cost of hedge = ",e[z2]," sd = ", s[z2]))
(paste("U min =", z3, "u = ",u[z3],"cost of hedge = ",e[z3]," sd = ", s[z3]))
matplot(A, type = "l", lty = 1:4, xlab = "??t", col = 1)
lab_for_leg = c("E", "Sd", "95% quantile","E + variance")
legend(legend = lab_for_leg, "bottomright", cex = 0.6, lty = 1:4)
abline( v = c(z1,z2,z3), lty = 6, col = "grey")
abline( h = threshold, lty = 1, col = "grey")
text(c(z1,z1,z2,z2,z3,z3,z3),c(e[z1],s[z1],s[z2],e[z2],e[z3],s[z3],u[z3]),round(c(e[z1],s[z1],s[z2],e[z2],e[z3],s[z3],u[z3]),3), pos = 3, cex = 0.7)
e2 <- e
e2[q > threshold] <- max(e)
z4 <- which.min(e2)
z5 <- which.min(q)
if( q[z5] < threshold ){
  print(paste(" min VaR = ", q[z4], "at", z4 ,"E(cost | VaR < threshold = " ,e[z4], " s = ", s[z4]))
} else {
  print(paste("optimization failed, min VaR = ", q[z5], "at", z5 , "where cost = ", e[z5], " s = ", s[z5])) 
}

