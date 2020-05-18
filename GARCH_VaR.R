###########
### VaR ###
###########
library(TSA)
library( zoo )
library(forecast)
library( fGarch )


# What is Value-at-Risk?
x <- seq( -4, 4, length = 100 )
hx <- dnorm(x) # 100 elements from the densitiy function of normal distribution
lvl = 0.05 # Level of VaR

plot( x, hx, type = "l", lty = 2, xlab = "x value",
     ylab = "Density", main = "Normal distribution" )
abline( v = -qnorm( 1 - lvl ), col = "red", lwd = 2 )


# Empirical example

ford <- as.data.frame( quantmod::getSymbols( "F", src = "yahoo", from = "2000-01-30", 
                                             to = "2020-01-30", auto.assign = FALSE ), row.names = NULL )

plot( ford$F.Adjusted, type = "l" )

# Save it to time-series format (otherwise the JB test do not run)
adj_price <- ts( ford$F.Adjusted, start = c( 2000, 1, 30 ), end = c( 2020, 1, 30 ), frequency = 250 )

plot( adj_price )

# Calculate returns
r <- diff( ford$F.Adjusted ) / ford$F.Adjusted[ -length( ford$F.Adjusted ) ] # simple returns

# Plot returns
plot( r, type = "l" )

# Empirical VaR:
hist( r, breaks = 100 )
f_emp_VaR = quantile( -r, 1 - lvl )
abline( v = -f_emp_VaR, col = "red", lwd = 2 )
f_emp_VaR
# One day VaR for $10m portfolio of only Ford stocks
m10 = 10000000
m10 * f_emp_VaR



# General method
r_roll1 = rollapply( r, 1, sum )
f_emp1_VaR = quantile( -r_roll1, 1 - lvl )
f_emp1_VaR

# 2-day empirical VaR
# Create rolling 2-day cumulative returns
r_roll2 = rollapply( r, 2, sum )
hist( r_roll2, breaks = 100 )
f_emp2_VaR = quantile( -r_roll2, 1 - lvl )
abline( v = -f_emp2_VaR, col = "red", lwd = 2 )
f_emp2_VaR

# 10-day empirical VaR
r_roll10 = rollapply( r, 10, sum )
hist( r_roll10, breaks = 100 )
f_emp10_VaR = quantile( -r_roll10, 1 - lvl )
abline( v = -f_emp10_VaR, col = "red", lwd = 2 )
f_emp10_VaR


hist( r[ 3200:4200 ], breaks = 100 )
f_emp_VaR1000 = quantile( -r[ 3200:4200 ], 1 - lvl )
m10 = 10000000
m10*f_emp_VaR1000


# Is it accurate?
par( mfrow = c( 2, 1) )
plot( r, type = 'l' )
plot( r^2, type = "l" )


# Using GARCH model!
# Computing 1% Value-at-Risk for Ford returns

# Fit ARMA(1,5)-GARCH(1,1) to Ford returns
f_garch = garchFit( ~arma( 2, 1 ) + garch( 1, 1 ), data = r, trace = F )

# Forecast returns + volatility one step
f_g_f = predict( f_garch, h = 1 )
# Define one-step forecast return and std dev
f1_r = f_g_f$meanF[ 1 ] # 1-step return
f1_v = f_g_f$st[ 1 ] # std dev

# Compute one day ahead VaR
f1_VaR = -f1_r + qnorm( 1 - lvl ) * f1_v
f1_VaR
f1_VaR * 10000000

# Multi-period VaR:
# VaR(h)= -r(1)-r(2)-r(3)-...-r(h) + z(crit_value) * sqrt(Var[e(1)]+Var[e(2)]+...+Var[e(h)])
# Two day ahead
f2_VaR = -sum( f_g_f$meanF[ 1:2 ] ) + qnorm( 1 - lvl ) * sqrt( sum( f_g_f$st[ 1:2 ]^2 ) )
f2_VaR

# 10-day ahead VaR
f10_VaR = -sum( f_g_f$meanF[ 1:10 ] ) + qnorm( 1 - lvl ) * sqrt( sum( f_g_f$st[ 1:10 ]^2 ) )
f10_VaR

# One day VaR for $10m portfolio of only Ford stocks
m10 = 10000000
f1_10m_VaR = m10 * f10_VaR
f1_10m_VaR # Worst 1% of the time, expect to lose at least this much

# Compare to empirical VaR:
f_emp10_VaR * 10000000


##########################
### Expected Shortfall ###
##########################

# Return distribution can get weird
y <- seq( -4, 4, length = 100 )
hy <- dnorm(x) # 100 elements from the densitiy function of normal distribution
hy[ 1:15 ] <- c( 0.005, 0.01, 0.03, 0.07, 0.1, 0.12, 0.13, 0.12, 0.1, 0.07, 0.03, 0.01, 0.007, 0.005, 0.006 )  
dev.off()
plot( x, hy, type = "l", lty = 2, xlab = "x value",
      ylab = "Density", main = "Normal distribution" )
# Is the VaR realistic?

# Use Expected Shortfall! The expected value of the last x%
f_emp1_EF <- -mean( r[ r <= -quantile( -r, 1 - lvl ) ] )
f_emp1_EF
f_emp1_EF * 10000000

# Empirical VaR:
f_emp1_VaR * 10000000




### Estimating GARCH model ###


#Compute ACF
r_lag = 20;
r_acf = acf( r, lag.max = r_lag, plot = F )
plot( r_acf[ 2:length( r_acf$acf ) ] )

# Choose ARMA(2,7)
# Estimate
ford_arma = arima( r, order = c( 2, 0, 1 ) )
ford_arma

# Do Ljung-Box tests
jb_lag = 10
Box.test( ford_arma$residuals, type = 'Ljung', lag = jb_lag, fitdf = 2 + 7 )
# Residuals are white noise for the parsimonious model


# Forecast
# 5-step forecast
r_t <- ts( r, start = c( 2000, 1, 30 ), end = c( 2020, 4, 30 ), frequency = 250 )
T <- length( r )
ford_arma_fc = arima( r[ 1:( T - 6 ) ], order = c( 2, 0, 1 ) )
ford_fc = predict( ford_arma_fc, n.ahead = 5 )$pred
# Plot forecast
par( mfcol = c( 2, 1 ) )
plot( r[ ( T - 10 ):T ], type = 'b' )
plot( ford_fc, include = 5, type = 'b' )

# GARCH
resid <- ford_arma$residuals
dev.off()
plot( ford_arma$residuals, type = "l" )

f_sq = ford_arma$residuals^2
f_sq_acf = acf( f_sq, lag.max = 20, plot = F )

par( mfcol = c( 2, 1 ) )
plot( f_sq, type = 'l' )
plot( f_sq_acf[ 2:length( f_sq_acf$acf ) ] )

# Now GARCH(1,1):
f_garch = garchFit( ~arma( 2, 1 ) + garch( 1, 1 ), data = r, trace = F )
f_garch_sr = f_garch@residuals / f_garch@sigma.t #standardized residuals
Box.test( f_garch_sr^2, type = 'Ljung', lag = 10 )

# Forecast using GARCH(1,1):
f_garch_f = predict( f_garch, h = 10 ) # 10 steps ahead
f_f_sd = f_garch_f$st # save forecast std deviations (sqrt(volatility))

# Plot forecast:
dev.off()
T = length( r ) # save length of series for easy reference
f_garch_plot = c( f_garch@sigma.t[ (T-9):T ], f_f_sd ) # plot last 10 periods plus forecast
plot( f_garch_plot, type = 'l' )
lines( c( rep( NA, 10 ), f_f_sd ), col = 'red', type = 'b' ) # color forecast red

