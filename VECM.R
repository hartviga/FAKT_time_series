######################
### Rolling window ###
######################

library( vars )
library( urca )
library( tseries )
#install.packages( "tsDyn" )
library( tsDyn )

# Relationships change with the economic cycles
# Therefore we need dynamic estimation
# Apply rolling window estimation with 500-day window

# Download returns
SP <- quantmod::getSymbols( "^GSPC", scr = "yahoo", from = "2004-12-17", to = "2017-12-30", auto.assign = FALSE)
SP_return <- quantmod::dailyReturn( SP$GSPC.Close )
DX <- quantmod::getSymbols( "DX", scr = "yahoo", from = "2004-12-17", to = "2017-12-30", auto.assign = FALSE)
DX_return <- quantmod::dailyReturn( DX$DX.Close )
BP <- quantmod::getSymbols( "BP", scr = "yahoo", from = "2004-12-17", to = "2017-12-30", auto.assign = FALSE)
BP_return <- quantmod::dailyReturn( BP$BP.Close )
CL <- quantmod::getSymbols( "CL", scr = "yahoo", from = "2004-12-17", to = "2017-12-30", auto.assign = FALSE)
CL_return <- quantmod::dailyReturn( CL$CL.Close )

returns <- as.data.frame( matrix( c( SP_return, DX_return, BP_return, CL_return ), ncol = 4 ) )
colnames( returns ) <- c( "SP", "DX", "BP", "CL" )

plot.ts( returns )

# Calculate rolling window VAR for the data
# How does the relationship between S&P500 and crude oil change in time?
# Plot the value of coefficients and the p values

# This function returns all the VAR models estimated
RollingVAR <- function( data, window_length ){
  rolling_var <- list()
  n <- nrow( data )
  index <- 1
  for( window_end in c( window_length:n ) ){
    window_start <- window_end - window_length + 1
    var_lag <- VARselect( data[ window_start:window_end, ] )
    rolling_var[ index ] <- VAR( data[ window_start:window_end, ], p = var_lag$selection[ 3 ], type = "both" )
    index <- index + 1
  }
  return( rolling_var )
}

# This function extracts the coefficient of the first lag of a given variable
RollingVARCoefficient <- function( data, window_length, equation_nr, variable_nr ){
  rolling_phi <- c()
  n <- nrow( data )
  index <- 1
  for( window_end in c( window_length:n ) ){
    window_start <- window_end - window_length + 1
    var_lag <- VARselect( data[ window_start:window_end, ] )
    rolling_var <- VAR( data[ window_start:window_end, ], p = var_lag$selection[ 3 ], type = "both" )
    rolling_phi[ index ] <- Bcoef( rolling_var )[ equation_nr, variable_nr ]
    index <- index + 1
  }
  return( rolling_phi )
}

# See results
window_length <- 500
rolling_var <- RollingVAR( returns, window_length )
rolling_phi <- RollingVARCoefficient( returns, window_length, 4, 1 )


# Get Date from the indices of quantmod variables
# The effect of S&P500 on crude oil over time
layout( matrix( c( 1, 1, 2, 3), 2, 2, byrow = TRUE ) )
plot( index(SP)[ window_length:nrow( SP ) ], rolling_phi, type = "l",
      main = "Effect of S&P500 on crude oil", ylab = "Coef",
      xlab = "Time" )
plot( index(SP)[ window_length:nrow( SP ) ], CL$CL.Adjusted[ window_length:nrow( SP ) ], type = "l",
      ylab = "Price", xlab = "Crude oil" )
plot( index(SP)[ window_length:nrow( SP ) ], SP$GSPC.Adjusted[ window_length:nrow( SP ) ], type = "l",
      ylab = "Price", xlab = "S&P500" )


############
### VECM ###
############

# VAR is not capable of capturing long-run relationships
# Cointegration has important role in both economics and finance

# Create cointegrating variables

# Create AR(1) process
z <- rep(0, 10000)
for (i in 2:10000) z[i] <- 0.7* z[i-1] + rnorm(1)
plot.ts( z )

# Sum the values --> the process becomes first-order integrated
z_RW <- cumsum( z )
plot.ts( z_RW )

# Define variables so their linear combination is stationary
p <- q <- r <- rep( 0, 10000 )
p <- 0.3 * z_RW + rnorm( 10000 )
q <- 0.6 * z_RW + rnorm( 10000 )
r <- 0.2 * z_RW + rnorm( 10000 )

df <- data.frame( p,q,r )

ts.plot( df )

# Execute Johansen cointegration test to see if they cointegrate indeed
jotest = ca.jo( df, K=2 )
summary( jotest )
# r=2

# We can check whether the linear combinations are stationary
s <- 1 * p -  * q + 13.956499 * r
ts.plot( s )
adf.test( s )

v <- 1 * p - 0.4242500 * q - 0.2276377 * r 
ts.plot( v )
adf.test( v )

# Apply VECM
my_VECM <- VECM( df[ 1:9900, ], lag = 2, r = 2 )
summary( my_VECM )
# The two cointegrating vectors are significant in each equation

# We can plot IRF for VECM as well
plot( irf( my_VECM, impulse = "p", response = "q" ) )

# Make prediction
pred <- predict( my_VECM, n.ahead = 100 )
plot( 1:200, tail( df$p, 200 ), type = "l", xlab = "Date", 
      ylab = "Value", main = "Prediction with VECM" ) +
lines( 101:200, pred[ , 1 ], col = "red" )


########################
### Price Simulation ###
########################

n <- 200
phi <- 0.1

library( RColorBrewer )
library( extrafont )
windowsFonts( A = windowsFont( "Times New Roman" ) )

par( mar = c( 4, 4, 1, 3 ), mfrow = c( 1, 1 ), cex.axis = 1 ,cex.lab = 1.2 )

plot( NA, ylim = c( 0, 20 ), xlim = c( 0, n ), family = "A",
      ylab = "Simulated prices", xlab = "Time" )

colors <- rainbow( 50 )
for( i in 1:50 ){
  y <- rep(0, n)
  for ( j in 2:n ){ y[ j ] <- phi * y[ j - 1 ] + rnorm( 1 ) }
  y <- 0.05 * cumsum( y[ 1:n ] ) + c( 1:n ) * 0.005
  y <- 1 * exp( y )
  lines( y, col = colors[ i ], lwd=2 )
} 




 