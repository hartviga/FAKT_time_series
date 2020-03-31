library( lattice )
library( forecast )
library( tseries )

#############################
### Building ARIMAX model ###
#############################

# Build ARIMA and ARIMAX model on the returns of GE! Let's see which one is more accurate!

# ARIMA

ge <- quantmod::getSymbols( "GE", scr = "yahoo", from = "2016-12-17", to = "2017-12-30", auto.assign = FALSE)
plot( ge$GE.Adjusted )

ge <- quantmod::dailyReturn( ge$GE.Adjusted )
plot( ge )
tsdisplay( ge )

# Test stationarity
adf.test( ge )
kpss.test( ge )

# Search the best ARMA model based on autoarima
ge_arima_model <- auto.arima( ge, trace = T )

summary( ge_arima_model )

# Plot the results
tsdiag( ge_arima_model )
qqnorm( resid( ge_arima_model ) )    
qqline( resid( ge_arima_model ) )


# Forecast with the model
# Out-of-sample
forecast( ge_arima_model ) 
plot( forecast( ge_arima_model ), xlim = c( 250, 270 ) )

# In-sample 
ge_train_model <- arima( ge[ 1:250 ], order = c( 0, 0, 2 ) )
forecast( ge_train_model )
plot( forecast ( ge_train_model ) )


# Forecast error measures
accuracy( forecast( ge_train_model ) )
summary( ge_train_model )
accuracy( forecast( ge_train_model ), ge[ 251:260 ] )

# ARIMAX
# Include exogen variables in the model
# Download S&P500
# Why should it be a good explanatory variable?

sp <- quantmod::getSymbols( "^GSPC", scr = "yahoo", from = "2016-12-17", to = "2017-12-30", auto.assign = FALSE)
sp <- quantmod::dailyReturn( sp$GSPC.Adjusted )

plot( sp )
tsdisplay( sp )

ge_arimax_model <- auto.arima( ge, trace = T, xreg = sp )
summary( ge_arimax_model )

# In-sample
ge_train_arimax <- Arima( ge[ 1:250 ], order = c( 0, 0, 2 ), xreg = sp[ 1:250 ] )
summary( ge_train_arimax )
forecast( ge_train_arimax, xreg = sp[ 251:260 ] )
plot( forecast ( ge_train_arimax, xreg = sp[ 251:260 ] ) )


# Forecast error measure
accuracy( forecast( ge_train_arimax, xreg = sp[ 251:260 ] ) )
summary( ge_train_arimax )
accuracy( forecast( ge_train_arimax, xreg = sp[ 251:260 ] ), ge[ 251:260 ] )


# Plots
std_ge <- resid( model1 ) - mean( resid( model1 ) )
std_ge <- std_ge / sqrt( var( resid( model1 ) ) )
hist( std_ge, breaks = 20, prob = T )
curve(dt(x, 1000), from = -5, to = 5, col = "orange", 
      xlab = "quantile", ylab = "density", lwd = 2, add = TRUE )
curve(dt(x, 10), from = -5, to = 5, col = "dark green", add = TRUE, lwd = 2 )
curve(dt(x, 1), from = -5, to = 5, col = "grey40", add = TRUE, lwd = 2 )
legend("topleft", legend = paste0("DF = ", c(1, 10, 30)),
       col = c("grey40", "dark green", "orange"),
       lty = 1, lwd = 2)


  

