library( sarima )
library( forecast )

# load AirPassengers default dataset
# Contains monthly data of flight passengers
data( "AirPassengers" )
AP <- AirPassengers

# Plot the time series to see how it looks
plot( AP, ylab = "Passengers (1000s)", type = "o", pch = 20 )

# Decompose the time series!
# First do it by hand, and then with 'decompose' function




# Check the stationarity of the series!
adf.test( AP )
kpss.test( AP )

# Find the best ARMA model! What transformation do you need to model it?
auto.arima ( log(AP), seasonal = TRUE )
plot( log(AP) )

# Check the correlogram for the original series!
par( mfrow = c( 2, 1 ) )
acf( AP, main = "ACF" )
pacf( AP, main = "PACF" )
dev.off()

# Take the first differences, does it seem to work?
# Try at least two methods! Remember we are not always working with 'ts' varoables!




# Try to remove seasonality be taking the right differences! 

AP12 = diff( AP, 12 )
plot( AP12 )

AP12_diff = diff( AP12 )
plot( AP12_diff )

par( mfrow = c( 2, 1 ) )
acf( AP12_diff, main = "ACF" )
pacf( AP12_diff, main = "PACF" )
dev.off()

# Take the logs!
logAP12 = diff( log( AP ), 12 )
logAP12_diff = diff( logAP12 )
plot( logAP12_diff )

par( mfrow = c( 2, 1 ) )
acf( logAP12_diff, main = "ACF" )
pacf( logAP12_diff, main = "PACF" )
dev.off()

# Build SARIMA model using 'arima' and 'sarima' functions:

seasonal_arima <- arima( log( AP ), order = c( 0, 1, 1 ), seasonal = list( order = c( 0, 1, 1 ), period = 12) )
summary( seasonal_arima )
tsdiag( seasonal_arima )
plot( resid( seasonal_arima ) )
 

my_sarima <- sarima( log( AP ) ~ 
                      0 | ma(1, c( -0.3 ) ) + sma( 12, 1, c( -0.1 ) ) + 
                       i( 1 ) + si( 12, 1 ), 
                    ss.method = "base" )
summary( my_sarima )
