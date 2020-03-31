library( lattice )
library( latticeExtra )
library( grid )
library( gridExtra )


# Loading data

GSPC <- quantmod::getSymbols( "^GSPC", from = "1900-01-01", auto.assign = FALSE )
plot( GSPC$GSPC.Adjusted, type = "l" )
LRM <- data.frame( abs( quantmod::dailyReturn( GSPC$GSPC.Adjusted ) ) )
colnames( LRM ) <- "GSPC_return"
LRM$WN <- rnorm( nrow( LRM ) )
LRM$AR1 <- as.numeric( arima.sim( list( ar = 0.7 ), n = nrow( LRM ) ) )
LRM$RW <- cumsum( rnorm( nrow( LRM ) ) )
LRM$index <- 1:nrow( LRM )



# Plot the four time series and their ACF! What are the characteristics of each?

TSs <- colnames( LRM[ , 1:4 ] )
res <- lapply( TSs, function( ts ) xyplot( as.formula( paste0( ts, "~ index" ) ), data = LRM, type = "l", main = ts ) )
do.call( grid.arrange, res )


res <- lapply( TSs, function( ts ) xyplot( acf ~ lag, data = acf( LRM[[ ts ]], 200, plot = FALSE ), type = "h", main = ts,
                                           ylim = c( -0.1, 1.1 ) ) )
do.call( grid.arrange, res )



# Comparing ACF of ARFIMA and AR processes 

xyplot( acf ~ lag, groups = p,
        data = rbind( data.frame( p = "S&P500", acf = acf( LRM$GSPC_return, lag.max = 200, plot = FALSE )$acf, lag = 0:200 ),
                      do.call( rbind, lapply( c( seq( 10, 50, 10 ), 100 ), function( x )
                        data.frame( p = paste0( "AR, p=", x ),
                                    acf = ARMAacf( ar = ar( LRM$GSPC_return, aic = FALSE, order.max = x )$ar, lag.max = 200 ),
                                    lag = 0:200 ) ) ) ),
        type = c( "h", rep( "l", 6 ) ), auto.key = list( columns = 4, points = FALSE, lines = TRUE ), distribute.type = TRUE )



### Simulate ARFIMA

d <- 0.4
arfima_acf <-  arfima::tacvfARFIMA( dfrac = d, maxlag = 100 )/arfima::tacvfARFIMA( dfrac = d, maxlag = 100 )[1]

plot( c( 0:100 ), arfima_acf, type = "h", xlab = "lag", ylab = "ACF", main = paste0( "d=", d ) )

arfima_sim <- arfima::arfima.sim( 1000, list( d = d ) )
plot( 1:1000, arfima_sim, type = "l", xlab = "t", ylab = "y", main = paste0( "d=", d ) )


# Fit ARIMA modells on the simulation, which is the best model?

par( mfrow = c( 2, 1 ) )
pacf( arfima_sim )
acf( arfima_sim )

my_arima <- arima( arfima_sim, order = c( 0, 0, 10 ) )
summary( my_arima )

### Estimate ARFIMA

fit <- fracdiff::fracdiff( LRM[[ "GSPC_return" ]], nar = 0, nma = 0, drange = c( 0, 0.5 ) )
summary( fit )

fit <- fracdiff::fracdiff( LRM[[ "GSPC_return" ]], nar = 1, nma = 1, drange = c( 0, 0.5 ) )
summary( fit )
