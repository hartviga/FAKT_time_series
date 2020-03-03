################
# First Seminar
################

# Variable types

# Generally we use integer, double, string, vector, list and data frame variables. 
# Most cases they also work with time series (imagine a data frame where the first column is the date). 
# Still there are specific functions that require 'ts' variables.
# Let's see how it works.

my_first_integer <- 1
my_first_integer
my_first_string <- "This class is awesome"
my_first_string
my_first_vector <- c( 1:20 )
my_first_vector
my_first_list <- list( my_first_integer, my_first_string, my_first_vector )
my_first_list
my_first_df <- data.frame( my_first_vector )
plot( my_first_df$my_first_vector, type = "l" )
my_first_ts <- ts( my_first_vector, start = c( 2010 ), frequency = 4 )
plot.ts( my_first_ts )


# Simulate AR
# What is stationarity? When is an AR(1) stable?
# Create an AR(1) process with 2000 observations with arima.sim function. 
# Plot it as a time series variables that starts from 01/01/2017 with daily frequency (day count basis: Act/360). 
# Then plot only the 1001:2000 observartions on the same plot as the previous plot.


phi1 <- 0.5
N <- 2000
y <- arima.sim( list( ar = phi1 ), n = N )
par( mfrow = c( 2, 1 ) )
plot( y, type = "l" )
plot( y[ 1001:2000 ], type = "l" )

# Check the ACF of the PACF of the process! 
# Rerun the simulation with n = 50, 100, 500, 1000! 
# How does the autocorrelation change?
acf( y, lag.max = 20 )
pacf( y, lag.max = 20 )
# Try the same by setting standard deviation higher with 'sd' parameter in arima.sim.

# Run AR(1) model to see if the estimatation does give back the same coefficient that you used as an input.
# Then, repeat it 100 times and check how often the phi coefficient is significant!
# (Hint: extract coefficient with dollar coef[ 1 ] and variance matrix with dollar var.coef) Try MA(6) also, and explain the results.

ar1 <- arima( y, order = c( 1, 0, 0 ) )
ar1

# Run MA model for the simulated AR(1) process! 
# Interpret the results!

# Which model do we choose? 
# How can we check if a model is appropriate?
# (Hint: use Box.test, AIc and BIC functions)


# Empirical analysis
ARWR <- as.data.frame( quantmod::getSymbols( "ARWR", src = "yahoo", from = "1994-01-30", to = "2020-01-30", auto.assign = FALSE ), row.names = NULL )
ARWR_price <- ARWR$ARWR.Adjusted

# Calculate the log returns and find the best ARMA model!
