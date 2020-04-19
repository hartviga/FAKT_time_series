###########
### VAR ###
###########


library( vars )
library( lattice )
library( forecast )
library( tseries )

# Example with Canada built in dataset
data( Canada )


# Plot returns to analyse stationarity
plot.ts( Canada )

# Execute KPSS test
lapply( Canada, kpss.test, null = "Trend" )
# Based on tests variables are not trend stationary

# Take first differences
diff_canada <- as.data.frame( lapply( Canada, diff, differences = 4 ) )
lapply( diff_canada, kpss.test )
# KPSS test p values big p values
# Therefore the series are stationary and VAR modelling is appropriate

# Choose lag for VAR
var_lag <- VARselect( diff_canada )
var_lag$selection

# Build VAR with lag suggested by Schwarz criteria
var_canada <- VAR( diff_canada, p = var_lag$selection[ 3 ], type = "both" )
summary( var_canada )

# See the plotted results
plot( var_canada )

# Impulse response functions helps to test the system by shocking a variable
plot( irf( var_canada, impulse = "e", n.ahead = 10, ortho = T ) )

# We can plot only one irf
plot( irf( var_canada, impulse = "e", response = "U", n.ahead = 10, ortho = T ) )

# Through the IRFs we can create the forecast error variance decompositon of the VAR
fevd_canada <- fevd( var_canada )

par(mar = c(2, 4, 2, 3), oma = c(0, 0, 0, 0))
plot( fevd( var_canada ) )
# This helps to see the relationships of the variables in a longer horizon


#######################
### Individual task ###
#######################

# Analyse the following financial data with VAR

# Download returns
SP <- quantmod::getSymbols( "^GSPC", scr = "yahoo", from = "2016-12-17", to = "2017-12-30", auto.assign = FALSE)
SP_return <- quantmod::dailyReturn( SP$GSPC.Close )
DX <- quantmod::getSymbols( "DX", scr = "yahoo", from = "2016-12-17", to = "2017-12-30", auto.assign = FALSE)
DX_return <- quantmod::dailyReturn( DX$DX.Close )
BP <- quantmod::getSymbols( "BP", scr = "yahoo", from = "2016-12-17", to = "2017-12-30", auto.assign = FALSE)
BP_return <- quantmod::dailyReturn( BP$BP.Close )
CL <- quantmod::getSymbols( "CL", scr = "yahoo", from = "2016-12-17", to = "2017-12-30", auto.assign = FALSE)
CL_return <- quantmod::dailyReturn( CL$CL.Close )

returns <- as.data.frame( matrix( c( SP_return, DX_return, BP_return, CL_return ), nrow = 260, ncol = 4 ) )
colnames( returns ) <- c( "SP", "DX", "BP", "CL" )









