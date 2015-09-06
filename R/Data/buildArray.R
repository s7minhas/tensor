####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
load(paste0(inPath,'sampInfo.rda')) # loads expFiles, impFiles, cntries, dates
####

####
# Load directed dyadic dataset
load(paste0(inPath, 'dirDyad.rda'))

# Alliance dataframe ends at 2012
# dirDyad = dirDyad[dirDyad$year<2013,]

# Log Exports
dirDyad$exportsLog = log( dirDyad$exports + 1 )

# Standardize material conflict variable
dirDyad$matlConfStd = ( dirDyad$matlConf - mean( dirDyad$matlConf ) ) / sd( dirDyad$matlConf )

# Check for NAs
# stopifnot( sum(is.na(dirDyad)) == 0 )
####

####
# Set up array for endogenous covariates
source(paste0(rFuncs, 'mltrHelpers.R'))

# Cast into array format
expArr = castArray(dirDyad, 'exportsLog')
confArr = castArray(dirDyad, 'matlConfStd')

# Combine arrays
Z = array( dim=append( dim(expArr), 2, after=2 ) )
Z[,,1,] = expArr
Z[,,2,] = confArr

# Set up relational covariates
ZM = Z # ij, main effect

ZR = aperm(Z, c(2,1,3,4)) # ji, reciprocal effect

ZT = Z # ijk, transitive effect
for( var in 1:dim(ZT)[3] ){
	for( mnth in 1:dim(ZT)[4] ){
		XS = ( ZT[,,var,mnth] + t(ZT[,,var,mnth]) )/2
		ZT[,,var,mnth] = XS %*% XS
	}
}

# Set up lagging strategy and create DV array
Y = Z[,,,-1]

# Combine relational covariates into one array
X = array( dim=append( dim(Y), 3, after=3 ) )
X[,,,1,] = ZM[,,,-dim(Z)[4]]
X[,,,2,] = ZR[,,,-dim(Z)[4]]
X[,,,3,] = ZT[,,,-dim(Z)[4]]
####

####
# Create arrays for exogenous covariates
ptaX = prepMLTR(var='ptaCnt', incMain=TRUE, incRecip=FALSE, incTrans=TRUE)
distX = prepMLTR(var='minDistLog', incMain=TRUE, incRecip=FALSE, incTrans=FALSE)
# allyX = prepMLTR(var='ally', incMain=TRUE, incRecip=FALSE, incTrans=TRUE)
i_polX = prepMLTR(var='i_polity', incMain=TRUE, incRecip=FALSE, incTrans=FALSE)
i_gdpLogX = prepMLTR(var='i_gdpLog', incMain=TRUE, incRecip=FALSE, incTrans=FALSE)
i_popLogX = prepMLTR(var='i_popLog', incMain=TRUE, incRecip=FALSE, incTrans=FALSE)
i_wrldExpLogX = prepMLTR(var='i_wrldExpLog', incMain=TRUE, incRecip=FALSE, incTrans=FALSE)
####

####
# Combine into X array across third dimension
X = aperm(apply(X, c(1,2,5), 'c'), c(2,3,1,4)) # First combine relational endogenous terms
tmp = array(dim = c(dim(X)[1:2], dim(X)[3] + 7, dim(X)[4] ) )
tmp[,,1:6,] = X
tmp[,,7:8,] = ptaX
tmp[,,9,] = distX[,,1,]
tmp[,,10,] = i_polX[,,1,]
tmp[,,11,] = i_gdpLogX[,,1,]
tmp[,,12,] = i_popLogX[,,1,]
tmp[,,13,] = i_wrldExpLogX[,,1,]
X = tmp
####

####
# Label and save

## labels for DV
dimnames(Y)[1:2] = dimnames(expArr)[1:2]
dimnames(Y)[[3]] = c('exports', 'matlConf')
dimnames(Y)[[4]] = dimnames(expArr)[[3]][-1]

## Labels for X
dimnames(X)[1:2] = dimnames(Y)[1:2]
dimnames(X)[[4]] = dimnames(Y)[[4]]
dimnames(X)[[3]] = c(
	'exp_ij', 'mconf_ij', 'exp_ji', 'mconf_ji', 'exp_ijk', 'mconf_ijk',
	'pta_ij', 'pta_ijk', 
	'dist_ij', 'pol_ij', 'gdp_ij', 'pop_ij', 'wrldexp_ij'
	)

## save 
save(Y, X, file=paste0(inPath, "YX.rda"))
####