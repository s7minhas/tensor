####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
####

####
# Load and combine processed data
setwd(inPath)

# Trade data 
load('dyadExp.rda') # loads expData object

# Monadic variables (polity from CRISP, gdp & pop from imf)

## CRISP
load('crisp.rda')
### Check for matches
stopifnot( length(intersect(expData$i_id, crisp$id))==nrow(crisp) )
stopifnot( length(intersect(expData$j_id, crisp$id))==nrow(crisp) )
### Merge in variables
crispVars = c('polity', 'gdp', 'gdpCap', 'pop', 'gdpLog', 'gdpCapLog', 'popLog')
for(var in crispVars){
	expData$tmp = crisp[,var][match(expData$i_id, crisp$id)]
	names(expData)[ncol(expData)] = paste('i', var, sep='_')
	expData$tmp = crisp[,var][match(expData$j_id, crisp$id)]
	names(expData)[ncol(expData)] = paste('j', var, sep='_') }
rm(list='crisp')

## World Exports
load('wrldExprts.rda')
### Check for matches
stopifnot( length(intersect(expData$i_id, expMoData$id))==nrow(expMoData) )
stopifnot( length(intersect(expData$j_id, expMoData$id))==nrow(expMoData) )
### Merge in variables
imfVars = c('wrldExp', 'wrldExpLog' )
for(var in imfVars){
	expData$tmp = expMoData[,var][match(expData$i_id, expMoData$id)]
	names(expData)[ncol(expData)] = paste('i', var, sep='_')
	expData$tmp = expMoData[,var][match(expData$j_id, expMoData$id)]
	names(expData)[ncol(expData)] = paste('j', var, sep='_') }
rm(list='expMoData')

# Dyadic variables (alliance from COW, geo from cshapes, quad from ICEWS)

## Aliance
load('ally.rda')
### Check for matches
stopifnot( length( intersect(ally$id, expData$id) ) == nrow(ally) )
### Merge in variable
ally$ally[which(is.na(ally$ally) & ally$t<as.Date('01/01/13', '%m/%d/%y'))] = 0
expData$ally = ally$ally[match(expData$id, ally$id)]
rm(list='ally')

## Distance
load('distData.rda')
### Check for matches
stopifnot( length( intersect(distData$id, expData$id) ) == nrow(distData) )
### Merge in variables
distVars = c('capDist', 'minDist', 'capDistLog', 'minDistLog')
for(var in distVars){
	expData$tmp = distData[,var][match(expData$id, distData$id)]
	names(expData)[ncol(expData)] = var }

## ICEWS Quad variables
load('quad.rda')
### Check for matches
stopifnot( length( intersect(quadData$id, expData$id) ) == nrow(quadData) )
### Merge in variables
quadVars = c('verbCoop', 'matlCoop', 'verbConf', 'matlConf')
for(var in quadVars){
	expData$tmp = quadData[,var][match(expData$id, quadData$id)]
	names(expData)[ncol(expData)] = var }

## PTAs
load('desta.rda')
### Check for matches
stopifnot( length( intersect(desta$id, expData$id) ) == nrow(desta) )
### Merge in variables
dVars = c('ptaCnt', 'pta')
for(var in dVars){
	expData$tmp = desta[,var][match(expData$id, desta$id)]
	names(expData)[ncol(expData)] = var }	
####

####
# Save
dirDyad = expData
save(dirDyad, file=paste0(inPath, 'dirDyad.rda'))
####