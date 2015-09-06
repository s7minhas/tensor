####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
load(paste0(inPath,'sampInfo.rda')) # loads expFiles, impFiles, cntries, dates
load(paste0(inPath,'frame.rda')) # loads frame
####

####
# Load datasets from cshapes
load(paste0(inPath, 'distMats.rda'))
load(paste0(inPath, 'panel.rda'))

# Subset to analysis frame
capMats = capMats[char(2001:2012)]
minMats = minMats[char(2001:2012)]

# Extend data to 2014
capMats$'2014' = capMats$'2013' = capMats$'2012'
minMats$'2014' = minMats$'2013' = minMats$'2012'
####

####
# Clean up each distance matrix
addNamesMelt = function(d){
	dCntries = data.frame(orig = rownames(d), stringsAsFactors=FALSE)
	dCntries$dName = panel$cname[match(rownames(d), panel$GWCODE)]
	dCntries = dCntries[which(dCntries$dName %in% cntries$cname),]
	dClean = d[dCntries$orig, dCntries$orig]
	rownames(dClean) = colnames(dClean) = dCntries$dName
	return( melt(dClean) )	
}

capDyad = lapply(capMats, addNamesMelt ) %>% do.call(what='rbind')
minDyad = lapply(minMats, addNamesMelt ) %>% do.call(what='rbind')
####

####
# Add country labels and cleanup
cleanDyad = function(distName, dDyad){
	dDyad = dDyad[which(dDyad$Var1 != dDyad$Var2),]
	names(dDyad)[c(1:3)] = c('i', 'j', distName)
	dDyad$year = num(substr(rownames(dDyad), 1, 4))
	dDyad$id = paste(dDyad$i, dDyad$j, dDyad$year, sep='_')
	return(dDyad)
}

capDyad = cleanDyad('capDist', capDyad)
minDyad = cleanDyad('minDist', minDyad)
####

####
# Merge dist vars into frame
frame$tmp = paste(frame$i, frame$j, frame$year, sep='_')
frame$capDist = capDyad$capDist[match(frame$tmp, capDyad$id)]
frame$minDist = minDyad$minDist[match(frame$tmp, minDyad$id)]

# Cleanup
distData = frame[,c('i','j','t','id','capDist','minDist')]
distData$capDistLog = log(distData$capDist)
distData$minDistLog = log(distData$minDist + 1)
####

####
# Save to binaries
save(distData, file=paste0(inPath,'distData.rda'))
####