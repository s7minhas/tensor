####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
load(paste0(inPath,'sampInfo.rda')) # loads frame, expFiles, impFiles
####

####
# csv files with export data
tPath = paste0(inPath, 'trade/')
expFiles = list.files(tPath)[grepl('Value of Exports', list.files(tPath))]
####

####
# Create directed dyadic datasets
# Function to process IMF DOT csv Sheets
processIMF = function(file, path=tPath, verbose=TRUE){
	if(verbose){print(paste0('Processing ', file, '...'))}
	slice = read.csv(paste0(path, file), header=FALSE)
	names(slice) = paste(
		char(unlist(slice[1,])), 
		char(unlist(slice[2,])), sep='_')
	slice = slice[c(-1,-2),]
	names(slice)[1] = 'j'
	slice = melt(slice, id='j')
	ids = strsplit(char(slice$variable), '_')
	slice$i = unlist(lapply(ids, function(x) x[1]))
	slice$t = unlist(lapply(ids, function(x) x[2]))
	slice = slice[,c('i', 'j', 't', 'value')]
	slice$j = slice$j %>% char()
	slice$value = num(slice$value)

	# Subset slice by dates in sampFrame
	slice = slice[which(slice$t %in% dates$tdate), ]
	stopifnot( length(unique(slice$t)) ==  nrow(dates) )

	# Return object
	return(slice)
}

expData = lapply(expFiles, function(f){ processIMF(f) }) %>% do.call('rbind', .)
####

####
# Cleaning 
# Remove Thailand i-j cases
expData = expData[which(expData$i!=expData$j),]

# Add in cnames and ccodes
tCntries = c(expData$i, expData$j) %>% unique() %>% char() %>% data.frame(.,stringsAsFactors=FALSE); names(tCntries)='tCntry'
tCntries$cname = countrycode(tCntries$tCntry, 'country.name', 'country.name')
toDrop = c('European Union', 'Middle East, North Africa, Afghanistan, and Pakistan', 'Sub-Saharan Africa', 'Belgium-Luxembourg not specified', 'Czechoslovakia not specified', 'Falkland Islands', 'French Territories: French Polynesia', 'Kosovo, Republic of', 'Serbia and Montenegro', 'Serbia Montenegro not specified', 'Serbia, Republic of', 'Timor-Leste, Dem. Rep. of', 'U.S.S.R. not specified', 'West Bank and Gaza', 'Yugoslavia not specified', 'SACCA excluding South Africa')
tCntries = tCntries[which(!tCntries$tCntry %in% toDrop),] # Drop units/countries that aren't in CRISP or countrycode mislabeled
tCntries = tCntries[!is.na(tCntries$cname),] # Drop groupings of countries
# Subset to countries in cntries from sampFrame.R
tCntries = tCntries[which(tCntries$cname %in% cntries$cname),] # Leads to dropping of mostly small countries

# Subset expdata by tCntries are relabel
expData = expData[which(expData$i %in% tCntries$tCntry),]
expData = expData[which(expData$j %in% tCntries$tCntry),]
expData$i = tCntries$cname[match(expData$i, tCntries$tCntry)]
expData$j = tCntries$cname[match(expData$j, tCntries$tCntry)]

# Add in cleaned date variable
expData$t = dates$date[match(expData$t, dates$tdate)]

# Add in unique id
expData = data.table( expData )
expData[,id:=paste(i,j,t,sep='_')]

# Merge with frame
load(paste0(inPath,'frame.rda'))
frame$exports = expData$value[match(frame$id, expData$id)]
frame$exports[is.na(frame$exports)] = 0
expData = frame
####

####
# Save data
save(expData, file=paste0(inPath, 'dyadExp.rda'))
####