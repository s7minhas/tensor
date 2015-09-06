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
# Get out world trade for each country
# Function to process IMF DOT csv Sheets
processMonadIMF = function(file, path=tPath, verbose=TRUE){
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

	# Subset slices by exports to world 
	stopifnot( 'World' %in% slice$j )
	slice = slice[slice$j=='World',]

	# Subset slice by dates in sampFrame
	slice = slice[which(slice$t %in% dates$tdate), ]
	stopifnot( length(unique(slice$t)) ==  nrow(dates) )

	# Return object
	return(slice)
}

expMoData = lapply(expFiles, function(f){ processMonadIMF(f) }) %>% do.call('rbind', .)
names(expMoData)[ncol(expMoData)] = 'wrldExp'
expMoData$wrldExpLog = log( expMoData$wrldExp )
####

####
# Cleaning 
# Add in cnames and ccodes
tCntries = expMoData$i %>% unique() %>% char() %>% data.frame(.,stringsAsFactors=FALSE); names(tCntries)='tCntry'
tCntries$cname = countrycode(tCntries$tCntry, 'country.name', 'country.name')
toDrop = c('European Union', 'Middle East, North Africa, Afghanistan, and Pakistan', 'Sub-Saharan Africa', 'Belgium-Luxembourg not specified', 'Czechoslovakia not specified', 'Falkland Islands', 'French Territories: French Polynesia', 'Kosovo, Republic of', 'Serbia and Montenegro', 'Serbia Montenegro not specified', 'Serbia, Republic of', 'Timor-Leste, Dem. Rep. of', 'U.S.S.R. not specified', 'West Bank and Gaza', 'Yugoslavia not specified', 'SACCA excluding South Africa')
tCntries = tCntries[which(!tCntries$tCntry %in% toDrop),] # Drop units/countries that aren't in CRISP or countrycode mislabeled
tCntries = tCntries[!is.na(tCntries$cname),] # Drop groupings of countries
# Subset to countries in cntries from sampFrame.R
tCntries = tCntries[which(tCntries$cname %in% cntries$cname),] # Leads to dropping of mostly small countries

# Subset expMoData by tCntries are relabel
expMoData = expMoData[which(expMoData$i %in% tCntries$tCntry),]
expMoData$i = tCntries$cname[match(expMoData$i, tCntries$tCntry)]

# Add in cleaned date variable
expMoData$t = dates$date[match(expMoData$t, dates$tdate)]

# Add cname and id vector
expMoData$id = paste(expMoData$i, expMoData$t, sep='_')
####

####
# Save
save(expMoData, file=paste0(inPath, 'wrldExprts.rda'))
####