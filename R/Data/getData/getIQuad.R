####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
load(paste0(inPath,'sampInfo.rda')) # loads expFiles, impFiles, cntries, dates
load(paste0(inPath,'frame.rda')) # loads frame
####

####
# sql server
loadPkg('RMySQL')
mysqlSetup('shahryarm', 'green29tumble', 'event_data')
on.exit(dbDisconnect(conn))
####

####
# Load relevant cameo event datasets
verbCoop = dbGetQuery(conn, "SELECT * FROM my_tables.tensorVerbCoop;")
matlCoop = dbGetQuery(conn, "SELECT * FROM my_tables.tensorMatlCoop;")
verbConf = dbGetQuery(conn, "SELECT * FROM my_tables.tensorVerbConf;")
matlConf = dbGetQuery(conn, "SELECT * FROM my_tables.tensorMatlConf;")
quadList = list(
	verbCoop=verbCoop, matlCoop=matlCoop, 
	verbConf=verbConf, matlConf=matlConf )

# Clean up quad data pull
quadList = lapply(quadList, function(q){

	# Subset to relevant temporal frame
	q$date = as.Date(paste(1, q$month, q$year, sep='/'), '%d/%m/%Y')
	q = q[which(q$date %in% dates$date),]

	# Subset to relevant countries
	q$i = countrycode(q$source_country, 'iso3c', 'country.name')
	q$j = countrycode(q$target_country, 'iso3c', 'country.name')

	# Remove i-i interactions
	q = q[which(q$i != q$j), ]

	# Following countries are dropped by removing NAs
	# KOS (Kosovo), TKL (Tokelau), BES (Bonaire, Sint Eustatius and Saba) 
	q = na.omit(q)

	# Subset countries to those in sampinfo
	q = q[which(q$i %in% cntries$cname),]
	q = q[which(q$j %in% cntries$cname),]

	# Create ID and subset to relev vars
	q$id = paste(q$i, q$j, q$date, sep='_')
	return( q[,c(5, 9)] ) 
	} )
####

####
# Merge each quad into frame
for(ii in 1:length(quadList)){
	frame$tmp = quadList[[ii]][,1][match(frame$id, quadList[[ii]][,2])]
	frame$tmp[is.na(frame$tmp)] = 0
	names(frame)[ncol(frame)] = names(quadList)[ii] }

# Clean up data
quadData = frame[,c('i','j','t','id',names(quadList))]
####

####
# Save
save(quadData, file=paste0(inPath, 'quad.rda'))
####