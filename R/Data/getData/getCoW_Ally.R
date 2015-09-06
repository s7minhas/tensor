####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
load(paste0(inPath,'sampInfo.rda')) # loads expFiles, impFiles, cntries, dates
load(paste0(inPath,'frame.rda')) # loads frame
####

############################
# Download file from ICOW site
allyURL = 'http://www.correlatesofwar.org/data-sets/formal-alliances/alliances-data-dta-zip/at_download/file'
allyName = paste0(inPath, 'ally.zip')
if(!file.exists(allyName)) { download.file(allyURL, allyName) }
ally = unzip(allyName, 
	'version4.1_dta/alliance_v4.1_by_directed_yearly.dta') %>% read.dta()
unlink(paste0(getwd(), '/version4.1_dta'), 
    recursive=TRUE, force=TRUE)
############################

############################
# Subset alliance dataset by relevant characteristics
# Subset to after 2001
ally = ally[ally$year>=2001,]

# Limit to defense treaties, and remove duplicate treaty refs
relVars = c('ccode1', 'ccode2', 'state_name1', 'state_name2','year')
ally = unique(ally[ ally$defense==1, relVars]) %>% unique()
############################

############################
# Clean up countrynames to match other datasets
ally$i = countrycode(ally$state_name1, 'country.name', 'country.name')
ally$j = countrycode(ally$state_name2, 'country.name', 'country.name')

# Subset by cntries in sampFrame
ally = ally[which(ally$i %in% cntries$cname),]
ally = ally[which(ally$j %in% cntries$cname),]

# Add id column and merge into frame
ally$ally = 1

ally = data.table( ally )
ally[,id:=paste(i,j,year,sep='_')]
ally = data.frame( ally )

frame = data.table( frame )
frame[,tmp:=paste(i,j,year,sep='_')]
frame = data.frame( frame )

frame$ally = ally$ally[match(frame$tmp, ally$id)]
ally = frame[,c('i','j','t','id','ally')]

# Cleanup, note that ally data ends at 12/2012
# ally$ally[ which( is.na(ally$ally) & 
# 	ally$t < as.Date('01/01/13',"%m/%d/%y") ) ] = 0
############################

############################
# Save
save(ally, file=paste0(inPath, 'ally.rda'))
############################