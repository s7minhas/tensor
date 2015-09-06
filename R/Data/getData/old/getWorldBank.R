####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
load(paste0(inPath,'sampInfo.rda')) # loads expFiles, impFiles, cntries, dates
####

####
# get gdp/pop variables
wbVars = c(	
	'NY.GDP.MKTP.KD', # GDP, constant 2005 US dollars
	'NY.GDP.PCAP.KD', # GDP per capita, constant 2005 US dollars
	'SP.POP.TOTL' # population	
	)
loadPkg('WDI')
wdi=WDI(country='all', indicator=wbVars, start=2001, end=2015, extra=TRUE)

# Add logged versions of GDP and population
wdi$gdpLog = log(wdi$NY.GDP.MKTP.KD)
wdi$gdpCapLog = log(wdi$NY.GDP.PCAP.KD)
wdi$popLog = log(wdi$SP.POP.TOTL)

# Subset to countries in sampFrame
wdi$cname = countrycode(wdi$iso2c, 'iso2c', 'country.name')
wdi = wdi[which(wdi$cname %in% cntries$cname),
	c('cname', 'year', wbVars, 'gdpLog', 'gdpCapLog', 'popLog')]
wdi$id = paste(wdi$cname, wdi$year, sep='_')
####

####
# Save as sample frame
wbData=expand.grid(cname=cntries$cname, t=dates$date)
wbData$year = dates$year[match(wbData$t, dates$date)]
wbData$id = paste(wbData$cname, wbData$year, sep='_')
wbData = merge(wbData, wdi[,c('id',wbVars,'gdpLog','gdpCapLog','popLog')], 
	by='id', all.x=T, all.y=F)
wbData$id = paste(wbData$cname, wbData$t, sep='_')
wbData = wbData[,c('id', wbVars,'gdpLog','gdpCapLog','popLog')]
####

####
# Save
save(wbData, file=paste0(inPath, 'wbData.rda'))
####