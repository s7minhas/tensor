####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
load(paste0(inPath,'sampInfo.rda')) # loads frame, expFiles, impFiles
####

####
loadPkg('CRISP')
# get monthly level polity from CRISP
crispVars = c(  'DEMOC', 'AUTOC', # Polity
	'NY.GDP.MKTP.KD', # GDP, constant 2005 US dollars
	'NY.GDP.PCAP.KD', # GDP per capita, constant 2005 US dollars
	'SP.POP.TOTL') # population	  
data(crisp.data); crisp=crisp.data; rm(list='crisp.data')

crisp = crisp[,c('country','ccode','date','year',crispVars)]
crisp$country = char(crisp$country)
crisp$ccode = num(crisp$ccode)
crisp$polity = crisp$DEMOC - crisp$AUTOC + 11

# Add in US data...10s all the way through
## Polity
slice = crisp[crisp$country=='FRANCE',]
slice$country = 'United States';slice$ccode = 2;slice$polity = 10
## IMF WEI Dataset
imf = read.csv(paste0(inPath, 'weoreptc.csv'))
relVars = c('Population', 'Gross domestic product, current prices', 'Gross domestic product per capita, current prices')
relUnits = c('U.S. dollars', 'Persons')
imfUS = imf %>% .[.$Country=='United States',] %>% .[.$Subject.Descriptor %in% relVars,] %>% .[.$Units %in% relUnits,] %>% print()
names(imfUS)[7:(ncol(imfUS) - 1)] = 1999:2014
imfUS = imfUS[, c( 'Subject.Descriptor',char(1999:2014) ) ] %>% melt(.,id='Subject.Descriptor') %>% dcast(., variable ~ Subject.Descriptor)
names(imfUS) = c('year', 'gdpCapNom', 'gdpNom', 'SP.POP.TOTL')
for(ii in 1:ncol(imfUS)){ imfUS[,ii] = num(imfUS[,ii]) }

# Bring in deflator from WB to get GDP measures in constant 2005 dollars
# Defaltor data taken from http://www.bea.gov//national/nipaweb/DownSS2.asp, section 1, table 1.1.9, base year 2009
usDefl = read.csv(paste0(inPath, 'us_gdp_defl_bea.csv')) 
imfUS$defl = usDefl$defl[match(imfUS$year, usDefl$year)]
imfUS$defl05 = imfUS$defl[imfUS$year==2005]
# Convert GDP figure to real US dollars
imfUS$NY.GDP.MKTP.KD = imfUS$gdpNom * (imfUS$defl05/imfUS$defl)
# Calculate real GDP per capita from GDP (billions) and pop data (millions)
imfUS$NY.GDP.PCAP.KD = (imfUS$NY.GDP.MKTP.KD * 1000000000) / (imfUS$SP.POP.TOTL * 1000000)

# Add in US data to slice
slice$NY.GDP.MKTP.KD = imfUS$NY.GDP.MKTP.KD[match(slice$year, imfUS$year)]
slice$NY.GDP.PCAP.KD = imfUS$NY.GDP.PCAP.KD[match(slice$year, imfUS$year)]
slice$SP.POP.TOTL = imfUS$SP.POP.TOTL[match(slice$year, imfUS$year)]

# bind back into CRISP
crisp = rbind(crisp, slice)

# Limit to countries in sample
# Removes New Zealand, Papua New Guinea, S. Korea
crisp = crisp[which(crisp$country %in% cntries$crisp),]

# Dates in sample
crisp = crisp[which(crisp$date %in% dates$date),]

# Add cname and id vector
crisp$cname = cntries$cname[match(crisp$country, cntries$crisp)]
crisp$id = paste(crisp$cname, crisp$date, sep='_')
####

####
# Rename and add logged versions
names(crisp)[7:9] = c('gdp', 'gdpCap', 'pop')
crisp$gdpLog = log(crisp$gdp + 1)
crisp$gdpCapLog = log(crisp$gdpCap)
crisp$popLog = log(crisp$pop + 1)
####

####
# Save
save(crisp, file=paste0(inPath, 'crisp.rda'))
####