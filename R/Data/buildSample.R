####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensor/R/setup.R');
	mysqlSetup('shahryarm', 'green29tumble', 'event_data') 
}

if( Sys.info()['user']=='mw160' ){ source('~/git/tensor/R/setup.R') }

on.exit(dbDisconnect(conn))
####

####
# Load table with country ids in events database
cntryIDs = dbGetQuery(conn, "SELECT * FROM countries")
####

####
# Pull out top fifty highest income countries from CRISP
library(CRISP)
data(crisp.data)
loadPkg(c('doBy', 'countrycode'))

# Average GDPs over 2001-2014 to determine top fifty countries
gdpCntry=summaryBy(NY.GDP.MKTP.KD ~ country, data=crisp.data, FUN=mean)
gdpCntry = gdpCntry[order(gdpCntry[,2], decreasing=TRUE),]
topFifty = as.character(gdpCntry[1:49,1])

# Add in the United States
topFifty = append(topFifty, 'UNITED STATES')

# Remove underscores in names
topFifty = gsub('_', ' ', topFifty)

# Some conversions so we can match with the 
# country id in the sql database
topFifty=countrycode(topFifty, 'country.name', 'country.name')
topFifty[topFifty=="KOREA, REPUBLIC OF" ]="SOUTH KOREA"
topFifty[topFifty=="VENEZUELA, BOLIVARIAN REPUBLIC OF"]="VENEZUELA"

# Check to make sure all countries that are in 
# top fifty list are in the events database
setdiff(topFifty, cntryIDs[,2])

# Subset the country ID database from sql by the top fifty list
cntryIDs=cntryIDs[which(cntryIDs[,2] %in% topFifty),]

# Save result
save(cntryIDs, file=paste0(inPath, 'cntryIDs.rda'))
####