####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }
load(paste0(inPath,'sampInfo.rda')) # loads frame, expFiles, impFiles
load(paste0(inPath,'frame.rda')) # loads frame
####

####
# Download file from ICOW site
destaURL = 'http://www.designoftradeagreements.org/wp-content/uploads/DESTA_dyadic27March15.xlsx'
destaName = paste0(inPath, 'desta.xlsx')
if(!file.exists(destaName)) { 
	download.file(destaURL, destaName) }
loadPkg("openxlsx")
desta = read.xlsx(destaName, sheet=1)
####

####
# Add cleaned country names
cntry = c(desta$country1, desta$country2) %>% unique() %>% data.frame(cntry = .)
cntry$cname = countrycode(cntry$cntry, 'country.name', 'country.name')
desta$cname1 = cntry$cname[match(desta$country1, cntry$cntry)]
desta$cname2 = cntry$cname[match(desta$country2, cntry$cntry)]

# Subset to relev variables in desta
desta = desta[,c('cname1','cname2','Year','Name')]
names(desta)[3] = 'year'
####

####
# set up dir dyad desta frame

# Create count of PTAs by year
destaExp = lapply(1:nrow(desta), function(i){
	expand.grid(
		char( desta[i,paste0('cname',1:2)] ), 
		char( desta[i,paste0('cname',1:2)] ),
		desta[i,'year']:2014) 
	}) %>% do.call('rbind',.) %>% .[.[,1] != .[,2],]
cnts = paste(destaExp[,1],destaExp[,2],destaExp[,3],sep='_') %>% table() %>% cbind() %>% data.frame() 
cnts$id = rownames(cnts) ; names(cnts)[1] = 'cnt' ; rownames(cnts) = NULL
cnts$idD = unlist(lapply(strsplit(cnts$id, '_'), function(i) paste(i[1], i[2], sep='_')))

# Merge in PTA variable into frame
frame = data.table( frame ); frame[,tmp:=paste(i,j,year,sep='_')]; frame = data.frame( frame ) # add id year to frame
frame$ptaCnt = cnts$cnt[match(frame$tmp, cnts$id)]
frame$ptaCnt[is.na(frame$ptaCnt)] = 0
frame$pta = ifelse(frame$ptaCnt>=1, 1, 0)
####

####
# Save
desta = frame
save(desta, file=paste0(inPath, 'desta.rda'))
####