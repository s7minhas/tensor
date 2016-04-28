####
if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensor/R/setup.R');
	load('~/Dropbox/Research/icewsServerInfo.rda')
	mysqlSetup(user=icewsSN, pw=icewsPwd, db='event_data', host=icewsHost) 
}

if( Sys.info()['user']=='mw160' ){ source('~/git/tensor/R/setup.R') }

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

# Load in data file with sample info
load(paste0(inPath, 'cntryIDs.rda'))
####

####
# Subset list of quad variables by the fifty cntries in cntryIDs
quadList = lapply(quadList, function(quad){
	quad[which(
		quad$source_country %in% cntryIDs$ISOA3Code &
		quad$target_country %in% cntryIDs$ISOA3Code ) , ]
	})

# Drop instances where source and target are the same
quadList = lapply(quadList, function(quad){
	quad[ which( quad$source_country != quad$target_country ), ] })

# Add date variable
quadList = lapply(quadList, function(quad){
	quad$date = as.Date( paste0(
		quad$month,'/1/',
		substr(quad$year,3,4)
		),  "%m/%d/%y")
	return(quad)
	})
####

####
# Set up arrays

# Cntry dimension
cntries=cntryIDs$ISOA3Code
cntryDim=length(cntries)

# Time dimension
times=unique(quadList[[1]]$date)[121:288] # 1/1/2001 to 1/12/2014
timeDim=length(times)

# Var dimension
vars=names(quadList)
varDim=length(quadList)

# Empty array
allQuad = array(dim=c(cntryDim, cntryDim, varDim, timeDim))
dimnames(allQuad)[[1]]=dimnames(allQuad)[[2]]=cntries
dimnames(allQuad)[[3]]=vars
dimnames(allQuad)[[4]]=as.character(times)

# Fill in
## Double for loop but its quick
loadPkg('reshape2')
for(v in 1:varDim){
	for(t in 1:timeDim){
		quadV = quadList[[v]]
		quadVT = quadV[which(quadV$date == times[t] ),3:5]
		val=names(quadVT)[3]
		mat=acast(quadVT, source_country ~ target_country, value.var=val)
		allQuad[rownames(mat),colnames(mat),v,t] = mat
		allQuad[,,v,t][is.na(allQuad[,,v,t])]=0
	}
}
save(allQuad, quadList, file=paste0(inPath, 'rawDV.rda'))
####

####
# Get Z Scores
Z <- aperm(apply(allQuad,c(1,2,3),zscores) ,c(2,3,4,1))
####

####
# Relational covariates

ZM <- Z # ij, main effect
ZR <- aperm(Z,c(2,1,3,4)) # ji, reciprocal effect

ZT <- Z ; ZT[is.na(ZT)] <- 0 # ijk, transitive effect
for(j in 1:dim(ZT)[3]){for(k in 1:(dim(ZT)[4]))
{
  XS <- (ZT[,,j,k]+t(ZT[,,j,k]))/2
  ZT[,,j,k] <- XS%*%XS
}}
####

####
# Lag by one month 

## Y
Y <- Z[,,,-1]

## X
# Combine relational covariates into one array
X <- array( dim=append(dim(Y), 3, after=3) )
X[,,,1,] <- ZM[,,,-dim(Z)[4]]
X[,,,2,] <- ZR[,,,-dim(Z)[4]]
X[,,,3,] <- ZT[,,,-dim(Z)[4]]
####

####
# Standardize

## center 
Y <- sweep(Y,c(1,2,3),apply(Y,c(1,2,3),mean),"-") 
X <- sweep(X,c(1,2,3,4),apply(X,c(1,2,3,4),mean),"-")

## scale 
Y <- sweep(Y,c(3),apply(Y,c(3),sd,na.rm=TRUE),"/")
X <- sweep(X,c(3,4),apply(X,c(3,4),sd),"/")
####

####
# Label and save

## labels 
dimnames(Y)[1:3] <- dimnames(X)[1:3] <- dimnames(Z)[1:3] 
dimnames(X)[[4]] <- c("ij","ji","ijk") 
dimnames(Y)[[4]] <- dimnames(X)[[5]] <- dimnames(Z)[[length(dim(Z))]][-1]

## save 
save(Y, X, file=paste0(inPath, "YXsm.rda"))
####