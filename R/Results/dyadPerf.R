if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensor/R/setup.R') }
if( Sys.info()['user']=='mw160' ){ source('~/git/tensor/R/setup.R') }

####
# Load raw DV
# load(paste0(inPath, 'rawDV.rda'))
outPath='~/Research/WardProjects/tensor/Text/Graphics/'
####

####
# data
load(paste0(inPath, "YXsm.rda"))
dnX = dimnames(X)
X = aperm( apply(X,c(1,2,5),"c")  ,c(2,3,1,4))
dimnames(X)[1:2] = dnX[1:2]
dimnames(X)[[3]] =  c(outer( dnX[[3]],dnX[[4]],paste0) )

# Checks
cntries = dimnames(Y)[[1]]
dates = dimnames(Y)[[4]]

# Create dyadic frames for data
dyadData = melt(Y[,,'verbCoop',])
names(dyadData) = c('source','target','date','verbCoop')
dyadData$matlCoop = melt(Y[,,'matlCoop',])[,4]
dyadData$verbConf = melt(Y[,,'verbConf',])[,4]
dyadData$matlConf = melt(Y[,,'matlConf',])[,4]

# Add in independent variables
dyadData$verbCoopL = melt(X[,,'verbCoopij',])[,4]
dyadData$matlCoopL = melt(X[,,'matlCoopij',])[,4]
dyadData$verbConfL = melt(X[,,'verbConfij',])[,4]
dyadData$matlConfL = melt(X[,,'matlConfij',])[,4]

# Drop i - i observations
dyadData = dyadData[which(dyadData$source != dyadData$target),]
####

####
# Run dyadic model
forms = list(
	formula('verbCoop ~ verbCoopL + matlCoopL'),
	formula('matlCoop ~ verbCoopL + matlCoopL'),
	formula('verbConf ~ verbConfL + matlConfL'),
	formula('matlConf ~ verbConfL + matlConfL') )

mods = lapply(forms, function(form) mod = lm(form, data=dyadData) )
rmses = lapply(mods, function(mod) sqrt( mean( resid(mod)^2 ) ) )

# Org resids by data
dats = lapply(mods, function(mod){
	data.frame( dyadData[,c('source','target','date')], resid=resid(mod) ) })
# Calc rmse by time series
dats = lapply(dats, function(dat){ 
	dat$id = paste(dat$source, dat$target, sep='_'); return(dat) })
rmseTime = lapply(dats, function(dat){
	rmse = tapply(dat$resid, dat$id, FUN=function(x){ sqrt(mean(x^2)) } )
	source = unlist(lapply(strsplit(names(rmse),'_'),function(x) x[1]))
	target = unlist(lapply(strsplit(names(rmse),'_'),function(x) x[2]))
	return( data.frame(source, target, rmse, row.names=NULL) )
	})

# Combine for gg
ggData = do.call('rbind', rmseTime)
cnt = ( length(cntries)*length(cntries) ) - length(cntries)
ggData$var = c( 
	rep('Verbal Cooperation', cnt), 
	rep('Material Cooperation', cnt), 
	rep('Verbal Conflict', cnt), 
	rep('Material Conflict', cnt)
	)

# Subset
toPlot = c('Verbal Conflict', 'Material Conflict')
ggData = ggData[which(ggData$var %in% toPlot ),]
ggData$var = factor(ggData$var, levels=toPlot)

ggIPerf=ggplot(ggData, aes(x=source, y=target, fill=rmse)) 
ggIPerf=ggIPerf + xlab('') + ylab('')
ggIPerf=ggIPerf + geom_tile(colour='darkgrey')
ggIPerf=ggIPerf + scale_fill_gradient2(name='', low='white', high='red')
ggIPerf=ggIPerf + scale_x_discrete(expand=c(0,0)) + scale_y_discrete(expand=c(0,0))
ggIPerf=ggIPerf + facet_wrap(~var)
ggIPerf=ggIPerf + theme(
	axis.ticks=element_blank(), 
	legend.position='top', legend.key.width=unit(2,'cm'),
	panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	axis.text.x = element_text(angle=45, hjust=1, size=4),
	axis.text.y = element_text(size=4)
	)
ggIPerf
####
