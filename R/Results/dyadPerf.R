if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
<<<<<<< HEAD
	source('~/Research/WardProjects/tensorZ/R/setup.R') }

############################
# data
load(paste0(inPath, "YX.rda"))

# Change outpath to presentation
outPath = '~/Research/WardProjects/tensorZ/Presentation/Graphics/'
############################

############################
# Add labels
cntries=dimnames(Y)[[1]]
dvs = dimnames(Y)[[3]]
ivs = dimnames(X)[[3]]
############################

############################
# Recreate dyadic frames for data
dyadData = melt(Y[,,'exports',])
names(dyadData) = c('source','target','date','exports')
dyadData$matlConf = melt(Y[,,'matlConf',])[,4]

# Add in independent variables
for(iv in ivs){
	dyadData$tmp = melt(X[,,iv,])[,4]
	names(dyadData)[ncol(dyadData)]=iv }

# Drop i - i observations
dyadData = dyadData[which(dyadData$source != dyadData$target),]
############################

############################
# Run dyadic model
forms = lapply(dvs, function(dv){
	formula( paste0( dv, ' ~ ', paste(ivs, collapse=' + ') ) ) })

mods = lapply(forms, function(form) mod = lm(form, data=dyadData) )

# Save mod results for coefCompare.R
ddCoefData = lapply(mods, function(x){ summary(x)$'coefficients' })
save(ddCoefData, file='~/Dropbox/Research/WardProjects/tensorZ/Data/fromModel/ddCoef.rda')

# Model results
loadPkg('apsrtable')
fname = file(paste0(outPath, 'dyadcoef.tex'))
writeLines( apsrtable(
	mods[[1]], mods[[2]], 
	model.names=c('Log(Exports)', 'Std(Matl. Conf.)')
	) , fname)
close(fname)

# Aggregate performance
rmses = lapply(mods, function(mod) sqrt( mean( resid(mod)^2 ) ) ) %>% unlist()
rsq = lapply(mods, function(mod) summary(mod)$'r.squared' ) %>% unlist()

aggPerf = cbind(rmses, rsq)
colnames(aggPerf) = c('RMSE', 'R$^{2}$')
rownames(aggPerf) = c('Log(Exports)', 'Std(Matl. Conf.)')
aggPerf = round(aggPerf, 2)

print.xtable(xtable(aggPerf), 
	file=paste0(outPath, 'dyadrsq.tex'),
	include.rownames=TRUE, 
	sanitize.text.function=identity
	)
############################

############################
=======
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

>>>>>>> origin/master
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
<<<<<<< HEAD
cnt = ( length(cntries)^2 ) - length(cntries)
ggData$var = c( 
	rep('Log(Exports)', cnt), 
	rep('Std(Matl. Conf.)', cnt)
	)

# Subset
toPlot = c('Log(Exports)', 'Std(Matl. Conf.)')
ggData = ggData[which(ggData$var %in% toPlot ),]
ggData$var = factor(ggData$var, levels=toPlot)

############################
# Save for use in perfCompare.R
fname = paste0('~/Dropbox/Research/WardProjects/tensorZ/Data/fromModel/rmseDyad.rda')
perfDyad = ggData
save(perfDyad, file=fname)
############################

# Replace country name with ISO abbreviation
cntry = c(char(ggData$source), char(ggData$target)) %>% unique() %>% data.frame()
names(cntry) = 'cname'; cntry$iso = countrycode(cntry$cname, 'country.name', 'iso3c')
ggData$source = cntry$iso[match(ggData$source, cntry$cname)]
ggData$target = cntry$iso[match(ggData$target, cntry$cname)]

tilePerf = function(dv){
	# Subset to relev var
	ggData = ggData[ggData$var==dv,]
	# Add cat indicator for RMSE
	ggData$valueCat = ggData$rmse %>% cut(., 
		breaks=quantile(., probs=seq(0,1,100/9/100)), 
		include.lowest=TRUE, ordered_result=TRUE, dig.lab=1)
	# Plot	
	ggIPerf=ggplot(ggData, aes(x=source, y=target, fill=valueCat)) 
	ggIPerf=ggIPerf + xlab('') + ylab('')
	ggIPerf=ggIPerf + geom_tile(colour='darkgrey')
	# ggIPerf=ggIPerf + scale_fill_gradient2(name='', low='white', high='red')
	ggIPerf=ggIPerf + scale_fill_brewer(name='', palette='Reds')	
	ggIPerf=ggIPerf + scale_x_discrete(expand=c(0,0)) + scale_y_discrete(expand=c(0,0))
	ggIPerf=ggIPerf + facet_wrap(~var)
	ggIPerf=ggIPerf + theme(
		axis.ticks=element_blank(), 
		legend.position='right', legend.key.width=unit(.1,'cm'), legend.key.height=unit(1,'cm'),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
		axis.text.x = element_text(angle=45, hjust=1, size=2),
		axis.text.y = element_text(size=2)
		)
	return(ggIPerf)
}

ePerf=tilePerf('Log(Exports)')
fname=paste0(outPath, 'dyadexpiperf.pdf')	
ggsave(filename=fname, plot=ePerf, width=7, height=5)

mPerf=tilePerf('Std(Matl. Conf.)')
fname=paste0(outPath, 'dyadmconfiperf.pdf')	
ggsave(filename=fname, plot=mPerf, width=7, height=5)
############################
=======
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
>>>>>>> origin/master
