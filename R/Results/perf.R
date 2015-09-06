if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
<<<<<<< HEAD
	source('~/Research/WardProjects/tensorZ/R/setup.R') }

############################
# data
load(paste0(inPath, "YX.rda"))

# results
load(paste0(outPath, 'tensorTradeConf'))

# Change outpath to presentation
outPath = '~/Research/WardProjects/tensorZ/Presentation/Graphics/'
############################

############################
# Burn
burn = 1200

# Add labels
cntries=dimnames(Y)[[1]]
dvs = dimnames(Y)[[3]]
ivs = dimnames(X)[[3]]

# Add labels to posterior object
dimnames(BPS[[1]])[[1]] = cntries
dimnames(BPS[[1]])[[2]] = cntries
dimnames(BPS[[2]])[[1]] = cntries
dimnames(BPS[[2]])[[2]] = cntries

dimnames(BPS[[3]])[[1]] = dvs
dimnames(BPS[[3]])[[2]] = ivs
=======
	source('~/Research/WardProjects/tensor/R/setup.R') }
if( Sys.info()['user']=='mw160' ){ source('~/git/tensor/R/setup.R') }

############################
# data
load(paste0(inPath, "YXsm.rda"))
dnX = dimnames(X)
X = aperm( apply(X,c(1,2,5),"c")  ,c(2,3,1,4))
dimnames(X)[1:2] = dnX[1:2]
dimnames(X)[[3]] =  c(outer( dnX[[3]],dnX[[4]],paste0) )

# Create list of data
yL = list( Y[,,1:2,], Y[,,3:4,] )
xL = list( X[,,c(1:2,5:6,9:10),], X[,,c(3:4,7:8,11:12),] )
############################

############################
# Load mcmc mult results
pds = c('Coop', 'Conf')
BPS = lapply(pds, function(pd){
	load( paste0(outPath, 'tensorMultModel', pd) ) 
	return(BPS) } )
names(BPS) = pds

# Add labels
cntries=rownames(Y[,,1,1])
dvs = list( c('verbCoop', 'matlCoop'), c('verbConf', 'matlConf') )
ivs = lapply(dvs, function(x) {
	as.vector(outer(x, c('_ij', '_ji', '_ijk'), paste0)) } )

for(ii in 1:length(BPS)){
	# Label UV
	dimnames(BPS[[ii]][[1]])[[1]]=cntries
	dimnames(BPS[[ii]][[1]])[[2]]=cntries
	dimnames(BPS[[ii]][[2]])[[1]]=cntries
	dimnames(BPS[[ii]][[2]])[[2]]=cntries
	# Add labels to coef matrix
	dimnames( BPS[[ii]][[3]] )[[1]] = dvs[[ii]]
	dimnames( BPS[[ii]][[3]] )[[2]] = ivs[[ii]]
	# Add ID label
	BPS[[ii]][[4]] = pds[ii]
}

mapping = rbind(
	c('verbCoop', 'Verbal Cooperation', 'Verb.~Coop.'),
	c('matlCoop', 'Material Cooperation', 'Matl.~Coop.'),
	c('verbConf', 'Verbal Conflict', 'Verb.~Conf.'),
	c('matlConf', 'Material Conflict', 'Matl.~Conf.') )

makeLabel = function(x, long=TRUE){
	if(long){ col = 2 } else { col = 3 }
	x = as.character(x)
	ids = unique(x)
	for(id in ids){
		x = gsub(strsplit(id, '[', fixed=TRUE)[[1]][1], 
				mapping[match(strsplit(id, '[', fixed=TRUE)[[1]][1], mapping[,1]),col], 
				x, fixed=TRUE) }
	return( factor( x ) )
}
############################

############################
# outPath='~/Research/WardProjects/tensor/Text/Graphics/'
outPath='~/Desktop/'
>>>>>>> origin/master
############################

############################
# Calculate posterior means for B
<<<<<<< HEAD
getPost = function(beta, toBurn=-(1:burn)){
	b1 = apply(beta[[1]][,,toBurn], c(1:2), mean)
	b2 = apply(beta[[2]][,,toBurn], c(1:2), mean)
	b3 = apply(beta[[3]][,,toBurn], c(1:2), mean)
=======
getPost = function(beta, pd, burn=-(1:1000)){
	b1 = apply(beta[[pd]][[1]][,,burn], c(1:2), mean)
	b2 = apply(beta[[pd]][[2]][,,burn], c(1:2), mean)
	b3 = apply(beta[[pd]][[3]][,,burn], c(1:2), mean)
>>>>>>> origin/master
	return( list(b1, b2, b3) )
}

# Calculate predicted values
<<<<<<< HEAD
pred = tprod(X, getPost(BPS))
for(var in 1:dim(pred)[3] ){
	for(yr in 1:dim(pred)[4] ){
		diag(pred[,,var,yr]) = 0 } }
=======
predVals = lapply(1:length(pds), function(pd){
	pred = tprod( xL[[pd]], getPost( BPS, pd ) )
	for(var in 1:dim(pred)[3] ){
		for(yr in 1:dim(pred)[4] ){
			diag(pred[,,var,yr]) = 0 } }
	return( pred ) })
>>>>>>> origin/master

# Aggregate performance
rsq = function(YP, Y){ 1-apply((YP-Y)^2,3,mean)/apply(Y^2,3,mean) }
rmse = function(YP, Y){ apply((YP-Y)^2, 3, function(x){ sqrt(mean(x)) }) }
<<<<<<< HEAD
rmsePerf = rmse(pred, Y )
rsqPerf = rsq(pred, Y)
aggPerf = cbind(rmsePerf, rsqPerf)
colnames(aggPerf) = c('RMSE', 'R$^{2}$')
rownames(aggPerf) = c('Log(Exports)', 'Std(Matl. Conf.)')
aggPerf = round(aggPerf, 2)

print.xtable(xtable(aggPerf), 
	file=paste0(outPath, 'rsq.tex'),
	include.rownames=TRUE, 
	sanitize.text.function=identity
	)

# Indiv perf, calc: RMSE by i-j for var
indivPerf = apply((pred-Y)^2, 1:3, function(x){ sqrt( mean( x ) ) })
=======
aggPerf = lapply(1:length(pds), function(pd){
	return( rmse(predVals[[pd]], yL[[pd]] ) ) })
aggPerf = do.call('rbind', aggPerf)
rownames(aggPerf) = pds
aggPerf
# print.xtable(xtable(aggPerf), file=paste0(outPath, 'rsq.tex'))

# Indiv perf, calc: RMSE by i-j for var
indivPerf = lapply(1:length(pds), function(pd){
	yp=predVals[[pd]]; y=yL[[pd]]
	apply((yp-y)^2, 1:3, function(x){ sqrt( mean( x ) ) }) } )
>>>>>>> origin/master
############################

############################
# Tile maps
<<<<<<< HEAD
perf = indivPerf
ggData = NULL
for(var in 1:dim(perf)[3]){
	pull = perf[,,var]
	meltPull = melt( pull )
	meltPull = cbind(var = dimnames(perf)[[3]][var], meltPull)
	ggData = rbind(ggData, meltPull)
}

# Add labels for DV
ggData$var = char(ggData$var)
ggData$var[ggData$var=='exports'] = 'Log(Exports)'
ggData$var[ggData$var=='matlConf'] = 'Std(Matl. Conf.)'
ggData$var = factor(ggData$var, levels=unique(ggData$var))

############################
# Save for use in perfCompare.R
fname = paste0('~/Dropbox/Research/WardProjects/tensorZ/Data/fromModel/rmseMLTR.rda')
perfMltr = ggData
perfMltr = perfMltr[perfMltr$Var1 != perfMltr$Var2,]
names(perfMltr) = c('var', 'source', 'target', 'rmse')
perfMltr = perfMltr[,c('source', 'target', 'rmse', 'var')]
save(perfMltr, file=fname)
############################

# Replace country name with ISO abbreviation
cntry = c(char(ggData$Var1), char(ggData$Var2)) %>% unique() %>% data.frame()
names(cntry) = 'cname'; cntry$iso = countrycode(cntry$cname, 'country.name', 'iso3c')
ggData$Var1 = cntry$iso[match(ggData$Var1, cntry$cname)]
ggData$Var2 = cntry$iso[match(ggData$Var2, cntry$cname)]

tilePerf = function(dv){
	# Subset to relev var
	ggData = ggData[ggData$var==dv,]
	# Add cat indicator for RMSE
	ggData$valueCat = ggData$value %>% cut(., 
		breaks=quantile(., probs=seq(0,1,100/9/100)), 
		include.lowest=TRUE, ordered_result=TRUE, dig.lab=1)
	# Plot
	ggIPerf=ggplot(ggData, aes(x=Var1, y=Var2, fill=valueCat)) 
	ggIPerf=ggIPerf + xlab('') + ylab('')
	ggIPerf=ggIPerf + geom_tile(colour='lightgrey')
	# ggIPerf=ggIPerf + scale_fill_gradient2(name='', low='white', high='red')
	ggIPerf=ggIPerf + scale_fill_brewer(name='', palette='Reds')	
=======
lapply(1:length(pds), function(pd){
	perf = indivPerf[[pd]]
	ggData = NULL
	for(var in 1:dim(perf)[3]){
		pull = perf[,,var]
		meltPull = melt( pull )
		meltPull = cbind(var = dimnames(perf)[[3]][var], meltPull)
		ggData = rbind(ggData, meltPull)
	}
	ggData$var = makeLabel(ggData$var)
	ggData$var = factor(ggData$var, levels=unique(ggData$var))

	ggIPerf=ggplot(ggData, aes(x=Var1, y=Var2, fill=value)) 
	ggIPerf=ggIPerf + xlab('') + ylab('')
	ggIPerf=ggIPerf + geom_tile(colour='darkgrey')
	ggIPerf=ggIPerf + scale_fill_gradient2(name='', low='white', high='red')
>>>>>>> origin/master
	ggIPerf=ggIPerf + scale_x_discrete(expand=c(0,0)) + scale_y_discrete(expand=c(0,0))
	ggIPerf=ggIPerf + facet_wrap(~var)
	ggIPerf=ggIPerf + theme(
		axis.ticks=element_blank(), 
<<<<<<< HEAD
		legend.position='right', legend.key.width=unit(.1,'cm'), legend.key.height=unit(1,'cm'),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
		axis.text.x = element_text(angle=45, hjust=1, size=2),
		axis.text.y = element_text(size=2)
		)
	return(ggIPerf)
}

ePerf=tilePerf('Log(Exports)')
fname=paste0(outPath, 'expiperf.pdf')	
ggsave(filename=fname, plot=ePerf, width=7, height=5)

mPerf=tilePerf('Std(Matl. Conf.)')
fname=paste0(outPath, 'mconfiperf.pdf')	
ggsave(filename=fname, plot=mPerf, width=7, height=5)
=======
		legend.position='top', legend.key.width=unit(2,'cm'),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
		axis.text.x = element_text(angle=45, hjust=1, size=4),
		axis.text.y = element_text(size=4)
		)
	fname=paste0(outPath, pds[pd], '_iperf.pdf')	
	ggIPerf
	ggsave(filename=fname, plot=ggIPerf, width=8, height=5)
} )
>>>>>>> origin/master
############################