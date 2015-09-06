if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
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
# Load mcmc mult results
# Set burn in
burn = 1500
BPS = lapply(BPS, function(x){ x[,,-(1:burn)] })

# Add labels
cntries=dimnames(Y)[[1]]
dvs = dimnames(Y)[[3]]
ivs = dimnames(X)[[3]]
pds = dimnames(X)[[4]]

# Add labels to posterior object
dimnames(BPS[[1]])[[1]] = cntries
dimnames(BPS[[1]])[[2]] = cntries
dimnames(BPS[[2]])[[1]] = cntries
dimnames(BPS[[2]])[[2]] = cntries

dimnames(BPS[[3]])[[1]] = dvs
dimnames(BPS[[3]])[[2]] = ivs

# Covar names
covars = data.frame(iv=ivs)
covars$ivClean[covars$iv=='exp_ij'] = 'Log(Exports)$_{ij, t-1}$'
covars$ivClean[covars$iv=='exp_ji'] = 'Log(Exports)$_{ji, t-1}$'
covars$ivClean[covars$iv=='exp_ijk'] = 'Log(Exports)$_{ijk, t-1}$'
covars$ivClean[covars$iv=='mconf_ij'] = 'Std(Matl. Conf.)$_{ij, t-1}$'
covars$ivClean[covars$iv=='mconf_ji'] = 'Std(Matl. Conf.)$_{ji, t-1}$'
covars$ivClean[covars$iv=='mconf_ijk'] = 'Std(Matl. Conf.)$_{ijk, t-1}$'
covars$ivClean[covars$iv=='pta_ij'] = 'PTAs$_{ij, t-1}$'
covars$ivClean[covars$iv=='pta_ijk'] = 'PTAs$_{ijk, t-1}$'
covars$ivClean[covars$iv=='dist_ij'] = 'Distance$_{ij, t-1}$'
covars$ivClean[covars$iv=='pol_ij'] = 'Polity$_{i, t-1}$'
covars$ivClean[covars$iv=='gdp_ij'] = 'Log(GDP)$_{i, t-1}$'
covars$ivClean[covars$iv=='pop_ij'] = 'Log(Population)$_{i, t-1}$'
covars$ivClean[covars$iv=='wrldexp_ij'] = 'Log(Total~Exports)$_{i, t-1}$'

# Run substantive effects for
voi = 'pta_ij'
for(voi in covars$iv[1:9]){
############################

############################
# Marg effect of PTA vars
getMeanPost = function(beta){
	b1 = apply(beta[[1]], c(1:2), mean)
	b2 = apply(beta[[2]], c(1:2), mean)
	b3 = apply(beta[[3]], c(1:2), mean)
	return( list(b1, b2, b3) )
}

# Get scenario values
varDesc = matrix(NA, nrow=length(ivs), ncol=1, dimnames=list(ivs, c('directed')))
varDesc[,'directed'] = c(T, T, T, T, T, T, F, T, T, F, F, F, F)
scenVals = getScenVals(data=X, vars=ivs, time=pds, infoMat=varDesc)

# Organize into scenario array
# Parameters to determine size of scenario
scenX = getScenArray(varToVary=voi, valsForScen=scenVals, 
	dim12names=cntries, dim3names=ivs)
dimnames(scenX)[[4]] = scenVals[[voi]][[2]] %>% unique()

# Parallelize calculation of marginal effects
parrPacks = c('foreach', 'doParallel'); loadPkg(parrPacks)
cl = makeCluster(8); registerDoParallel(cl)
scenPreds <- foreach( ii = 1:dim(BPS[[1]])[3] ) %dopar% {
	# Pull out an iteration a time from BPS
	beta = lapply(BPS, function(b){ b[,,ii] })
	# Avg B1 and B2 across countries
	beta[1:2] = lapply(beta[1:2], function(b){
		mu = mean( as.vector( b ) )
		bMu = matrix(mu, nrow=nrow(beta[[1]]), ncol=ncol(beta[[1]]) )
		return(bMu) })
	# Calculate predicted values
	scenP = tprod(scenX, beta)
	# Set diagnoals to missing
	for(v in 1:dim(scenP)[3]){ for(z in 1:dim(scenP)[4]){ diag(scenP[,,v,z]) = NA } }
	# Add labels
	dimnames(scenP)[c(1:2,4)] = dimnames(scenX)[c(1:2,4)]
	dimnames(scenP)[[3]] = dvs
	return(scenP)
}; stopCluster(cl)

# Reorganize into usable dataframe for agg and plotting
scenPreds = scenPreds %>% 
	lapply(., function(p){ pmat = p[1,2,,] %>% melt(.) }) %>% 
	do.call('rbind', .)
scenPreds$iter = rep(1:dim(BPS[[1]])[3], each= (length(dvs) * dim(scenX)[4]) )
############################

############################
# Plot predictions with error

# Get mean and quantiles for iterations to plot
loadPkg('doBy')
q90 = function(x){ quantile(x, probs=c(0.95, 0.05)) }
q95 = function(x){ quantile(x, probs=c(0.975, 0.025)) }
ggData = summaryBy(value ~ Var1 + Var2, data=scenPreds, FUN=c(mean, q90, q95), keep.names=TRUE)
names(ggData) = c('dv', 'var', 'mu', 'q90Hi', 'q90Lo', 'q95Hi', 'q95Lo')

# Fix DV labels
ggData$dv = char(ggData$dv)
ggData$dv[ggData$dv=='exports'] = 'Log(Exports)'
ggData$dv[ggData$dv=='matlConf'] = 'Stdz(Matl. Conf.)'

# Plot
ggEff = ggplot(ggData, aes(x=var, y=mu, ymin=q95Lo, ymax=q95Hi))
ggEff = ggEff + geom_line() + geom_ribbon(alpha=0.3)
ggEff = ggEff + geom_ribbon(aes(ymin=q90Lo, ymax=q90Hi), alpha=0.5)
ggEff = ggEff + facet_wrap(~dv, nrow=2, scales='free_y')
ggEff = ggEff + xlab('') + ylab('')
ggEff = ggEff + theme(
	legend.position='none',
	panel.grid = element_blank(),
	axis.ticks=element_blank()
	)

# Add histogram
# Get voi data from X array
vOrig = X[,,voi,] 
for(ii in 1:dim(vOrig)[3]){
	diag(vOrig[,,ii]) = NA 
	if( !varDesc[voi,'directed'] ){
		vOrig[,,ii][ lower.tri(vOrig[,,ii]) ] = NA			
	}
}
vOrig = vOrig %>% melt(.) %>% na.omit(.)

# Add some coloring to designate interquartile and 90$ of distribution
tmp = density( vOrig$value )
vDense = data.frame( x=tmp$x, y=tmp$y )
vDense$qIQ = vDense$x >= quantile( vOrig$value, 0.25 ) & vDense$x <= quantile(vOrig$value, 0.75)
vDense$q90 = vDense$x >= quantile( vOrig$value, 0.05 ) & vDense$x <= quantile(vOrig$value, 0.90)

# Plot histogram
ggDense = ggplot() + xlab(covars$ivClean[covars$iv==voi]) + ylab('')
ggDense = ggDense + geom_line(data=vDense, aes(x=x,y=y))
ggDense = ggDense + geom_ribbon(data=subset(vDense,q90),
  aes(x=x,ymax=y),ymin=0,alpha=0.5)
ggDense = ggDense + geom_ribbon(data=subset(vDense,qIQ),
  aes(x=x,ymax=y),ymin=0,alpha=0.9)
ggDense = ggDense + theme(
	legend.position='none', 
	panel.grid = element_blank(), 
	axis.ticks = element_blank())

# Arrange and save plots
loadPkg('gridExtra')
fname=paste0(outPath, voi, '_effect.tex')
tikz(fname, width=4, height=7, standAlone = FALSE)
grid.arrange(ggEff, ggDense, ncol=1, nrow=2, heights=c(4,1))
dev.off()
############################
}