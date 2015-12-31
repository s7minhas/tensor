if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensor/R/setup.R') }
if( Sys.info()['user']=='mw160' ){ source('~/git/tensor/R/setup.R') }

############################
# data
load(paste0(inPath, "YXsm.rda"))
############################

############################
# Load mcmc mult results
pds = c('Coop', 'Conf')
BPS = lapply(pds, function(pd){
	load( paste0(outPath, 'tensorMultModel', pd) ) 
	return(BPS) } )
names(BPS) = pds

# Set burn in
burn = 1001

# Add labels
cntries=rownames(Y[,,1,1])
dvs = list( 
	c('verbCoop', 'matlCoop'), 
	c('verbConf', 'matlConf') 
	)
ivs = lapply(dvs, function(x) {
	as.vector(outer(x, c('[ij~t-1]', '[ji~t-1]', '[ijk~t-1]'), paste0)) } )

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
outPath='~/Research/WardProjects/tensor/Text/Graphics/'
outPath='~/Research/WardProjects/tensor/Text/epsGraphics/'
############################

############################
# Trace plots by row
lapply(BPS, function(beta){
	dvs=dim(beta[[3]])[1]
	traceData = NULL
	for(ii in 1:dvs){
		tbeta = data.frame( t(beta[[3]][ii,,]) )
		tbeta = cbind(dv=dimnames(beta[[3]])[[1]][ii], tbeta, Iteration=1:nrow(tbeta))
		traceData = rbind(traceData, tbeta)
	}
	traceData=traceData[traceData$Iteration %% 5 == 0,] # thin
	traceData = melt(traceData, id=c('dv', 'Iteration'))

	traceData$dv = makeLabel(traceData$dv, long=TRUE)

	ggTrace=ggplot(traceData, aes(x=Iteration, y=value, color=variable))
	ggTrace=ggTrace + geom_line() + facet_wrap(~dv)
	ggTrace=ggTrace + xlab('') + ylab('')
	ggTrace=ggTrace + scale_color_brewer(name='', type='qual')
	ggTrace=ggTrace + theme(
		axis.ticks=element_blank(),
		legend.position='none',
		panel.background=element_blank()
		)
	# fname=paste0(outPath, beta[[4]], '_trace.pdf')	
	fname=paste0(outPath, beta[[4]], '_trace.eps')	
	print( ggTrace )
	ggsave(filename=fname, plot=ggTrace, width=6, height=4)
	} )
############################

############################
# Summary of beta[3] Posterior distributions
summStats = function(x){
	mu=mean(x)
	qts=quantile(x, probs=c(0.025,0.975,0.05,0.95))
	return( c(mu, qts) )
}

# Generate data for posterior distributions
lapply(BPS, function(beta){
	tmp=beta[[3]]
	tmp=tmp[,,burn:dim(tmp)[3]] # Burn 1k
	# Assemble data for coef
	coefData=NULL
	for(ii in 1:dim(tmp)[1]){
		mod=t(tmp[ii,,])
		modSumm=apply(mod, 2, summStats)
		rownames(modSumm)=c('mu', paste0(c('lo','up'),95), paste0(c('lo','up'),90))
		coefSlice = data.frame(iv=rownames(t(modSumm)), t(modSumm), row.names=NULL)
		coefSlice = cbind(dv = dimnames(tmp)[[1]][ii], coefSlice)
		coefData = rbind(coefData, coefSlice) }

	# Add in variable for colors
	coefData$sig = NULL
	coefData$sig[coefData$lo90 > 0 & coefData$lo95 < 0] = "Positive at 90"
	coefData$sig[coefData$lo95 > 0] = "Positive"
	coefData$sig[coefData$up90 < 0 & coefData$up95 > 0] = "Negative at 90"
	coefData$sig[coefData$up95 < 0] = "Negative"
	coefData$sig[coefData$lo90 < 0 & coefData$up90 > 0] = "Insig"
	coefp_colors = c("Positive"=rgb(54, 144, 192, maxColorValue=255), 
	                "Negative"= rgb(222, 45, 38, maxColorValue=255),
	                "Positive at 90"=rgb(158, 202, 225, maxColorValue=255), 
	                "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
	                "Insig" = rgb(150, 150, 150, maxColorValue=255))

	# Map variables to labels (very lazy way)
	coefData$dv = makeLabel(coefData$dv, long=TRUE) 
	coefData$dv = factor(coefData$dv, levels=unique(coefData$dv))
	coefData$iv = makeLabel(coefData$iv, long=FALSE) 
	coefData$iv = factor(coefData$iv, levels=unique(coefData$iv))

	# Plot
	# coefData$iv = factor(coefData$iv, levels=dimnames(beta[[3]])[[2]])
	ggCoef=ggplot(coefData, aes(x=iv, y=mu, color=sig))
	ggCoef=ggCoef + geom_point() + facet_wrap(~dv, nrow=2, scales = 'free_y')
	ggCoef = ggCoef + geom_linerange(aes(ymin=lo95, ymax=up95), alpha = .3, size = 0.3)
	ggCoef = ggCoef + geom_linerange(aes(ymin=lo90, ymax=up90),alpha = 1, size = 1)
	ggCoef = ggCoef + geom_errorbar(aes(ymin=lo95,ymax=up95),linetype = 1,width = 0.1)	
	ggCoef=ggCoef + geom_hline(yintercept=0, color='red', linetype='dashed')
	ggCoef=ggCoef + geom_vline(xintercept=c(2.5, 4.5), color='grey', linetype='dashed')
	ggCoef=ggCoef + ylab('') + scale_colour_manual(values = coefp_colors)
	ggCoef=ggCoef + scale_x_discrete("", labels = parse(text = levels(coefData$iv)))
	ggCoef=ggCoef + theme(
		axis.ticks=element_blank(),
		axis.text.x = element_text(angle=45, hjust=1),
		panel.background = element_blank(),
		legend.position='none',
		panel.grid.major=element_blank(),
		panel.grid.minor=element_blank()
		)
	# fname=paste0(outPath, beta[[4]], '_coef.pdf')		
	fname=paste0(outPath, beta[[4]], '_coef.eps')		
	ggCoef
	ggsave(filename=fname, plot=ggCoef, width=6, height=6)
	})
############################

############################
# Network plots
loadPkg('shape')
source(paste0(rFuncs, "rda.r") )
genCntryMap=FALSE; source(paste0(rFuncs, "genColors.r") )
proc_rr=function(Y,X){
	k=dim(X)[2]
	A=t(Y)%*%(X%*%t(X))%*%Y
	eA=eigen(A,symmetric=T)
	Ahalf=eA$vec[,1:k]%*%diag(sqrt(eA$val[1:k]),nrow=k)%*%t(eA$vec[,1:k])
	t(t(X)%*%Y%*%solve(Ahalf)%*%t(Y)) }

alpha = .01
set.seed(6886) 
beta = BPS[[1]]
B = beta[[1]]
LB = apply( B[,,burn:dim(B)[3]], c(1,2), quantile, prob=alpha )
UB = apply( B[,,burn:dim(B)[3]], c(1,2), quantile, prob=1-alpha )
BSIG = 1*( LB*UB >0 )
BPOS = 1*( LB>0 ) 

loadPkg('igraph')
# Laziness
tcols = rep('white', length(cntries))
tcols[which(
	cntries %in% 
		c('RUS', 'NOR', 'FIN', 'SWE', 'CAN', 'DNK')
		)]='black'

lapply(BPS, function(beta){
	# fname=paste0(outPath, beta[[4]], '_net.pdf')	
	# pdf(file=fname, width=12, height=5)
	fname=paste0(outPath, beta[[4]], '_net.eps')	
	postscript(file=fname, width=12, height=5, fonts=c("serif", "Palatino"), horizontal=FALSE, onefile = FALSE, paper = "special")	
	par(mfrow=c(1,2), mar=c(1,1,1,1), mgp=c(1.5,.5,0))		
	B = beta[[1]]
	LB = apply( B[,,burn:dim(B)[3]], c(1,2), quantile, prob=alpha )
	UB = apply( B[,,burn:dim(B)[3]], c(1,2), quantile, prob=1-alpha )
	BSIG = 1*( LB*UB >0 )
	BPOS = 1*( LB>0 ) 
	rownames(BPOS)=colnames(BPOS)=dimnames(beta[[1]])[[1]]
	g = graph.adjacency(BPOS, mode='directed', diag=FALSE)
	set.seed(6886)
	plot.igraph(g, 
		layout=layout.auto,
		vertex.label.color=tcols, 
		vertex.color=ccols, 
		vertex.label.cex=(igraph::degree(g)+3.5)/19,
		vertex.size=igraph::degree(g)+3.5,
		edge.arrow.size=0.4,
		asp=FALSE
		)

	B = beta[[2]]
	LB = apply(B,c(1,2),quantile,prob=alpha)
	UB = apply(B,c(1,2),quantile,prob=1-alpha)
	BSIG = 1*( LB*UB >0 )
	BPOS = 1*(LB>0)
	rownames(BPOS)=colnames(BPOS)=dimnames(beta[[1]])[[1]]
	g = graph.adjacency(BPOS, mode='directed', diag=FALSE)
	set.seed(6886)
	plot.igraph(g, 
		layout=layout.auto,
		vertex.label.color=tcols, 
		vertex.color=ccols, 
		vertex.label.cex=(igraph::degree(g)+3.5)/19,
		vertex.size=igraph::degree(g)+3.5,
		edge.arrow.size=0.4,
		asp=FALSE
		)
	dev.off()
	})

# Other checks
b = BPS[[2]][[1]]
bMu = apply( b[,,burn:dim(b)[3]], c(1,2), mean )
bDiag = diag(bMu)
bOffDiag = bMu
diag(bOffDiag) = NA
mean(abs(bDiag))/mean(abs(as.vector(bOffDiag)), na.rm=TRUE)


b = BPS[[2]][[2]]
bMu = apply( b[,,burn:dim(b)[3]], c(1,2), mean )
bDiag = diag(bMu)
bOffDiag = bMu
diag(bOffDiag) = NA
mean(abs(bDiag))/mean(abs(as.vector(bOffDiag)), na.rm=TRUE)
############################

############################
# Slope graphs

# Params
beta = BPS[[1]]
B12 = list(beta[[1]], beta[[2]])

# Aggregate over iterations
mapB12 = lapply(B12, function(B){
	alpha = 0.01
	Bmean = apply(B[,,burn:dim(B)[3]], c(1,2), mean)
	Bsig = apply(B[,,burn:dim(B)[3]], c(1,2), function(x){ 
		lo=quantile(x, prob=alpha)
		hi=quantile(x, prob=1-alpha)
		if(lo * hi > 0){ 1 } else { 0 } } )
	diag(Bsig) = 0
	Bfin = Bmean * Bsig	
	return( Bfin )
} )

# Set up data
map = mapB12[[1]]
slopeData = melt(map)

# Organize country points
tmp=ggplot(slopeData, aes(color=Var1)) 
tmp=tmp + geom_point(aes(x=0, y=Var1), size=4)
tmp=tmp + geom_point(aes(x=6, y=Var2, color=Var2), size=4)
tmp=tmp + geom_segment(
	data=slopeData[slopeData$value!=0,],
	aes(x=0, xend=6, y=Var1, yend=Var2)
	# size=slopeData[slopeData$value!=0,]$value*6
	)
# tmp=tmp + geom_text(
# 	aes(x=6, y=Var2, label=Var2), 
# 	color='black', size=3, hjust=-.5
# 	)
tmp=tmp + xlab('') + ylab('')
tmp=tmp + theme(
	legend.position='none',
	panel.grid.major=element_blank(),
	panel.grid.minor=element_blank(),
	panel.border=element_blank(),
	axis.text.x=element_blank(),
	axis.ticks=element_blank()
	)
tmp
############################