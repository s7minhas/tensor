if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensor/R/setup.R') }

if( Sys.info()['user']=='mw160' ){ source('~/git/tensor/R/setup.R') }

####
# Load raw DV
load(paste0(inPath, 'rawDV.rda'))
outPath='~/Research/WardProjects/tensor/Text/Graphics/'
####

####
# Descriptive chart to look at temporal trends

# Aggregate to monthly level
counts=do.call('cbind', lapply(quadList, function(quad){
	vec=tapply(quad[,5], quad[,6], sum)
} ) )
ggData=data.frame(time=as.Date(rownames(counts)), counts, row.names=NULL)
ggData=melt(ggData, id='time')
tmp=ggplot(ggData, aes(x=time, y=value, color=variable))
tmp=tmp + geom_line() + facet_wrap(~variable, scales='free_y')
tmp + xlab('') + ylab('')

# Post 2001
ggData = ggData[which(
	ggData$time >= as.Date('2001-01-01') & 
	ggData$time < as.Date('2015-01-01')
	),]

# Add labels
ggData$variable = as.character( ggData$variable )
ggData$variable[ggData$variable=='verbCoop'] = 'Verbal Cooperation'
ggData$variable[ggData$variable=='verbConf'] = 'Verbal Conflict'
ggData$variable[ggData$variable=='matlCoop'] = 'Material Cooperation'
ggData$variable[ggData$variable=='matlConf'] = 'Material Conflict'
ggData$variable = factor(ggData$variable, 
	levels=c(
		'Verbal Cooperation', 'Material Cooperation',
		'Verbal Conflict', 'Material Conflict'
		) )

ggDV=ggplot(ggData, aes(x=time, y=value))
ggDV=ggDV + geom_line() + facet_wrap(~variable, scales='free_y')
ggDV=ggDV + xlab('') + ylab('')
ggDV=ggDV + theme(
	legend.position='none',
	panel.background=element_blank(),
	axis.ticks=element_blank(),
	panel.grid.major=element_blank(),
	panel.grid.minor=element_blank()
	)
ggDV
fname=paste0(outPath, 'dvMonthly.pdf')	
ggsave(filename=fname, plot=ggDV, width=14, height=6)
####

####
# Show relations between particular countries
cntries=c('USA', 'CHN')
ggData = do.call('rbind', lapply(quadList, function(quad){
	slice = quad[ which(
		quad$source_country %in% cntries & quad$target_country %in% cntries &
		quad$date >= as.Date('2001-01-01') & quad$date < as.Date('2015-01-01')
		), ]
	slice$var = names(slice)[5]
	names(slice)[5] = 'value'
	rownames(slice) = NULL
	return( slice )
	}) )

# Add labels
ggData$var = as.character( ggData$var )
ggData$var[ggData$var=='verb_coop'] = 'Verbal Cooperation'
ggData$var[ggData$var=='verb_conf'] = 'Verbal Conflict'
ggData$var[ggData$var=='matl_coop'] = 'Material Cooperation'
ggData$var[ggData$var=='matl_conf'] = 'Material Conflict'
ggData$var = factor(ggData$var, 
	levels=c(
		'Verbal Cooperation', 'Material Cooperation',
		'Verbal Conflict', 'Material Conflict'
		) )

ggData$id = paste(ggData$source_country, ggData$target_country, sep=' to ')
ggData$id = as.character( ggData$id )
ggData$id[ggData$id=='CHN to USA'] = 'China to United States'
ggData$id[ggData$id=='USA to CHN'] = 'United States to China'
ggData$id = factor(ggData$id)

# Subset to just verbal variables
ggData = ggData[which(ggData$var %in% c('Verbal Cooperation', 'Verbal Conflict')),]

ggDV=ggplot(ggData, aes(x=date, y=value, color=id))
ggDV=ggDV + geom_line() + facet_wrap(~var, scales='free_y')
ggDV=ggDV + xlab('') + ylab('')
ggDV=ggDV + theme(
	legend.position='top',
	legend.title=element_blank(),
	panel.background=element_blank(),
	axis.ticks=element_blank(),
	panel.grid.major=element_blank(),
	panel.grid.minor=element_blank()
	)
ggDV
fname=paste0(outPath, 'US_CHN_Monthly.pdf')	
ggsave(filename=fname, plot=ggDV, width=14, height=4)
####

####
source(paste0(rFuncs, 'genColors.R'))
quad=allQuad[,,1,1] # verbCoop, 1-1-2001
netQuad = as.network(quad, directed=TRUE)
set.edge.value(netQuad, "weight", quad)
fname=paste0(outPath, 'verbCoop_01012001.pdf')	
pdf(file=fname, width=8, height=5)
par(mar=c(1,1,1,1), mgp=c(1.5,.5,0))
plot.network(netQuad,
	displaylabels=TRUE, 
	mode = "fruchtermanreingold",
	usearrows=TRUE,
	edge.col='grey',
	vertex.col=ccols,
	edge.lwd=get.edge.value(netQuad, 'weight')/10
	)
dev.off()
####