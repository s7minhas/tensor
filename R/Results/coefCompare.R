if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensorZ/R/setup.R') }

############################
# Load rmse by dyad for MLTR and normal dyadic model
load(paste0(outPath, 'mcmcCoef.rda')) # coefData
load(paste0(outPath, 'ddCoef.rda')) # ddCoefData

# wkrspc params
printPlot = FALSE

# Change outpath to presentation
outPath = '~/Research/WardProjects/tensorZ/Presentation/Graphics/'
############################

############################
# Clean up results from dir dyad model
ddCoefData = do.call('rbind', ddCoefData)
# Drop intercept
ddCoefData = ddCoefData[-which(rownames(ddCoefData) %in% '(Intercept)'), ]
vars = rownames(ddCoefData)
vars[vars=='exp_ij'] = 'Log(Exports)$_{ij, t-1}$'
vars[vars=='exp_ji'] = 'Log(Exports)$_{ji, t-1}$'
vars[vars=='exp_ijk'] = 'Log(Exports)$_{ijk, t-1}$'
vars[vars=='mconf_ij'] = 'Std(Matl. Conf.)$_{ij, t-1}$'
vars[vars=='mconf_ji'] = 'Std(Matl. Conf.)$_{ji, t-1}$'
vars[vars=='mconf_ijk'] = 'Std(Matl. Conf.)$_{ijk, t-1}$'
vars[vars=='pta_ij'] = 'PTAs$_{ij, t-1}$'
vars[vars=='pta_ijk'] = 'PTAs$_{ijk, t-1}$'
vars[vars=='dist_ij'] = 'Distance$_{ij, t-1}$'
vars[vars=='pol_ij'] = 'Polity$_{i, t-1}$'
vars[vars=='gdp_ij'] = 'Log(GDP)$_{i, t-1}$'
vars[vars=='pop_ij'] = 'Log(Population)$_{i, t-1}$'
vars[vars=='wrldexp_ij'] = 'Log(Total~Exports)$_{i, t-1}$'
dv = c( 
	rep('Log(Exports)', length(vars)/2),
	rep('Std(Matl. Conf.)', length(vars)/2) )
ddCoefData = ddCoefData %>% data.frame(.) %>% cbind(.,iv=vars,dv=dv)
############################

############################
# Quick tabular comparison of sig pos v sig neg results
# Create necessary vars for dd and subset
ddCoefData$sig = ifelse(ddCoefData$Pr...t.. <= 0.05,1,0)
ddCoefData$pos = ifelse(ddCoefData$Estimate >=0, 1, 0)
ddSigPos = ddCoefData[,c('dv', 'iv' ,'sig','pos')]
coefData$sig = ifelse(coefData$lo95*coefData$up95 > 0, 1, 0)
coefData$pos = ifelse(coefData$mu >= 0, 1, 0)
mltrSigPos = coefData[,c('dv','iv','sig', 'pos')]

# Combine and clean
sigPos = rbind(ddSigPos, mltrSigPos)
sigPos$mod = rep(c('dd','mltr'), each=nrow(ddSigPos))
sigPos$sign = ''
sigPos$sign[which(sigPos$sig==1 & sigPos$pos==1)] = '+'
sigPos$sign[which(sigPos$sig==1 & sigPos$pos==0)] = '-'

# Quick table
dcast(sigPos, iv~dv+mod, value.var='sign')
############################