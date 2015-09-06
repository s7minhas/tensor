if( Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m' ) { 
	source('~/Research/WardProjects/tensor/R/setup.R') 
}

if( Sys.info()['user']=='mw160' ){ source('~/git/tensor/R/setup.R') }

#### 
# Load ALM function
source(paste0(rFuncs, "functions_als.r"))
source(paste0(rFuncs, "functions_bayes.r"))
#### 

#### 
# data
load(paste0(inPath, "YXsm.rda"))

dnX = dimnames(X)
X = aperm( apply(X,c(1,2,5),"c")  ,c(2,3,1,4))
dimnames(X)[1:2] = dnX[1:2]
dimnames(X)[[3]] =  c(outer( dnX[[3]],dnX[[4]],paste0) )

# Restrict to coop measures
Y = Y[,,3:4,]
X = X[,,c(3:4,7:8,11:12),]
####

#### MCMC 
# mcmc function parameters
NS = 7500 ; NB = 500 ; sdens = 100 ; plot = FALSE
seed = 6886 ; rstart = FALSE
# output name
fname="tensorMultModelConf" 
setwd(outPath)
# run
source(paste0(rFuncs, "mcmc.r") )