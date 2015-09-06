#### country colors
#source('~/Research/WardProjects/tensor/setup.R')
cll = read.csv(paste0(inPath, "old/country_ll.csv"))
cll = cll[ is.element(as.character(cll[,1]),dimnames(Y)[[1]]) , ]
cll = cll[order(as.character(cll[,1])),]

rlon = pi*cll[,2]/180
rlat = pi*cll[,3]/180

slon =  (rlon-min(rlon))/(max(rlon)-min(rlon))
slat =  (rlat-min(rlat))/(max(rlat)-min(rlat))
ccols = rgb( slon^2,slat^2,(1-sqrt(slon*slat))^2)

cnames = dimnames(Y)[[1]]
####