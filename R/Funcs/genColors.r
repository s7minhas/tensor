####
<<<<<<< HEAD
if(!'Y' %in% ls()){ load(paste0(inPath, "YX.rda")) }
cntries=dimnames(Y)[[1]]
loadPkg('cshapes')
cmap = wmap = cshp(date=as.Date('2001-1-1'))
wmap@data$cname = countrycode(wmap@data$CNTRY_NAME, 'country.name', 'country.name')
wmap@data$cname[wmap@data$CNTRY_NAME=='Congo, DRC'] = "CONGO, THE DEMOCRATIC REPUBLIC OF"
wmap = wmap[which(as.character(wmap$cname) %in% cntries),]
coords=coordinates(wmap)
rownames(coords)=wmap$cname
=======
if(!'Y' %in% ls()){ load(paste0(inPath, "YXsm.rda")) }
cntries=dimnames(Y)[[1]]
loadPkg('cshapes')
cmap = wmap = cshp(date=as.Date('2001-1-1'))
wmap = wmap[which(as.character(wmap$ISO1AL3) %in% cntries),]
coords=coordinates(wmap)
rownames(coords)=wmap$ISO1AL3
>>>>>>> origin/master
coords=coords[cntries,]

# Create colors
rlon = pi*coords[,1]/180
rlat = pi*coords[,2]/180

slon =  (rlon-min(rlon))/(max(rlon)-min(rlon))
slat =  (rlat-min(rlat))/(max(rlat)-min(rlat))
ccols = rgb( slon^2,slat^2,(1-sqrt(slon*slat))^2)

# Generate legend map
if(genCntryMap){
<<<<<<< HEAD
	mapCol = ccols[match(wmap$cname, cntries)]
	mapCol[is.na(mapCol)] = 'grey'
	fname=paste0(outPath, 'map.pdf')	
	pdf(file=fname, width=8, height=4)
	plot(wmap, col=mapCol)
=======
	mapCol = ccols[match(cmap$ISO1AL3, cntries)]
	mapCol[is.na(mapCol)] = 'grey'
	fname=paste0(outPath, 'map.pdf')	
	pdf(file=fname, width=8, height=4)
	plot(cmap, col=mapCol)
>>>>>>> origin/master
	dev.off()
}