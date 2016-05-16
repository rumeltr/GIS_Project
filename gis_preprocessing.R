library("vegan")
library("plyr")
library("rgdal")

tompkins<-read.table("~/Downloads/ebd_US-NY-109_prv_relFeb-2016/ebd_US-NY-109_prv_relFeb-2016.txt",header = T,sep = "\t",quote = "",comment.char = "",stringsAsFactors = F)
tompkins_valid<-tompkins[tompkins$APPROVED==1 & tompkins$ALL.SPECIES.REPORTED == 1 & tompkins$CATEGORY=="species",]
tompkins_hotspots<-unique(tompkins[tompkins_valid$LOCALITY.TYPE=="H",c("LOCALITY","LATITUDE","LONGITUDE")])

tompkins_species<-unique(tompkins_valid$SCIENTIFIC.NAME)

tompkins_valid_uniqued<-unique(tompkins_valid[,c("SCIENTIFIC.NAME","OBSERVATION.COUNT","LOCALITY","SAMPLING.EVENT.IDENTIFIER")])

tompkins_valid_uniqued[tompkins_valid_uniqued$OBSERVATION.COUNT=="X","OBSERVATION.COUNT"]<-"0"
tompkins_valid_uniqued$OBSERVATION.COUNT<-as.numeric(tompkins_valid_uniqued$OBSERVATION.COUNT)

hotspot_species_accumulator<-lapply(tompkins_hotspots$LOCALITY,function(hotspot){
  print(hotspot)
  species_tick<-rep(tompkins_valid[tompkins_valid_uniqued$LOCALITY==hotspot,"SCIENTIFIC.NAME"],times=tompkins_valid_uniqued[tompkins_valid_uniqued$LOCALITY==hotspot,"OBSERVATION.COUNT"])
  flush.console()
  return(data.frame(HOTSPOT=rep(times=length(species_tick),x=hotspot),SPECIES=species_tick))
})

hotspot_species<-rbind.fill(hotspot_species_accumulator)

hotspot_species$HOTSPOT<-as.character(hotspot_species$HOTSPOT)
hotspot_species$SPECIES<-as.character(hotspot_species$SPECIES)

div_tab<-diversity(table(hotspot_species))
div<-data.frame(LOCALITY=names(div_tab),DIVERSITY=as.numeric(div_tab),stringsAsFactors = F)

tompkins_hotspots$DIVERSITY<-sapply(tompkins_hotspots$LOCALITY,function(x){
  as.character(div[div$LOCALITY==x,"DIVERSITY"])
})

tompkins_hotspots$DIVERSITY_NUMERIC<-as.numeric(tompkins_hotspots$DIVERSITY)
tompkins_hotspots<-tompkins_hotspots[,c("LOCALITY","LATITUDE","LONGITUDE","DIVERSITY_NUMERIC")]
colnames(tompkins_hotspots)<-c("LOCALITY","LATITUDE","LONGITUDE","DIVERSITY")

tompkins_hotspots<-tompkins_hotspots[!(is.na(tompkins_hotspots$DIVERSITY)),]
tompkins_hotspots<-tompkins_hotspots[order(tompkins_hotspots$DIVERSITY,decreasing = T),]

write.csv(tompkins_hotspots,"~/Desktop/tompkins_hotspots.csv")