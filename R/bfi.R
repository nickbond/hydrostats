bfi <-
function(flow.ts,a,ts="mean") {
   	
     full.flow.ts<-flow.ts[,c("Date","Q")]
     #names(full.flow.ts)<-c("Date","Q")
   	red.flow.ts<-full.flow.ts[complete.cases(full.flow.ts[,"Date"],full.flow.ts[,"Q"]),]
   	
  Date<-red.flow.ts[,"Date"]
   Q<-red.flow.ts[,"Q"]
   	
	
  	bf<-lh3(Q,a)
  	bfi<-ifelse(Q==0,0,bf/Q)
   	out<-data.frame(Date,bf,bfi)
  
  	out<-merge(full.flow.ts,out, by="Date",all.x=T)
  	names(out)<-c("Date","Q","bf","bfi")
   	
   	  if(ts=="daily") {
  return(out)
  }
  else {
 
if  (ts=="annual") {
  a.obs<-aggregate(full.flow.ts$Q,by=list(year=strftime(full.flow.ts$Date,format="%Y")),function(x) {sum(!is.na(x))})
  names(a.obs)<-c("year","obs")
 a.bf<-aggregate(out[2:4],by=list(year=strftime(out$Date,format="%Y")),mean,na.rm=T)
out<-merge(a.obs,a.bf,by="year",all.x=T)

  return(out)
   
}

 
if (ts=="mean") 	{
 	all.days<-nrow(full.flow.ts)
  obs<-nrow(red.flow.ts)
 prop.obs=obs/all.days
   
   	return(list(prop.obs=prop.obs, MDF=mean(out[,2],na.rm=T),Q50=median(out[,2],na.rm=T), mean.bf=mean(out[,3],na.rm=T),mean.bfi=mean(out[,3],na.rm=T)/mean(out[,2],na.rm=T)))

}
}
}
