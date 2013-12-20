ann.cv <-
function(flow.ts) {
	flow.ts$year<-strftime(flow.ts$Date,format="%Y")
ann.stats<-ddply(flow.ts,"year",summarize,mean=mean(Q,na.rm=T),sd=sd(Q,na.rm=T))

	out<-sd(ann.stats$mean, na.rm=T)/mean(ann.stats$mean,na.rm=T)*100

return(list(ann.cv=out))
}
