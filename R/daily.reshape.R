daily.reshape <-
function(flow.ts) {
		flow.ts$Date<-as.Date(flow.ts$Date)
		flow.ts<-data.frame(flow.ts,day=as.character(strftime(flow.ts$Date,format="%j")), month=strftime(flow.ts$Date,format="%b"), year=as.numeric(strftime(flow.ts$Date,format="%Y")))
		flow.ts$month<-factor(flow.ts$month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), ordered=T)
		flow.ts.melt<-melt(flow.ts, id.vars=c("Date","day","month","year"), measure.vars=c("Q"))
		flow.ts.sum<-cast(flow.ts.melt, day~variable, subset=variable=="Q", quantile,c(0.05,0.25,0.50,0.75,0.95),na.rm=T)
		names(flow.ts.sum)<-c("day","Q.05","Q.25","Q.50","Q.75","Q.95")
		flow.ts.sum<-na.omit(flow.ts.sum)
		flow.ts.sum<-data.frame(all.dates=seq.Date(as.Date("2008-01-01"),as.Date("2008-12-31"),by=1),flow.ts.sum)
		return(flow.ts.sum)
	}
