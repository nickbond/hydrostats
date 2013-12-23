 high.spells <-
function(flow.ts,quant=0.9, user.threshold=FALSE, defined.threshold=0.9, ind.days=5, duration=TRUE, volume=TRUE,plot=TRUE, ignore.zeros=FALSE,ctf.threshold=0.1, ann.stats=TRUE,ann.stats.only=FALSE,inter.flood=FALSE) {
	gauge<-deparse(substitute(flow.ts))
  
  if (ncol(flow.ts)>2) {
    record.year<-flow.ts[,'Year']
    }
  else {
    record.year<-strftime(flow.ts[[1]],format="%Y")
    flow.ts<-data.frame(flow.ts,Year=record.year)
}
  n.years<-nlevels(as.factor(record.year))

if(ann.stats==T) {

	flow.ts.comp<-na.omit(flow.ts) 
  
  n.days<-tapply(flow.ts.comp[[2]],flow.ts.comp[[3]],length)
  n.most.days<-which(n.days>350)
  flow.ts.comp<-flow.ts.comp[which(flow.ts.comp[[3]] %in% names(n.days)),]
  record.year<-flow.ts.comp[[3]]
  n.years<-nlevels(as.factor(record.year))
  
ann.max<-tapply(flow.ts.comp[[2]],record.year, max)

ann.max.day.no<-tapply(flow.ts.comp[[2]],record.year, which.max)-1


correct.ann.max.day<-day.dist(names(ann.max.day.no),ann.max.day.no)

max.ann.flow.threshold<-min(ann.max,na.rm=T)
  
ann.max.spells<-ifelse(flow.ts[,2]>max.ann.flow.threshold,1,0)
ann.max.spell.runs<-rle(ann.max.spells)
avg.ann.duration<-mean(ann.max.spell.runs$lengths[which(ann.max.spell.runs$values==1)],na.rm=T)
max.ann.duration<-max(ann.max.spell.runs$lengths[which(ann.max.spell.runs$values==1)], na.rm=T)
cv.ann.duration<-(sd(ann.max.spell.runs$lengths[which(ann.max.spell.runs$values==1)],na.rm=T)/mean(ann.max.spell.runs$lengths[which(ann.max.spell.runs$values==1)],na.rm=T))*100

}

if(ann.stats.only==T) {

  return(list(avg.max.ann=mean(ann.max[is.finite(ann.max)],na.rm=T), cv.max.ann=(sd(ann.max,na.rm=T)/mean(ann.max,na.rm=T)*100), 
              flood.timing=correct.ann.max.day[[1]], flood.predictability=correct.ann.max.day[[2]],
              flood.skewness=mean(ann.max,na.rm=T)/mean(flow.ts[,2],na.rm=T),avg.ann.duration=avg.ann.duration,cv.ann.duration=cv.ann.duration))

}
else {

if(user.threshold==T) {
  
    
flow.threshold<-defined.threshold  

}   

else {


if(ignore.zeros==T) {
  
flow.threshold<-quantile(flow.ts[which(flow.ts[[2]]>ctf.threshold),2],quant,na.rm=T)
names(flow.threshold)<-NULL 
}
else
{
  flow.threshold<-quantile(flow.ts[,2],quant,na.rm=T)
  names(flow.threshold)<-NULL 
}

}

    
  


high.flows<-ifelse(flow.ts[,2]>flow.threshold,1,0)

if (ind.days>0) {
  
high.flow.runs<-rle(high.flows)
too.short<-which(high.flow.runs$lengths<ind.days & high.flow.runs$values==0)   
spell.factor<-rep(seq_along(high.flow.runs$lengths), times=high.flow.runs$lengths)  
add.to.spell<-which(spell.factor %in% too.short)
high.flows[add.to.spell]<-1
}


high.flow.runs<-rle(high.flows)



high.flow.av<-mean(flow.ts[which(high.flows==1),2],na.rm=T)
high.flow.sd<-sd(flow.ts[which(high.flows==1),2],na.rm=T)


  
good.high.flow.runs<-which(!is.na(high.flow.runs$values))
flow.runs.values<-high.flow.runs$values[good.high.flow.runs]
flow.runs.lengths<-high.flow.runs$lengths[good.high.flow.runs]

n.events<-length(flow.runs.values[flow.runs.values==1])
flood.frequency<-n.events/n.years




if(duration==TRUE) {
  
  avg.duration<-mean(high.flow.runs$lengths[which(high.flow.runs$values==1)],na.rm=T)
  med.duration<-quantile(high.flow.runs$lengths[which(high.flow.runs$values==1)],0.50,na.rm=T, names=F)
  max.duration<-max(high.flow.runs$lengths[which(high.flow.runs$values==1)], na.rm=T)
  sd.duration<-sd(high.flow.runs$lengths[which(high.flow.runs$values==1)], na.rm=T)
  cv.duration<-sd.duration/avg.duration*100
}

if(inter.flood==TRUE) {
  
  avg.interval<-mean(high.flow.runs$lengths[which(high.flow.runs$values==0)],na.rm=T)
  min.interval<-min(high.flow.runs$lengths[which(high.flow.runs$values==0)], na.rm=T)
  max.interval<-max(high.flow.runs$lengths[which(high.flow.runs$values==0)], na.rm=T)

  sd.interval<-sd(high.flow.runs$lengths[which(high.flow.runs$values==0)], na.rm=T)
  cv.interval<-sd.interval/avg.interval*100
return(list(ofs.threshold=flow.threshold, avg.ofs.interval=avg.interval,min.ofs.interval=min.interval, max.ofs.interval=max.interval))
  }

if(volume==TRUE) {
spell.factor <- rep(seq_along(high.flow.runs$lengths), times=high.flow.runs$lengths) 
spells<-split(flow.ts[[2]],spell.factor)
spell.volumes<-flow.ts[[2]]
spell.volumes<-sapply(spells,sum)
spell.volumes.below.threshold<-sapply(spells,length)*flow.threshold
spell.volumes<-spell.volumes[which(high.flow.runs$values==1)]-spell.volumes.below.threshold[which(high.flow.runs$values==1)]

}


if(plot==TRUE) {
plot(flow.ts[[1]],flow.ts[[2]],type="l", main=gauge, xlab="Date", ylab="Q")

points(flow.ts[which(high.flows==1),1],flow.ts[which(high.flows==1),2],col="red",cex=0.25)
       
abline(h=flow.threshold)
}  

}

if (ann.stats==F) {
	return(list(n.years=n.years, high.spell.threshold=flow.threshold, n.events=n.events, spell.frequency=flood.frequency, ari=1/flood.frequency, avg.high.spell.duration=avg.duration, med.high.spell.duration=med.duration, max.high.spell.duration=max.duration, avg.spell.volume=mean(spell.volumes,na.rm=T), avg.spell.peak=high.flow.av, sd.spell.peak=high.flow.sd))
}

else {
  return(list(n.years=n.years,high.spell.threshold=flow.threshold, n.events=n.events, spell.freq=flood.frequency, ari=1/flood.frequency, avg.high.spell.duration=avg.duration, med.high.spell.duration=med.duration, max.high.spell.duration=max.duration, avg.spell.volume=mean(spell.volumes,na.rm=T), avg.spell.peak=high.flow.av, sd.spell.peak=high.flow.sd, 
  						avg.max.ann=mean(ann.max[is.finite(ann.max)],na.rm=T),  cv.max.ann=(sd(ann.max,na.rm=T)/mean(ann.max,na.rm=T)*100), 
            flood.skewness=mean(ann.max,na.rm=T)/mean(flow.ts[,2],na.rm=T), flood.timing=correct.ann.max.day[[1]], 
            flood.predictability=correct.ann.max.day[[2]],avg.ann.duration=avg.ann.duration,cv.ann.duration=cv.ann.duration))
}
         

}
