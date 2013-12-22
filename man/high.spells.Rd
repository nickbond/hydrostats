\name{high.spells}
\alias{high.spells}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates high flow spell statistics
}
\description{
Calculates a suite of statistics describing flood characteristics, such as the timing, frequency and duration of events. The event threshold can be defined as a flow quantile (e.g. upper 90th percentile) or a specific threshold volume (e.g. ML/day).



}
\usage{
high.spells(flow.ts, quant = 0.9, user.threshold = F, defined.threshold, \cr ind.days = 5, duration = T, volume = T, plot = T, ignore.zeros = F,\cr ctf.threshold = 0.1, ann.stats = T, ann.stats.only = F, \cr inter.flood = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{flow.ts}{Dataframe consisting of date in column one and flow in column two. Date must be in POSIX format (see ts.format).
    }
  \item{quant}{Percentile/quantile to use for defining event magnitude (default 0.9). A value of 0.9 is the upper 90th percentile (i.e. a volume exceeded 10\% of the time).
  }
  \item{user.threshold}{
logical. If TRUE, the function returns results based on a user defined volume to define spell events (see defined.threshold). 
}
  \item{defined.threshold}{
A user supplied threshold for defining spells. This would typically be derived from hydraulic models or similar knowledge pertaining to a gauge site.
}
  \item{ind.days}{
Periods between spells of less than ind.days (default 5) are considered to be 'in spell' for the purpose of further calculations. A value of 0 means spells 1 day apart are considered indpedendent.}
  \item{duration}{
logical. Should spell duration be returned?}
  \item{volume}{
logical. Should spell volumes be returned? Note that for days considered 'in-spell', the returned values have the threshold volume subtracted first, and hence reflect the amount of water that was flowing past the gauge above threshold. This is most useful in water planning scenarios.}
  \item{plot}{
logical. Should the time-series be plotted? Data points considered 'within spell' are identifed using red circles and the threshold is identified with a horizontal line.}
  \item{ignore.zeros}{
logical. If TRUE, days below a user defined cease-to-flow threshold (default 0.1) will be excluded when estimating the spell threshold for a given percentile. This is primarily of interest in highly ephemeral rivers, where flow may only occur for a small fraction of the time. In such cases, the inclusion of zeros will skew estimates of high flow events downwards, which may be undesirable.
}
  \item{ctf.threshold}{
values below this threshold are treated as zero for the purpose of percentile based calculations (see ignore zeros).
}
  \item{ann.stats}{
logical. If TRUE, the function returns results describing the annual series (i.e. that describing the characteristics of the largest flood event in each year of the time-series).
}
  \item{ann.stats.only}{
logical. If TRUE, statistics describing the annual series only are returned.}

  \item{inter.flood}{
logical. If TRUE, statistics describing inter-flood spell characteristics are reported.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
flood indices
\item{n.years }{Description of 'comp1'}
\item{high.spell.threshold }{Description of 'comp2'}
\item{n.events }{Description of 'comp2'}
\item{spell.freq}{Description of 'comp2'}
\item{ari}{Description}
\item{avg.high.spell.duration}{Description of 'comp2'}
\item{med.high.spell.duration}{Description of 'comp2'}
\item{max.high.spell.duration}{Description of 'comp2'}
\item{avg.spell.volume}{Description of 'comp2'}
\item{avg.spell.peak}{Description of 'comp2'}
\item{sd.spell.peak}{Description of 'comp2'}

interflood indices
\item{average.interval}{Description of 'comp1'}
\item{min.interval}{Description of 'comp2'}
\item{max.interval }{Description of 'comp2'}

Annual statistics
\item{avg.max.ann}{Description of 'comp2'}
\item{cv.max.ann}{Description of 'comp2'}
\item{flood.skewness}{Description of 'comp2'}
\item{flood.timing}{Description of 'comp2'}
\item{flood.predictability}{Description of 'comp2'}
\item{avg.ann.duration}{Description of 'comp2'}
\item{cv.ann.duration}{Description of 'comp2'}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

data(Cooper)
Cooper<-ts.format(Cooper)

high.spells(Cooper, quant=0.9)

high.spells(Cooper, quant=0.9, ann.stats=F, plot=F)

high.spells(Cooper, quant=0.9, ann.stats=F, ignore.zeros=T)


## The function is currently defined as
high.spells <-
function(flow.ts,quant=0.9, user.threshold=F, defined.threshold, ind.days=5, duration=T, volume=T,plot=T, ignore.zeros=F,ctf.threshold=0.1, ann.stats=T,ann.stats.only=F,inter.flood=F) {
	gauge<-deparse(substitute(flow.ts))
  
  if (ncol(flow.ts)>2) {
    record.year<-flow.ts[,'Year']
    }
  else {
    record.year<-strftime(flow.ts[[1]],format="%Y")
    flow.ts<-data.frame(flow.ts,year=record.year)
}
  n.years<-nlevels(as.factor(record.year))

if(ann.stats==T) {
  #calculate annual minimum flow
  flow.ts.comp<-na.omit(flow.ts) 
  
  #record.year<-strftime(flow.ts.comp[[1]],format="%Y")
  n.days<-tapply(flow.ts.comp[[2]],flow.ts.comp[[3]],length)
  n.most.days<-which(n.days>350)
  flow.ts.comp<-flow.ts.comp[which(flow.ts.comp[[3]] %in% names(n.days)),]
  record.year<-flow.ts.comp[[3]]
  n.years<-nlevels(as.factor(record.year))
  
ann.max<-tapply(flow.ts.comp[[2]],record.year, max)

ann.max.day.no<-tapply(flow.ts.comp[[2]],record.year, which.max)-1
##ann.max.date<-as.Date(paste(names(ann.max.day.no),01,01,sep="-"))+ann.max.day.no


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
#names(flow.threshold)<-NULL #normallyhide

}   

else {


if(ignore.zeros==T) {
  
flow.threshold<-quantile(flow.ts[which(flow.ts[[2]]>ctf.threshold),2],quant,na.rm=T)
names(flow.threshold)<-NULL #normally hide
}
else
{
  flow.threshold<-quantile(flow.ts[,2],quant,na.rm=T)
  names(flow.threshold)<-NULL #normallyhide
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


#flood.days<-as.numeric(strftime(flow.ts[which(high.flows==1),1],format="%j"))
  
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

%%end function

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line