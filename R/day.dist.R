day.dist <-
function(years,days) {

year.lengths<-sapply(years,getDays)
theta<-c()
adjust<-0.5*((2*pi)/365)
theta<-days*(2*pi/year.lengths)-adjust
x<-mean(cos(theta))
y<-mean(sin(theta))
m.theta<-atan2(y,x)
m.deg<-(m.theta*365/(2*pi))+0.5
if(m.deg<0) m.deg<-m.deg+365


r<-sqrt((x^2)+(y^2))
sd.rad<-sqrt(-2*log(r))
sd.deg<-sd.rad*(365/(2*pi))

return(list(mean.doy=round(m.deg,0),sd.doy=round(sd.deg,0)))
}
