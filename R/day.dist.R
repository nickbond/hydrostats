day.dist <-
function(years,days) {

year.lengths<-sapply(years,getDays)
    theta<-c()
theta<-days*(2*pi/year.lengths)
x<-sum(sin(theta))
y<-sum(cos(theta))
m.theta<-atan2(x,y)
m.deg<-(180*m.theta/pi)
if(m.deg<0) m.deg<-m.deg+360

R<-sqrt((x^2)+(y^2))
Rm<-R/length(theta)
rad.sd<-1-Rm

return(cbind(m.deg,rad.sd))
}
