doy.plot.log <-
function(flow.ts.sum, title="",ylim=c(0.1,10000)) {
	plot<-ggplot(flow.ts.sum, aes(x=all.dates, group=1))
	plot + scale_x_date(breaks=date_breaks("1 month"),labels=date_format("%b"))+ 
		geom_ribbon(aes(ymin=Q.05,ymax=Q.95),fill= "gray80") +
		geom_ribbon(aes(ymin=Q.25,ymax = Q.75), fill = "gray60") +
		scale_y_continuous(labels=comma, trans=log10_trans())+
		#coord_trans(y="log10")+
		coord_cartesian(ylim = ylim) +
		geom_line(aes(y = Q.50), colour="black") + labs(title=title, x="Date",y=expression("Daily Discharge (Ml.day"^-1*")"), vjust=0.5)+  #(m"^3*".s"^-1*")")
		theme_bw()
}
