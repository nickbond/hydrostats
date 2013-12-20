daily.cv <-
function(flow.ts) {
	
	daily.cv<-(sd(flow.ts$Q, na.rm=T)/mean(flow.ts$Q, na.rm=T))*100

return(list(daily.cv=daily.cv))}
