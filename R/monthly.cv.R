monthly.cv <-
function(flow.ts) {
	
	monthly.cv<-(sd(flow.ts$Q, na.rm=T)/mean(flow.ts$Q, na.rm=T))*100
	
	return(list(monthly.cv=monthly.cv))}
