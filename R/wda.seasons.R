wda.seasons <-
function(flow.ts) {
flow.ts$Date<-ymd(flow.ts[,'Date'])
flow.ts$year<-year(flow.ts[,'Date'])

flow.ts.mdf<-ddply(flow.ts,.(year),summarize, MDF=mean(Q))
flow.ts.quants<-quantile(flow.ts.mdf$MDF,c(0.3,0.7))

flow.ts.mdf$season<-ifelse(flow.ts.mdf$MDF<flow.ts.quants[1],"dry",
													 ifelse(flow.ts.mdf$MDF>=flow.ts.quants[2],"wet","average"))

flow.ts$season<-flow.ts.mdf$season[match(flow.ts$year,flow.ts.mdf$year)]

return(flow.ts)
}
