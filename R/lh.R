lh <-
function(Q, a){

  qf<- rep(Q[1],length(Q))
  qb<-rep(Q[1],length(Q))
  
  qf[1]<-(Q[2]-Q[1])*((1+a)/2)
  
  for(i in 2:length(Q))     {
    
    qf[i] <-a*qf[i-1]+((Q[i]-Q[i-1])*((1.0+a)/2))
    
    }
 for (i in 1:length(Q)) {
    
    if(qf[i] < 0) { 
      qb[i]<-Q[i]  } 
    else 
      
    if (Q[i] > qf[i]){
      qb[i] <- Q[i]-qf[i]
    }
    
    else 
      qb[i]<-0

}
    return(qb)
    
}
