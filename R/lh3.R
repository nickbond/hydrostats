lh3 <-
function(Q,a) {
  qb1<-lh(Q,a)
  qb2<-lh(rev(qb1),a)
  qb3<-lh(rev(qb2),a)
  
  return(qb3)
  
}
