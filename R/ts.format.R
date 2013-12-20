ts.format <-
function(x) {
x[[1]]<-strptime(x[[1]],format="%d/%m/%Y")
names(x)[1]<-"Date"
return(x)
}
