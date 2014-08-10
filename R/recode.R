recode <- function(data, oldvalue, newvalue) {
	
	# convert any factors to characters
	
	if (is.factor(data))     data     <- as.character(data)
	if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
	if (is.factor(newvalue)) newvalue <- as.character(newvalue)
	
	# create the return vector
	
	newvec <- data
	
	# put recoded values into the correct position in the return vector
	
	for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
	
	return(newvec)
	
}