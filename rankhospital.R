rankhospital <- function(state, outcome, num = "best") {
	data.full <- read.csv("outcome-of-care-measures.csv")	#extract file
	data.full <- data.full[,c(7,2,11,17,23)]				#keeps the good columns
	
	## Picks the right column
	col <- if (outcome == "heart attack") {
		3
	} else if (outcome == "heart failure") {
		4
	} else if (outcome == "pneumonia") {
		5
	} else {
		stop(" invalid outcome")	#invalid outcome
	}
	## reject if invalid state
	if (!any(data.full$State == state)) {
		stop(" invalid state")		##invalid state
	}
	data.full <- data.full[data.full$State == state,]
	data.full[,col] <- as.numeric(as.character(data.full[,col]))
	
	## ordering and removing NA's
	data.ordered <- data.full[order(data.full[,col], data.full[,2], na.last=TRUE, decreasing=FALSE),]
	data.ordered <- data.ordered[complete.cases(data.ordered),]
	
	if (num == "best") {
		return(data.ordered[1,2])
	} else if (num == "worst") {
		return(data.ordered[nrow(data.ordered),2])
	} else if ( (num >= 1) & (num <= nrow(data.ordered)) ) {
		return(data.ordered[num,2])
	} else {
		return("NA")
	}
}