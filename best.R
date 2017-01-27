## This function finds the best hospital in the state depending on the kind of decease. 
best <- function(state, outcome) {
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
	data.ordered[1,2]
}
