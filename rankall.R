rankall <- function(outcome, num = "best") {
	data.full <- read.csv("outcome-of-care-measures.csv")	#extract file
	data.full <- data.full[,c(7,2,11,17,23)]				#keeps the good columns
	
	states.all <- levels(data.full$State)					#list of the states
	
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
	# removing cases without the required data
	data.full[,col] <- as.numeric(as.character(data.full[,col]))
	data.full <- data.full[complete.cases(data.full),]
	
	## function to find the hospital name in each state
	find <- function(state) {
		## extracting the given state data and ordering it
		data.state <- data.full[data.full$State == state,]
		data.ordered <- data.state[order(data.state[,col], data.state[,2], na.last=TRUE, decreasing=FALSE),]
		
		## finding the hospital name 
		if (num == "best") {
			name <- data.ordered[1,2]
		} else if (num == "worst") {
			name <- data.ordered[nrow(data.ordered),2]
		} else if ( (num >= 1) & (num <= nrow(data.ordered)) ) {
			name <- data.ordered[num,2]
		} else {
			name <- "NA"
		}
		name				#hospital name
		
	}
	
	lapply(states.all, find)
}