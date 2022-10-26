#' Calculates latencies of all instances of a behavioral pattern
#'
#' Checks all files in a folder and calculates latencies of all instances of a user-selected
#' behaviors recorded in the files. The files
#' should result from \code{score_events()}.
#' @param folder character. Path to a folder that contains the recorded behaviors.
#' @param behaviour character. Symbol of a scored behaviour for which to calculate latencies.
#' @export
#' @return data.frame Saves results of the ethogram with durations of each behaviour in each 
#'     experiment to a file and returns the data.frame invisibly.
#' @importFrom stats aggregate
#' @importFrom utils read.table write.table
#' @importFrom plyr rbind.fill


latency <- function(folder = ".", behaviour = NA){
	subory <- dir(folder, full.names = T)
	
	# prepare available behaviours
	ethogram <- sum_events(folder = folder, save = FALSE)
	
	ids <- rownames(ethogram)
	bhvs <- colnames(ethogram)[-1]
	
	# choose for which behaviour to calculate the latency
	while(!(behaviour %in% bhvs)){
	behaviour <- readline(paste("Choose one from the available behaviours and press [enter]:", paste(bhvs, collapse = ", "), " "))
	}
	
	ktore <- !is.na(ethogram[, behaviour])
	
	res <- data.frame(ID = character(), instance = numeric(), latency = numeric())
	
	# find all instances of the behaviour and return their latency	
		for(i in 1:length(subory)){
			if(!ktore[i]) next
			temp <- read.table(subory[i], sep = "\t", header = TRUE)
			temp$events <- substr(tolower(temp$events), 1, 1)
			temp <- temp[temp$events == behaviour,]
			res <- rbind(res, data.frame(ID = ids[i], instance = 1:nrow(temp), latency = temp$time.s))
		}
	
	
	res
}