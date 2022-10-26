#' Score behavioral patterns from external video recordings
#'
#' Writes times when the user indicates a behavior started. Optionally converts the 
#' recorded times of the beginnings of specific behaviors to a vector, indicating in what
#' seconds the object performed a behavior.
#'
#' @param duration numeric. Duration (in minutes) of the video recording for which to  
#'                 score the behavior. 
#' @param save logical. If \code{TRUE}, saves the results into a tab-delimited file.
#' @param  file character. File name where to save the results.
#' @param vect logical. If \code{TRUE}, returns a binary vector of seconds when the
#'    		   behavior was recorded.
#' @export
#' @return Returns data.frame with columns events (codes for recorded behaviors), 
#'         stroke.time (elapsed process time when the change in behavior was recorded), time.s 
#'         time.s (start of the behavior in seconds from the beginning of recording), and
#'         duration (duration of the behavior in seconds).
#'
#'         If \code{vect = TRUE}, returns a binary vector for when the selected behavior
#'         was recorded.
#' @note Use \code{vect = TRUE} with \code{save = TRUE}, otherwise recordings of all other
#'       behaviors get lost.


score_events <-
function(duration = 15, save = TRUE, file = "score.events.txt", vect = FALSE){
	cat("Usage: choose keys to represent events for scoring. Hit [enter] to record the given symbol\n")
	
	first.symbol = readline("What will be the first symbol? ")
	score.events = c(first.symbol, 0)
	
	# duration in seconds
	duration = duration * 60
	
	temp = readline("Press [enter] to start scoring")
	
	
	scoring.start = proc.time()[3]
	while(proc.time()[3] - scoring.start < duration){
		temp = readline("score... ")
		score.events = c(score.events, temp, proc.time()[3] - scoring.start)
	}
	
	score.events = as.data.frame(matrix(score.events, byrow = TRUE, 
				   ncol = 2, dimnames = list(NULL, c("events","stroke.time"))))
	
	score.events[, 2] = round(as.numeric(as.character(score.events[,2])),3)
	
	if(save){
		new.file = readline(sprintf("Enter filename for saving results. If left empty, the results file will be %s. If ok, press [enter] ", file))
		file = ifelse(new.file == "", file, new.file)
		write.table(score.events, file = file, col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")
	}
		
	score.events$time.s = round(score.events$stroke.time, 0)
	
	cat("Duration of an event is given in seconds, the last scored event lasted until the end of the allocated time\n")
	
	for(i in 1:nrow(score.events)-1){
		score.events$duration[i] = score.events$time.s[i+1] - score.events$time.s[i]
	}
	
	score.events = score.events[-nrow(score.events), ]
	
	score.events$duration[nrow(score.events)] = duration - score.events$time.s[nrow(score.events)]
	
	write.table(score.events, file = file, col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")

	if(vect){
		event = readline("For which symbol do you wish to make a binary vector? ")
		a = NULL
		if(event %in% levels(score.events[, 1])){
			for(i in 1:nrow(score.events)){
				a = c(a, bin_duration(data = score.events[i,], event = event))
			}				
			score.events <- a
		}
		else{ message(paste("Symbol", event, "was not recorded, returing a matrix. Function bin.event() will convert it into a vector."))}
	}
	return(score.events)
}
