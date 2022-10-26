#' Prepares binary vector of when a behavior was recorded
#'
#' @param tab data.frame. Output of \code{score_events()}
#' @param event character, which recorded behavior to output
#' @param save logical, whether to save the result into a file
#' @param file character. Filename without extension.
#' @export

bin_event <-
function(tab, event, save = TRUE, file="bin.vector"){
	if(missing(event)){
		event <- tab$events[1]
	}
	vect = unname(unlist(apply(tab, 1, FUN = function(x) bin_duration(x, event = event))))
	cat(vect,file = paste0(file, event, ".txt"))
	return(vect)
}
