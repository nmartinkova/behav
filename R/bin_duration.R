#' Calculate duration of a behavior
#'
#' Calculates time in seconds between two consecutive changes of behaviors.
#' @param data row of output from score.events()
#' @param event character, specifying an existing recorded behavior
#' @export
#' @return Binary vector of seconds when the behavior was recorded.
#' @note DO NOT USE WITHOUT bin_event()


bin_duration <-
function(data, event){
	if(data["events"] == event){	vect = rep(1, data["duration"]) }
	else{ vect = rep(0, data["duration"])	}
	return(vect)
}
