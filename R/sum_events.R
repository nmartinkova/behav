#' Summarizes behavioral patterns
#'
#' Checks all files in a folder and summarizes behaviors recorded in the files. The files
#' should result from score_events().
#' @param folder character. Path to a folder that contains the recorded behaviors.
#' @param save logical. Whether the results should be saved to a file.
#' @param file character. File name where the results will be stored. Make sure to NOT 
#'             store the results where the behavior recording files are! Not used if
#'             \code{save = FALSE}.
#' @param troubleshoot logical. Prints current filename if \code{TRUE}.
#' @export
#' @return Returns the data.frame with behaviour durations in all files in the folder. 
#'     Optionally, saves results of the ethogram with durations of each behaviour to a 
#'     file.
#' @importFrom stats aggregate
#' @importFrom utils read.table write.table
#' @importFrom plyr rbind.fill


sum_events <- function(folder = ".", save = FALSE, file = "sum.events.txt", troubleshoot = FALSE){
	subory <- dir(folder, full.names = T, recursive = TRUE)
	
	ids <- sub(".+/", "", subory)
	ids <- sub("\\.txt", "", ids)
	
	temp <- read.table(subory[1], sep = "\t", header = TRUE)
	total <- sum(temp$duration)
	temp$events <- substr(tolower(temp$events), 1, 1)
	sums <- aggregate(temp$duration ~ temp$events, FUN = sum)
	
	res <- data.frame(matrix(c(total, sums[, 2]), nrow = 1, dimnames = list(ids[1], c("total", sums[, 1]))))
	
	if(length(subory) > 1){
		for(i in 2:length(subory)){
			if(troubleshoot) cat("working on", subory[i], "\n")
			temp <- read.table(subory[i], sep = "\t", header = TRUE)
			total <- sum(temp$duration)
			temp$events <- substr(tolower(temp$events), 1, 1)
			sums <- aggregate(temp$duration ~ temp$events, FUN = sum)
			res <- plyr::rbind.fill(res, 
						 data.frame(matrix(c(total, sums[, 2]), nrow = 1, dimnames = list(ids[i], c("total", sums[, 1]))))
						 )
		}
	
	}
	row.names(res) <- ids
	if(save){
		write.table(res, file = file, sep = "\t", row.names = TRUE, col.names = TRUE)
	}
	res
}