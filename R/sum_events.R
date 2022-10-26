#' Summarizes behavioral patterns
#'
#' Checks all files in a folder and summarizes behaviors recorded in the files. The files
#' should result from score_events().
#' @param folder character. Path to a folder that contains the recorded behaviors.
#' @param file character. File name where the results will be stored. Make sure to NOT 
#'             store the results where the behavior recording files are!
#' @export
#' @return Saves results to a file and returns the data.frame invisibly.
#' @importFrom stats aggregate
#' @importFrom utils read.table write.table
#' @importFrom plyr rbind.fill


sum_events <- function(folder = ".", file = "sum.events.txt"){
	subory <- dir(folder, full.names = T)
	
	ids <- sub(".+/", "", subory)
	ids <- sub("\\.txt", "", ids)
	
	temp <- read.table(subory[1], sep = "\t", header = TRUE)
	total <- sum(temp$duration)
	temp$events <- substr(tolower(temp$events), 1, 1)
	sums <- aggregate(temp$duration ~ temp$events, FUN = sum)
	
	res <- data.frame(matrix(c(total, sums[, 2]), nrow = 1, dimnames = list(ids[1], c("total", sums[, 1]))))
	
	if(length(subory > 1)){
		for(i in 2:length(subory)){
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
	write.table(res, file = file, sep = "\t", row.names = TRUE, col.names = TRUE)
	invisible(res)
}