#' Merge BORIS behavioural observation files
#'
#' This function reads multiple observation files exported from the BORIS
#' software (either Excel `.xlsx` or tab-delimited `.tsv`) and combines them
#' into one table. It calculates for each individual and behaviour the duration
#' of each event, its cumulative duration within the recording, and the
#' relative frequency of that behaviour. The merged table can then be used for
#' calculating behavioural diversity indices or other analyses.
#'
#' @param files Character vector giving the paths to BORIS export files.
#'   Files can be a mix of Excel (\code{.xlsx}) and tab-delimited text
#'   (\code{.tsv}). Each file must contain at least the columns
#'   \emph{Observation id}, \emph{Subject}, and \emph{Behavior}.
#' @param outputFile Character string giving the path and name of the file where
#'   the merged table will be saved. The directory is created automatically if
#'   it does not exist.
#'
#' @details
#' The function automatically recognises whether a file contains durations
#' (column \emph{Duration (s)}) or pairs of start/stop times (column
#' \emph{Time}). For duration data it uses the recorded durations directly;
#' for start/stop data it calculates durations as the difference between
#' consecutive time values.
#'
#' For each animal–behaviour combination the following variables are returned:
#' \itemize{
#'   \item \code{duration} – duration of the behavioural event (s),
#'   \item \code{cumDuration} – cumulative duration within the observation,
#'   \item \code{freq} – relative frequency of the behaviour,
#'   \item \code{file} – source file name.
#' }
#'
#' The output table can be used for further analyses, such as calculating
#' Shannon’s diversity index, Simpson’s index, or summarising behavioural
#' richness for each individual.
#'
#' @return
#' A tab-delimited text file is written to \code{outputFile}. The function does
#' not return any object to the R console.
#'
#' @examples
#' \dontrun{
#' mergeBoris(
#'   files = c("ZooZlin.xlsx", "Namibia.tsv"),
#'   outputFile = "Data/elephantBehaviour.txt"
#' )
#' }
#'
#' @note
#' Designed for students working with BORIS behavioural observations. The
#' function requires only the \pkg{readxl} package and runs on base R
#' otherwise.
#' importFrom("utils", "install.packages")
#' importFrom("readxl", "read_excel")
#' @export
mergeBoris <- function(files, outputFile) {

  # create output directory if missing
  if (basename(outputFile) != outputFile) {
    if (!dir.exists(dirname(outputFile))) dir.create(dirname(outputFile), recursive = TRUE)
  }

  vsetko <- NULL

  for (i in seq_along(files)) {
    # read a BORIS file
    if (tools::file_ext(files[i]) == "tsv") {
      dat <- read.table(files[i],
        sep = "\t", header = TRUE,
        quote = "\"", comment.char = ""
      )
    } else {
      dat <- as.data.frame(readxl::read_excel(files[i], sheet = 1))
      names(dat) <- make.names(names(dat))
    }


    # check what kind of data it has
    if ("Duration..s." %in% colnames(dat)) {
      suppressWarnings(dat$duration <- as.numeric(dat[, "Duration..s."]))
      dat$freq <- dat$cumDuration <- NA
      dat$animal <- paste(dat[, "Observation.id"], dat[, "Subject"], sep = "-")

      for (j in unique(dat$animal)) {
        ktore <- dat$animal == j
        dat1 <- dat[ktore, ]
        dat1 <- dat1[!is.na(dat1$duration), ]
        if (nrow(dat1) == 0) next

        dat1$cumDuration <- cumsum(dat1$duration)
        dat1$freq <- dat1$duration / sum(dat1$duration)
        dat1$file <- files[i]

        vsetko <- rbind(
          vsetko,
          dat1[, c("animal", "Behavior", "duration", "cumDuration", "freq", "file")]
        )
      }
    } else {
      dat$animal <- paste(dat[, "Observation.id"], dat[, "Subject"], sep = "-")
      dat$duration <- dat$freq <- dat$cumDuration <- NA

      for (Animal in unique(dat$Subject)) {
        ktore <- dat$Subject == Animal
        dat1 <- dat[ktore, ]
        dat1$Time <- as.numeric(dat1$Time)

        for (j in seq(1, nrow(dat1), by = 2)) {
          dat1[j, "duration"] <- dat1[j + 1, "Time"] - dat1[j, "Time"]
        }

        dat1 <- dat1[!is.na(dat1$duration), ]
        dat1$cumDuration <- cumsum(dat1$duration)
        dat1$freq <- dat1$duration / sum(dat1$duration)
        dat1$file <- files[i]

        vsetko <- rbind(
          vsetko,
          dat1[, c("animal", "Behavior", "duration", "cumDuration", "freq", "file")]
        )
      }
    }
  }

  vsetko$Behavior <- sub("\r\n", "", vsetko$Behavior, perl = TRUE)
  vsetko$animal <- sub("\r\n", "", vsetko$animal, perl = TRUE)

  if (is.null(vsetko)) stop("No valid data found in input files.")
  write.table(vsetko, file = outputFile, sep = "\t", row.names = FALSE)
}
