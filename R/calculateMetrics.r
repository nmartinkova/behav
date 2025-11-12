#' Calculate behavioural diversity metrics from BORIS observations
#'
#' This function summarises each individual's behaviour using a set of diversity
#' and temporal metrics, allowing comparison between elephants observed in zoos
#' and in the wild, and between adults and juveniles.
#' 
#' It computes indices describing both how many different behaviours were
#' expressed and how evenly the animal distributed its time among them,
#' as well as the *temporal structure* of its activity.
#' Optionally, it can visualise the metrics as grouped boxplots.
#'
#' @inheritParams plotRarefaction
#' @param plotMetrics Logical. If `TRUE`, plots boxplots of the selected metrics
#'   by group.
#' @param whichPlot Character vector naming one or more metrics to display.
#'   Partial matching is allowed. Available metrics are:
#'   `"ShannonH"`, `"HillD2"`, `"PielouEvenness"`,
#'   `"transitionsPerHour"`, and `"medianBout"`.
#' @param cols Character vector of four colours used for plotting,
#'   corresponding to zoo adult, zoo juvenile, wild adult, and wild juvenile.
#'
#' @details
#' The function removes the `"Mimo dohled"` category and computes, for each animal:
#' 
#' * **timeOnCamera** – Total duration (s) during which the animal was visible and scored.
#' * **ShannonH** – Shannon diversity index.
#'   Quantifies overall behavioural diversity, combining both richness (number of behaviours)
#'   and evenness (balance in time spent on each behaviour).
#'   Higher values indicate a broader and more evenly distributed behavioural repertoire.
#'
#' * **HillD2** – Simpson reciprocal diversity (Hill number of order 2).
#'   Expressed as the *effective number of equally common behaviours*.
#'   A value of 5 means the activity distribution is as diverse as if five behaviours
#'   were expressed equally often.
#'
#' * **PielouEvenness** – Pielou’s evenness index.
#'   Measures how equally the animal divides its time among all behaviours.
#'   Values near 1 denote balanced activity; low values indicate dominance of few behaviours.
#'
#' * **transitionsPerHour** – Frequency of behavioural switches.
#'   The number of changes between behaviours per hour of observation.
#'   Reflects temporal flexibility: higher rates imply frequent switching between activities.
#'
#' * **medianBout** – Median duration (s) of individual behaviour bouts.
#'   Represents the typical length of a continuous behaviour.
#'   Shorter bouts indicate more dynamic behaviour; longer bouts suggest more persistent activity.
#'
#' When `plotMetrics = TRUE`, the function produces grouped boxplots comparing the
#' four categories: zoo adults, zoo juveniles, wild adults, and wild juveniles.
#' These plots help visualise whether, for example, wild elephants show greater
#' behavioural diversity or evenness than zoo animals, or whether juveniles
#' switch behaviours more frequently than adults.
#'
#' @return A data frame where each row corresponds to one animal and columns include:
#'   `animal`, `zoo`, `adult`, `file`, and the computed metrics described above.
#'   If `plotMetrics = TRUE`, boxplots of selected metrics are drawn.
#'
#' @examples
#' \dontrun{
#' dat <- read.table("Data/elephantBehaviour.txt", header = TRUE, sep = "\t")
#' res <- calculateMetrics(dat, plotMetrics = TRUE,
#'                         whichPlot = c("Shannon", "Hill"))
#' head(res)
#' }
#'
#' @export
calculateMetrics <- function(
    x,
    zoo = c("Zoo", "Beekse", "Safaripark", "Tiergarten"),
    age = c("Mládě", "Zyqarri"),
    plotMetrics = FALSE,
    whichPlot = "ShannonH",
    cols = c("#3C8ABF", "#A1BCD7", "#768D1A", "#B1BE94")) {
  # match plotting
metricNames <- c("ShannonH", "HillD2", "PielouEvenness", "transitionsPerHour", "medianBout")
  whichPlot <- metricNames[pmatch(whichPlot, metricNames, duplicates.ok = TRUE)]
  whichPlot <- whichPlot[!is.na(whichPlot)]
  if (length(whichPlot) == 0){ warning("Take indexy nemam. Nekreslim.")
  }
  # remove non-behaviour rows
  x <- x[x$Behavior != "Mimo dohled", ]

  # identify zoo/wild and adult/juvenile animals
  x$zoo <- grepl(paste(zoo, collapse = "|"), x$file)
  x$adult <- !grepl(paste(age, collapse = "|"), x$animal)

  res <- unique(x[, c("animal", "zoo", "adult", "file")])
  res[, c("timeOnCamera", "ShannonH", "HillD2", "PielouEvenness", "transitionsPerHour", "medianBout")] <- NA

  for (i in 1:nrow(res)) {
    datInd <- x[sapply(x$animal == res$animal[i], isTRUE), ]
    # duration of individual's behaviours
    behDuration <- tapply(datInd$duration, datInd$Behavior, sum)

    # check data
    if (sum(behDuration) == 0 | length(behDuration) <= 1) {
      warning(paste(res$animal[i], "zo suboru", res$file[i], "ma prilis malo typov spravania."))
      next
    }

    # overall frequency of behaviours
    p <- behDuration / sum(behDuration)
    res[i, "timeOnCamera"] <- sum(datInd$duration)

    # diversity indices
    res[i, "ShannonH"] <- -sum(p * log(p)) # Shannon
    res[i, "HillD2"] <- 1 / sum(p^2) # Simpson reciprocal
    res[i, "PielouEvenness"] <- -sum(p * log(p)) / log(length(p)) # Pielou evenness

    # transitions
    b <- as.character(datInd$Behavior)
    res[i, "transitionsPerHour"] <- sum(b[-1] != b[-length(b)]) / (sum(datInd$duration) / 3600)
    res[i, "medianBout"] <- median(datInd$duration, na.rm = TRUE)
  }

  if (plotMetrics) {
    par(mfrow = c(1, length(whichPlot)), mar = c(4, 4, 0, 0) + .3)
    for (i in 1:length(whichPlot)) {
      boxplot(res[, whichPlot[i]] ~ factor(res$zoo, levels = c(TRUE, FALSE)) + factor(res$adult, levels = c(TRUE, FALSE)),
        col = cols,
        xlab = "", axes = FALSE,
        ylab = switch(whichPlot[i],
          ShannonH = "Shannon diversity",
          HillD2 = "Simpson reciprocal diversity",
          PielouEvenness = "Pielou's evenness",
          transitionsPerHour = "Behavioural transitions per hour",
          medianBout = "Median duration of behaviour bout (s)"
        )
      )
      axis(2, las = 1)
      axis(1, at = 1:4, labels = rep(c("adult", "juvenile"), 2))
      mtext(c("ZOO", "Wild"), side = 1, line = 2.5, at = c(1.5, 3.5))
      box()
    }
  }
  return(res)
}
