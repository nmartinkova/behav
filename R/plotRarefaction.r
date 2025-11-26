#' Plot behavioural rarefaction curves using time-slice bootstrap resampling
#'
#' This function shows how the number of distinct behaviours (behavioural richness)
#' increases with observation time for elephants observed in zoos and in the wild.
#' It uses a *time-slice bootstrap approach*, where random time windows of increasing
#' length are sampled from each recording, and the number of unique behaviours within
#' each window is counted. Averaging these results across individuals gives a smooth
#' estimate of how rapidly new behaviours appear during observation.
#'
#' The function is designed for student use and provides an intuitive comparison
#' of behavioural diversity between animals of different origin (zoo vs. wild)
#' and age class (adult vs. juvenile).
#'
#'
#' @param x A data frame created by \code{mergeBoris()}, containing columns `animal`, `Behavior`,
#'   `file`, and `cumDuration`.
#' @param zoo Character vector of text patterns used to recognise zoo files
#'   (default includes `"Zoo"`, `"Beekse"`, `"Safaripark"`, `"Tiergarten"`).
#' @param age Character vector of text patterns identifying juveniles in the `animal` column
#'   (default includes `"Mládě"`, `"Zyqarri"`).
#' @param cols Character vector of four colours used for the groups in order:
#'   ZOO adult, ZOO juvenile, Wild adult, Wild juvenile.
#'   The default palette uses blue for zoo animals and green for wild animals, with lighter
#'   tones representing juveniles.
#' @param nBoot Integer giving the number of bootstrap repetitions per fraction of data.
#'   A higher number gives smoother confidence intervals but takes longer to compute.
#'
#' @details
#' The analysis removes the category `"Mimo dohled"` and separates animals into four groups:
#' zoo adults, zoo juveniles, wild adults, and wild juveniles.
#' For each individual, total observation time is divided into a series of 50
#' fractional time slices ranging from 1% to 100%.
#' For each slice width *w = fT*, where *f* is a fraction and *T* is the total
#' observation time for the individuals, random time windows of length *w* are repeatedly drawn
#' across the individual’s observation period.
#' A behaviour counts as observed in that window if its duration overlaps with the window in time.
#'
#' For each time slice, the function computes the mean number of unique behaviours and the
#' 2.5% and 97.5% quantiles across bootstrap replicates to form 95% confidence intervals.
#' These curves are then averaged across individuals within each group.
#'
#' The resulting plot displays the mean behavioural richness (y-axis)
#' against the *fraction of total observation time* (x-axis),
#' allowing comparisons of how quickly new behaviours appear between groups.
#' @return Invisibly returns the input data frame `x` with extra columns indicating which
#' animals were categorised as zoo vs. wild in column `zoo` and adult vs. juvenile in 
#' column `age`. The function produces a plot with four curves:
#' blue tones for zoo animals, green tones for wild animals, solid lines for adults, and
#' lighter lines for juveniles.
#'
#' @examples
#' \dontrun{
#' dat <- read.table("Data/elephantBehaviour.txt", header = TRUE, sep = "\t")
#' plotRarefaction(dat)
#' }
#'
#' @export
plotRarefaction <- function(
  x,
  zoo = c("Zoo", "Beekse", "Safaripark", "Tiergarten"),
  age = c("Mládě", "Zyqarri"),
  cols = c("#3C8ABF", "#A1BCD7", "#768D1A", "#B1BE94"),
  nBoot = 500
) {
  # remove non-behaviour rows
  x <- x[x$Behavior != "Mimo dohled", ]

  # identify zoo/wild and adult/juvenile animals
  x$zoo <- grepl(paste(zoo, collapse = "|"), x$file)
  x$adult <- !grepl(paste(age, collapse = "|"), x$animal)

  # fractions of total observation time
  fracs <- seq(0.01, 1, length.out = 50)

  ## ---------------------------
  ## time-slice rarefaction for one animal
  ## ---------------------------
  timeSliceRarefaction <- function(datInd) {
    startTime <- datInd$cumDuration - datInd$duration
    endTime   <- datInd$cumDuration
    Tobs      <- max(datInd$cumDuration, na.rm = TRUE)

    windowRichness <- function(winStart, winEnd) {
      overlaps <- (startTime < winEnd) & (endTime > winStart)
      if (!any(overlaps)) return(0L)
      length(unique(datInd$Behavior[overlaps]))
    }

    meanRich <- numeric(length(fracs))
    lowRich  <- numeric(length(fracs))
    highRich <- numeric(length(fracs))

    for (k in seq_along(fracs)) {
      f <- fracs[k]
      w <- f * Tobs
      if (w <= 0) { meanRich[k] <- lowRich[k] <- highRich[k] <- 0; next }

      # bootstrap moving time windows
      if (w >= Tobs) {
        boots <- windowRichness(0, w)
      } else {
        winStarts <- runif(nBoot, 0, Tobs - w)
        boots <- numeric(nBoot)
        for (b in seq_len(nBoot)) {
          s <- winStarts[b]
          e <- s + w
          boots[b] <- windowRichness(s, e)
        }
      }

      meanRich[k] <- mean(boots)
      lowRich[k]  <- quantile(boots, 0.025, names = FALSE)
      highRich[k] <- quantile(boots, 0.975, names = FALSE)
    }

    data.frame(mean = meanRich, low = lowRich, high = highRich)
  }

  ## ---------------------------
  ## group-level bootstrap
  ## ---------------------------
  groupList <- list()
  k <- 1

  for (z in c(TRUE, FALSE)) {    # ZOO vs Wild
    for (a in c(TRUE, FALSE)) {  # Adult vs Juvenile
      inds <- unique(x$animal[x$zoo == z & x$adult == a])
      if (length(inds) == 0) next

      indCurves <- vector("list", length(inds))
      for (i in seq_along(inds)) {
        datInd <- x[x$animal == inds[i], ]
        indCurves[[i]] <- timeSliceRarefaction(datInd)
      }

      # average across individuals in the group
      matMean <- do.call(cbind, lapply(indCurves, `[[`, "mean"))
      matLow  <- do.call(cbind, lapply(indCurves, `[[`, "low"))
      matHigh <- do.call(cbind, lapply(indCurves, `[[`, "high"))

      groupList[[k]] <- data.frame(
        groupMean = rowMeans(matMean, na.rm = TRUE),
        groupLow  = rowMeans(matLow,  na.rm = TRUE),
        groupHigh = rowMeans(matHigh, na.rm = TRUE)
      )
      k <- k + 1
    }
  }

  ## ---------------------------
  ## Plot
  ## ---------------------------
  xvals <- fracs # * max(x$cumDuration, na.rm = TRUE)
  ylim  <- c(0, max(sapply(groupList, function(g) max(g, na.rm = TRUE))))

  plot(1, type = "n",
       xlim = c(0, 1),
       ylim = ylim,
       xlab = "Fraction of total observation time",
       ylab = "Mean behavioural richness",
       las = 1)

  for (i in seq_along(groupList)) {
    polygon(c(xvals, rev(xvals)),
            c(groupList[[i]]$groupHigh, rev(groupList[[i]]$groupLow)),
            col = adjustcolor(cols[i], alpha.f = 0.25),
            border = NA)
    lines(xvals, groupList[[i]]$groupMean, col = cols[i], lwd = 2)
  }

  legend("bottomright",
         legend = c("ZOO adult", "ZOO juvenile", "Wild adult", "Wild juvenile"),
         col = cols, lwd = 2, bty = "n")

  invisible(x)
}
