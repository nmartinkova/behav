#' Plot behavioural rarefaction curves from BORIS observations
#'
#' This function visualises how the number of distinct behaviours increases with observation time
#' for different groups of elephants observed in zoos and in the wild. It uses simple bootstrap
#' resampling to estimate mean behavioural richness and 95% confidence intervals for each group.
#' The function is intended for student use and provides an intuitive comparison of behavioural
#' diversity between animals of different origin and age class.
#'
#' @param x A data frame created by [mergeBoris()], containing columns `animal`, `Behavior`,
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
#' The function divides the dataset into four groups:
#' zoo adults, zoo juveniles, wild adults, and wild juveniles.
#' For each group, it repeatedly resamples the behavioural observations and counts
#' how many unique behaviours are seen at different fractions of the total observation time.
#' The results are averaged across individuals and displayed as rarefaction curves with
#' shaded 95% confidence intervals.
#'
#' This analysis helps visualise how quickly new behaviours appear during observation,
#' and compare overall behavioural richness among groups.
#'
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
plotRarefaction <- function(x, zoo = c("Zoo", "Beekse", "Safaripark", "Tiergarten"), age = c("Mládě", "Zyqarri"), cols = c("#3C8ABF", "#A1BCD7", "#768D1A", "#B1BE94"), nBoot = 100) {
  # find zoo animals and juveniles
  x$zoo <- grepl(paste(zoo, collapse = "|"), x$file)
  x$adult <- !grepl(paste(age, collapse = "|"), x$animal)

  fracs <- seq(0.01, 1, length.out = 20)

  getRichness <- function(dat, frac) {
    n <- nrow(dat)

    if (n == 0) {
      return(NA)
    }

    sampled <- dat[sample(seq_len(n),
      size = floor(frac * n),
      replace = TRUE
    ), ]

    return(length(unique(sampled$Behavior)))
  }

  groupList <- list()
  k <- 1

  for (z in c(TRUE, FALSE)) { # ZOO, wild
    for (a in c(TRUE, FALSE)) { # adult, juvenile
      inds <- unique(x$animal[x$zoo == z & x$adult == a])

      groupBoot <- array(NA, dim = c(nBoot, length(fracs), length(inds)))

      for (i in seq_along(inds)) {
        datInd <- x[x$animal == inds[i], ]
        for (f in seq_along(fracs)) {
          groupBoot[, f, i] <- replicate(nBoot, getRichness(datInd, fracs[f]))
        }
      }

      # mean richness per individual then group-level mean and CI
      groupMean <- apply(groupBoot, 2, function(y) mean(y, na.rm = TRUE))
      groupLow <- apply(groupBoot, 2, function(y) quantile(y, 0.025, na.rm = TRUE))
      groupHigh <- apply(groupBoot, 2, function(y) quantile(y, 0.975, na.rm = TRUE))

      groupList[[k]] <- data.frame(groupMean, groupLow, groupHigh)
      k <- k + 1
    }
  }

  plot(1,
    type = "n",
    xlim = c(0, max(x$cumDuration, na.rm = TRUE)),
    ylim = c(0, max(unlist(groupList), na.rm = TRUE)),
    xlab = "Cumulative observation time (s)",
    ylab = "Mean behavioural richness",
    las = 1
  )
  # shading for CI
  for (i in seq_along(groupList)) {
    xvals <- fracs * max(x$cumDuration, na.rm = TRUE)
    polygon(c(xvals, rev(xvals)),
      c(groupList[[i]]$groupHigh, rev(groupList[[i]]$groupLow)),
      col = adjustcolor(cols[i], alpha.f = 0.25),
      border = NA
    )

    # mean line
    lines(xvals, groupList[[i]]$groupMean, col = cols[i], lwd = 2)
  }
  legend("bottomright",
    legend = c("ZOO adult", "ZOO juvenile", "Wild adult", "Wild juvenile"),
    col = cols, lwd = 2, bty = "n"
  )

  invisible(x)
}
