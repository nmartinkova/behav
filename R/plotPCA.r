#' @inheritParams plotRarefaction
#'
#' @title PCA of behavioural composition
#'
#' @description
#' Performs a Principal Components Analysis (PCA) on the relative time
#' each elephant spends performing different behaviours, using a
#' Hellinger transformation for proportional data.
#' The function produces a multiframe figure that visualises individual
#' behavioural profiles in PCA space and the contribution of each behaviour
#' to the retained principal components.
#'
#' @details
#' Each individual's behaviour durations are summed and converted to
#' relative proportions of total visible time.
#' The Hellinger transformation (`sqrt(p)`) is applied to reduce the influence
#' of dominant behaviours and make Euclidean distances more appropriate
#' for proportional data.
#'
#' The analysis identifies the main gradients in behavioural composition.
#'
#' The number of principal components shown in the loading heatmap is
#' estimated using the *broken-stick* criterion, which retains components
#' explaining more variance than expected under random partitioning.
#'
#' The function creates three panels:
#' 1. PCA scatter plot of individuals (PC1 vs PC2).
#' 2. Heatmap showing absolute loadings of each behaviour on the retained PCs.
#' 3. Colour legend for loading magnitudes.
#'
#' **Note:** If you receive the error
#' `"Error in plot.new(): figure margins too large"`,
#' enlarge the plotting window or output device (e.g., use `pdf(..., width = 10, height = 12)`
#' or a larger image size) before running the function.
#'
#' @return Invisibly returns a list containing:
#' * `pca` - the PCA result (`prcomp` object)
#' * `data` - the Hellinger-transformed behavioural proportion matrix
#'
#' @examples
#' \dontrun{
#' dat <- read.table("Data/elephantBehaviour.txt", header = TRUE, sep = "\t")
#' res <- plotPCA(dat)
#' # To view variable loadings, use:
#' res$pca
#' # To view the Hellinger-transformed behavioural proportion matrix, use:
#' res$data
#' }
#'
#' @export

plotPCA <- function(
    x,
    zoo = c("Zoo", "Beekse", "Safaripark", "Tiergarten"),
    age = c("Mládě", "Zyqarri"),
    cols = c("#3C8ABF", "#A1BCD7", "#768D1A", "#B1BE94")    
    ) {
  # remove non-behaviour rows
  x <- x[x$Behavior != "Mimo dohled", ]

  # identify zoo/wild and adult/juvenile
  x$zoo <- grepl(paste(zoo, collapse = "|"), x$file)
  x$adult <- !grepl(paste(age, collapse = "|"), x$animal)

  # build behaviour by individual matrix (sum of durations)
  mat <- with(x, tapply(duration, list(animal, Behavior), sum, na.rm = TRUE))
  mat[is.na(mat)] <- 0

  # relative time proportions
  p <- mat / rowSums(mat)

  # Hellinger transformation
  p.hell <- sqrt(p)

  # PCA
  pca <- prcomp(p.hell, scale. = FALSE)

  # group codes
  animals <- rownames(p)
  zooFlag <- x[match(animals, x$animal), "zoo"]
  adultFlag <- x[match(animals, x$animal), "adult"]

  # assign colours
  groupIndex <- 1 + (!adultFlag) + 2 * (!zooFlag)
  # 1 Zoo-adult, 2 Zoo-juvenile, 3 Wild-adult, 4 Wild-juvenile

  # estimate number of PCs to show
  eig <- pca$sdev^2
  # Broken-stick
  n <- length(eig)
  npcs <- sum(eig / sum(eig) > sapply(1:n, function(k) sum(1 / (k:n)) / n))

  # plot
  layout(matrix(c(rep(4, 6), rep(1, 9), rep(4, 6), rep(2, 20), 3), nrow = 2, byrow = TRUE))

  # PCA individual scatter on PC1 and PC2
  par(mar = c(4, 4, 0, 0) + .3)
  plot(pca$x[, 1:2],
    asp = 1,
    col = cols[groupIndex], pch = 19,
    xlab = paste0("PC1 (", round(100 * summary(pca)$importance[2, 1], 1), "%)"),
    ylab = paste0("PC2 (", round(100 * summary(pca)$importance[2, 2], 1), "%)"),
    las = 1
  )
  legend("topleft",
    legend = c("Zoo adult", "Zoo juvenile", "Wild adult", "Wild juvenile"),
    col = cols, pch = 19, bty = "n"
  )

  # PCA interpretation
  par(mar = c(22, 4, 0, 0) + .3)
  image(1:(nrow(pca$rotation) + 1), 1:(npcs + 1), abs(pca$rotation[, 1:npcs]),
    axes = F, xlab = "", ylab = ""
  )
axis(2, at = 1.5:(npcs + .5), labels = colnames(pca)[1:npcs], las = 1)
  axis(1, at = (1:nrow(pca$rotation)) + .5, labels = rownames(pca$rotation), las = 2, cex.axis = .8)
  for (i in 1:nrow(pca$rotation)) {
    for (j in 1:npcs) {
      if (pca$rotation[i, j] > 0) {
        segments(i, j, i + 1, j + 1, col = "grey")
      } else {
        segments(i + 1, j, i, j + 1, col = "grey")
      }
    }
  }
  box()

  # legend
  par(mar = c(10, 0.3, 0, 2.5) + .1)
  cisla <- seq(0, max(abs(pca$rotation[, 1:npcs])), length.out = 100)
  image(1, cisla, matrix(cisla, nrow = 1),
    axes = F, col = rev(hcl.colors(100, "YlOrBr")),
    xlab = "", ylab = ""
  )
  axis(4, las = 1)
  box()




  invisible(list(pca = pca, data = p.hell))
}
