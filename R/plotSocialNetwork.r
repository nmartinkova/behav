#' Plot directed social networks of dyadic interactions
#'
#' This function visualises directed social interaction networks for each
#' elephant group (site or reserve) based on dyadic behaviours recorded
#' in BORIS and processed with \code{mergeBoris()}.
#' Networks are plotted separately for each group to avoid mixing
#' individuals that were never observed together.
#'
#' @inheritParams plotRarefaction
#' @inheritParams calculateDyadicMetrics
#' @param outputFile Character string giving the path and name of the file where the plot
#'  will be saved. The directory is created automatically if
#'   it does not exist.
#' @param whichMatrix Character string specifying which matrix with dyadic interactions
#'   to plot. Default is adjacency.
#'
#' @details
#' If group identifiers in columns \code{zoo}, \code{adult} and \code{group} are missing,
#' the function automatically:
#' \itemize{
#'   \item identifies zoo vs wild individuals,
#'   \item identifies adult vs juvenile individuals,
#'   \item extracts group identity from the \code{animal} name
#'         (text before the first dash),
#'   \item collapses zoo sessions into a single stable group per site,
#'         while keeping separate sessions for wild observations.
#' }
#'
#' For each group, dyadic interaction durations are converted into a
#' directed adjacency matrix using \code{calculateDyadicMetrics()}.
#'
#' In the resulting network plot:
#' \itemize{
#'   \item nodes represent individuals,
#'   \item arrows represent directed interactions (A \eqn{\rightarrow} B),
#'   \item arrow thickness is proportional to the total duration of interaction,
#'   \item node colour represents zoo/wild and adult/juvenile category.
#' }
#'
#' Directed networks allow interpretation of:
#' \itemize{
#'   \item who initiates interactions,
#'   \item whether interactions are asymmetric,
#'   \item potential dominance or caregiving structure.
#' }
#'
#' Groups are plotted in separate panels because animals from different
#' sites were not observed interacting with each other.
#'
#' @return Invisibly returns the adjacency matrix \code{A}.
#' The main output is a set of network plots, one per group.
#'
#' @examples
#' \dontrun{
#' dat <- read.table("Data/elephantBehaviour.txt", header = TRUE, sep = "\t")
#' plotSocialNetwork(dat)
#' # To plot a single site, specify the group in the input data
#' plotSocialNetwork(dat[dat$group == "Namibie 12.11.2025",])
#' }
#'
#' @import igraph
#' @importFrom grDevices pdf dev.off
#' @export


plotSocialNetwork <- function(
  x,
  zoo = c("Zoo", "Beekse", "Safaripark", "Tiergarten"),
  age = c("Mládě", "Zyqarri"),
  cols = c("#3C8ABF", "#A1BCD7", "#768D1A", "#B1BE94"),
  outputFile = NULL,
  whichMatrix = "adjacency",
  dyadic = NULL
) {
  # create folder for output if needed
  if (is.null(outputFile)) outputFile <- "socialNetwork.pdf"
  if (tools::file_ext(outputFile) != "pdf") outputFile <- paste0(tools::file_path_sans_ext(outputFile), ".pdf")
  if (basename(outputFile) != outputFile) {
    if (!dir.exists(dirname(outputFile))) dir.create(dirname(outputFile), recursive = TRUE)
  }

  # identify zoo/wild and adult/juvenile animals
  if (!all(c("zoo", "adult", "group") %in% colnames(x))) {
    x$zoo <- grepl(paste(zoo, collapse = "|"), x$file)
    x$adult <- !grepl(paste(age, collapse = "|"), x$animal)
    x$group <- sub("-.+", "", x$animal)
    x$group[x$zoo] <- sub(" [0-9].+", "", x$group[x$zoo])
  }

  ## colour index: 1 Z-adult, 2 Z-juv, 3 W-adult, 4 W-juv
  groupIndex <- function(zooFlag, adultFlag) {
    1 + (!adultFlag) + 2 * (!zooFlag)
  }


  sites <- sort(unique(x$group))


  pdf(outputFile, width = 4 * ifelse(length(sites) == 1, 1, 2), height = 4 * ceiling(length(sites) / 2))
  par(
    mfrow = c(ceiling(length(sites) / 2), ifelse(length(sites) == 1, 1, 2)),
    mar = c(2, 2, 2, 1) + .3
  )

  ## -------------------------
  ## Loop sites
  ## -------------------------

  for (s in sites) {
    xsub <- x[x$group == s, ]
    xsub$animal <- sub(".+-", "", xsub$animal)
    xsub$partner <- sub(".+-", "", xsub$partner)
    
    if (nrow(xsub) == 0) next

    ## compute dyadic metrics
    dy <- calculateDyadicMetrics(xsub, dyadic = dyadic)
    A <- dy[[whichMatrix]]
    
    animals <- rownames(A)

    ## group colours
    zooFlag <- xsub[match(animals, xsub$animal), "zoo"]
    adultFlag <- xsub[match(animals, xsub$animal), "adult"]
    nodeCol <- cols[groupIndex(zooFlag, adultFlag)]

    ## -------------------------
    ## DIRECTED network
    ## -------------------------
    gdir <- igraph::graph_from_adjacency_matrix(
      A,
      mode = "directed",
      weighted = TRUE,
      diag = FALSE
    )

    plot(gdir,
      edge.arrow.size = 0.3,
      edge.width = igraph::E(gdir)$weight / max(igraph::E(gdir)$weight) * 5,
      vertex.color = nodeCol,
      vertex.size = 18,
      vertex.label.cex = 0.8,
      main = s
    )
  }

  dev.off()
  invisible(A)
}
