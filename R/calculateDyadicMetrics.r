#' Calculate dyadic interaction metrics between individuals
#'
#' This function summarises social interactions between pairs of animals
#' based on behaviours that involve two individuals (dyadic behaviours).
#' It produces matrices describing directed interaction strength, asymmetry,
#' reciprocity, and partner preference, together with a dyad-level summary
#' table. The outputs help students describe social structure in elephant
#' groups observed with BORIS.
#'
#' @param x A data frame created by \code{mergeBoris()}, containing at least
#'   the columns \code{animal}, \code{Behavior}, \code{duration}, and a column
#'   specifying the interaction partner (default column name \code{"partner"}).
#' @param dyadic Optional character vector listing which behaviours should be
#'   treated as dyadic. If not supplied, a default list of typical dyadic
#'   elephant behaviours is used (touching, entwining trunks, pushing, etc.).
#' @param partnerCol Name of the column in \code{x} that identifies the partner
#'   involved in each dyadic behaviour.
#'
#' @details
#' The input table contains one row per behavioural bout performed by a focal
#' individual. For dyadic behaviours, the student must also supply the partner
#' for each bout (usually exported from BORIS or extracted from the behaviour
#' description). The function keeps only behaviours listed as dyadic and rows
#' where a partner is provided.
#'
#' The function then calculates:
#'
#' \strong{1. Directed adjacency matrix}  
#' The total duration that individual A directed towards individual B.  
#' This describes the strength of one-way interactions.
#'
#' \strong{2. Asymmetry matrix}  
#' The difference between \deqn{A \to B} and \deqn{B \to A}.  
#' Positive values indicate that A interacts more with B than vice versa,
#' which is useful for describing dominance or caregiving asymmetry.
#'
#' \strong{3. Reciprocity matrix}  
#' The level of balance between two individuals:  
#' \deqn{Rec = min(A \to B, B \to A) / max(A \to B, B \to A)}  
#' Values close to 1 indicate mutually exchanged interactions; values near 0
#' show one-sided or unreturned interactions.
#'
#' \strong{4. Partner preference matrix}  
#' For each individual, the adjacency matrix is normalised by row so that all
#' outgoing interactions sum to 1. Each value represents the proportion of an
#' individual's total dyadic time allocated to each partner.  
#' This highlights preferred partners or uneven social focus.
#'
#' \strong{5. Dyadic summary table}  
#' A compact table with one row per pair (A,B), including directed durations,
#' asymmetry, and reciprocity. This format is convenient for interpretation,
#' reporting, or plotting social networks.
#'
#' These metrics allow the student to describe who interacts with whom,
#' whether interactions are balanced or one-sided, and which individuals show
#' stronger partner preferences.
#'
#' @return A list with five components:
#' \itemize{
#'   \item \code{adjacency} - directed interaction durations of A towards B.
#'   \item \code{asymmetry} - (A towards B) minus (B towards A)
#'   \item \code{reciprocity} - dyadic reciprocity values
#'   \item \code{partnerPreference} - row-normalised adjacency (A's preference)
#'   \item \code{dyadicSummary} - table summarising each dyad
#' }
#'
#' @examples
#' \dontrun{
#' dat <- read.table("Data/elephantBehaviour.txt", header = TRUE, sep = "\t")
#' out <- calculateDyadicMetrics(dat, partnerCol = "partner")
#' out$dyadicSummary   # view dyadic table
#' }
#'
#' @export
calculateDyadicMetrics <- function(
    x,
    dyadic = NULL,
    partnerCol = "partner"
) {

if(is.null(dyadic)){
dyadic <- c(
  "Být poblíž jiného jedince",
  "Čichání a dotyk na těle druhého jedince",                            
 "Čichání ke genitáliím druhého jedince",
"Dotyk chobotem do tlamy druhého jedince",
"Hraní si mláděte s jiným mládětem",
"Natažení chobotu k druhému slonovi",
"Odehnání jiného zvířete",                                              
"Odehnání níže postaveného jedince",                                   
"Odhánění níže postaveného jedince (krok , pohození hlavou proti němu)",
"Podrbání se o druhého jedince",
"Položení chobotu nebo hlavy na záda druhého slona",
"Přetlačování samců",
"Samice se nastavuje samci",                                           
 "Slůně saje mléko/slonice kojí mládě",
"Tlačení před sebou, odtlačení nebo štouchání druhého jedince",        
"Ústup  jednoho slona před druhým"
)

}
  ## -------------------------
  ## 1. Filter dyadic rows
  ## -------------------------
  x <- x[x$Behavior %in% dyadic, ]

  if (!partnerCol %in% names(x)) {
    stop("Stlpec '", partnerCol, "' nie je v tabulke. Nema ine meno?")
  }

  if (any(is.na(x[[partnerCol]]))) {
    warning("Niektore dyadicke spravania nemaju uvedeneho partnera. Mazem ich.")
  x <- x[!is.na(x[[partnerCol]]) & x[[partnerCol]] != "", ]

  }

  ## focal and partner IDs
 focal  <- x$animal
  partner <- x[[partnerCol]]

  animals <- sort(unique(c(focal, partner)))

  ## -------------------------
  ## 2. Adjacency matrix (directed)
  ## -------------------------
  A <- matrix(0,
              nrow = length(animals),
              ncol = length(animals),
              dimnames = list(animals, animals))

  # accumulate durations of directed interactions
  for (i in seq_len(nrow(x))) {
    A[focal[i], partner[i]] <- A[focal[i], partner[i]] + x$duration[i]
  }

  ## -------------------------
  ## 3. Asymmetry matrix
  ## asym(A,B) = A -> B – B -> A
  ## -------------------------
  Asym <- A - t(A)

  ## -------------------------
  ## 4. Reciprocity per dyad
  ## Rec = min(A -> B, B -> A) / max(A -> B, B -> A)
  ## -------------------------
  Rec <- matrix(NA,
                nrow = length(animals),
                ncol = length(animals),
                dimnames = list(animals, animals))

  for (a in animals) {
    for (b in animals) {
      if (a == b) next
      v1 <- A[a, b]
      v2 <- A[b, a]
      if (v1 == 0 & v2 == 0) {
        Rec[a, b] <- NA
      } else {
        Rec[a, b] <- min(v1, v2) / max(v1, v2)
      }
    }
  }

  ## -------------------------
  ## 5. Partner preference
  ## row-normalised adjacency
  ## -------------------------
  rowSumsA <- rowSums(A)
  Pref <- sweep(A, 1, rowSumsA, FUN = "/")
  Pref[is.na(Pref)] <- 0

  ## -------------------------
  ## 6. Dyadic summary table
  ## -------------------------
  combos <- which(upper.tri(A), arr.ind = TRUE)

  dyadSummary <- data.frame(
    A = rownames(A)[combos[, 1]],
    B = colnames(A)[combos[, 2]],
    duration_A_to_B = A[combos],
    duration_B_to_A = t(A)[combos],
    asymmetry = Asym[combos],
    reciprocity = round(Rec[combos], 3),
    stringsAsFactors = FALSE
  )

  ## -------------------------
  ## Return everything
  ## -------------------------
  list(
    adjacency = A,
    asymmetry = Asym,
    reciprocity = Rec,
    partnerPreference = Pref,
    dyadicSummary = dyadSummary
  )
}
