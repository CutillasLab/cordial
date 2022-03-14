#' Cancer cell line genetic dependencies from RNAi screens
#'
#' A dataset of cancer dependencies from the Cancer Dependency Map (DepMap)
#' project - a collaboration of the Broad Institute (Cambridge, Massachusetts,
#' USA) and the Wellcome Sanger Institute (Hinxton, Cambridgeshire, UK).
#'
#' Cancer cell line genetic dependencies estimated using the DEMETER2 model
#' applied to three large-scale RNAi screening datasets: the Broad Institute
#' Project Achilles, Novartis Project DRIVE, and the Marcotte et al. breast
#' cell line dataset.
#'
#' Dataset posted on 09.04.2020, 18:06 by Cancer Data Science.
#'
#' @format A \code{data.table} with 711 rows (cell lines), and 16,816 columns
#' (6 cell line metadata, 16,810 genes):
#' \describe{
#'   \item{\strong{depmap_id}}{Cell line DepMap ID. The \code{data.table} \code{key}}
#'   \item{\strong{cell_line_display_name}}{Cell line display name}
#'   \item{\strong{lineage_1}}{Cancer cell line lineage}
#'   \item{\strong{lineage_2}}{Cancer cell line lineage subtype}
#'   \item{\strong{lineage_3}}{Cancer cell line lineage sub-subtype}
#'   \item{\strong{lineage_4}}{Cancer cell line lineage molecular subtype}
#'   \item{\strong{...}}{Genes}
#' }
#'
#' @source \url{https://figshare.com/articles/dataset/DEMETER2_data/6025238}
"rnai_DT"
