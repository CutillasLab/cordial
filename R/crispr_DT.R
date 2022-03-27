#' Cancer cell line genetic dependencies from CRISPR-Cas9 screens
#'
#' A dataset of cancer dependencies from the Cancer Dependency Map (DepMap)
#' project - a collaboration of the Broad Institute (Cambridge, Massachusetts,
#' USA) and the Wellcome Sanger Institute (Hinxton, Cambridgeshire, UK).
#'
#' Cancer cell line genetic dependencies estimated using the CERES model
#' applied to the Avana library CRISPR-Cas9 genome-scale knockout (prefixed
#' with Achilles).
#'
#' \strong{Dataset posted on 01.02.2021, 20:21 by Broad DepMap.}
#'
#' @format A \code{data.table} with 945 rows (cell lines), and 17,648 columns
#' (6 cell line metadata, 17,642 genes):
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
#' @source \url{https://doi.org/10.6084/m9.figshare.13681534.v1}
"crispr_DT"
