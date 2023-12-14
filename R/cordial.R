#' cordial: A Pleasant Tonic For Parallel Correlation Analysis In R
#'
#' The \code{cordial} package provides functions to compute pairwise Pearson's
#' correlations of a dataset, or specified targets simultaneously in parallel.
#' Conveniently includes the ability to filter the input dataset and select a
#' subset of columns to compute correlations. Outputs Pearson's product moment
#' correlation coefficients, p-values, adjusted p-values, linear model slope
#' and observation counts in long-format.
#'
#' @section \code{cordial} functions:
#' \itemize{
#' \item \code{\link{cor_map}} for correlation analysis of a dataset.
#' \item \code{\link{cor_target}} for correlation analysis of a single target
#' in parallel.
#' \item \code{\link{cor_targets}} for correlation analysis of a single target
#' sequentially.
#' \item \code{\link{cor_target_map}} for correlation analysis of multiple
#' targets in parallel.
#' \item \code{\link{start_parallel}} for parallel processing.
#' \item \code{\link{end_parallel}} for sequential processing.
#' }
#'
#' @section \code{cordial} external data:
#' \itemize{
#' \item \code{\link{rnai_DT}} - Cancer cell line genetic dependencies from RNAi
#' screens of the DepMap project.
#' \item \code{\link{crispr_DT}} - Cancer cell line genetic dependencies from
#' CRISPR-Cas9 screens of the DepMap project.
#' \item \code{\link{cellmeta_DT}} - Cancer cell line metadata of the DepMap
#' project.
#' }
#'
#' @docType package
#' @name cordial
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr separate
#' @importFrom purrr imap
#' @importFrom purrr map2
#' @import future
#' @import furrr
#' @import collapse
#' @import data.table
NULL
#> NULL
