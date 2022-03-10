#' Correlation analysis of multiple targets in parallel
#'
#' Computes pairwise Pearson's correlations in parallel for multiple target
#' columns of a dataset using \code{\link[stats:cor.test]{cor.test}}, with the
#' ability to filter the input dataset and select a subset of columns to
#' compute correlations. Returns a long-format
#' \code{\link[data.table:data.table]{data.table}} of Pearson's product moment
#' correlation coefficients, p-values and adjusted p-values.
#'
#' @section Subset:
#' \code{\link{cor_target_map}} conveniently allows filtering
#' (\code{filter_rows}) of the input \code{dataset} by performing a cross-join
#' (\code{\link[data.table:CJ]{CJ}}) with a named \code{\link[base:list]{list}}
#' referring to values present within the \code{dataset} itself, or a separate
#' \code{metadata} \code{\link[data.table:data.table]{data.table}}. If the
#' \code{dataset} contains non-numeric columns they must be omitted by
#' selecting (\code{select_cols}) the columns to compute pairwise correlations.
#' This mechanism also allows limiting of the correlations to perform. The
#' subsetting algorithm is identical to that in \code{\link{cor_target}}
#' and \code{\link{cor_map}}.
#'
#' @section Correlation analysis:
#' \code{\link{cor_target_map}} uses \code{\link[stats:cor.test]{cor.test}} to
#' test for an association between paired samples computed with Pearson's
#' product moment correlation coefficient. Correlations are computed with
#' incomplete cases removed
#' (\code{\link[stats:cor.test]{cor.test(..., na.action = "na.omit")}}); see
#' \code{\link[stats:na.omit]{na.omit}}.
#'
#' Adjusted p-values are computed with \code{\link[stats:p.adjust]{p.adjust}}
#' using one of the \code{\link[stats:p.adjust.methods]{p.adjust.methods}}:
#' \code{c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr",
#' "none")}. Default \code{"BH"} (alias \code{"fdr"}) is the Benjamini &
#' Hochberg (1995) false discovery rate multiple testing adjustment method.
#'
#' @section Output:
#' A \code{\link[data.table:data.table]{data.table}} in long-format is returned
#' with no pairwise duplicates: \eqn{corr(X,Y)} without \eqn{corr(Y,X)}. If
#' \code{filter_rows} has been supplied, the filters are included.
#'
#' @section Parallelisation:
#' \code{\link{cor_target_map}} computes correlations in parallel if an
#' asynchronous \code{\link[future:plan]{future::plan}} is set prior to
#' executing \code{\link{cor_target_map}}. See \code{\link{start_parallel}}.
#'
#' \code{\link[furrr:future_map]{furrr::future_map}} is used to map
#' simultaneously in parallel each element in \code{target} for processing via
#' \code{\link[stats:cor.test]{cor.test}}. Specifically,
#' \code{\link{cor_target_map}} by default maps \code{\link{cor_targets}} (a
#' sequential variant of \code{\link{cor_target}}) to avoid nested parallel
#' operations: the nested parallel operations both attempt to utilise the full
#' complement of CPUs/cores which would result in inefficient load balancing.
#'
#' @section Utilisation:
#' \code{\link{cor_target_map}} differs from \code{\link{cor_map}} in that
#' pairwise correlations are computed for multiple specified \code{target}
#' columns, with correlations limited to the columns specified in
#' \code{select_cols}; whereas \code{\link{cor_map}} computes correlations for
#' all pairs of columns specified in \code{select_cols}.
#' \code{\link{cor_target_map}} varies from \code{\link{cor_target}} in that it
#' allows specifying multiple \code{target}s.
#'
#' @param dataset A \code{\link[data.table:data.table]{data.table}}. Must be in
#' column-major order.
#'
#' @param target A character vector. Column names in \code{dataset} to compute
#' correlations with (specified in \code{select_cols}), which must be of type
#' numeric.
#'
#' @param select_cols A vector of column names (character), or indices
#' (numeric). The columns to use for computing correlations, which must be of
#' type numeric.
#'
#' @param filter_rows A named \code{\link[base:list]{list}}. Values specify
#' which rows to subset. Names correspond to column names in \code{dataset}, or
#' \code{metadata} if supplied.
#'
#' @param metadata A \code{\link[data.table:data.table]{data.table}}. Must be
#' in column-major order. Optional input containing data to filter
#' \code{dataset} by. If supplied, \code{metadata} and \code{dataset} must
#' possess the same \code{\link[data.table:setkey]{key}} column.
#'
#' @param self A character scalar. Self-correlations are included if
#' \code{"yes"} (default), or omitted if \code{"no"}.
#'
#' @param method A character scalar. Correction method for p-value adjustment,
#' passed to \code{\link[stats:p.adjust]{p.adjust}}. One of: \code{c("holm",
#' "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")}; \code{"BH"}
#' (alias \code{"fdr"}) (default).
#'
#' @param fun A function. Currently, only compatible with
#' \code{\link[cordial:cor_targets]{cordial::cor_targets}} (default), or
#' \code{\link[cordial:cor_target]{cordial::cor_target}}. See Parallelisation.
#'
#' @return A \code{\link[data.table:data.table]{data.table}} in long-format of
#' Pearson's product moment correlation coefficients (\code{r}), p-values
#' (\code{p}) and adjusted p-values (\code{q}) for pairwise Pearson's
#' correlations.
#'
#' @seealso \itemize{
#' \item \code{\link{cor_target}} for correlation analysis of a single target
#' in parallel.
#' \item \code{\link{cor_map}} for correlation analysis of a dataset.
#' \item \code{\link{start_parallel}} for parallel processing.
#' }
#'
#' @export
#'
#' @examples
#' # Input
#' mtcars_DT <- data.table::as.data.table(mtcars, keep.rownames = "id")
#' data.table::setkey(mtcars_DT, id)
#'
#' # Vector of targets
#' vec_tar <- c("mpg", "cyl", "disp", "hp", "drat", "wt")
#'
#' # Correlation analysis
#' cor_target_map(mtcars_DT, vec_tar, select_cols = colnames(mtcars_DT[, !("id")]))
cor_target_map <- function(
  dataset,
  target,
  select_cols = colnames(dataset),
  filter_rows = NULL,
  metadata = NULL,
  self = "yes",
  method = "BH",
  fun = cordial::cor_targets
) {

  # Pearson correlation analysis for a vector of targets (in parallel)
  # Calls `cor_targets()`

  # Load packages
  require(magrittr)
  require(purrr)
  require(future)
  require(furrr)
  require(collapse)
  require(data.table)

  # Argument error check
  if (!(is.character(target)) || (length(target) <= 1L)) {
    stop(
      '
      Error: `target` must be a "character" vector:
      ℹ `target` must be of type "character".
      ℹ `target` must be a vector of length > 1.
      ℹ Did you mean to supply a single target?
      ℹ Use `cor_target()` instead for a single target.
      ✖ `target` is not a "character" vector.
      ',
      call. = FALSE
    )
  }

  # Map in parallel
  future_cor <- future::future(
    {
      furrr::future_map(
        .x = target,
        .f = ~ fun(
          dataset = dataset,
          target = .x,
          select_cols = select_cols,
          filter_rows = filter_rows,
          metadata = metadata,
          self = self,
          method = method
        ),
        .options = furrr::furrr_options(globals = FALSE)
      )
    },
    packages = c("magrittr", "purrr", "furrr", "collapse", "data.table", "cordial"),
    globals = list(
      dataset = dataset,
      target = target,
      select_cols = select_cols,
      filter_rows = filter_rows,
      metadata = metadata,
      self = self,
      method = method,
      fun = fun
    )
  )
  value_cor <- future::value(future_cor)

  # Create single output data.table
  # (excluding 'try errors')
  result_DT <- Filter(
    function(x) data.table::is.data.table(x), value_cor
  ) %>%
    data.table::rbindlist()

  # Return output
  return(result_DT)
}
