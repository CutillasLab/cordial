#' Correlation analysis of a single target sequentially
#'
#' Computes pairwise Pearson's correlations sequentially for a single target
#' column of a dataset using \code{\link[stats:cor.test]{cor.test}}, with the
#' ability to filter the input dataset and select a subset of columns to
#' compute correlations. Returns a long-format
#' \code{\link[data.table:data.table]{data.table}} of Pearson's product moment
#' correlation coefficients, p-values and adjusted p-values.
#'
#' @section Subset:
#' \code{\link{cor_targets}} conveniently allows filtering (\code{filter_rows})
#' of the input \code{dataset} by performing a cross-join
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
#' \code{\link{cor_targets}} uses \code{\link[stats:cor.test]{cor.test}} to test
#' for an association between paired samples computed with Pearson's product
#' moment correlation coefficient. Correlations are computed with incomplete
#' cases removed
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
#' @section Utilisation:
#' \code{\link{cor_targets}} is a sequential variant of
#' \code{\link{cor_target}} and differs from \code{\link{cor_map}} in that
#' pairwise correlations are computed for a single specified \code{target}
#' column, with correlations limited to the columns specified in
#' \code{select_cols}; whereas \code{\link{cor_map}} computes correlations for
#' all pairs of columns specified in \code{select_cols}.
#' \code{\link{cor_target_map}} varies from \code{\link{cor_targets}} in that it
#' allows specifying multiple \code{target}s.
#'
#' @param dataset A \code{\link[data.table:data.table]{data.table}}. Must be in
#' column-major order.
#'
#' @param target A character scalar. A column name in \code{dataset} to compute
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
#' @return A \code{\link[data.table:data.table]{data.table}} in long-format of
#' Pearson's product moment correlation coefficients (\code{r}), p-values
#' (\code{p}) and adjusted p-values (\code{q}) for pairwise Pearson's
#' correlations.
#'
#' @seealso \itemize{
#' \item \code{\link{cor_target_map}} for correlation analysis of multiple
#' targets in parallel.
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
#' # Correlation analysis
#' cor_target(mtcars_DT, "mpg", select_cols = colnames(mtcars_DT[, !("id")]))
cor_targets <- function(
  dataset,
  target,
  select_cols = colnames(dataset),
  filter_rows = NULL,
  metadata = NULL,
  self = "yes",
  method = "BH"
) {

  # Pearson correlation analysis for a single target (sequential)
  # Is called by `cor_target_map()`

  # Load packages
  require(magrittr)
  require(purrr)
  require(collapse)
  require(data.table)


  # Argument error checks

  if (isFALSE(data.table::is.data.table(dataset))) {
    stop(
      '
      Error: `dataset` must be a `data.table`:
      ℹ `dataset` has been supplied.
      ✖ `dataset` is not a `data.table`.
      ',
      call. = FALSE
    )
  }

  if (any(!(is.character(target))) || (any(target %!in% colnames(dataset)))) {
    stop(
      '
      Error: `target` must index within `dataset`:
      ℹ `target` must be of type "character".
      ℹ `target` must be column names in `dataset`.
      ✖ `target` does not index within `dataset`.
      ',
      call. = FALSE
    )
  }

  if ((length(self) > 1L) || (!(self %in% c("yes", "no")))) {
    stop(
      '
      Error: `self` must be a single value:
      ℹ `self` must be single value.
      ℹ `self` must be one from `c("yes", "no")`.
      ✖ `self` is incorrect.
      ',
      call. = FALSE
    )
  }

  if (!is.null(metadata)) {
    if (isFALSE(data.table::is.data.table(metadata))) {
      stop(
        '
      Error: `metadata` must be a `data.table`:
      ℹ `metadata` has been supplied.
      ✖ `metadata` is not a `data.table`.
      ',
      call. = FALSE
      )
    }
    if (attr(dataset, "sorted") != attr(metadata, "sorted")) {
      stop(
        '
        Error: `dataset` and `metadata` must be "sorted" by the same column:
        ℹ Did you mean to input separate `metadata`?
        ℹ Set key with `data.table::setkey(x, ...)`.
        ✖ `dataset` and `metadata` attribute "sorted" is not identical.
        ',
        call. = FALSE
      )
    }
    if (any(select_cols %!in% colnames(metadata))) {
      stop(
        '
        Error: `select_cols` must index within `metadata`:
        ℹ `metadata` has been supplied.
        ✖ `select_cols` does not index within `metadata`.
        ',
        call. = FALSE
      )
    }
  }

  if (!is.null(filter_rows)) {
    if ((!is.list(filter_rows)) || (!all(nzchar(names(filter_rows))))) {
      stop(
        '
        Error: `filter_rows` must be a named list:
        ℹ `filter_rows` must be a list.
        ℹ All elements of `filter_rows` must be named.
        ✖ `filter_rows` is not a named list.
        ',
        call. = FALSE
      )
    }
    if (is.null(metadata)) {
      if (any(select_cols %!in% colnames(dataset))) {
        stop(
          '
          Error: `select_cols` must index within `dataset`:
          ℹ `filter_rows` has been supplied.
          ✖ `select_cols` does not index within `dataset`.
          ',
          call. = FALSE
        )
      }
    } else if (any(select_cols %!in% colnames(metadata))) {
      stop(
        '
        Error: `select_cols` must index within `metadata`:
        ℹ `filter_rows` has been supplied.
        ℹ `metadata` has been supplied.
        ✖ `select_cols` does not index within `metadata`.
        ',
        call. = FALSE
      )
    }
  }


  # Subset dataset
  if (!is.null(filter_rows)) {
    if (is.null(metadata)) {
      # Filter using dataset
      dataset <- dataset[
        do.call(CJ, filter_rows),
        .SD,
        .SDcols = select_cols,                      # Select columns for analysis
        on = names(filter_rows),
        nomatch = NULL                              # Omit non-matching rows
      ]
    } else {
      # Filter using metadata
      dataset <- dataset[
        metadata[
          do.call(CJ, filter_rows),
          .SD,                                      # Only `key` is returned to join as
          .SDcols = data.table::key(metadata),      # subset with dataset on shared key
          on = names(filter_rows),
          nomatch = NULL                            # Omit non-matching rows
        ],
        .SD,
        .SDcols = select_cols,                      # Select columns for analysis
        on = data.table::key(dataset),              # Join on `key`
        nomatch = NULL                              # Omit non-matching rows
      ]
    }
  } else {
    dataset <- dataset[, select_cols, with = FALSE] # Select columns
  }


  try({
    # Correlation analysis
    # (Sequential for the subsequent parallel mapping of multiple targets)
    result_cor <-
      dataset[, purrr::map2(
        .f = function(.x, .y) {
          cor.test(.x, .y,
                   method = "pearson", na.action = "na.omit"
          )[c("estimate", "p.value")]
        },
        .x = .SD, .y = list(get(target))
      ),
      .SDcols = select_cols
      ]

    # Create output data.table (long format)
    result_DT <- data.table::data.table(
      Target = target,
      Correlation = colnames(result_cor),
      r = unlist(result_cor[1L], use.names = FALSE),
      p = unlist(result_cor[2L], use.names = FALSE)
    )

    # Calculate adjusted p-value
    result_DT[, `:=`(q = p.adjust(p, method = (method)))]

    if (!is.null(filter_rows)) {
      # Include filters used
      result_DT <- cbind(result_DT, collapse::qDT(filter_rows)) %>%
        {
          .[
            , names(filter_rows)
            := purrr::imap(
              .x = filter_rows,
              .f = ~ paste(filter_rows[[.y]], collapse = ", ")
            )
          ]
        }
    }

    # Order output
    data.table::setorder(result_DT, Target, q)

    # Return output
    if (self == "yes") {
      # Include self-correlation
      # Fill NA values for self-correlations
      data.table::setnafill(
        result_DT,
        type = "const", fill = 0.0, nan = NA, cols = c("p", "q")
      )
      return(result_DT)
    } else if (self == "no") {
      # Exclude self-correlation
      return(result_DT[!(Target == Correlation)])
    }
  })
}
