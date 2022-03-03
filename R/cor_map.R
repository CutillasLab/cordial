#' Correlation analysis of a dataset
#'
#' Computes pairwise Pearson's correlations of a dataset using
#' \code{\link[collapse:pwcor]{collapse::pwcor}}, with the ability to filter
#' and subset the input. Returns a long-format
#' \code{\link[data.table:data.table]{data.table}} of Pearson's product moment
#' correlation coefficients, p-values, adjusted p-values, and observation
#' counts.
#'
#' @section Subset:
#' \code{\link{cor_map}} conveniently allows filtering (\code{filter_rows}) of
#' the input \code{dataset} by performing a cross-join
#' (\code{\link[data.table:CJ]{CJ}}) with a named \code{\link[base:list]{list}}
#' referring to values present within the \code{dataset} itself, or a separate
#' \code{metadata} \code{\link[data.table:data.table]{data.table}}. If the
#' \code{dataset} contains non-numeric columns they must be omitted by
#' selecting (\code{select_cols}) the columns to compute pairwise correlations.
#' This mechanism also allows limiting of the correlations to perform. The
#' subsetting algorithm is identical in \code{\link{cor_target}} and
#' \code{\link{cor_target_map}}.
#'
#' @section Correlation analysis:
#' \code{\link{cor_map}} uses \code{\link[collapse:pwcor]{collapse::pwcor}} to
#' test for an association between paired samples computed with Pearson's
#' product moment correlation coefficient (same as
#' \code{\link[stats:cor.test]{cor.test}}). Correlations are computed on
#' complete pairs of observations for each pair of variables (same as
#' \code{\link[stats:cor]{cor(..., use = "pairwise.complete.obs")}}).
#'
#' Adjusted p-values are computed with \code{\link[stats:p.adjust]{p.adjust}}
#' using the one of the \code{\link[stats:p.adjust.methods]{p.adjust.methods}}:
#' \code{c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr",
#' "none")}. Default \code{"BH"} (alias \code{"fdr"}) is the Benjamini &
#' Hochberg (1995) false discovery rate multiple testing adjustment method.
#'
#' @section Output:
#' A \code{\link[data.table:data.table]{data.table}} in long-format is returned
#' with no pairwise duplicates. If \code{filter_rows} has been supplied, the
#' filters are included.
#'
#' @param dataset A \code{\link[data.table:data.table]{data.table}}. Must be in
#' column-major order.
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
#' of Pearson's product moment correlation coefficients (\code{r}), p-values
#' (\code{p}), adjusted p-values (\code{q}), and observation counts
#' (\code{n}) for pairwise Pearson's correlations.
#'
#' @seealso \itemize{
#' \item \code{\link{cor_target}} for correlation analysis of a single target.
#' \item \code{\link{cor_target_map}} for correlation analysis of multiple targets.
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
#' cor_map(mtcars_DT, select_cols = colnames(mtcars_DT[, !("id")]))
cor_map <- function(
  dataset,
  select_cols = colnames(dataset),
  filter_rows = NULL,
  metadata = NULL,
  self = "yes",
  method = "BH"
) {

  # Pearson correlation analysis for an entire dataset

  # Load packages
  require(magrittr)
  require(purrr)
  require(tidyr)
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
        Error: `select_cols` must index within `metadata`.
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
          Error: `select_cols` must index within `dataset`.
          ℹ `filter_rows` has been supplied.
          ✖ `select_cols` does not index within `dataset`.
          ',
          call. = FALSE
        )
      }
    } else if (any(select_cols %!in% colnames(metadata))) {
      stop(
        '
        Error: `select_cols` must index within `metadata`.
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
    dataset <- dataset[, (select_cols)]             # Select columns
  }


  try({
    # Correlation analysis
    cor_result <- collapse::pwcor(
      dataset,
      N = TRUE, P = TRUE, array = TRUE,
      use = "pairwise.complete.obs"
    )

    # Create output data.table (long format)
    result_DT <- data.table::transpose(
      collapse::qDT(aperm(cor_result)),
      keep.names = "Correlation"
    ) %>%
      tidyr::separate(.,
               col = 1L, into = c("Correlation", "Target"), sep = "[.]"
      ) %>%
      data.table::setnames(.,
               old = c("V1", "V2", "V3"),
               new = c("r", "n", "p")
      ) %>%
      data.table::setcolorder(.,
                  c("Target", "Correlation", "n", "r", "p")
      ) %>%
      {
        unique(
          .[, c("Target", "Correlation") :=
              list(
                pmin(Target, Correlation),
                pmax(Target, Correlation)
              )
          ],
          by = c("Target", "Correlation")
        )
      }

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
