#' Correlation analysis of a single target
#'
#' Computes pairwise Pearson's correlations of a single target using
#' \code{\link[stats:cor.test]{cor.test}}, with the ability to filter and
#' subset the input. Returns a long-format
#' \code{\link[data.table:data.table]{data.table}} of Pearson's product moment
#' correlation coefficients, p-values and adjusted p-values.
#'
#' @param dataset
#' @param target
#' @param select_cols
#' @param filter_rows
#' @param metadata
#' @param self
#' @param method
#'
#' @return
#' @export
#'
#' @examples
cor_target <- function(
  dataset,
  target,
  select_cols = colnames(dataset),
  filter_rows = NULL,
  metadata = NULL,
  self = "yes",
  method = "BH"
) {

  # Pearson correlation analysis for a single target (in parallel)

  # Load packages
  require(magrittr)
  require(purrr)
  require(future)
  require(furrr)
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
    dataset <- dataset[, (select_cols)]             # Select columns
  }


  try({
    # Correlation analysis
    future_cor <- future::future(
      {
        dataset[, furrr::future_map2(
          .f = function(.x, .y) {
            cor.test(.x, .y,
                     method = "pearson", na.action = "na.omit"
            )[c("estimate", "p.value")]
          },
          .x = .SD, .y = .(get(target)),
          .options = furrr::furrr_options(globals = FALSE)
        ),
        .SDcols = select_cols
        ]
      },
      packages = c("furrr", "data.table"),
      globals = list(
        dataset = dataset,
        target = target,
        select_cols = select_cols
      )
    )
    value_cor <- future::value(future_cor)

    # Create output data.table (long format)
    result_DT <- data.table::data.table(
      Target = target,
      Correlation = colnames(value_cor),
      r = unlist(value_cor[1L], use.names = FALSE),
      p = unlist(value_cor[2L], use.names = FALSE)
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
