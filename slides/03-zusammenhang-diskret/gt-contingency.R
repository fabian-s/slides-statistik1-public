################################################################################
# Colored contingency tables for LaTeX/beamer output
#
# gt_contingency() emits a LaTeX `tabular` from a matrix of (absolute or relative)
# frequencies WITHOUT margins. Optional features:
#   * proportional font sizing keyed to cell values (magnitude at a glance,
#     and a grayscale-print-safe redundant encoding alongside color)
#   * cell background colors keyed to the Pearson residuals
#     (h_ij - expected_ij) / sqrt(expected_ij), i.e. deviation from the
#     independence expectation (RdBu-style diverging scale)
#   * automatic row/column margins
#
# Emit with results='asis': cat(gt_contingency(mat, ...)).
# Requires colortbl in the LaTeX preamble (provided by header.tex).
# Row/column labels are inserted verbatim into LaTeX — escape &, %, _, #
# yourself if labels contain them (German words and $math$ are fine).
################################################################################

#' Create a styled contingency table for LaTeX output
#'
#' @param table_matrix Matrix of frequencies (rows x columns, WITHOUT margins).
#' @param row_names,col_names Optional labels (default: matrix dimnames).
#' @param proportional_size Scale font sizes to cell values?
#' @param size_transform "linear", "sqrt", or a function applied before scaling.
#' @param size_range Numeric c(min, max) font-size range in pt.
#' @param independence_colors Color cells by deviation from independence?
#' @param color_scale Length-3 colors c(below, neutral, above).
#' @param color_intensity Max saturation (0-1).
#' @param show_margins Show marginal totals?
#' @param margin_size_factor Font scaling for margins relative to cells.
#' @param digits Decimal places (NULL for integers).
#' @return A LaTeX `tabular` string.
gt_contingency <- function(
  table_matrix,
  row_names = NULL,
  col_names = NULL,
  proportional_size = TRUE,
  size_transform = "sqrt",
  size_range = c(8, 18),
  independence_colors = TRUE,
  color_scale = c("#D73027", "#F7F7F7", "#4575B4"),
  color_intensity = 0.7,
  show_margins = TRUE,
  margin_size_factor = 0.85,
  digits = NULL
) {
  if (!is.matrix(table_matrix)) table_matrix <- as.matrix(table_matrix)
  stopifnot(
    is.numeric(table_matrix),
    all(is.finite(table_matrix)),
    all(table_matrix >= 0),
    sum(table_matrix) > 0
  )

  if (is.null(row_names)) row_names <- rownames(table_matrix)
  if (is.null(col_names)) col_names <- colnames(table_matrix)
  if (is.null(row_names))
    row_names <- paste0("Row", seq_len(nrow(table_matrix)))
  if (is.null(col_names))
    col_names <- paste0("Col", seq_len(ncol(table_matrix)))

  row_sums <- rowSums(table_matrix)
  col_sums <- colSums(table_matrix)
  total <- sum(table_matrix)

  # expected frequencies under independence + Pearson residuals
  expected <- outer(row_sums, col_sums) / total
  pearson_resid <- (table_matrix - expected) / sqrt(expected)

  # wrap a value in a LaTeX font-size command matching a pt size
  latex_size <- function(value, size_pt) {
    if (!proportional_size) return(as.character(value))
    if (size_pt < 7) {
      paste0("{\\tiny ", value, "}")
    } else if (size_pt < 9) {
      paste0("{\\scriptsize ", value, "}")
    } else if (size_pt < 11) {
      paste0("{\\footnotesize ", value, "}")
    } else if (size_pt < 13) {
      paste0("{\\small ", value, "}")
    } else if (size_pt < 15) {
      paste0("{\\normalsize ", value, "}")
    } else if (size_pt < 17) {
      paste0("{\\large ", value, "}")
    } else if (size_pt < 20) {
      paste0("{\\Large ", value, "}")
    } else {
      paste0("{\\LARGE ", value, "}")
    }
  }

  trans_fn <- if (is.character(size_transform)) {
    switch(
      size_transform,
      "linear" = function(x) x,
      "sqrt" = sqrt,
      function(x) x
    )
  } else {
    size_transform
  }

  trans_vals <- trans_fn(as.vector(table_matrix))
  trans_range <- range(trans_vals, na.rm = TRUE)

  normalize_size <- function(val) {
    trans_val <- trans_fn(val)
    denom <- trans_range[2] - trans_range[1]
    if (denom == 0) return(mean(size_range))
    raw <- size_range[1] +
      (trans_val - trans_range[1]) / denom * (size_range[2] - size_range[1])
    # clamp: margins (row/col/total sums) lie outside the cell-value range and
    # would otherwise extrapolate to huge font sizes
    min(max(raw, size_range[1]), size_range[2])
  }

  max_abs_resid <- max(abs(pearson_resid), na.rm = TRUE)

  # background color for a Pearson residual (NULL = no color)
  get_color <- function(resid) {
    if (!independence_colors) return(NULL)
    if (is.na(resid) || !is.finite(resid) || max_abs_resid == 0) return(NULL)
    norm_resid <- resid / max_abs_resid * color_intensity
    if (abs(norm_resid) < 0.01) {
      NULL
    } else if (norm_resid < 0) {
      ratio <- abs(norm_resid)
      grDevices::colorRampPalette(c(color_scale[2], color_scale[1]))(100)[
        round(ratio * 99) + 1
      ]
    } else {
      ratio <- norm_resid
      grDevices::colorRampPalette(c(color_scale[2], color_scale[3]))(100)[
        round(ratio * 99) + 1
      ]
    }
  }

  fmt_value <- function(x) {
    if (!is.null(digits)) format(round(x, digits), nsmall = digits) else
      as.character(round(x))
  }

  # format one cell: value -> size command -> optional \cellcolor
  format_cell <- function(val, resid = NULL, size_factor = 1) {
    formatted_val <- latex_size(
      fmt_value(val),
      normalize_size(val) * size_factor
    )
    bg_color <- if (!is.null(resid)) get_color(resid) else NULL
    if (!is.null(bg_color)) {
      rgb_vals <- grDevices::col2rgb(bg_color) / 255
      sprintf(
        "\\cellcolor[rgb]{%.3f,%.3f,%.3f}%s",
        rgb_vals[1],
        rgb_vals[2],
        rgb_vals[3],
        formatted_val
      )
    } else {
      formatted_val
    }
  }

  # body cells
  body_rows <- character(nrow(table_matrix))
  for (i in seq_len(nrow(table_matrix))) {
    cells <- character(ncol(table_matrix))
    for (j in seq_len(ncol(table_matrix))) {
      cells[j] <- format_cell(table_matrix[i, j], pearson_resid[i, j])
    }
    row_data <- c(row_names[i], cells)
    if (show_margins) {
      row_data <- c(
        row_data,
        format_cell(row_sums[i], size_factor = margin_size_factor)
      )
    }
    body_rows[i] <- paste(row_data, collapse = " & ")
  }

  header <- paste(
    c("", col_names, if (show_margins) "$\\sum$" else NULL),
    collapse = " & "
  )

  if (show_margins) {
    col_margin <- vapply(
      col_sums,
      format_cell,
      character(1),
      size_factor = margin_size_factor
    )
    total_val <- format_cell(total, size_factor = margin_size_factor)
    margin_row_latex <- paste(
      c("$\\sum$", col_margin, total_val),
      collapse = " & "
    )
  }

  n_total_cols <- ncol(table_matrix) + if (show_margins) 1 else 0
  col_spec <- paste0("l|", paste(rep("c", n_total_cols), collapse = ""))

  paste0(
    "\\begin{tabular}{",
    col_spec,
    "}\n",
    header,
    " \\\\\n",
    "\\hline\n",
    paste(body_rows, collapse = " \\\\\n"),
    " \\\\\n",
    if (show_margins) paste0("\\hline\n", margin_row_latex, " \\\\\n") else "",
    "\\end{tabular}"
  )
}
