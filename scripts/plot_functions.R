# Shared plotting helpers for LSOPC_RAGE_Study
# These functions format existing results; they do not perform PCoA or LEfSe analysis.

# Apply a consistent, publication-friendly ggplot2 theme.
publication_theme <- function(base_size = 11) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install ggplot2 to use publication_theme().")
  }
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(colour = "black"),
      axis.text = ggplot2::element_text(colour = "black"),
      legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}

# Summarize independent observations by group (mean, SD, n, SEM).
# Technical replicates should be aggregated before using this helper.
summarize_by_group <- function(data, group_col, value_col) {
  stopifnot(is.data.frame(data), group_col %in% names(data),
            value_col %in% names(data))
  group <- data[[group_col]]
  value <- data[[value_col]]
  keep <- !is.na(group) & is.finite(value)
  split_values <- split(value[keep], group[keep], drop = TRUE)
  if (!length(split_values)) stop("No valid observations found.")
  result <- data.frame(
    group = names(split_values),
    n = vapply(split_values, length, integer(1)),
    mean = vapply(split_values, mean, numeric(1)),
    sd = vapply(split_values, function(x) if (length(x) > 1) stats::sd(x) else NA_real_, numeric(1)),
    row.names = NULL
  )
  result$sem <- result$sd / sqrt(result$n)
  result
}

# Export an existing ggplot to PDF; this does not recompute any analysis.
save_figure_pdf <- function(plot, filename, width = 6, height = 4) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install ggplot2 to use save_figure_pdf().")
  }
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(filename = filename, plot = plot,
                  width = width, height = height, units = "in", device = "pdf")
  invisible(filename)
}

# Named treatment-group palette documented in Figure 2 R Markdown.
lsopc_group_colors <- c(
  "Control" = "#D98391", "AGEs" = "#EBA075",
  "L-LSOPC" = "#80B49E", "H-LSOPC" = "#6C9EBC"
)

# Descriptive statistics only: inferential tests are deliberately not inferred.
summary_mean_sd <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(c(n = 0, mean = NA_real_, sd = NA_real_))
  c(n = length(x), mean = mean(x), sd = if (length(x) > 1) sd(x) else NA_real_)
}

# Report the provenance of a figure without implying the plot was re-analysed.
analysis_provenance <- function(method, input, limitations = NULL) {
  message("Method: ", method, " | Input: ", input)
  if (!is.null(limitations)) message("Limitation: ", limitations)
  invisible(TRUE)
}
