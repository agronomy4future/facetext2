#' Easy Text Annotation for ggplot2 Faceted Plots (facet_grid)
#'
#' @description
#' An extension of facetext() that supports two-variable faceted plots
#' created with facet_grid(). Places text labels inside each panel using
#' relative coordinates (0 to 1), where x = 0 is the left edge and
#' x = 1 is the right edge of each panel. Panel order follows the factor
#' level order of the facet variables.
#'
#' @param x Numeric vector of relative x positions (0 = left, 1 = right).
#' @param y Numeric vector of relative y positions (0 = bottom, 1 = top).
#' @param label Character vector of text labels to display.
#' @param data The same data frame passed to ggplot().
#' @param facet_var Character string. The column-direction variable in
#'   facet_grid() (e.g. "Year").
#' @param facet_var2 Character string. The row-direction variable in
#'   facet_grid() (e.g. "Genotype").
#' @param x_var Character string. The variable name mapped to the x-axis.
#' @param yrange Numeric vector of length 2. The y-axis range c(min, max).
#'   Must match scale_y_continuous(limits = ...). Default is c(0, 100).
#' @param color Character vector of color names. A single value applies the
#'   same color to all labels. A vector of the same length as label applies
#'   individual colors per label. Default is "black".
#' @param ... Additional arguments passed to geom_text() such as size,
#'   family, or fontface.
#'
#' @return A list containing a geom_text() layer to be added to a ggplot
#'   object via the + operator.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' df= data.frame(
#'   Genotype   = rep(c("Cultivar_1", "Cultivar_2", "Cultivar_3"), each = 4),
#'   Nitrogen   = rep(c("N0", "N1"), times = 6),
#'   Year       = rep(c(2023, 2024), each = 2, times = 3),
#'   Yield_Mean = c(48, 62, 55, 65, 62, 58, 50, 60, 57, 63, 59, 61),
#'   Yield_se   = rep(0.7, 12)
#' )
#'
#' # Fix panel order using factor levels
#' df$Year     = factor(df$Year,     levels = c(2024, 2023))
#' df$Genotype = factor(df$Genotype, levels = c("Cultivar_1", "Cultivar_2", "Cultivar_3"))
#'
#' # 3 columns x 2 rows = 6 panels
#' #
#' #            Cultivar_1   Cultivar_2   Cultivar_3
#' # 2024       [panel 4]    [panel 5]    [panel 6]
#' # 2023       [panel 1]    [panel 2]    [panel 3]
#'
#'
#' if(!require(remotes)) install.packages("remotes")
#' if (!requireNamespace("facetext2", quietly = TRUE)) {
#'    remotes::install_github("agronomy4future/facetext2", force= TRUE)
#' }
#' library(remotes)
#' library(facetext2)
#'
#' ggplot(df, aes(x = Nitrogen, y = Yield_Mean)) +
#'   geom_bar(stat = "identity", position = "dodge") +
#'   scale_y_continuous(limits = c(0, 100)) +
#'
#'   facet_grid(Year ~ Genotype) +
#'
#'   facetext2(data=df, facet_var="Genotype", facet_var2="Year", x_var="Nitrogen",
#'          x=c(0.5,0.5,0.5,0.5,0.5,0.6), yrange=c(0, 100), y=c(0.6,0.6,0.6,0.6,0.6,0.6),
#'          label=c("1","2","3","4","5","6"), color=c("red","black","red","blue","black","blue"),
#'          size=5, family="serif") +
#'
#'   labs(x = "Nitrogen", y = "Yield") +
#'   theme_classic(base_size = 18)
#'
#' * Github: https://github.com/agronomy4future/facetext2
#' - All Rights Reserved © J.K Kim (kimjk@agronomy4future.com)
facetext2 <- function(x, y, label,
                      data,
                      facet_var,
                      facet_var2,
                      x_var,
                      yrange = c(0, 100),
                      color = "black",
                      ...) {

  # ── Validate variable names
  if (!x_var %in% names(data)) {
    stop(paste0("\n[facetext2 error] x_var = '", x_var, "' not found in data.\n",
                "Available columns: ", paste(names(data), collapse = ", "), "\n"))
  }
  if (!facet_var %in% names(data)) {
    stop(paste0("\n[facetext2 error] facet_var = '", facet_var, "' not found in data.\n",
                "Available columns: ", paste(names(data), collapse = ", "), "\n"))
  }
  if (!facet_var2 %in% names(data)) {
    stop(paste0("\n[facetext2 error] facet_var2 = '", facet_var2, "' not found in data.\n",
                "Available columns: ", paste(names(data), collapse = ", "), "\n"))
  }

  # ── Recycle or validate color vector
  if (length(color) == 1) {
    color <- rep(color, length(label))
  } else if (length(color) != length(label)) {
    warning(paste0("\n[facetext2 warning] length(color) = ", length(color),
                   " does not match length(label) = ", length(label), ".\n",
                   "Recycling color to match label length.\n"))
    color <- rep_len(color, length(label))
  }

  # ── Auto-detect x-axis type
  x_values <- data[[x_var]]
  xrange <- if (is.numeric(x_values)) {
    c(min(x_values, na.rm = TRUE), max(x_values, na.rm = TRUE))
  } else {
    c(1, length(unique(x_values)))
  }

  # ── Convert relative coordinates to actual plot coordinates
  real_x <- xrange[1] + x * diff(xrange)
  real_y <- yrange[1] + y * diff(yrange)

  # ── Build annotation data frame
  df_anno <- data.frame(
    x_pos     = real_x,
    y_pos     = real_y,
    label     = label,
    txt_color = color
  )

  # ── Two facet variables (facet_grid)
  # Panel order follows factor level order
  facet1_vals <- unique(data[[facet_var]])   # column direction
  facet2_vals <- unique(data[[facet_var2]])  # row direction

  grid_vals <- expand.grid(
    facet_var2_val = facet2_vals,  # row first
    facet_var_val  = facet1_vals   # column second
  )
  df_anno[[facet_var]]  <- grid_vals$facet_var_val
  df_anno[[facet_var2]] <- grid_vals$facet_var2_val

  # ── Return geom_text without touching global color scale
  list(
    geom_text(data = df_anno,
              aes(x = x_pos, y = y_pos, label = label),
              color = df_anno$txt_color,   # outside aes() → no scale conflict
              inherit.aes = FALSE, ...)
  )
}
