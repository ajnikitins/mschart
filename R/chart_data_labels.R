#' @export
#' @title Modify data labels settings
#' @description Data labels show details about data series. This function indicate that
#' data labels should be displayed. See [chart_labels_text()] for modifying
#' text settings associated with labels.
#' @param x an `ms_chart` object.
#' @param num_fmt `character(1)`: number formatting specifies number format properties which
#' indicate how to format and render the numeric values. It can be "General", "0.00", "#,##0",
#' "#,##0.00", "mm-dd-yy", "m/d/yy h:mm", etc.
#' @param position `character(1)`: it specifies the position of the data label.
#' It should be one of 'b', 'ctr', 'inBase', 'inEnd', 'l',
#' 'outEnd', 'r', 't'.
#' When grouping is 'clustered', it should be one of 'ctr','inBase','inEnd','outEnd'.
#' When grouping is 'stacked', it should be one of 'ctr','inBase','inEnd'.
#' When grouping is 'standard', it should be one of 'b','ctr','l','r','t'.
#' @param show_legend_key show legend key if TRUE.
#' @param show_val show values if TRUE.
#' @param show_cat_name show categories if TRUE.
#' @param show_serie_name show names of series if TRUE.
#' @param show_percent show percentages if TRUE.
#' @param separator separator for displayed labels.
chart_data_labels <- function(x, num_fmt = "General", position = "ctr",
                                show_legend_key = FALSE, show_val = FALSE,
                                show_cat_name = FALSE, show_serie_name = FALSE,
                                show_percent = FALSE, separator = ", " ){

  out <- labels_options(num_fmt = num_fmt, position = position,
       show_legend_key = show_legend_key, show_val = show_val,
       show_cat_name = show_cat_name, show_serie_name = show_serie_name,
       show_percent = show_percent, separator = separator)
  x$label_settings <- out
  x
}

#' @export
#' @title Data labels settings
#' @description Data labels show details about data series. This function indicate that
#' data labels should be displayed. Similar to [chart_data_labels()], but encapsulates
#' data label settings for use in [chart_data_labels_series()] for series-level data
#' labels and [chart_data_labels_override()] for data point-level labels.
#' @param num_fmt `character(1)`: number formatting specifies number format properties which
#' indicate how to format and render the numeric values. It can be "General", "0.00", "#,##0",
#' "#,##0.00", "mm-dd-yy", "m/d/yy h:mm", etc.
#' @param position `character(1)`: it specifies the position of the data label.
#' It should be one of 'b', 'ctr', 'inBase', 'inEnd', 'l',
#' 'outEnd', 'r', 't'.
#' When grouping is 'clustered', it should be one of 'ctr','inBase','inEnd','outEnd'.
#' When grouping is 'stacked', it should be one of 'ctr','inBase','inEnd'.
#' When grouping is 'standard', it should be one of 'b','ctr','l','r','t'.
#' @param show_legend_key show legend key if TRUE.
#' @param show_val show values if TRUE.
#' @param show_cat_name show categories if TRUE.
#' @param show_serie_name show names of series if TRUE.
#' @param show_percent show percentages if TRUE.
#' @param separator separator for displayed labels.
labels_options <- function( num_fmt = "General", position = "ctr",
                            show_legend_key = FALSE, show_val = FALSE,
                            show_cat_name = FALSE, show_serie_name = FALSE,
                            show_percent = FALSE, separator = ", " ) {

  if( !position %in% st_dlblpos ){
    stop("position should be one of ", paste0(shQuote(st_dlblpos), collapse = ", " ))
  }

  out <- list(num_fmt = num_fmt, position = position,
              show_legend_key = show_legend_key, show_val = show_val,
              show_cat_name = show_cat_name, show_serie_name = show_serie_name,
              show_percent = show_percent, separator = separator)
  class(out) <- "labels_options"
  out
}

#' @export
#' @title Modify data labels settings
#' @description Specify mappings from levels in the data to displayed data labels.
#' Overrides settings from [chart_data_labels()].
#' @param x an `ms_chart` object.
#' @param values a named list of [labels_options()] objects to map data labels to.
#' It is a named list, the values will be matched based on the names.
#' If it contains only one [labels_options()] object, it will be associated to all existing series.
#' @examples
#' library(officer)
#'
#' label_settings <- list(
#'   serie1 = labels_options(show_val = TRUE),
#'   serie2 = labels_options(show_cat_name = TRUE),
#'   serie3 = labels_options(show_percent = TRUE)
#' )
#'
#' barchart <- ms_barchart(
#'   data = browser_data,
#'   x = "browser", y = "value", group = "serie")
#' barchart <- chart_data_labels(barchart, show_val = TRUE)
#' barchart <- chart_data_labels_series( barchart,
#'   values = label_settings )
#' @family Series customization functions
chart_data_labels_series <- function(x, values = NULL) {

  serie_names <- names(x$series_settings$fill)

  if( inherits(values, "labels_options") ){
    values <- rep(list(values), length(serie_names) )
    values <- setNames(values, serie_names)
  }
  if( is.null(values) || !is.list(values) || length(values) < 1 ){
    stop("values must be a list of labels_options objects", call. = FALSE)
  }

  if( !all( sapply(values, inherits, what = "labels_options") ) ){
    stop("values must be a list of labels_options objects", call. = FALSE)
  }

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))

  x$series_settings$label_settings[names(values)] <- values
  x
}

#' @export
#' @title Override data labels settings
#' @description Specify data point specific mappings from levels in the data to displayed data labels.
#' @param x an `ms_chart` object.
#' @param values a named list of lists of [labels_options()] objects or `NA`s to map data labels to.
#' It is a named list, the values will be matched based on the names.
#' Values are mapped to data points by position. Each list must match the respective series' length.
#' `NA` means that the series-level setting will be used.
#' @examples
#' library(officer)
#'
#' label_settings <- list(
#'   serie1 = labels_options(show_val = TRUE),
#'   serie2 = labels_options(show_cat_name = TRUE),
#'   serie3 = labels_options(show_percent = TRUE)
#' )
#'
#' label_settings_override <- list(
#'   serie1 = list(NA, labels_options(show_val = TRUE, show_legend_key = TRUE), NA, NA, NA, NA)
#' )
#'
#' barchart <- ms_barchart(
#'   data = browser_data,
#'   x = "browser", y = "value", group = "serie")
#' barchart <- chart_data_labels(barchart, show_val = TRUE)
#' barchart <- chart_data_labels_series( barchart,
#'   values = label_settings )
#' barchart <- chart_data_labels_override(barchart, values = label_settings_override)
#' @family Data point customization functions
#' @seealso [chart_data_labels()] [chart_data_labels_series()]
chart_data_labels_override <- function(x, values) {

  serie_names <- names(x$series_settings$fill)

  if( is.null(values) || !is.list(values) || length(values) < 1 ){
    stop("values must be a list of lists of labels_options objects", call. = FALSE)
  }

  if( !all( sapply(values, function(x) all(sapply(x, function(val) inherits(val, what = "labels_options") || is.na(val)))) ) ){
    stop("values must be a list of lists labels_options objects or NAs", call. = FALSE)
  }

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))

  serie_lengths <- vapply(x$data_series[, names(values), drop = FALSE], length, integer(1))
  values_lengths <- vapply(values, length, integer(1))

  if( !all(serie_lengths == values_lengths))
    stop("values's lengths do not match series' lengths: ", paste0(shQuote(serie_lengths), collapse = ", "))

  x$series_settings_override$label_settings[names(values)] <- values
  x
}

to_pml.labels_options <- function(x, add_ns = FALSE, with_position = TRUE, show_label = FALSE , ...){

  txpr <- ""
  if( !is.null( x$labels_fp )){
    txpr <- ooxml_txpr(x$labels_fp)
  }

  str_ <- paste0("<c:dLbls>",
                 if(with_position) sprintf("<c:dLblPos val=\"%s\"/>", x$position),
                 sprintf("<c:numFmt formatCode=\"%s\" sourceLinked=\"0\"/>", x$num_fmt),
                 sprintf("<c:separator val=\"%s\"/>", x$separator),
                 sprintf("<c:showBubbleSize val=\"%.0f\"/>", FALSE),
                 sprintf("<c:showCatName val=\"%.0f\"/>", as.integer(x$show_cat_name)),
                 sprintf("<c:showLegendKey val=\"%.0f\"/>", x$show_legend_key),
                 sprintf("<c:showPercent val=\"%.0f\"/>", x$show_percent),
                 sprintf("<c:showSerName val=\"%.0f\"/>", as.integer(x$show_serie_name)),
                 sprintf("<c:showVal val=\"%.0f\"/>", as.integer(x$show_val)),
                 txpr,
                 if(show_label){
                   paste0(
                     "<c:extLst>",
                       "<c:ext uri=\"{CE6537A1-D6FC-4f65-9D91-7224C49458BB}\" xmlns:c15=\"http://schemas.microsoft.com/office/drawing/2012/chart\">",
                         "<c15:dlblFieldTable/>",
                         "<c15:showDataLabelsRange val=\"1\"/>",
                       "</c:ext>",
                     "</c:extLst>"
                   )
                 },
                 "</c:dLbls>")

  str_
}

