#' @export
#' @title Modify labels font settings
#' @description Specify mappings from levels in the data to displayed text font settings.
#' @param x an `ms_chart` object.
#' @param values a named list of [fp_text()] objects to map data labels to.
#' It is a named list, the values will be matched based on the names.
#' If it contains only one [fp_text()] object, it will be associated to all existing series.
#' @examples
#' library(officer)
#'
#' fp_text_settings <- list(
#'   serie1 = fp_text(font.size = 7, color = "red"),
#'   serie2 = fp_text(font.size = 0, color = "purple"),
#'   serie3 = fp_text(font.size = 19, color = "wheat")
#' )
#'
#' barchart <- ms_barchart(
#'   data = browser_data,
#'   x = "browser", y = "value", group = "serie")
#' barchart <- chart_data_labels(barchart, show_val = TRUE)
#' barchart <- chart_labels_text( barchart,
#'   values = fp_text_settings )
#' @family Series customization functions
chart_labels_text <- function(x, values){

  serie_names <- names(x$series_settings$fill)

  if( inherits(values, "fp_text") ){
    values <- rep(list(values), length(serie_names) )
    values <- setNames(values, serie_names)
  }
  if( is.null(values) || !is.list(values) || length(values) < 1 ){
    stop("values must be a list of fp_text objects", call. = FALSE)
  }

  if( !all( sapply(values, inherits, what = "fp_text") ) ){
    stop("values must be a list of fp_text objects", call. = FALSE)
  }

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))

  x$series_settings$labels_fp[names(values)] <- values
  x
}

#' @export
#' @title Override labels font settings
#' @description Specify data point specific mappings from levels in the data to displayed text font settings.
#' @param x an `ms_chart` object.
#' @param values a named list of lists of [fp_text()] objects or `NA`s to map data labels to.
#' It is a named list, the values will be matched based on the names.
#' Values are mapped to data points by position. Each list must match the respective series' length.
#' `NA` means that the series-level setting will be used.
#' @examples
#' library(officer)
#'
#' fp_text_settings <- list(
#'   serie1 = fp_text(font.size = 7, color = "red"),
#'   serie2 = fp_text(font.size = 0, color = "purple"),
#'   serie3 = fp_text(font.size = 19, color = "wheat")
#' )
#'
#' fp_text_settings_override <- list(
#'   serie1 = list(NA, NA, fp_text(font.size = 7, color = "blue"), NA, NA, NA)
#' )
#'
#' barchart <- ms_barchart(
#'   data = browser_data,
#'   x = "browser", y = "value", group = "serie")
#' barchart <- chart_data_labels(barchart, show_val = TRUE)
#' barchart <- chart_labels_text( barchart,
#'   values = fp_text_settings )
#' barchart <- chart_labels_text_override(barchart, values = fp_text_settings_override)
#' @family Data point customization functions
#' @seealso [chart_labels_text()]
chart_labels_text_override <- function(x, values){

  serie_names <- names(x$series_settings$fill)

  if( is.null(values) || !is.list(values) || length(values) < 1 ){
    stop("values must be a list of lists of fp_text objects", call. = FALSE)
  }

  if( !all( sapply(values, function(x) all(sapply(x, function(val) inherits(val, what = "fp_text") || is.na(val)))) ) ){
    stop("values must be a list of lists fp_text objects or NAs", call. = FALSE)
  }

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))

  serie_lengths <- vapply(x$data_series[, names(values), drop = FALSE], length, integer(1))
  values_lengths <- vapply(values, length, integer(1))

  if( !all(serie_lengths == values_lengths))
    stop("values's lengths do not match series' lengths: ", paste0(shQuote(serie_lengths), collapse = ", "))

  x$series_settings_override$labels_fp[names(values)] <- values
  x
}


#' @export
#' @title Modify fill colour
#' @description Specify mappings from levels in the data to displayed fill colours.
#' @param x an `ms_chart` object.
#' @param values `character(num of series|1)`: a set of colours values to map data values to.
#' It is a named vector, the values will be matched based on the names.
#' If it contains only one colour, this colour will be associated to all existing series.
#' @examples
#' my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width",  group = "Species")
#' my_scatter <- chart_data_fill(my_scatter,
#'   values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
#' @family Series customization functions
chart_data_fill <- function(x, values){

  valid_cols <- is_valid_color(values)
  if( any(!valid_cols) )
    stop("invalid color(s) in argument values")

  serie_names <- names(x$series_settings$fill)

  if( length(values) == 1 ){
    values <- rep(values, length(serie_names))
    names(values) <- serie_names
  }

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))

  x$series_settings$fill[names(values)] <- values
  x
}

#' @export
#' @title Override fill colour
#' @description Specify data point specific mappings from levels in the data to displayed fill colours.
#' @param x an `ms_chart` object.
#' @param values a named list of vectors of colour values or `NA`s to map data values to.
#' It is a named list, the values will be matched based on the names.
#' Values are mapped to data points by position. Each list must match the respective series' length.
#' `NA` means that the series-level setting will be used.
#' @examples
#' my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width",  group = "Species")
#' my_scatter <- chart_data_fill(my_scatter,
#'   values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
#'
#' my_scatter <- chart_data_fill_override(my_scatter,
#'   values = list(virginica = c("#FF6161", rep(NA, 149))))
#' @family Data point customization functions
#' @seealso [chart_data_fill()]
chart_data_fill_override <- function(x, values){

  valid_cols <- sapply(values, function(x) all(is_valid_color(x)))
  if( any(!valid_cols) )
    stop("invalid color(s) in argument values")

  serie_names <- names(x$series_settings$fill)

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))

  serie_lengths <- vapply(x$data_series[, names(values), drop = FALSE], length, integer(1))
  values_lengths <- vapply(values, length, integer(1))

  if( !all(serie_lengths == values_lengths))
    stop("values's lengths do not match series' lengths: ", paste0(shQuote(serie_lengths), collapse = ", "))

  x$series_settings_override$fill[names(values)] <- values
  x
}

#' @export
#' @title Modify marker stroke colour
#' @description Specify mappings from levels in the data to displayed marker stroke colours.
#' @param x an `ms_chart` object.
#' @param values `character(num of series)`: a set of colours values to map data values to.
#' It is a named vector, the values will be matched based on the names.
#' If it contains only one colour, this colour will be associated to all existing series.
#' @examples
#' my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width",  group = "Species")
#' my_scatter <- chart_data_fill(my_scatter,
#'   values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
#' my_scatter <- chart_data_stroke(my_scatter,
#'   values = c(virginica = "black", versicolor = "black", setosa = "black") )
#' @family Series customization functions
chart_data_stroke <- function(x, values){

  valid_cols <- is_valid_color(values)
  if( any(!valid_cols) )
    stop("invalid color(s) in argument values")

  serie_names <- names(x$series_settings$colour)

  if( length(values) == 1 ){
    values <- rep(values, length(serie_names))
    names(values) <- serie_names
  }

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))


  x$series_settings$colour[names(values)] <- values
  x
}

#' @export
#' @title Override marker stroke colour
#' @description Specify data point specific mappings from levels in the data to displayed marker stroke colour.
#' @param x an `ms_chart` object.
#' @param values a named list of vectors of colour values or `NA`s to map data values to.
#' It is a named list, the values will be matched based on the names.
#' Values are mapped to data points by position. Each list must match the respective series' length.
#' `NA` means that the series-level setting will be used.
#' @examples
#' my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width",  group = "Species")
#' my_scatter <- chart_data_fill(my_scatter,
#'   values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
#' my_scatter <- chart_data_stroke(my_scatter,
#'   values = c(virginica = "black", versicolor = "black", setosa = "black") )
#'
#' my_scatter <- chart_data_stroke_override(my_scatter,
#'   values = list(virginica = c("red", rep(NA, 149))))
#' @family Data point customization functions
#' @seealso [chart_data_stroke()]
chart_data_stroke_override <- function(x, values){

  valid_cols <- sapply(values, function(x) all(is_valid_color(x)))
  if( any(!valid_cols) )
    stop("invalid color(s) in argument values")

  serie_names <- names(x$series_settings$colour)

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))

  serie_lengths <- vapply(x$data_series[, names(values), drop = FALSE], length, integer(1))
  values_lengths <- vapply(values, length, integer(1))

  if( !all(serie_lengths == values_lengths))
    stop("values's lengths do not match series' lengths: ", paste0(shQuote(serie_lengths), collapse = ", "))

  x$series_settings_override$colour[names(values)] <- values
  x
}

#' @export
#' @title Modify symbol
#' @description Specify mappings from levels in the data to displayed symbols.
#' @param x an `ms_chart` object.
#' @param values `character(num of series)`: a set of symbol values to map data values to.
#' It is a named vector, the values will be matched based on the names.
#' Possible values are: 'circle', 'dash', 'diamond', 'dot', 'none', 'plus',
#' 'square', 'star', 'triangle', 'x', 'auto'.
#' If it contains only one symbol, this symbol will be associated to all existing series.
#' @examples
#' my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width",  group = "Species")
#' my_scatter <- chart_data_fill(my_scatter,
#'   values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
#' my_scatter <- chart_data_stroke(my_scatter,
#'   values = c(virginica = "black", versicolor = "black", setosa = "black") )
#' my_scatter <- chart_data_symbol(my_scatter,
#'   values = c(virginica = "circle", versicolor = "diamond", setosa = "circle") )
#' @family Series customization functions
chart_data_symbol <- function(x, values){

  if( !all(values %in% st_markerstyle) ){
    stop("values should have values matching ", paste0(shQuote(st_markerstyle), collapse = ", " ))
  }

  serie_names <- names(x$series_settings$symbol)

  if( length(values) == 1 ){
    values <- rep(values, length(serie_names))
    names(values) <- serie_names
  }

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))


  x$series_settings$symbol[names(values)] <- values
  x
}

#' @export
#' @title Override symbol
#' @description Specify data point specific mappings from levels in the data to displayed symbols.
#' @param x an `ms_chart` object.
#' @param values a named list of vectors of symbol values (see [chart_data_symbol()] for possible values) or `NA`s to map data values to.
#' It is a named list, the values will be matched based on the names.
#' Values are mapped to data points by position. Each list must match the respective series' length.
#' `NA` means that the series-level setting will be used.
#' @examples
#' my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width",  group = "Species")
#' my_scatter <- chart_data_fill(my_scatter,
#'   values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
#' my_scatter <- chart_data_stroke(my_scatter,
#'   values = c(virginica = "black", versicolor = "black", setosa = "black") )
#' my_scatter <- chart_data_symbol(my_scatter,
#'   values = c(virginica = "circle", versicolor = "diamond", setosa = "circle") )
#'
#' my_scatter <- chart_data_symbol_override(my_scatter,
#'   values = list(virginica = c("diamond", rep(NA, 149))))
#' @family Data point customization functions
#' @seealso [chart_data_symbol()]
chart_data_symbol_override <- function(x, values){

  valid_symbols <- sapply(values, function(x) all(x %in% st_markerstyle | is.na(x)))
  if( !all(valid_symbols) ){
    stop("values should have values matching ", paste0(shQuote(st_markerstyle), collapse = ", " ))
  }

  serie_names <- names(x$series_settings$symbol)

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))

  serie_lengths <- vapply(x$data_series[, names(values), drop = FALSE], length, integer(1))
  values_lengths <- vapply(values, length, integer(1))

  if( !all(serie_lengths == values_lengths))
    stop("values's lengths do not match series' lengths: ", paste0(shQuote(serie_lengths), collapse = ", "))

  x$series_settings_override$symbol[names(values)] <- values
  x
}

#' @export
#' @title Modify symbol size
#' @description Specify mappings from levels in the data to displayed size of symbols.
#' @param x an `ms_chart` object.
#' @param values `double(num of series)`: a set of size values to map data values to.
#' It is a named vector, the values will be matched based on the names.
#' If it contains only one size, this size will be associated to all existing series.
#' @examples
#' my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width",  group = "Species")
#' my_scatter <- chart_data_fill(my_scatter,
#'   values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
#' my_scatter <- chart_data_stroke(my_scatter,
#'   values = c(virginica = "black", versicolor = "black", setosa = "black") )
#' my_scatter <- chart_data_symbol(my_scatter,
#'   values = c(virginica = "circle", versicolor = "diamond", setosa = "circle") )
#' my_scatter <- chart_data_size(my_scatter,
#'   values = c(virginica = 20, versicolor = 16, setosa = 20) )
#' @family Series customization functions
chart_data_size <- function(x, values){

  if( !is.numeric(values) )
    stop("values should be numeric values")
  if( any( sign(values) < 0 ) )
    stop("values should not contain negative values")

  serie_names <- names(x$series_settings$size)

  if( length(values) == 1 ){
    values <- rep(values, length(serie_names))
    names(values) <- serie_names
  }

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))

  x$series_settings$size[names(values)] <- values
  x
}

#' @export
#' @title Override symbol size
#' @description Specify data point specific mappings from levels in the data to displayed size of symbols.
#' @param x an `ms_chart` object.
#' @param values a named list of vectors of size values or `NA`s to map data values to.
#' It is a named list, the values will be matched based on the names.
#' Values are mapped to data points by position. Each list must match the respective series' length.
#' `NA` means that the series-level setting will be used.
#' @examples
#' my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width",  group = "Species")
#' my_scatter <- chart_data_fill(my_scatter,
#'   values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
#' my_scatter <- chart_data_stroke(my_scatter,
#'   values = c(virginica = "black", versicolor = "black", setosa = "black") )
#' my_scatter <- chart_data_size(my_scatter,
#'   values = c(virginica = 20, versicolor = 16, setosa = 20) )
#'
#' my_scatter <- chart_data_size_override(my_scatter,
#'   values = list(virginica = c(30, rep(NA, 149))))
#' @family Data point customization functions
#' @seealso [chart_data_size()]
chart_data_size_override <- function(x, values){

  valid_numbers <- all(sapply(values, function(x) sapply(x, function (val) is.numeric(val) | is.na(val))))
  if( !valid_numbers ){
    stop("values should be numeric values")
  }
  valid_signs <- any(sapply(values, function(x) any(!is.na(x) & x < 0)))
  if( valid_signs )
    stop("values should not contain negative values")

  serie_names <- names(x$series_settings$size)

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))

  serie_lengths <- vapply(x$data_series[, names(values), drop = FALSE], length, integer(1))
  values_lengths <- vapply(values, length, integer(1))

  if( !all(serie_lengths == values_lengths))
    stop("values's lengths do not match series' lengths: ", paste0(shQuote(serie_lengths), collapse = ", "))

  x$series_settings_override$size[names(values)] <- values
  x
}

#' @export
#' @title Modify line width
#' @description Specify mappings from levels in the data to displayed line width between symbols.
#' @param x an `ms_chart` object.
#' @param values `double(num of series)`: a set of size values to map data values to.
#' It is a named vector, the values will be matched based on the names.
#' If it contains only one size, this size will be associated to all existing series.
#' @examples
#' my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width",  group = "Species")
#' my_scatter <- chart_settings(my_scatter, scatterstyle = "lineMarker")
#' my_scatter <- chart_data_fill(my_scatter,
#'   values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
#' my_scatter <- chart_data_stroke(my_scatter,
#'   values = c(virginica = "black", versicolor = "black", setosa = "black") )
#' my_scatter <- chart_data_symbol(my_scatter,
#'   values = c(virginica = "circle", versicolor = "diamond", setosa = "circle") )
#' my_scatter <- chart_data_size(my_scatter,
#'   values = c(virginica = 20, versicolor = 16, setosa = 20) )
#' my_scatter <- chart_data_line_width(my_scatter,
#'   values = c(virginica = 2, versicolor = 3, setosa = 6) )
#' @family Series customization functions
chart_data_line_width <- function(x, values){

  if( !is.numeric(values) )
    stop("values should be numeric values")
  if( any( sign(values) < 0 ) )
    stop("values should not contain negative values")

  serie_names <- names(x$series_settings$line_width)

  if( length(values) == 1 ){
    values <- rep(values, length(serie_names))
    names(values) <- serie_names
  }

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))

  x$series_settings$line_width[names(values)] <- values
  x
}

#' @export
#' @title Override line width
#' @description Specify data point specific mappings from levels in the data to displayed line width between symbols.
#' @param x an `ms_chart` object.
#' @param values a named list of vectors of size values or `NA`s to map data values to.
#' It is a named list, the values will be matched based on the names.
#' Values are mapped to data points by position. Each list must match the respective series' length.
#' `NA` means that the series-level setting will be used.
#' @examples
#' my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width",  group = "Species")
#' my_scatter <- chart_data_fill(my_scatter,
#'   values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
#' my_scatter <- chart_data_stroke(my_scatter,
#'   values = c(virginica = "black", versicolor = "black", setosa = "black") )
#' my_scatter <- chart_data_size(my_scatter,
#'   values = c(virginica = 20, versicolor = 16, setosa = 20) )
#' my_scatter <- chart_data_line_width(my_scatter,
#'   values = c(virginica = 2, versicolor = 3, setosa = 6) )
#'
#' my_scatter <- chart_data_line_width(my_scatter,
#'   values = list(virginica = c(7, rep(NA, 149))))
#' @family Data point customization functions
#' @seealso [chart_data_line_width()]
chart_data_line_width_override <- function(x, values){

  valid_numbers <- all(sapply(values, function(x) sapply(x, function (val) is.numeric(val) | is.na(val))))
  if( !valid_numbers ){
    stop("values should be numeric values")
  }
  valid_signs <- any(sapply(values, function(x) any(!is.na(x) & x < 0)))
  if( valid_signs )
    stop("values should not contain negative values")

serie_names <- names(x$series_settings$line_width)

if( !all(names(values) %in% serie_names ) )
  stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))

serie_lengths <- vapply(x$data_series[, names(values), drop = FALSE], length, integer(1))
values_lengths <- vapply(values, length, integer(1))

if( !all(serie_lengths == values_lengths))
  stop("values's lengths do not match series' lengths: ", paste0(shQuote(serie_lengths), collapse = ", "))

x$series_settings_override$line_width[names(values)] <- values
x
}

#' @export
#' @title Modify line style
#' @description Specify mappings from levels in the data to displayed line style.
#' @param x an `ms_chart` object.
#' @param values `character(num of series)`: a set of line style values to map data values to.
#' It is a named vector, the values will be matched based on the names.
#' Possible values are: 'none', 'solid', 'dashed', 'dotted'.
#' If it contains only one line style, this style will be associated to all existing series.
#' @examples
#' my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width",  group = "Species")
#' my_scatter <- chart_data_fill(my_scatter,
#'   values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
#' my_scatter <- chart_data_stroke(my_scatter,
#'   values = c(virginica = "black", versicolor = "black", setosa = "black") )
#' my_scatter <- chart_data_symbol(my_scatter,
#'   values = c(virginica = "circle", versicolor = "diamond", setosa = "circle") )
#' my_scatter <- chart_data_line_style(my_scatter,
#'   values = c(virginica = "solid", versicolor = "dotted", setosa = "dashed") )
#' @family Series customization functions
chart_data_line_style <- function(x, values){

  if( !all(values %in% st_linestyle) ){
    stop("values should have values matching ", paste0(shQuote(st_linestyle), collapse = ", " ))
  }

  serie_names <- names(x$series_settings$line_style)

  if( length(values) == 1 ){
    values <- rep(values, length(serie_names))
    names(values) <- serie_names
  }

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))


  x$series_settings$line_style[names(values)] <- values
  x
}

#' @export
#' @title Override line style
#' @description Specify data point specific mappings from levels in the data to displayed line style
#' @param x an `ms_chart` object.
#' @param values a named list of vectors of line style values (see [chart_data_line_style()] for possible values) or `NA`s to map data values to.
#' It is a named list, the values will be matched based on the names.
#' Values are mapped to data points by position. Each list must match the respective series' length.
#' `NA` means that the series-level setting will be used.
#' @examples
#' my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width",  group = "Species")
#' my_scatter <- chart_data_fill(my_scatter,
#'   values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
#' my_scatter <- chart_data_stroke(my_scatter,
#'   values = c(virginica = "black", versicolor = "black", setosa = "black") )
#' my_scatter <- chart_data_symbol(my_scatter,
#'   values = c(virginica = "circle", versicolor = "diamond", setosa = "circle") )
#' my_scatter <- chart_data_line_style(my_scatter,
#'   values = c(virginica = "solid", versicolor = "dotted", setosa = "dashed") )
#'
#' my_scatter <- chart_data_line_style_override(my_scatter,
#'   values = list(virginica = c("dashed", rep(NA, 149))))
#' @family Data point customization functions
#' @seealso [chart_data_line_style()]
chart_data_line_style_override <- function(x, values){

  valid_symbols <- sapply(values, function(x) all(x %in% st_linestyle | is.na(x)))
  if( !all(valid_symbols) ){
    stop("values should have values matching ", paste0(shQuote(st_linestyle), collapse = ", " ))
  }

  serie_names <- names(x$series_settings$line_style)

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))

  serie_lengths <- vapply(x$data_series[, names(values), drop = FALSE], length, integer(1))
  values_lengths <- vapply(values, length, integer(1))

  if( !all(serie_lengths == values_lengths))
    stop("values's lengths do not match series' lengths: ", paste0(shQuote(serie_lengths), collapse = ", "))

  x$series_settings_override$line_style[names(values)] <- values
  x
}

#' @export
#' @title Smooth series
#' @description Specify mappings from levels in the data to smooth or not lines. This
#' feature only applies to [ms_linechart()].
#' @param x an `ms_chart` object.
#' @param values  `integer(num of series)`: a set of smooth values to map data values to.
#' It is a named vector, the values will be matched based on the names.
#' Possible values are 0 or 1
#' If it contains only one integer it will be associated to all existing series.
#' @examples
#' linec <- ms_linechart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width", group = "Species")
#'linec <- chart_data_smooth(linec,
#'   values = c(virginica = 0, versicolor = 0, setosa = 0) )
#' @family Series customization functions
chart_data_smooth <- function(x, values){
  as_bool <- c(1,0)

  if( !all(values %in% as_bool) ){
    stop("smooth can only take values of 0 or 1")
  }

  serie_names <- names(x$series_settings$symbol)

  if( length(values) == 1 ){
    values <- rep(values, length(serie_names))
    names(values) <- serie_names
  }

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))

  x$series_settings$smooth[names(values)] <- values
  x
}
