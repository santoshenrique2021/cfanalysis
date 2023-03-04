#Step 3 - Functions
# library(dplyr)      #Data manipulation 
# library(ggplot2)    #Graphics 
# library(tibble)     #Modern data frame 
# library(readxl)     #Open an Excel file

#' all_levels
#'
#' @param dataset the dataset you wish to retrieve levels for
#' @param col_name the column name you wish to retrieve levels for
#'
#' @return returns levels of a categorical variable
#' @export
#'
#' @examples
#' \dontrun{
#' all_levels(dataset = soccer, col_name = result_19_20)}
all_levels <- function(dataset, col_name) {
  col_name <- dplyr::enquo(col_name)
  
  dataset |> dplyr::group_by(!!col_name) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::select(-2) |>
    dplyr::rename(values = !!col_name)
}

#' ab_fr
#'
#' @param dataset the dataset for which you wish to retrieve absolute frequency.
#' @param col_name the column name for which you wish to retrieve absolute frequency.
#'
#' @return returns the absolute frequency for a specified column.
#' @export
#'
#' @examples
#' \dontrun{
#' ab_fr(dataset = soccer, col_name =  result_19_20)}
ab_fr <- function(dataset, col_name) {
  col_name <- dplyr::enquo(col_name)
  dataset |>
    dplyr::group_by(!!col_name) |>
    dplyr::summarise(absolute_frequency = dplyr::n())
}

#' re_fr
#'
#' @param dataset the dataset for which you wish to retrieve relative frequency.
#' @param col_name the column name for which you wish to retrieve relative frequency.
#'
#' @return returns the relative frequency for a specified column.
#' @export
#'
#' @examples
#' \dontrun{
#' re_fr(dataset = soccer, col_name =  result_19_20)}
re_fr <- function(dataset, col_name) {
  col_name <- dplyr::enquo(col_name)
  dataset |>
    dplyr::group_by(!!col_name) |>
    dplyr::summarise(total = dplyr::n()) |>
    dplyr::mutate (relative_frequency = round(100 * (total / sum(total)), 2)) |>
    dplyr::select(!!col_name, relative_frequency)
}

#' full_fr
#'
#' @param dataset the dataset for which you wish to retrieve full frequency.
#' @param col_name the column name for which you wish to retrieve full frequency.
#'
#' @return returns the full frequency (absolute and relative) for a specified column.
#' @export
#'
#' @examples
#' \dontrun{
#' full_fr(dataset = soccer, col_name = result_19_20)}
full_fr <- function(dataset, col_name) {
  col_name <- dplyr::enquo(col_name)
  dataset |>
    dplyr::group_by(!!col_name) |>
    dplyr::summarise(absolute_frequency = dplyr::n()) |>
    dplyr::mutate (relative_frequency = round(100 * (
      absolute_frequency / sum(absolute_frequency)
    ), 2))
}

#' co_ab_fr
#'
#' @param dataset the dataset for which you wish to retrieve comparative 
#' absolute frequency.
#' @param col_name_1 the first column name for which you wish to retrieve 
#' comparative absolute frequency.
#' @param col_name_2 the second column name for which you wish to retrieve 
#' comparative absolute frequency.
#'
#' @return returns the comparative of the absolute frequency for the a variable 
#' in different moments.
#' @export
#'
#' @examples
#' \dontrun{
#'co_ab_fr(dataset = soccer,
#'         col_name_1 = result_19_20,
#'         col_name_2 = result_20_21)}
co_ab_fr <- function(dataset, col_name_1, col_name_2) {
  col_name_1 <- dplyr::enquo(col_name_1)
  col_name_2 <- dplyr::enquo(col_name_2)
  values <- dataset |> dplyr::group_by(!!col_name_1) |>
    dplyr::summarise(n = dplyr::n()) |> dplyr::select(-2) |>
    dplyr::rename(values = !!col_name_1)
  
  dataset |>
    dplyr::select(!!col_name_1,!!col_name_2) |>
    sapply(function(x)
      table(x)) |>
    tibble::as_tibble() |>
    tibble::add_column(values) |>
    dplyr::relocate(values) |>
    dplyr::mutate(dif_abs = abs(!!col_name_1-!!col_name_2))
}

#' co_rl_fr
#'
#' @param dataset the dataset for which you wish to retrieve comparative 
#' relative frequency.
#' @param col_name_1 the first column name for which you wish to retrieve 
#' comparative relative frequency.
#' @param col_name_2 the second column name for which you wish to retrieve 
#' comparative relative frequency.
#'
#' @return returns the comparative of the relative frequency for the a variable 
#' in different moments.
#' @export
#'
#' @examples
#' \dontrun{
#'co_rl_fr(dataset = soccer,
#'         col_name_1 = result_19_20,
#'         col_name_2 = result_20_21)}
co_rl_fr <- function(dataset, col_name_1, col_name_2) {
  col_name_1 <- dplyr::enquo(col_name_1)
  col_name_2 <- dplyr::enquo(col_name_2)
  values <- dataset |> dplyr::group_by(!!col_name_1) |>
    dplyr::summarise(n = dplyr::n()) |> dplyr::select(-2) |>
    dplyr::rename(values = !!col_name_1)
  
  dataset |>
    dplyr::select(!!col_name_1,!!col_name_2) |>
    sapply(function(x)
      round(table(x) * 100 / nrow(dataset), 2)) |>
    tibble::as_tibble() |>
    tibble::add_column(values) |>
    dplyr::relocate(values) |>
    dplyr::mutate(dif_abs = abs(!!col_name_1-!!col_name_2))
}

#' plot_bar
#'
#' @param dataset the dataset for which you wish to retrieve the bar plot.
#' @param col_name_1 the first column name for which you wish to retrieve the 
#' bar plot.
#' @param col_name_2 the second column name for which you wish to retrieve the 
#' bar plot.
#'
#' @return returns the bar plot for the a variable in different moments.
#' @export
#'
#' @examples
#' \dontrun{
#'plot_bar(dataset = df,
#'         col_name_1 = result_19_20,
#'         col_name_2 = result_20_21)}
plot_bar <- function(dataset, col_name_1, col_name_2) {
  col_name_1 <- dplyr::enquo(col_name_1)
  col_name_2 <- dplyr::enquo(col_name_2)
  
  d_frame <- dataset |>
    dplyr::select(!!col_name_1,!!col_name_2) |>
    tibble::tibble() |> stack()
  
  ggplot(d_frame, aes(ind, fill = values)) +
    geom_bar(position = "fill", colour = "black") +
    scale_fill_brewer(palette = "Spectral") +
    theme_classic() +
    ylab("Distribuition of the values") +
    xlab("Variable") +
    ggtitle("Comparison of the variable in different moments") +
    theme(
      plot.title = element_text(size = 12, face = "bold.italic"),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 10, face = "bold")
    ) +
    coord_flip() +
    stat_count(geom = 'text',
               aes(label = after_stat(count)),
               position = position_fill(vjust = 0.5))
  
}

#' tv_d
#'
#' @param dataset the dataset for which you wish to retrieve the total variation 
#' distance.
#' @param col_name_1 the first column name for which you wish to retrieve the 
#' total variation distance.
#' @param col_name_2 the second column name for which you wish to retrieve the 
#' total variation distance.
#'
#' @return returns the total variation distance for a categorical variable.
#' @export
#'
#' @examples
#' \dontrun{
#'tv_d(dataset = df,
#'     col_name_1 = result_19_20,
#'     col_name_2 = result_20_21)}
tv_d <- function(dataset, col_name_1, col_name_2) {
  col_name_1 <- dplyr::enquo(col_name_1)
  col_name_2 <- dplyr::enquo(col_name_2)
  
  dataset |>
    dplyr::select(!!col_name_1,!!col_name_2) |>
    sapply(function(x)
      round(table(x) * 100 / nrow(dataset), 2)) |>
    tibble::as_tibble() |>
    dplyr::mutate(dif_abs = abs(!!col_name_1-!!col_name_2)) |>
    dplyr::summarise(tv = sum(dif_abs) / 2)
}