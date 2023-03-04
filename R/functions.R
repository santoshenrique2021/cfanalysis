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
    dplyr::summarise(absolute_frequency = n())
}



## 3 - Relative frequency
re_fr <- function(dataset, col_name) {
  col_name <- dplyr::enquo(col_name)
  dataset |>
    dplyr::group_by(!!col_name) |>
    dplyr::summarise(total = n()) |>
    dplyr::mutate (relative_frequency = round(100 * (total / sum(total)), 2)) |>
    dplyr::select(!!col_name, relative_frequency)
}

re_fr(dataset = df, col_name =  result_19_20)

## 4 - Full frequency (absolute and relative)
full_fr <- function(dataset, col_name) {
  col_name <- dplyr::enquo(col_name)
  dataset |>
    dplyr::group_by(!!col_name) |>
    dplyr::summarise(absolute_frequency = n()) |>
    dplyr::mutate (relative_frequency = round(100 * (
      absolute_frequency / sum(absolute_frequency)
    ), 2))
  
}

full_fr(dataset = df, col_name = result_19_20)

## 5 - Comparative of the absolute frequency for the a variable in different moments
co_ab_fr <- function(dataset, col_name_1, col_name_2) {
  col_name_1 <- dplyr::enquo(col_name_1)
  col_name_2 <- dplyr::enquo(col_name_2)
  values <- dataset |> dplyr::group_by(!!col_name_1) |>
    dplyr::summarise(n = n()) |> dplyr::select(-2) |>
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

co_ab_fr(dataset = df,
         col_name_1 = result_19_20,
         col_name_2 = result_20_21)

## 6 - Comparative of the relative frequency for the a variable in different moments
co_rl_fr <- function(dataset, col_name_1, col_name_2) {
  col_name_1 <- dplyr::enquo(col_name_1)
  col_name_2 <- dplyr::enquo(col_name_2)
  values <- dataset |> dplyr::group_by(!!col_name_1) |>
    dplyr::summarise(n = n()) |> dplyr::select(-2) |>
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

co_rl_fr(dataset = df,
         col_name_1 = result_19_20,
         col_name_2 = result_20_21)

## 7 - Barplot for the a variable in different moments
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

plot_bar(dataset = df,
         col_name_1 = result_19_20,
         col_name_2 = result_20_21)

## 8 - Total variation distance for a categorical variable
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

tv_d(dataset = df,
     col_name_1 = result_19_20,
     col_name_2 = result_20_21)