#' Convert a tidystats list to a data frame
#'
#' \code{stats_list_to_df} converts a tidystats list to a data frame
#'
#' @examples
#' stats_list_to_df(results)
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom purrr map2_df
#'
#' @export
stats_list_to_df <- function(results) {

  # Loop over each element in the list and add the identifier information, then reorder columns
  df <- results %>%
    map2_df(names(results), mutate, identifier = `.y[[i]]`) %>%
    select(identifier, method, one_of("term"), estimate, std_error, statistic, one_of("df_model"),
           df_error, everything(), -`.y[[i]]`)

  return(df)
}
