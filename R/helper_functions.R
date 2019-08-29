#' Rename statistics columns
#'
#' Renames the statistics columns.
#'
#' @param x The output of a statistical model, converted to a data frame.
#'
#' @details This function matches each column name with an entry in a list of
#' statistical names and renames the column if necessary. The goal is to create
#' a naming scheme that makes statistical labels as consistent as possible.
#'
#' @importFrom rlang UQS
#'
#' @export
rename_columns <- function(x) {
  # List of statistical labels
  renamers <- c(
    "AIC" = "AIC",
    "BIC" = "BIC",
    "deviance" = "deviance",
    "Deviance" = "deviance",
    "Resid. Dev" = "residual deviance",
    "logLik" = "log-likelihood",
    "Df" = "df",
    "Chi.Df" = "df",
    "Chi Df" = "chi-squared df",
    "Sum Sq" = "SS",
    "Sum.Sq" = "SS",
    "Sum of Sq" = "SS",
    "Mean Sq" = "MS",
    "Mean.Sq" = "MS",
    "F" = "F",
    "F value" = "F",
    "F.value" = "F",
    "Pr(>F)" = "p",
    "num Df" = "numerator df",
    "den Df" = "denominator df",
    "Res.Df" = "denominator df",
    "Resid. Df" = "denominator df",
    "RSS" = "RSS",
    "Chisq" = "chi-squared",
    "Chi.sq" = "chi-squared",
    "LR.Chisq" = "LR chi-squared",
    "LR Chisq" = "LR chi-squared",
    "P(>|Chi|)" = "p",
    "Pr(>Chi)" = "p",
    "Pr(>Chisq)" = "p",
    "Pr..Chisq." = "p",
    "Pr..Chi." = "p",
    "Pr(>|z|)" = "p",
    "Pr(>|t|)" = "p",
    "Pr..F." = "p",
    "p.value" = "p",
    "edf" = "EDF",
    "Ref.df" = "df",
    "Estimate" = "b",
    "Std. Error" = "SE",
    "z value" = "z",
    "t value" = "t",

    "raw_alpha" = "raw alpha",
    "std.alpha" = "standardized alpha",
    "average_r" = "average r",
    "median_r" = "median r",
    "G6(smc)" = "G6",
    "S/N" = "signal/noise ratio",
    "ase" = "alpha SE",

    "mean" = "M",
    "sd" = "SD",

    "emmean" = "EMM"
  )

  # Rename columns
  colnames(x) <- dplyr::recode(colnames(x), UQS(renamers))

  return(x)
}

#' Translate statistic column
#'
#' Very often statistical methods use their own esoteric names for
#' common (and less common) statistical methods. This method
#' translates those esoteric names to hopefully more sensible names
#' using a dictionary.
#'
#' @param tibble A tibble with a column called 'statistic'
#' @param dictionary A dictionary
#'
#' @return The provided tibble with the values in the column
#'     'statistic' replaced according to the dictionary
#'
translate_statistic <- function(tibble, dictionary) {
  if(!is_dictionary(dictionary)) {
    stop("Provided dictionary is of a wrong format")
  }
  if(!tibble::is_tibble(tibble)) {
    stop("Provided tibble is not actually a tibble")
  }
  if(!("statistic" %in% names(tibble))) {
    stop("column 'statistic' not found in provided tibble")
  }

  # Depending on whether the column 'value' exists in the provided
  # tibble, dplyr's join either will or will not suffix the
  # dictionarys 'value' column with ".dict"
  val <- ifelse("value" %in% names(tibble),
                expr(value.dict),
                expr(value))
  
  dplyr::left_join(tibble,
                   dictionary,
                   by = c("statistic" = "key"),
                   suffix = c("",".dict")) %>%
    dplyr::mutate(statistic = dplyr::if_else(is.na(!!val),
                                             statistic,
                                             !!val)) %>%
      dplyr::select(-!!val) # Cleanup
}

#' Tests whether something is a dictionary
#'
#' @param object The object to test
#' @return TRUE if the object is a dictionary, FALSE otherwise
is_dictionary <- function(object) {
    tibble::is_tibble(object) &
        "key" %in% names(object) &
        "value" %in% names(object) &
        ncol(object) == 2
}

