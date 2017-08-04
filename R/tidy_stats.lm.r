#' tidy_stats method for an lm object
#'
#' Creates a tidystats data frame for an lm object.

#' @examples
#' lm_model <- lm(extra ~ group, data = sleep)
#' tidy_stats.lm(lm_model)

#'@import dplyr
#'@importFrom magrittr %>%

#'@export
tidy_stats.lm <- function(model) {
  # Create tidy stats data frame
  output <- data_frame(
    method = rep("Linear regression", length(model$coefficients))
  )

  # Add term(s)
  output$term <- names(model$coefficients)

  # Add estimates, standard errors, t values, and p values
  output <- bind_cols(output, as_data_frame(summary(model)$coefficients))

  # Add df
  output$df_res <- model$df.residual

  # Rename variables
  output <- rename(output,
                   estimate = Estimate,
                   std_error = `Std. Error`,
                   statistic = `t value`,
                   p_value = `Pr(>|t|)`)

  # Create a separate data frame to store the model fit statistics in
  output_fit <- data_frame(
    method = "Linear regression",
    term = "(Model)",
    statistic = summary(model)$fstatistic[1],
    df = summary(model)$fstatistic[2],
    df_res = summary(model)$fstatistic[3],
    r_squared = summary(model)$r.squared,
    adjusted_r_squared = summary(model)$adj.r.squared
  )

  output_fit$p_value <- pf(output_fit$statistic, output_fit$df, output_fit$df_res,
                           lower.tail = FALSE)

  # Add the model fit results to the coefficients results
  output <- bind_rows(output, output_fit)

  return(output)
}
