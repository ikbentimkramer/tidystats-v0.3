setup({
    dict <- tibble::tibble(key = "a", value = "x")
})

test_that("It throws appropriate errors", {
    input <- data.frame(stats = "a", stringsAsFactors = FALSE)
    input2 <- tibble::as_tibble(input)
    input3 <- tibble::as_tibble(input) %>% dplyr::rename(statistic = stats)

    expect_error(translate_statistic(!!input, dict))
    expect_error(translate_statistic(!!input2, dict))
    expect_silent(translate_statistic(!!input3, dict))
})

test_that("It works with and without a 'value' column in data", {
    input <- tibble::tibble(statistic = "a")
    input2 <- tibble::tibble(statistic = "a", value = 0.5)
    expected <-  tibble::tibble(statistic = "x")
    expected2 <- tibble::tibble(statistic = "x", value = 0.5)

    expect_identical(translate_statistic(!!input, dict), !!expected)
    expect_identical(translate_statistic(!!input2, dict), !!expected2)
})

test_that("It copies values if they do not occur in the dictionary",{
    input <- tibble::tibble(statistic = "b")

    expect_identical(translate_statistic(!!input, !!dict), !!input)
})
