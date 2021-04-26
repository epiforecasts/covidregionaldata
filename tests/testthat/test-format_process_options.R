base_options <- list(
  "calculate_columns_from_existing_data" = TRUE,
  "add_extra_na_cols" = TRUE,
  "set_negative_values_to_zero" = TRUE
)

test_that(
  "format_process_options returns base optons with un-named input",
  {
    my_options <- list(
      "nothing", 0
    )
    test <- format_process_options(my_options)
    expect_identical(test, base_options)
  }
)

test_that(
  "format_process_options alters all items",
  {
    my_options <- list(
      "calculate_columns_from_existing_data" = FALSE,
      "add_extra_na_cols" = FALSE,
      "set_negative_values_to_zero" = FALSE
    )
    test <- format_process_options(my_options)
    expect_identical(test, my_options)
  }
)

test_that(
  "format_process_options alters some items",
  {
    actual <- list(
      "calculate_columns_from_existing_data" = TRUE,
      "add_extra_na_cols" = TRUE,
      "set_negative_values_to_zero" = FALSE
    )
    my_options <- list(
      "set_negative_values_to_zero" = FALSE
    )
    test <- format_process_options(my_options)
    expect_identical(test, actual)
  }
)

test_that(
  "format_process_options returns base items with no input",
  {
    my_options <- list()
    test <- format_process_options(my_options)
    expect_identical(test, base_options)
  }
)

test_that(
  "format_process_options raises error when input is not a list",
  {
    my_options <- c()
    expect_error(
      format_process_options(my_options),
      "Process options must be a list"
    )
  }
)

test_that(
  "format_process_options raises error when function given is not avaliable",
  {
    my_options <- list("a_made_up_function_name" = TRUE)
    expect_error(
      format_process_options(my_options),
      "option not supported",
      fixed = TRUE
    )
  }
)

test_that(
  "format_process_options raises error when value given is not logical",
  {
    my_options <- list("set_negative_values_to_zero" = 100)
    expect_error(
      format_process_options(my_options),
      "argument for set_negative_values_to_zero is not logical",
      fixed = TRUE
    )
  }
)
