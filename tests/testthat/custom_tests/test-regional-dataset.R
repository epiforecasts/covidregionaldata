test_regional_dataset <- function(source, level, download = FALSE) {
  region <- eval(parse(
    text = paste0(
      source, "$new(level = '", level,
      "', verbose = FALSE)"
    )
  ))
  region$test(download = download)
}
