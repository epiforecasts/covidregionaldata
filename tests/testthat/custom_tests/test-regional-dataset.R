test_regional_dataset <- function(source, level, download = FALSE) {
  region <- eval(parse(
    text = paste0(
      source, "$new(level = '", level,
      "', verbose = FALSE)"
    )
  ))
  snapshot_dir <- "custom_data"
  region$test(
    download = download,
    snapshot_dir = snapshot_dir,
    all = TRUE
  )
}
