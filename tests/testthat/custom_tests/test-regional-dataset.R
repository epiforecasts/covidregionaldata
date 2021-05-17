test_regional_dataset <- function(source, level, download = FALSE) {
  region <- eval(parse(
    text = paste0(
      source, "$new(level = '", level,
      "', verbose = FALSE)"
    )
  ))
  snapshot_path <- paste0(
    "custom_data/", source,
    "_level_", level, ".rds"
  )
  nhs_snapshot_path <- paste0(
    "custom_data/", source,
    "_level_", level, "_nhs", ".rds"
  )
  region$test(
    download = download,
    snapshot_path = snapshot_path,
    nhs_included_path = nhs_snapshot_path
  )
}
