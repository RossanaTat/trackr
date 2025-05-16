
data_fut <- data_wdi |>
  filter(!is.na(y)) |>
  group_by(code) |>
  arrange(year) |>
  filter(row_number()==n()) |>
  # Store the last observation rounded in a new column. To be used as the starting point for future targets
  mutate(y_fut = if_else(row_number()==n(),round(y/granularity)*granularity,NA)) |>
  ungroup() |>
  as.data.table()

