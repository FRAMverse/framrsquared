# this imports the groups for the stock composition chart

coho_stock_comp_lut <- readr::read_csv("data-raw/coho_lut.csv") |>
  janitor::clean_names() |>
  dplyr::select(stock_id, stock_group)

usethis::use_data(coho_stock_comp_lut, overwrite = TRUE, internal = TRUE)
