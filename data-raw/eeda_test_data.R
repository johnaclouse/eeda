set.seed(47)
eeda_test_data <- tidyr::tibble(
  eg_factor_4 = factor(sample(c("Dragon", "Fish", "Raccoon", "Dog"), 2000, replace = TRUE)),
  eg_factor_4_na = eg_factor_4,
  eg_factor_12 = factor(sample(colors(), 2000, replace = TRUE)),
  eg_factor_12_na = eg_factor_12,
  high_cardinality = sample(state.name, 2000, replace = TRUE),
  high_cardinality_na = high_cardinality,
  eg_binary = sample(c(0, 1), 2000, replace = TRUE),
  eg_binary_na = eg_binary,
  eg_logical = sample(c(FALSE, TRUE), 2000, replace = TRUE),
  eg_logical_na = eg_logical,
  eg_continuous = runif(2000) * 100000,
  eg_continuous_na = eg_continuous,
  eg_integer = sample(1:100, 2000, replace = TRUE),
  eg_integer_na = eg_integer,
  eg_character = sample(
    ids::adjective_animal(n = 1500, style = "title"),
    2000,
    replace = TRUE
  ),
  eg_character_na = eg_character,
  eg_long_character = purrr::map_chr(
    1:2000,
    ~ paste(stringr::str_split(stringi::stri_rand_lipsum(1, FALSE), " ")[[1]][1:4], collapse = " ")
  ),
  eg_long_character_na = eg_long_character,
  eg_date = Sys.Date() - rpois(n = 2000, lambda = 1) * 100,
  eg_date_na = eg_date
)

eeda_test_data[sample(1:nrow(eeda_test_data), 200), "eg_factor_4_na"] <- NA
eeda_test_data[sample(1:nrow(eeda_test_data), 200), "eg_factor_12_na"] <- NA
eeda_test_data[sample(1:nrow(eeda_test_data), 200), "high_cardinality_na"] <- NA
eeda_test_data[sample(1:nrow(eeda_test_data), 200), "eg_continuous_na"] <- NA
eeda_test_data[sample(1:nrow(eeda_test_data), 200), "eg_binary_na"] <- NA
eeda_test_data[sample(1:nrow(eeda_test_data), 200), "eg_logical_na"] <- NA
eeda_test_data[sample(1:nrow(eeda_test_data), 200), "eg_integer_na"] <- NA
eeda_test_data[sample(1:nrow(eeda_test_data), 200), "eg_character_na"] <- NA
eeda_test_data[sample(1:nrow(eeda_test_data), 200), "eg_long_character_na"] <- NA
eeda_test_data[sample(1:nrow(eeda_test_data), 200), "eg_date_na"] <- NA

usethis::use_data(eeda_test_data, overwrite = TRUE)
