set.seed(100)
eeda_test_data <- tidyr::tibble(
  target = factor(sample(c("positive", "negative"), 2000, replace = TRUE, prob = c(0.1, 0.9))),
  key_age = round(rlnorm(n = 2000, meanlog = 3.5, sdlog = .34), 1),
  eg_factor_4 = factor(sample(c("dragon", "fish", "raccoon", "dog"), 2000, replace = TRUE, prob = c(0.3, 0.3, 0.3, 0.1))),
  eg_factor_4_na = eg_factor_4,
  eg_factor_12 = factor(sample(colors()[37:48], 2000, replace = TRUE)),
  eg_factor_12_na = eg_factor_12,
  eg_factor_50 = factor(sample(state.name, 2000, replace = TRUE)),
  eg_factor_50_na = eg_factor_50,
  eg_binary = sample(c(0, 1), 2000, replace = TRUE),
  eg_binary_na = eg_binary,
  eg_logical = sample(c(FALSE, TRUE), 2000, replace = TRUE),
  eg_logical_na = eg_logical,
  eg_continuous = as.numeric(NA),
  eg_continuous_na = eg_continuous,
  eg_integer_7 = sample(c(47, 53, 59, 61, 67, 71, 73), 2000, replace = TRUE),
  eg_integer_7_na = eg_integer_7,
  eg_integer_50 = sample(1:50, 2000, replace = TRUE),
  eg_integer_50_na = eg_integer_50,
  eg_character = sample(ids::adjective_animal(n = 1500, style = "title"), 2000, replace = TRUE),
  eg_character_na = eg_character,
  eg_long_character = purrr::map_chr(1:2000, ~ paste(stringr::str_split(stringi::stri_rand_lipsum(1, FALSE), " ")[[1]][1:4], collapse = " ")),
  eg_long_character_na = eg_long_character,
  eg_date = Sys.Date() - rpois(n = 2000, lambda = 1) * 100,
  eg_date_na = eg_date
)

n_rows <- nrow(eeda_test_data)

eeda_test_data <- eeda_test_data %>%
  dplyr::mutate(
    # create association between eg_factor_4 and target and between eg_factor_4 and key_age
    eg_factor_4 = factor(ifelse(target == "positive" & key_age > 40, "dragon", as.character(eg_factor_4))),

    # create association between eg_continous and key_age
    eg_continuous = runif(2000) * 100 + 10 * pmin(key_age, 45)^.53,

    # create association between eg_continous and target == Positive
    eg_continuous = eg_continuous + ifelse(target == "positive", 30, 0),

    # add NA values
    eg_continuous_na = replace(eg_continuous, sample(1:n_rows, 200), NA),
    eg_factor_4_na = replace(eg_factor_4, sample(1:nrow(eeda_test_data), 200), NA),
    eg_factor_12_na = replace(eg_factor_12, sample(1:nrow(eeda_test_data), 200), NA),
    eg_factor_50_na = replace(eg_factor_50, sample(1:nrow(eeda_test_data), 200), NA),
    eg_binary_na = replace(eg_binary, sample(1:nrow(eeda_test_data), 200), NA),
    eg_integer_7_na = replace(eg_integer_7, sample(1:nrow(eeda_test_data), 200), NA),
    eg_integer_50_na = replace(eg_integer_50, sample(1:nrow(eeda_test_data), 200), NA),
    eg_character_na = replace(eg_character, sample(1:nrow(eeda_test_data), 200), NA),
    eg_long_character_na = replace(eg_long_character, sample(1:nrow(eeda_test_data), 200), NA),
    eg_date_na = replace(eg_date, sample(1:nrow(eeda_test_data), 200), NA)
  )

usethis::use_data(eeda_test_data, overwrite = TRUE)
