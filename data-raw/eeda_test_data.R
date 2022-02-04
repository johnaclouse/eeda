# setup ----
n_rows <- 2000
set.seed(100)

create_events <- function(number_of_individuals = 100,
                          number_of_days = 365,
                          events_per_year = c(
                            1 / 365,
                            2.5 / 365,
                            12 / 365,
                            52 / 365
                          ),
                          event_weights = c(
                            0.15,
                            0.78,
                            0.06,
                            0.01
                          ),
                          visit_cost = c(150, 1500, 15000, 150000),
                          visit_weights = c(.8, .09, .009, 0.001)) {
  events <- data.frame(
    id = as.numeric(),
    day = as.numeric(),
    monetary = as.numeric()
  )
  for (i in 1:number_of_individuals) {
    rate <- sample(events_per_year, 1, prob = event_weights)
    for (j in 1:number_of_days) {
      if (rpois(1, rate) > 0) {
        new_event <-
          data.frame(
            id = i,
            day = j,
            monetary = sample(visit_cost, 1, prob = visit_weights) *
              (1 + rnorm(1) / 10)
          )
        events <- rbind(events, new_event)
      }
    }
  }
  return(events)
}



# events ----
events <-
  dplyr::left_join(
    tibble::tibble(id = 1:n_rows),
    create_events(n_rows) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(
        recency = min(day),
        frequency = dplyr::n(),
        monetary = round(sum(monetary))
      ),
    by = "id"
  ) %>%
  dplyr::distinct(
    id,
    recency,
    frequency,
    monetary
  ) %>%
  dplyr::mutate(rfm = efe::quantile_of_quantiles(., c("recency", "frequency", "monetary")))



# data set ----
eeda_test_data <- tidyr::tibble(
  target = factor(sample(c("positive", "negative"), n_rows, replace = TRUE, prob = c(0.1, 0.9))),
  key_age = round(rlnorm(n = n_rows, meanlog = 3.5, sdlog = .34), 1),
  key_recency = events$recency,
  key_frequency = events$frequency,
  key_monetary = events$monetary,
  key_rfm = events$rfm,
  key_is_left_handed = sample(c(0, 1), n_rows, replace = TRUE, prob = c(0.9, 0.1)),
  key_role = factor(sample(c("rouleur", "grimpeur", "puncheur", "sprinteurs", "domestique", "gc"), n_rows, replace = TRUE, prob = c(1, 3, 1, 1, 4, 1))),
  eg_factor_4 = factor(sample(c("dragon", "fish", "raccoon", "dog"), n_rows, replace = TRUE, prob = c(0.3, 0.3, 0.3, 0.1))),
  eg_factor_4_na = eg_factor_4,
  eg_factor_12 = factor(sample(colors()[37:48], n_rows, replace = TRUE)),
  eg_factor_12_na = eg_factor_12,
  eg_factor_50 = factor(sample(state.name, n_rows, replace = TRUE)),
  eg_factor_50_na = eg_factor_50,
  eg_binary = sample(c(0, 1), n_rows, replace = TRUE),
  eg_binary_na = eg_binary,
  eg_logical = sample(c(FALSE, TRUE), n_rows, replace = TRUE),
  eg_logical_na = eg_logical,
  eg_continuous = as.numeric(NA),
  eg_continuous_na = eg_continuous,
  eg_integer_7 = sample(c(47, 53, 59, 61, 67, 71, 73), n_rows, replace = TRUE),
  eg_integer_7_na = eg_integer_7,
  eg_integer_50 = sample(1:50, n_rows, replace = TRUE),
  eg_integer_50_na = eg_integer_50,
  eg_character = sample(ids::adjective_animal(n = 1500, style = "title"), n_rows, replace = TRUE),
  eg_character_na = eg_character,
  eg_long_character = purrr::map_chr(1:n_rows, ~ paste(stringr::str_split(stringi::stri_rand_lipsum(1, FALSE), " ")[[1]][1:4], collapse = " ")),
  eg_long_character_na = eg_long_character,
  eg_date = Sys.Date() - rpois(n = n_rows, lambda = 1) * 100,
  eg_date_na = eg_date
)



# modify data ----
eeda_test_data <- eeda_test_data %>%
  dplyr::mutate(
    # create association between eg_factor_4 and target and between eg_factor_4 and key_age
    eg_factor_4 = factor(ifelse(target == "positive" & key_age > 40, "dragon", as.character(eg_factor_4))),

    # create association between eg_continous and key_age
    eg_continuous = runif(n_rows) * 100 + 10 * pmin(key_age, 45)^.53,

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

    # calculate key_rfm
    # key_rfm =
    #   quantile()
  )



usethis::use_data(eeda_test_data, overwrite = TRUE)
