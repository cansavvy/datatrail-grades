# C. Savonen

# Download that Google Forms data

library(metricminer)
library(magrittr)

# Find .git root directory
root_dir <- rprojroot::find_root(rprojroot::has_dir(".git"))

# Authorize Google
auth_from_secret("google",
  refresh_token = Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
  access_token = Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
  cache = TRUE
)

curriculum_sheet <- googlesheets4::read_sheet(
"https://docs.google.com/spreadsheets/d/14PRS2qEed3E636QsorJFkgN94ikj3SeF1AgpoC-0bo0/edit#gid=971038940")

# Remove NAs
quiz_urls <- curriculum_sheet$quiz_url[!is.na(curriculum_sheet$quiz_url)]

#### Get the Google Forms data
google_forms <- get_multiple_forms(form_ids = quiz_urls, dataformat = "raw")

quiz_info <- data.frame(
  quiz_link = names(unlist(purrr::map(google_forms, ~ .x$form_metadata$result$info$documentTitle))),
  quiz = unlist(purrr::map(google_forms, ~ .x$form_metadata$result$info$documentTitle)),
  total_questions = unlist(purrr::map(google_forms, function(x) nrow(x$form_metadata$result$items)))
) %>% 
  dplyr::mutate(quiz_link = stringr::word(quiz_link, 2, sep = "forms\\/d\\/|\\/viewform"))

quiz_grades <- data.frame(
  quiz_link = names(unlist(purrr::map(google_forms, ~ .x$response_info$result$responses$respondentEmail))),
  email = unlist(purrr::map(google_forms, ~ .x$response_info$result$responses$respondentEmail)),
  score = unlist(purrr::map(google_forms, ~ .x$response_info$result$responses$totalScore))
) %>% 
  dplyr::mutate(quiz_link = stringr::word(quiz_link, 2, sep = "forms\\/d\\/|\\/viewform"))

grades <- dplyr::left_join(quiz_grades, quiz_info, by = "quiz_link") %>% 
  dplyr::mutate(grade_perc = score/total_questions) %>% 
  group_by(email, quiz) %>%
  filter(grade_perc == max(grade_perc)) %>% 
  dplyr::select(quiz, email, grade_perc) %>% 
  dplyr::arrange(email, quiz) %>% 
  tidyr::pivot_wider(names_from = quiz, 
                     values_from = grade_perc)

googlesheets4::write_sheet(grades,
                           ss = "https://docs.google.com/spreadsheets/d/1vW0RyyiEGhDzaPowPzz1TkYrLarjkwQIIX7DVWr4wQk/edit#gid=0", 
                           sheet = "quiz_grades")

sessionInfo()
