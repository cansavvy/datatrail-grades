# C. Savonen

# Download that Google Forms data

remotes::install_github("fhdsl/metricminer", auth = Sys.getenv("GH_PAT"))
remotes::install_github("datatrail-jhu/rgoogleclassroom", auth = Sys.getenv("GH_PAT"))

library(magrittr)
library(rgoogleclassroom)

# Find .git root directory
root_dir <- rprojroot::find_root(rprojroot::has_dir(".git"))

# token <- rgoogleclassroom::authorize()
# Authorize googleclassroom first
rgoogleclassroom::auth_from_secret(
  refresh_token = Sys.getenv("GOOGLECLASSROOM_REFRESH"),
  access_token = Sys.getenv("GOOGLECLASSROOM_ACCESS")
)

#course_id <- get_course_list()

quizzes <- get_coursework_list(course_id = "653622697082")

quiz_urls <- purrr::map(quizzes$courseWork$materials, ~ .x$form$formUrl)
quiz_name <- purrr::map(quizzes$courseWork$materials, ~ .x$form$title)

quiz_link_df <- data.frame(quiz_urls = unlist(quiz_urls),
                           quiz_name = unlist(quiz_name))

coursework_df <- data.frame(course_id = quizzes$courseWork$courseId,
                            id = quizzes$courseWork$id,
                            title = quizzes$courseWork$title)

quiz_df <- dplyr::left_join(coursework_df, quiz_link_df,
                            by = c("title" = "quiz_name"))
# Remove NAs
quiz_urls <- unlist(quiz_urls)

# Authorize metricminer now
# token <- metricminer::authorize("google")
# Authorize Google
metricminer::auth_from_secret("google",
                              refresh_token = Sys.getenv("METRICMINER_GOOGLE_REFRESH"),
                              access_token = Sys.getenv("METRICMINER_GOOGLE_ACCESS"),
                              cache = TRUE
)

#### Get the Google Forms data
google_forms <- metricminer::get_multiple_forms(form_ids = quiz_urls, dataformat = "raw")

quiz_info <- data.frame(
  quiz_link = names(unlist(purrr::map(google_forms, ~ .x$form_metadata$result$info$documentTitle))),
  quiz = unlist(purrr::map(google_forms, ~ .x$form_metadata$result$info$documentTitle)),
  total_questions = unlist(purrr::map(google_forms, function(x) nrow(x$form_metadata$result$items)))
) %>%
  dplyr::mutate(quiz_link = stringr::word(quiz_link, 2, sep = "forms\\/d\\/|\\/viewform|\\/edit"))

quiz_grades <- data.frame(
  quiz_link = names(unlist(purrr::map(google_forms, ~ .x$response_info$result$responses$respondentEmail))),
  email = unlist(purrr::map(google_forms, ~ .x$response_info$result$responses$respondentEmail)),
  score = unlist(purrr::map(google_forms, ~ .x$response_info$result$responses$totalScore))
) %>%
  dplyr::mutate(quiz_link = stringr::word(quiz_link, 2, sep = "forms\\/d\\/|\\/viewform|\\/edit"))

grades <- dplyr::left_join(quiz_grades, quiz_info, by = "quiz_link") %>%
  dplyr::mutate(grade_perc = score/total_questions) %>%
  dplyr::group_by(email, quiz) %>%
  dplyr::filter(grade_perc == max(grade_perc)) %>%
  dplyr::mutate(index_num = as.numeric(stringr::word(quiz, sep = " ", 1))) %>%
  dplyr::select(index_num, quiz, email, grade_perc) %>%
  dplyr::arrange(index_num, email) %>%
  dplyr::distinct() %>%
  dplyr::select(-index_num) %>%
  tidyr::pivot_wider(names_from = quiz,
                     values_from = grade_perc)

googlesheets4::write_sheet(grades,
                           ss = "https://docs.google.com/spreadsheets/d/1vW0RyyiEGhDzaPowPzz1TkYrLarjkwQIIX7DVWr4wQk/edit#gid=0",
                           sheet = "quiz_grades")

sessionInfo()
