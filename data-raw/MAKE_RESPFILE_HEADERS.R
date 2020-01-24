## run within the root 'truthiness' directory
devtools::load_all()
library("tidyverse")

colschema <- cols(.default = col_character(),
                  `Duration (in seconds)` = col_integer(),
                  Finished = col_logical(),
                  age = col_integer())

colnames <- readLines("data-raw/mock_prolific_file_anon.csv", 2L)[2] %>%
  strsplit(",") %>%
  pluck(1)

rating_cols <- grep("^[0-9]{1,3}_.*", colnames)
head_cols <- colnames[seq_len(min(rating_cols) - 1L)]
tail_cols <- colnames[(max(rating_cols) + 1L):length(colnames)]

head_rows <- read_csv("data-raw/mock_prolific_file_anon.csv",
                      col_names = FALSE,
                      col_types = cols(.default = col_character())) %>%
  slice(2:4) %>%
  select(-(rating_cols))

left_chunk <- head_rows %>%
  select(seq_len(min(rating_cols) - 1L))

right_chunk <- head_rows %>%
  select(rating_cols[1]:ncol(head_rows))

respfile_headers <- list(
  head_cols = head_cols,
  tail_cols = tail_cols,
  left_chunk = left_chunk,
  right_chunk = right_chunk)

usethis::use_data(respfile_headers, internal = TRUE,
                  overwrite = TRUE)
