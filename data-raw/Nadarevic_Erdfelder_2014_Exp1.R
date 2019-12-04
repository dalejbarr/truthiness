library("tidyverse")

dat <- read_csv("Nadarevic_Erdfelder_2014_Exp1.csv",
                col_types = "ciiiicicclccididc") %>%
  filter(runningtrial != "Buffer") %>%
  select(-experimentname) 

items <- dat %>%
  distinct(statement, set, status) %>%
  arrange(set, statement) %>%
  mutate(item_id = row_number()) %>%
  select(item_id, everything())

cbalance <- dat %>%
  select(-set, -status,
         -runningtrial) %>% # redundant with 'phase'
  inner_join(items, "statement") %>%
  count(subject, session, phase, set)

excluded_subjects <- distinct(dat, subject, phase) %>%
  count(subject) %>%
  filter(n < 3L)

dat2 <- anti_join(dat, excluded_subjects, "subject") %>%
  inner_join(items %>% select(-set, -status), "statement") %>%
  select(-runningtrial, -statement, -set, -status, -session,
         -irating, -irating.rt) %>%
  select(subject, item_id, everything()) %>%
  rename(rp2 = repeated_phase2, rp3 = repeated_phase3)

## derive patterns of item repetition for each participant
itmcond <- dat2 %>%
  select(subject, item_id, phase) %>%
  mutate(phase = paste0("p", phase), junk = TRUE) %>%
  spread(phase, junk, fill = FALSE) %>%
  mutate(cond = case_when( p1 &  p2 & !p3 ~ "old_10m",
                          p1 & !p2 &  p3 ~ "old_1w",
                          !p1 &  p2 & !p3 ~ "new_10m",
                          !p1 & !p2 &  p3 ~ "new_1w",
                          TRUE ~ NA_character_)) %>%
  select(-p1, -p2, -p3)

cblists <- itmcond %>%
  spread(item_id, cond)

alllists <- cblists %>%
  select(-subject) %>%
  distinct() %>%
  mutate(list_id = row_number()) %>%
  select(list_id, everything())

subjects <- inner_join(cblists, alllists,
                       setdiff(names(cblists), "subject")) %>%
  select(subject, list_id)

listcond <- cblists %>%
  select(-subject) %>%
  distinct() %>%
  mutate(list_id = row_number()) %>%
  select(list_id, everything()) %>%
  gather("item_id", "cond", -list_id, convert = TRUE) %>%
  arrange(list_id, item_id) %>%
  separate(cond, c("repetition", "delay"))

trials <- dat2 %>%
  select(subject, item_id, phase, trial, trating, filter)

## run a quick check

tcond <- trials %>%
  inner_join(subjects, "subject") %>%
  inner_join(listcond, c("list_id", "item_id")) %>%
  filter(phase != 1L) %>%
  count(subject, item_id, repetition, delay)

dcond <- dat2 %>%
  filter(phase != 1L) %>%
  select(subject, item_id, rp2, rp3)

inner_join(tcond, dcond, c("subject", "item_id")) %>%
  distinct(repetition, delay, rp2, rp3)

NE_exp1 <- subjects %>%
  rename(subj_id = subject) %>%
  inner_join(listcond, "list_id") %>%
  inner_join(items %>% select(-statement, -set), c("item_id")) %>%
  inner_join(trials %>% rename(subj_id = subject), c("subj_id", "item_id")) %>%
  filter(phase != 1L, filter == "selected") %>%
  select(-list_id, -phase, -filter, -status, -trial) %>%
  mutate(subj_id = factor(subj_id),
         item_id = factor(item_id),
         repetition = factor(repetition),
         R = if_else(repetition == "old", .5, -.5),
         D = if_else(delay == "10m", .5, -.5),
         delay = factor(delay),
         trating = factor(trating, levels = 1:6, ordered = TRUE))

NE_items <- items %>%
  mutate(item_id = factor(item_id))

usethis::use_data(NE_exp1, overwrite = TRUE)
usethis::use_data(NE_items, overwrite = TRUE)
