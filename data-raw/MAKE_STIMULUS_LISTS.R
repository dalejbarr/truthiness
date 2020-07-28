library("funfact") # devtools::install_github("dalejbarr/funfact")
library("tidyverse")

set.seed(62)

stimcond <- stim_lists(
  list(ivs = list(repetition = c("repeated", "new"),
		  interval = c("immediate", "1 day",
			       "1 week", "1 month"),
		  actual_truth = c(FALSE, TRUE)),
       n_item = 128L,
       between_item = "actual_truth")) %>%
  rename(stim_id = item_id) %>%
  as_tibble()

write_csv(stimcond, "stimulus_conditions.csv")

interest_list <- stimcond %>%
  filter(repetition == "repeated") %>%
  mutate(task = "interest",
    task_id = sprintf("IN%03d", stim_id)) %>%
  select(-repetition, -interval, -actual_truth) %>%
  mutate(phase_id = 1L)

truth_list <- stimcond %>%
  mutate(phase_id = recode(interval,
			   "immediate" = 1L,
			   "1 day" = 2L,
			   "1 week" = 3L,
			   "1 month" = 4L)) %>%
  mutate(task = "truth",
	 task_id = sprintf("TR%03d", stim_id)) %>%
  select(list_id, stim_id, task, task_id, phase_id)

plists <- bind_rows(interest_list, truth_list) %>%
  arrange(list_id, task) %>%
  nest(data = c(stim_id, task_id)) %>%
  mutate(order = map(data, ~sample(seq_len(nrow(.x))))) %>%
  unnest(c(data, order)) %>%
  arrange(list_id, phase_id, task, order) %>%
  select(phase_id, list_id, everything())

## run some tests

## make sure each phase has the right number of trials
count_items <- count(plists, list_id, phase_id, task)
stopifnot(nrow(count_items) == 40L)
stopifnot(identical(count_items[["n"]], rep(c(64L, 32L, 32L, 32L, 32L),
					    times = 8L)))

## make sure each statement appears in all conditions across lists
item_list_count <- stimcond %>%
  count(stim_id, repetition, interval)
stopifnot(nrow(item_list_count) == 128L * 8L)
stopifnot(all(item_list_count[["n"]] == 1L))

## work backwards and figure out what condition each statement is in
test_interest <- plists %>%
  filter(task == "interest") %>%
  select(list_id, stim_id) %>%
  mutate(repetition = "repeated")

test_cond <- plists %>%
  filter(task == "truth") %>%
  select(list_id, phase_id, stim_id) %>%
  left_join(test_interest, c("list_id", "stim_id")) %>%
  replace_na(list(repetition = "novel")) %>%
  mutate(interval = recode(phase_id,
			   "1" = "immediate",
			   "2" = "1 day",
			   "3" = "1 week",
			   "4" = "1 month"))

## make sure the rows in the two tables match
stopifnot(setequal(test_cond %>%
		   select(list_id, stim_id, repetition, interval),
		   stimcond %>%
		   select(list_id, stim_id, repetition, interval)))

## ok if we've got this far, we've passed all the tests. write it out
write_csv(plists, "presentation_lists.csv")

## statements_old <- read_csv("stimulus_materials.csv",
##                            col_types = "icl")

statements_pre <- read_csv("stimulus_topicCategorisations.csv",
                           col_types = "iclcc")

statements <- statements_pre %>%
  select(stim_id, statement, actual_truth)

statements_cat <- statements_pre %>%
  select(stim_id, `1st category`, `2nd category`) %>%
  pivot_longer(-stim_id, names_to = "order", values_to = "category") %>%
  filter(!is.na(category))

## now write out to separate files
todo <- plists %>%
  inner_join(statements %>% select(-actual_truth), "stim_id") %>%
  arrange(phase_id, task, list_id, order) %>%
  select(phase_id, list_id, task_id, statement) %>%
  split(list(.$phase_id, .$list_id))

if (dir.exists("qualtrics_files"))
  unlink("qualtrics_files", TRUE, TRUE)

dir.create("qualtrics_files", FALSE)

## write them out
walk(todo,
     function(x) {
       fname <- sprintf("qualtrics_files/P%dL%d.csv", x$phase_id[1], x$list_id[1])
       x %>% select(-phase_id, -list_id) %>% write_csv(fname)
     })

stimulus_conditions <- read_csv("stimulus_conditions.csv",
                                col_types = "iilcc") %>%
  mutate(list_id = factor(list_id, levels = 1:8),
         stim_id = factor(stim_id),
         repetition = factor(repetition, c("repeated", "new")),
         interval = factor(interval, c("immediate", "1 day",
                                       "1 week", "1 month")))

usethis::use_data(stimulus_conditions, overwrite = TRUE)

stimulus_materials <- statements %>%
  mutate(stim_id = factor(stim_id))

usethis::use_data(stimulus_materials, overwrite = TRUE)
