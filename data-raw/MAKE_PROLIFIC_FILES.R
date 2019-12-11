library("truthiness")
## the code below is used to create prolific files

## now write out to separate files
todo <- presentation_lists %>%
  inner_join(stimulus_materials %>% select(-actual_truth), "stim_id") %>%
  arrange(phase_id, task, list_id, order) %>%
  select(phase_id, list_id, task_id, statement) %>%
  split(list(.$phase_id, .$list_id))

if (dir.exists("prolific_files"))
  unlink("prolific_files", TRUE, TRUE)

dir.create("prolific_files", FALSE)

## write them out
walk(todo,
     function(x) {
       fname <- sprintf("prolific_files/P%dL%d.csv", x$phase_id[1], x$list_id[1])
       x %>% select(-phase_id, -list_id) %>% write_csv(fname)
     })
