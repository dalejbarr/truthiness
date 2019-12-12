library("tidyverse")

## let's replace the prolific IDs with fake ones
## they can be found in cols 12 and 258
pfile <- read_csv("mock_prolific_file.csv",  ## NOT provided
		  col_types = cols(.default = col_character()))

ix <- -(1:3)
pfile[["PID"]][ix] <-
  pfile[["PROLIFIC_PID"]][ix] <- 
  stringr::str_pad(seq_len(nrow(pfile) - 3L),
		   width = 24, side = "left",
		   pad = "_")

pfile[["ResponseId"]][ix] <-
  paste0("R", stringr::str_pad(seq_len(nrow(pfile) - 3L),
			       width = 16, side = "left",
			       pad = "_"))

pfile[["age"]][ix] <- "999"

pfile[["nationality"]][ix] <-
  pfile[["gender"]][ix] <-
  pfile[["UserLanguage"]][ix] <-
  pfile[["StartDate"]][ix] <-
  pfile[["EndDate"]][ix] <-
  pfile[["RecordedDate"]][ix] <-
  pfile[["comments"]][ix] <- "xxxxxxxx"

write_csv(pfile, "mock_prolific_file_anon.csv")
