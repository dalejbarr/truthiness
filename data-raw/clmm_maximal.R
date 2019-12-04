library("tidyverse")
library("ordinal")

time <- system.time(
  mod <- clmm(trating ~ R * D + 
                (1 + R + D + R:D | subj_id) +
                (1 + R + D + R:D | item_id),
              NE_exp1)
)
