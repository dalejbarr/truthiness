.oc <- as.character(toBibtex(citation("ordinal")))
.oc[1] <- sub("\\{,\\s*$", "{ordinal,", "@Misc{,")
writeLines(.oc, "refs.bib")
