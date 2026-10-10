prepared <- setNames(lapply(studies, read_data), studies)
saveRDS(prepared, prepared_data_file)
