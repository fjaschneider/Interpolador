## To Join tables

directory <- 'data/data_30min/' ## Choose the directory
list_tables <- list.files(directory)


for (i in 1:length(list_tables)) {
  table <- paste0(directory, "/", list_tables[i])
  table <- read.csv(table)
  
  complet_table <- rbind(complet_table, table)

}
