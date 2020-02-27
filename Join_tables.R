## To Join tables

directory <- 'data/data_30min/' ## Choose the directory
list_tables <- list.files(directory)


for (i in 1:length(list_tables)) {
  table <- paste0(directory, "/", list_tables[i])
  table <- read.csv(table)
  
  complet_table <- rbind(complet_table, table)

}

complet_table <- complet_table[ ,-1]
complet_table <- complet_table[(order(as.Date(complet_table$data))), ]
colnames(complet_table) <- c('Date_hour', 'P_mm_h', 'Q_l_s')

write.csv(complet_table, file = 'data/df_BCT_30min.csv', row.names = FALSE)
