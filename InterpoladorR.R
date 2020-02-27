## Adaptado de Felipe Bernardi

library(lubridate)


diretorio_entrada <- 'data/brut_data_BST/'
diretorio_saida <- 'data/data_30min_bst/'
list_eventos <- list.files(diretorio_entrada)
intervalo <- 30 #Define o intervalo da interpolacao em minutos!


for (k in 1:length(list_eventos)) {
  evento_arq <- paste0(diretorio_entrada, "/", list_eventos[k])
  evento <- read.csv(evento_arq)
  evento <- evento[ ,c(2,3,5,6)]
  colnames(evento) <- c("data_q", "vazao", "data_p", "prec")

  # ajuste da data
  evento$data_p <- ymd_hms(evento$data_p)
  evento$data_q <- ymd_hms(evento$data_q)
  
  #configuracao da serie
  data_inicial <- min(na.exclude(evento$data_p, evento$data_q))
  data_inicial <- as.POSIXct(data_inicial, origin="1970-01-01 UTC", tzone = "GMT-3")
  
  data_final <- max(na.exclude(evento$data_p, evento$data_q))
  data_final <- as.POSIXct(data_final, origin="1970-01-01 UTC", tzone = "GMT-3")
  
  # serie temporal
  serieperiodo <- interval(data_inicial,data_final)
  comprimentoserie <- time_length(serieperiodo,"minute")
  periodo <- data_inicial + minutes(seq(0,comprimentoserie,intervalo))
  periodo <- data.frame(numerica = as.numeric(periodo), data = periodo)
  
  # precipitacao acumulada
  evento$p_acum <- NA
  acumulada <- 0
  for(i in 1:nrow(evento)){
    acumulada <- evento$prec[i] + acumulada
    evento$p_acum[i] <- acumulada
  }
  
  # interpolacao precipitacao
  v_tempo <- as.vector(periodo$numerica) #periodo para interpolacao
  interpolada_p <- approx(evento$data_p, evento$p_acum, xout = v_tempo, yleft = 0)
  interpolada_p <- as.data.frame(interpolada_p)
  colnames(interpolada_p) <- c("numerica", "p_acum")
  interpolada_p$data <- as.POSIXct(interpolada_p$numerica, origin="1970-01-01 UTC", tzone = "GMT-3")
  
  #desacumula
  interpolada_p$p <- NA
  interpolada_p$p[1] <- interpolada_p$p_acum[1]
  for(i in 2:nrow(interpolada_p)){
    interpolada_p$p[i] <- interpolada_p$p_acum[i] - interpolada_p$p_acum[i-1]
  }
  
  
  
  # vazao acumulada
  evento$q_acum <- NA
  acumuladaq <- 0
  for(i in 1:nrow(evento)){
    acumuladaq <- evento$vazao[i] + acumuladaq
    evento$q_acum[i] <- acumuladaq
  }
  
  
  # interpolacao vazao
  interpolada_q <- approx(evento$data_q, evento$q_acum, xout = v_tempo, yleft = 0)
  interpolada_q <- as.data.frame(interpolada_q)
  colnames(interpolada_q) <- c("numerica", "q_acum")
  interpolada_q$data <- as.POSIXct(interpolada_q$numerica, origin="1970-01-01 UTC", tzone = "GMT-3")
  
  #desacumula vazao
  interpolada_q$q <- NA
  interpolada_q$q[1] <- interpolada_q$q_acum[1]
  for(i in 2:nrow(interpolada_q)){
    interpolada_q$q[i] <- interpolada_q$q_acum[i] - interpolada_q$q_acum[i-1]
  }
  
  
  # Saida
  saida <- data.frame(data = interpolada_p$data, precipitacao = interpolada_p$p, vazao = interpolada_q$q)
  dire_saida <- paste0(diretorio_saida, "/interpolado_30min_",list_eventos[k])
  write.csv(saida, file = dire_saida)
  
}
