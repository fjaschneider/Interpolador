library(lubridate)


evento -> read.csv('data/2014_BCT_1.CSV')




  colnames(evento) = c("data_p", "prec", "data_q", "vazao")
  # ajuste da data
  formato = guess_formats(as.character(evento$data_p[1]), orders = "dmy HM")
  evento$data_p = as.POSIXct(as.character(evento$data_p),format = formato[[2]], origin="1970-01-01 UTC", tzone = "GMT-3")
  evento$data_q = as.POSIXct(as.character(evento$data_q),format = formato[[2]], origin="1970-01-01 UTC", tzone = "GMT-3")
  
  # transformar data em numero
  evento$numerica_p = NA
  evento$numerica_q = NA
  evento$numerica_p = as.numeric(evento$data_p)
  evento$numerica_q = as.numeric(evento$data_q)
  
  #configuracao da serie
  data_inicial = min(na.exclude(evento$numerica_p, evento$numerica_q))
  data_inicial  =as.POSIXct(data_inicial, origin="1970-01-01 UTC", tzone = "GMT-3")
  
  data_final = max(na.exclude(evento$numerica_p, evento$numerica_q))
  data_final = as.POSIXct(data_final, origin="1970-01-01 UTC", tzone = "GMT-3")
  
  # serie temporal
  serieperiodo = interval(data_inicial,data_final)
  comprimentoserie <- time_length(serieperiodo,"minute")
  periodo <- data_inicial + minutes(seq(0,comprimentoserie,intervalo))
  periodo = data.frame(numerica = as.numeric(periodo), data = periodo)
  
  # precipitacao acumulada
  evento$p_acum = NA
  acumulada = 0
  for(i in 1:nrow(evento)){
    acumulada = evento$prec[i] + acumulada
    evento$p_acum[i] = acumulada
  }
  
  # interpolacao precipitacao
  v_tempo = as.vector(periodo$numerica) #periodo para interpolacao
  interpolada_p = approx(evento$numerica_p, evento$p_acum, xout = v_tempo, yleft = 0)
  interpolada_p = as.data.frame(interpolada_p)
  colnames(interpolada_p) = c("numerica", "p_acum")
  interpolada_p$data = as.POSIXct(interpolada_p$numerica, origin="1970-01-01 UTC", tzone = "GMT-3")
  
  #desacumula
  interpolada_p$p = NA
  interpolada_p$p[1] = interpolada_p$p_acum[1]
  for(i in 2:nrow(interpolada_p)){
    interpolada_p$p[i] = interpolada_p$p_acum[i] - interpolada_p$p_acum[i-1]
  }
  
  # interpolacao vazao
  interpolada_q = approx(evento$numerica_q, evento$vazao, xout = v_tempo, yleft = evento$vazao[1])
  interpolada_q =  as.data.frame(interpolada_q)
  colnames(interpolada_q) = c("numerica","vazao")
  interpolada_q$data = as.POSIXct(interpolada_q$numerica, origin="1970-01-01 UTC", tzone = "GMT-3")
  
  vazao_maxima = max(interpolada_q$vazao)
  
  # Saida
  saida = data.frame(data = interpolada_p$data, precipitacao = interpolada_p$p, vazao = interpolada_q$vazao)
  #saida = data.frame(data = interpolada_p$data, 5)
  dire_saida = paste0(diretorio, "/interpolado_30min_",lista_eventos[k])
  write.csv(saida, file = dire_saida)
}
