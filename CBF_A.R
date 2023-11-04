setwd("~/Estudos/Sports_Analytics/Betting")

library(rvest) # pacote para web scraping
library(tidyverse)
library(MASS)

dado_cbf <- read_html("https://www.cbf.com.br")

dado_cbf <- dado_cbf %>% 
  html_nodes(".swiper-slide")

rodada <- dado_cbf %>% 
  html_nodes(".aside-header .text-center") %>% 
  html_text()

resultados <- dado_cbf %>% 
  html_nodes(".aside-content .clearfix")

#casa <- resultados %>% 
#  html_nodes(".pull-left .time-sigla") %>% 
#  html_text()

casa <- resultados %>% 
  html_nodes(".pull-right") %>%
  html_attrs() %>%
  map(2) %>%
  unlist()

#fora_casa <- resultados %>% 
#  html_nodes(".pull-right .time-sigla") %>% 
#  html_text()

fora <- resultados %>% 
  html_nodes(".pull-left") %>%
  html_attrs() %>%
  map(2) %>%
  unlist()

placar <- resultados %>% 
  html_nodes(".partida-horario") %>% 
  html_text() %>% 
  str_extract("[0-9]{1}\ x\ [0-9]{1}")

placar_casa <- as.numeric(substr(placar,1,2))
placar_fora <- as.numeric(substr(placar,nchar(placar)-1,nchar(placar)))

rodada <- 0:(length(placar)-1) %/% 10 + 1

df <- data.frame(rodada = rodada,
                 casa = casa, 
                 placar_casa = placar_casa,
                 placar_fora = placar_fora,
                 fora = fora,
                 stringsAsFactors = FALSE) # %>% 
#filter(rodada <= 18)

# Organização dos dados

(times <- sort(unique(df$casa)))
jogospas <- df[!is.na(df$placar_casa),]
rodadaspas <- max(jogospas$rodada)
rodadaspas <- 30

# Se usar alisamento exponencial
amortecimento <- 1/5
vetor <- amortecimento
for (i in 1:(rodadaspas-1)) {
  vetor <- c(amortecimento*(1-amortecimento)^i,vetor)
}
vetor <- as.data.frame(vetor)
vetor$rodada <- rownames(vetor)
jogospas <- merge(jogospas, vetor, x.all=TRUE)
#rm(vetor)

# Se truncar rodadas passadas de análise
#rod_analise <- 10
#jogospas <- jogospas[jogospas$rodada <= (rodadaspas-rod_analise+1),]
#jogospas$vetor <- jogospas$vetor/sum(jogospas$vetor)


# Método média geométrica entre ataque e defesa
golcasapro <- matrix(0,1,length(times))
golcasacon <- matrix(0,1,length(times))
golforapro <- matrix(0,1,length(times))
golforacon <- matrix(0,1,length(times))
for (i in 1:length(times)) {
  golcasapro[i] <- sum(jogospas[which(jogospas$casa == times[i]),"placar_casa"]*
                      jogospas[which(jogospas$casa == times[i]),"vetor"]/
                      sum(jogospas[which(jogospas$casa == times[i]),"vetor"]))
  golcasacon[i] <- sum(jogospas[which(jogospas$casa == times[i]),"placar_fora"]*
                         jogospas[which(jogospas$casa == times[i]),"vetor"]/
                         sum(jogospas[which(jogospas$casa == times[i]),"vetor"]))
  golforapro[i] <- sum(jogospas[which(jogospas$fora == times[i]),"placar_fora"]*
                      jogospas[which(jogospas$fora == times[i]),"vetor"]/
                      sum(jogospas[which(jogospas$fora == times[i]),"vetor"]))
  golforacon[i] <- sum(jogospas[which(jogospas$fora == times[i]),"placar_casa"]*
                         jogospas[which(jogospas$fora == times[i]),"vetor"]/
                         sum(jogospas[which(jogospas$fora == times[i]),"vetor"]))
}
golcasapro <- as.data.frame(golcasapro)
names(golcasapro) <- times
golcasapro
golcasacon <- as.data.frame(golcasacon)
names(golcasacon) <- times
golcasacon
golforapro <- as.data.frame(golforapro)
names(golforapro) <- times
golforapro
golforacon <- as.data.frame(golforacon)
names(golforacon) <- times
golforacon


# Próxima rodada
(jogosporrodada <- trunc(length(times)/2))
#Se houver jogos adiados de rodadas passadas, acrescentar a qtde dos jogos
jogosprox <- df[is.na(df$placar_casa),][1:jogosporrodada,]
jogosatras <- nrow(jogosprox[jogosprox$rodada != (rodadaspas+1),])
jogosprox <- df[is.na(df$placar_casa),][1:(jogosporrodada+jogosatras),]
jogosprox$camp <- "Brasil A"

#Previsão de resultados
for (i in 1:nrow(jogosprox)) {
  jogosprox$lambda_casa[i] <- (golcasapro[1,jogosprox$casa[i]]*golforacon[1,jogosprox$fora[i]])*0.5
  
  jogosprox$lambda_fora[i] <- (golforapro[1,jogosprox$fora[i]]*golcasacon[1,jogosprox$casa[i]])*0.5
  
  jogosprox$h15casa[i] <- 0
  for (j in 2:10) {jogosprox$h15casa[i] <- jogosprox$h15casa[i] + 
    dpois(j,lambda=jogosprox$lambda_casa[i])*sum(dpois(0:j-2,lambda=jogosprox$lambda_fora[i]))
  }
  jogosprox$vitcasa[i] <- 0
  for (j in 1:10) {jogosprox$vitcasa[i] <- jogosprox$vitcasa[i] +
    dpois(j,lambda=jogosprox$lambda_casa[i])*sum(dpois(0:j-1,lambda=jogosprox$lambda_fora[i]))
  }
  
  jogosprox$EMPATE[i] <- 0
  for (j in 0:10) {jogosprox$EMPATE[i] <- jogosprox$EMPATE[i] +
    dpois(j,lambda=jogosprox$lambda_casa[i])*dpois(j,lambda=jogosprox$lambda_fora[i])
  }
  
  jogosprox$vitempcasa[i] <- jogosprox$vitcasa[i] + jogosprox$EMPATE[i]
  
  jogosprox$vitfora[i] <- 0
  for (j in 1:10) {jogosprox$vitfora[i] <- jogosprox$vitfora[i] +
    dpois(j,lambda=jogosprox$lambda_fora[i])*sum(dpois(0:j-1,lambda=jogosprox$lambda_casa[i]))
  }
  
  jogosprox$vitempfora[i] <- jogosprox$vitfora[i] + jogosprox$EMPATE[i]
  
  jogosprox$h15fora[i] <- 0
  for (j in 2:10) {jogosprox$h15fora[i] <- jogosprox$h15fora[i] + 
    dpois(j,lambda=jogosprox$lambda_fora[i])*sum(dpois(0:j-2,lambda=jogosprox$lambda_casa[i]))
  }
}

jogosprox <- jogosprox[,c("camp","rodada","casa","placar_casa","placar_fora","fora", "lambda_casa","lambda_fora",
                          "h15casa","vitcasa","vitempcasa","EMPATE","vitempfora","vitfora","h15fora")]
jogosprox$placar_casa <- NULL
jogosprox$placar_fora <- NULL
jogosprox[,c("h15casa","vitcasa","vitempcasa","EMPATE","vitempfora","vitfora","h15fora")] <- 
  round(jogosprox[,c("h15casa","vitcasa","vitempcasa","EMPATE","vitempfora","vitfora","h15fora")]*100,0)
jogosprox$E_empate <- ifelse(abs(jogosprox$vitcasa-jogosprox$vitfora)<=10 & jogosprox$EMPATE>=30,
                           paste("SIM ",jogosprox$vitcasa-jogosprox$vitfora), paste("NÃO",jogosprox$vitcasa-jogosprox$vitfora))

#Previsão de gols
for (i in 1:nrow(jogosprox)) {
  jogosprox$Umgol[i] <- 1 - dpois(0,lambda=jogosprox$lambda_casa[i])*dpois(0,lambda=jogosprox$lambda_fora[i])
  jogosprox$Doisgol[i] <- 1 - (dpois(0,lambda=jogosprox$lambda_casa[i])*sum(dpois(0:1,lambda=jogosprox$lambda_fora[i]))
                               +dpois(1,lambda=jogosprox$lambda_casa[i])*dpois(0,lambda=jogosprox$lambda_fora[i]))
  jogosprox$Ambos[i] <- 1 - (dpois(0,lambda=jogosprox$lambda_casa[i])*sum(dpois(0:10,lambda=jogosprox$lambda_fora[i]))
                            +dpois(0,lambda=jogosprox$lambda_fora[i])*sum(dpois(0:10,lambda=jogosprox$lambda_casa[i]))
                            -dpois(0,lambda=jogosprox$lambda_casa[i])*dpois(0,lambda=jogosprox$lambda_fora[i]))
  jogosprox$Tresgol[i] <- 1 - (dpois(0,lambda=jogosprox$lambda_casa[i])*sum(dpois(0:2,lambda=jogosprox$lambda_fora[i]))
                               +dpois(1,lambda=jogosprox$lambda_casa[i])*sum(dpois(0:1,lambda=jogosprox$lambda_fora[i]))
                               +dpois(2,lambda=jogosprox$lambda_casa[i])*dpois(0,lambda=jogosprox$lambda_fora[i]))
  jogosprox$Quatrogol[i] <- 1 - (dpois(0,lambda=jogosprox$lambda_casa[i])*sum(dpois(0:3,lambda=jogosprox$lambda_fora[i]))
                                 +dpois(1,lambda=jogosprox$lambda_casa[i])*sum(dpois(0:2,lambda=jogosprox$lambda_fora[i]))
                                 +dpois(2,lambda=jogosprox$lambda_casa[i])*sum(dpois(0:1,lambda=jogosprox$lambda_fora[i]))
                                 +dpois(3,lambda=jogosprox$lambda_casa[i])*dpois(0,lambda=jogosprox$lambda_fora[i]))
} 
jogosprox[,c("Umgol","Doisgol","Ambos","Tresgol","Quatrogol")] <- 
  round(jogosprox[,c("Umgol","Doisgol","Ambos","Tresgol","Quatrogol")]*100,0)
jogosprox[,c("lambda_casa","lambda_fora")] <- round(jogosprox[,c("lambda_casa","lambda_fora")],2)


