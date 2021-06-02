library(dplyr)
library(e1071)

#carregando o dataset
jogartenis <- readr::read_csv(file = "jogar-tenis.csv") %>% select(-Dia)

# utilizando o caret para o naive bayes ---------------------------------------

#treinando o jogar tênis
nb_no_e1071 <- e1071::naiveBayes(`Jogar Tênis` ~ ., data = jogartenis)

#espiando o modelo
nb_no_e1071

#criando dataset de teste
teste <- data.frame(
  Tempo = 'Ensolarado', 
  Temperatura = 'Intermediária', 
  Umidade = 'Alta', 
  Vento = 'Forte')

predict(nb_no_e1071, teste, type = "raw")



# implementando o modelo naive bayes na unha ---------------------------------

#carregando dataset jogar tenis
df <- readr::read_csv(file = "jogar-tenis.csv") %>% select(-Dia)

#guardando o número de variávéis preditoras + resposta
nvar <- dim(df)[2]

#calculando as probabilidades a priores
prob_pri <- prop.table(table(df[[nvar]]))
prob_pri

#Calculando as probabilidades condicionais
tab_cond <- NULL
prob_cond <- NULL
for(i in 1:(nvar-1)){
  tab_cond[[i]] <- table(df[[i]],df[[nvar]])
  prob_cond[[i]] <- prop.table(tab_cond[[i]],2)
} 

#espiando as probabilidades condicionais
prob_cond

#definindo um dataset de teste
teste <- data.frame(
  Tempo = 'Ensolarado', 
  Temperatura = 'Intermediária', 
  Umidade = 'Alta', 
  Vento = 'Forte')

#predizendo as classes do dataset teste
verossimilhanca <- c(1,1)
for(j in 1:(nvar-1)){
  verossimilhanca <- verossimilhanca*prob_cond[[j]][teste[,j],]
} 

predicao <- prob_pri * verossimilhanca 
predicao
prop.table(predicao)

# temp <- stringr::str_c("prob_cond[[",1:(nvar-1),"]][teste[,",1:(nvar-1),"],]")
# temp

 # prob_cond[[1]][teste[,1],]*
 # prob_cond[[2]][teste[,2],]*
 # prob_cond[[3]][teste[,3],]*
 # prob_cond[[4]][teste[,4],]*
 # prob_pri
