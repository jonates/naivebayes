# implementando o modelo naive bayes na unha ---------------------------------

#carregando dataset de treinamento
treino <- read.csv(file = "jogar-tenis.csv",encoding = 'UTF-8')
treino <- treino[,-1]

#definindo um dataset de teste
teste <- data.frame(
  Tempo = 'Ensolarado', 
  Temperatura = 'Intermediária', 
  Umidade = 'Alta', 
  Vento = 'Forte')

#guardando o número de variávéis preditoras + resposta
nvar <- dim(treino)[2]

#calculando as probabilidades a priores
prob_pri <- prop.table(table(treino[[nvar]]))
prob_pri

#Calculando as probabilidades condicionais
tab_cond <- NULL
prob_cond <- NULL
for(i in 1:(nvar-1)){
  tab_cond[[i]] <- table(treino[[i]],treino[[nvar]])
  prob_cond[[i]] <- prop.table(tab_cond[[i]],2)
} 

#espiando as probabilidades condicionais
prob_cond

#predizendo as classes do dataset teste
verossimilhanca <- c(1,1)
for(j in 1:(nvar-1)){
  verossimilhanca <- verossimilhanca*prob_cond[[j]][teste[,j],]
} 

predicao <- prob_pri * verossimilhanca 
predicao

prob_predicao <- prop.table(predicao)
  
prob_predicao[prob_predicao==max(prob_predicao)]


