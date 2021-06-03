# implementando o modelo naive bayes na unha ---------------------------------
# Jonatas Silva do Espirito Santo

cria_df_treino<-function(a){
  num_linhas = as.numeric(unlist(strsplit(a[1],split=" ")))
  guarda_dados = as.numeric(unlist(strsplit(a[2],split=" ")))
  for (i in 3:(num_linhas[1]+1-num_linhas[1])){
    dados_temp = as.numeric(unlist(strsplit(a[i],split=" ")))
    guarda_dados = rbind(guarda_dados, dados_temp)
  }
  guarda_dados
}

cria_df_treino<-function(a){
  num_linhas = as.numeric(unlist(strsplit(a[1],split=" ")))
  guarda_dados = as.numeric(unlist(strsplit(a[num_linhas[1]+1],split=" ")))
  guarda_dados
}

cria_treino<-function(){
  dados = readLines("stdin",n=-1, warn=FALSE);
  df_treino = cria_df(dados)
  instancias_treino = 1:(dim(df_treino)[1] - 1)
  treino = df[instancias_treino,]
  return(treino)
}

cria_teste<-function(){
  dados = readLines("stdin",n=-1, warn=FALSE);
  df_teste = cria_df(dados)
  teste = df[dim(df_teste)[1],]
  return(teste)
}


naive_bayes <- function(){

#carregando os dataset  
 treino <- cria_treino()
 teste <- cria_teste()


#guardando o número de variávéis preditoras + resposta
nvar <- dim(treino)[2]

#calculando as probabilidades a priores
prob_pri <- prop.table(table(treino[[nvar]]))

#Calculando as probabilidades condicionais
tab_cond <- NULL
prob_cond <- NULL
for(i in 1:(nvar-1)){
  tab_cond[[i]] <- table(treino[[i]],treino[[nvar]])
  prob_cond[[i]] <- prop.table(tab_cond[[i]],2)
} 

#predizendo as classes do dataset teste
verossimilhanca <- c(1,1)
for(j in 1:(nvar-1)){
  verossimilhanca <- verossimilhanca*prob_cond[[j]][teste[,j],]
} 

predicao <- prob_pri * verossimilhanca 

prob_predicao <- prop.table(predicao)
  
max_prob_predicao <- max(prob_predicao)

return(max_prob_predicao)

}

naive_bayes()
