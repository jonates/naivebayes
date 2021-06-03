# implementando o modelo naive bayes na unha ---------------------------------
# Jonatas Silva do Espirito Santo

cria_df_treino<-function(a){
  num_linhas = as.numeric(unlist(strsplit(a[1],split=" ")))
  guarda_dados = unlist(strsplit(a[2],split=" "))
  for (i in 3:(num_linhas[1]+1-num_linhas[2])){
    dados_temp = unlist(strsplit(a[i],split=" "))
    guarda_dados = rbind(guarda_dados, dados_temp)
  }
  guarda_dados
}

cria_df_teste<-function(a){
  num_linhas = as.numeric(unlist(strsplit(a[1],split=" ")))
  guarda_dados = unlist(strsplit(a[num_linhas[1]+1],split=" "))
  guarda_dados
}

cria_treino<-function(){
  #dados = readLines("stdin",n=-1, warn=FALSE)
  dados = readLines("dados_runcodes2.txt",n=-1, warn=FALSE)
  treino = cria_df_treino(dados)
  return(treino)
}

cria_teste<-function(){
  #dados = readLines("stdin",n=-1, warn=FALSE)
  dados = readLines("dados_runcodes2.txt",n=-1, warn=FALSE)
  teste = cria_df_teste(dados)
  return(teste)
}

naive_bayes <- function(){

#carregando os dataset  
 treino <- as.data.frame(cria_treino())
 rownames(treino) <- NULL
 teste <- as.vector(cria_teste())

 #guardando o número de variávéis preditoras + resposta
#nvar <- dim(treino)[2]
nvar <- ncol(treino)
#nvar

#calculando as probabilidades a priores
prob_pri <- prop.table(table(treino[,nvar]))
prob_pri

#Calculando as verossimilhanca
verossimilhanca <- c(1,1)
for(i in 1:(nvar-1)){
  tab_cond <- table(treino[,i],treino[,nvar])
  prob_cond <- (prop.table(tab_cond,2))[teste[i],]
  verossimilhanca <- verossimilhanca*prob_cond
}

# for(i in 1:(nvar-1)){
#   tab_cond <- table(treino[,i],treino[,nvar])
#   prob_cond <- as.data.frame(prop.table(tab_cond,2))
#   names(prob_cond) <- c('nivel','label','freq')
#   prob_cond_teste <- prob_cond[prob_cond$nivel==teste[i],'freq']
#   verossimilhanca <- verossimilhanca*prob_cond_teste
# }

# calculando a posteriori
predicao <- prob_pri * verossimilhanca 

#predizendo as classes do dataset teste
prob_predicao <- prop.table(predicao)
  
max_prob_predicao <- round(max(prob_predicao),3)

return(max_prob_predicao)

}

naive_bayes()
