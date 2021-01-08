#Instalando pacotes necess?rios
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caTools")
install.packages("caret")
install.packages("e1071")

#Carregando os pacotes
library(rpart)
library(rpart.plot)
library(caTools)
library(caret)
library(e1071)

getwd()
setwd("C:/Users/user/Desktop/Scripts R/Flamengo/Churn")



#leitura dos dados. Colocar o caminho onde o arquivo esta salvo.
#dados1=base com categoria nao informado na idade
#dados2=base para previsao


treino1<-read.csv2("C:/Users/user/Desktop/Scripts R/Flamengo/Churn/BaseChurnFlamengo.csv")
teste1<-read.csv2("C:/Users/user/Desktop/Scripts R/Flamengo/Churn/BaseChurn.csv")


#Verificando o tipo de cada variavel
str(treino1)
str(teste1)


#Transformando valor gasto em numerico
treino1$valor_gasto<-as.numeric(treino1$valor_gasto)
teste1$valor_gasto<-as.numeric(teste1$valor_gasto)


#Modelos univariados
model1<-rpart(Renovação~idade, data=treino1, method = "class")
model2<-rpart(Renovação~genero, data=treino1, method = "class")
model3<-rpart(Renovação~uf, data=treino1, method = "class")
model4<-rpart(Renovação~plano, data=treino1, method = "class")
model5<-rpart(Renovação~forma_pagamento, data=treino1, method = "class")
model6<-rpart(Renovação~duracao_plano, data=treino1, method = "class")
model7<-rpart(Renovação~renovação_automatica, data=treino1, method = "class")
model8<-rpart(Renovação~qtd_convidados, data=treino1, method = "class")
model9<-rpart(Renovação~Comprou_pacote, data=treino1, method = "class")
model10<-rpart(Renovação~valor_gasto, data=treino1, method = "class")
model11<-rpart(Renovação~tenure, data=treino1, method = "class")
model12<-rpart(Renovação~total_meses_ativos, data=treino1, method = "class")
model13<-rpart(Renovação~flag_ja_cancelou, data=treino1, method = "class")
model14<-rpart(Renovação~flag_upgrade, data=treino1, method = "class")
model15<-rpart(Renovação~flag_primeiro_contrato, data=treino1, method = "class")
model16<-rpart(Renovação~ingressos_comprados, data=treino1, method = "class")

#Criando a arvore de decisao
model_tot<-rpart(Renovação~genero+idade+uf+plano+forma_pagamento+duracao_plano+renovação_automatica+tenure+qtd_convidados+Comprou_pacote+valor_gasto
                 +total_meses_ativos+flag_ja_cancelou+flag_upgrade+flag_primeiro_contrato+ingressos_comprados,
              data=treino1, method = "class")

model_tot2<-rpart(Renovação~forma_pagamento+renovação_automatica+valor_gasto+total_meses_ativos,
                 data=treino1, method = "class")


#resultado "textual"
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
summary(model7)
summary(model8)
summary(model9)
summary(model10)
summary(model11)
summary(model12)
summary(model13)
summary(model14)
summary(model15)
summary(model16)
summary(model_tot)
summary(model_tot2)

#Resultado grafico
rpart.plot(model1)
rpart.plot(model2)
rpart.plot(model3)
rpart.plot(model4)
rpart.plot(model5)
rpart.plot(model6)
rpart.plot(model7)
rpart.plot(model8)
rpart.plot(model9)
rpart.plot(model10)
rpart.plot(model11)
rpart.plot(model12)
rpart.plot(model13)
rpart.plot(model14)
rpart.plot(model15)
rpart.plot(model16)
rpart.plot(model_tot)
rpart.plot(model_tot2)

#Fazendo previsoes
prev1<-predict(model_tot, newdata = teste1[-19], type = "class")
prev2<-predict(model_tot2, newdata = teste1[-19], type = "class")

#Analisando resultados
matriz_confusao1<- table(teste1[,19],prev1)
confusionMatrix(matriz_confusao1)

matriz_confusao2<-table(teste1[,19],prev2)
confusionMatrix(matriz_confusao2)


#Base escolhida: Base1
#Modelo escolhido: model_tot2

#juntando a base teste1 com a previs?o
teste1$prev2<-prev2
base_previsao2<-teste1

#Exportando a base de STs com a coluna de resultado da previs?o
#Colocar o caminho de onde quer salvar e o nome do arquivo
write.csv(base_previsao2,"C:/Users/user/Desktop/Scripts R/Flamengo/Churn/base_previsao.csv", row.names = FALSE)
