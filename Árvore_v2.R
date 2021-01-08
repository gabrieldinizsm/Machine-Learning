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



#leitura dos dados. Colocar o caminho onde o arquivo est? salvo.
#dados1=base com categoria n?o informado na idade
#dados2=base sem a categoria n?o informado na idade
#dados3=base com apenas STs com renova??o autom?tica e pagamento parcelado

dados1<-read.csv2("C:/Users/pfbp/Desktop/Base Churn Flamengo v4.csv")
dados2<-read.csv2("C:/Users/pfbp/Desktop/Base Churn Flamengo v5.csv")
dados3<-read.csv2("C:/Users/pfbp/Desktop/Base Churn Flamengo v6.csv")

#Verificando o tipo de cada vari?vel
str(dados1)
str(dados2)
str(dados3)

#Transformando valor gasto em num?rico
dados1$valor_gasto<-as.numeric(dados1$valor_gasto)
dados2$valor_gasto<-as.numeric(dados2$valor_gasto)
dados3$valor_gasto<-as.numeric(dados3$valor_gasto)

#Analisando dados1
attach(dados1)

#Dividindo o conjunto em treino1 e teste1
set.seed(123)
divisao<-sample.split(Renova??o,SplitRatio = 0.70)
treino1<-subset(dados1, divisao== TRUE)
teste1<-subset(dados1, divisao== FALSE)  


#Modelos univariados
model1<-rpart(Renova??o~faixa_etaria, data=treino1, method = "class")
model2<-rpart(Renova??o~genero, data=treino1, method = "class")
model3<-rpart(Renova??o~uf, data=treino1, method = "class")
model4<-rpart(Renova??o~plano2, data=treino1, method = "class")
model5<-rpart(Renova??o~forma_pagamento, data=treino1, method = "class")
model6<-rpart(Renova??o~duracao_plano, data=treino1, method = "class")
model7<-rpart(Renova??o~renova??o_automatica, data=treino1, method = "class")
model8<-rpart(Renova??o~qtd_convidados, data=treino1, method = "class")
model9<-rpart(Renova??o~Comprou_pacote, data=treino1, method = "class")
model10<-rpart(Renova??o~ultimo_login, data=treino1, method = "class")
model11<-rpart(Renova??o~valor_gasto, data=treino1, method = "class")
model12<-rpart(Renova??o~tenure, data=treino1, method = "class")
model13<-rpart(Renova??o~total_meses_ativos, data=treino1, method = "class")
model14<-rpart(Renova??o~flag_ja_cancelou, data=treino1, method = "class")
model15<-rpart(Renova??o~flag_primeiro_contrato, data=treino1, method = "class")
model16<-rpart(Renova??o~ingressos_comprados, data=treino1, method = "class")

#Criando a ?rvore de decis?o
model_tot<-rpart(Renova??o~genero+faixa_etaria+uf+plano2+forma_pagamento+duracao_plano+renova??o_automatica+tenure+ultimo_login+qtd_convidados+Comprou_pacote+valor_gasto
                 +total_meses_ativos+flag_ja_cancelou+flag_primeiro_contrato+ingressos_comprados,
              data=treino1, method = "class")

model_tot2<-rpart(Renova??o~forma_pagamento+renova??o_automatica+valor_gasto+total_meses_ativos,
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

#Resultado gr?fico
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

#Fazendo previs?es
prev1<-predict(model_tot, newdata = teste1[-21], type = "class")
prev2<-predict(model_tot2, newdata = teste1[-21], type = "class")

#Analisando resultados
matriz_confus?o1<-table(teste1[,21],prev1)
confusionMatrix(matriz_confus?o1)

matriz_confus?o2<-table(teste1[,21],prev2)
confusionMatrix(matriz_confus?o2)


#Analisando dados2
attach(dados2)

#Dividindo o conjunto em treino2 e teste2
set.seed(123)
divisao<-sample.split(Renova??o,SplitRatio = 0.70)
treino2<-subset(dados2, divisao== TRUE)
teste2<-subset(dados2, divisao== FALSE)  


#Modelos univariados
model1<-rpart(Renova??o~idade, data=treino2, method = "class")
model2<-rpart(Renova??o~genero, data=treino2, method = "class")
model3<-rpart(Renova??o~uf, data=treino2, method = "class")
model4<-rpart(Renova??o~plano2, data=treino2, method = "class")
model5<-rpart(Renova??o~forma_pagamento, data=treino2, method = "class")
model6<-rpart(Renova??o~duracao_plano, data=treino2, method = "class")
model7<-rpart(Renova??o~renova??o_automatica, data=treino2, method = "class")
model8<-rpart(Renova??o~qtd_convidados, data=treino2, method = "class")
model9<-rpart(Renova??o~Comprou_pacote, data=treino2, method = "class")
model10<-rpart(Renova??o~ultimo_login, data=treino2, method = "class")
model11<-rpart(Renova??o~valor_gasto, data=treino2, method = "class")
model12<-rpart(Renova??o~tenure, data=treino2, method = "class")
model13<-rpart(Renova??o~total_meses_ativos, data=treino2, method = "class")
model14<-rpart(Renova??o~flag_ja_cancelou, data=treino2, method = "class")
model15<-rpart(Renova??o~flag_primeiro_contrato, data=treino2, method = "class")
model16<-rpart(Renova??o~ingressos_comprados, data=treino2, method = "class")

#Criando a ?rvore de decis?o
model_tot<-rpart(Renova??o~genero+idade+uf+plano2+forma_pagamento+duracao_plano+renova??o_automatica+tenure+ultimo_login+qtd_convidados+Comprou_pacote+valor_gasto
                 +total_meses_ativos+flag_ja_cancelou+flag_primeiro_contrato+ingressos_comprados,
                 data=treino2, method = "class")

model_tot2<-rpart(Renova??o~forma_pagamento+renova??o_automatica+total_meses_ativos,
                  data=treino2, method = "class")


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

#Resultado gr?fico
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

#Fazendo previs?es
prev1<-predict(model_tot, newdata = teste2[-21], type = "class")
prev2<-predict(model_tot2, newdata = teste2[-21], type = "class")

#Analisando resultados
matriz_confus?o1<-table(teste2[,21],prev1)
confusionMatrix(matriz_confus?o1)

matriz_confus?o2<-table(teste2[,21],prev2)
confusionMatrix(matriz_confus?o2)


#Analisando dados3
attach(dados3)

#Dividindo o conjunto em treino3 e teste3
set.seed(123)
divisao<-sample.split(Renova??o,SplitRatio = 0.70)
treino3<-subset(dados3, divisao== TRUE)
teste3<-subset(dados3, divisao== FALSE)  


#Modelos univariados
model1<-rpart(Renova??o~faixa_etaria, data=treino3, method = "class")
model2<-rpart(Renova??o~genero, data=treino3, method = "class")
model3<-rpart(Renova??o~uf, data=treino3, method = "class")
model4<-rpart(Renova??o~plano2, data=treino3, method = "class")
model5<-rpart(Renova??o~forma_pagamento, data=treino3, method = "class")
model6<-rpart(Renova??o~duracao_plano, data=treino3, method = "class")
model7<-rpart(Renova??o~renova??o_automatica, data=treino3, method = "class")
model8<-rpart(Renova??o~qtd_convidados, data=treino3, method = "class")
model9<-rpart(Renova??o~Comprou_pacote, data=treino3, method = "class")
model10<-rpart(Renova??o~ultimo_login, data=treino3, method = "class")
model11<-rpart(Renova??o~valor_gasto, data=treino3, method = "class")
model12<-rpart(Renova??o~tenure, data=treino3, method = "class")
model13<-rpart(Renova??o~total_meses_ativos, data=treino3, method = "class")
model14<-rpart(Renova??o~flag_ja_cancelou, data=treino3, method = "class")
model15<-rpart(Renova??o~flag_primeiro_contrato, data=treino3, method = "class")
model16<-rpart(Renova??o~ingressos_comprados, data=treino3, method = "class")

#Criando a ?rvore de decis?o
model_tot<-rpart(Renova??o~genero+faixa_etaria+uf+plano2+duracao_plano+tenure+ultimo_login+qtd_convidados+Comprou_pacote+valor_gasto
                 +total_meses_ativos+flag_ja_cancelou+flag_primeiro_contrato+ingressos_comprados,
                 data=treino3, method = "class")

model_tot2<-rpart(Renova??o~plano2+valor_gasto+total_meses_ativos,
                  data=treino3, method = "class")


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

#Resultado gr?fico
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

#Fazendo previs?es
prev1<-predict(model_tot, newdata = teste3[-21], type = "class")
prev2<-predict(model_tot2, newdata = teste3[-21], type = "class")

#Analisando resultados
matriz_confus?o1<-table(teste3[,21],prev1)
confusionMatrix(matriz_confus?o1)

matriz_confus?o2<-table(teste3[,21],prev2)
confusionMatrix(matriz_confus?o2)

#Base escolhida: Base1
#Modelo escolhido: model_tot2

#juntando a base teste1 com a previs?o
teste1$prev2<-prev2
base_previs?o2<-teste1

#Exportando a base de STs com a coluna de resultado da previs?o
#Colocar o caminho de onde quer salvar e o nome do arquivo
write.csv(base_previs?o2,"C:/Users/pfbp/Desktop/base_previs?o.csv", row.names = FALSE)
