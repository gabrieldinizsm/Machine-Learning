setwd('C:/Users/user/Desktop/Scripts R/Flamengo/NovoModelo')

library(readxl)
library(nnet)
library(caret)
library(randomForest)
library(CatEncoders)
library(Boruta)
library(ROSE)
library(InformationValue)
library(MASS)
library(rpart)
library(rpart.plot)
library(car)
library(gmodels)
library(sqldf)
library(ROCR)

baseTreino<- read_excel("baseTreino.xlsx")

baseTeste<- read_excel("baseDezembroJaneiroGabriel.xlsx")

View(baseTreino)
View(baseTeste)

#Buscando valores nulls

any(is.null(baseTreino))
any(is.na(baseTreino))

#Buscando valores na

any(is.null(baseTeste))
any(is.na(baseTeste))

str (baseTreino)

baseTreino$estado_civil[baseTreino$estado_civil == "Não Informado"] <- "Não informado"

baseTreino$estado_civil[baseTreino$estado_civil %in% 
                                c("Divorciado(a)", "Separado(a)", "Viúvo(a)")] <- "Divorciado(a)"

baseTreino$UF[baseTreino$UF == "Rj"] <- "RJ"

baseTeste$estado_civil[baseTeste$estado_civil == "Não Informado"] <- "Não informado"

baseTeste$estado_civil[baseTeste$estado_civil %in% 
                                c("Divorciado(a)", "Separado(a)", "Viúvo(a)")] <- "Divorciado(a)"

baseTeste$UF[baseTreino$UF == "Rj"] <- "RJ"

#Cast pra factor e data

baseTreino$DATA_POSSIVEL_SAIDA <- as.Date(baseTreino$DATA_POSSIVEL_SAIDA)
baseTreino$UF <- as.factor(baseTreino$UF)
baseTreino$UF_GRUPO <- as.factor(baseTreino$UF_GRUPO)
baseTreino$PLANO <- as.factor(baseTreino$PLANO)
baseTreino$FORMA_PAGAMENTO <- as.factor(baseTreino$FORMA_PAGAMENTO)
baseTreino$PERIODICIDADE <- as.factor(baseTreino$PERIODICIDADE)
baseTreino$flag_recorrente <- as.factor(baseTreino$flag_recorrente)
baseTreino$RENOVACAO_AUTOMATICA <- as.factor(baseTreino$RENOVACAO_AUTOMATICA)
baseTreino$DURACAO_PLANO <- as.factor(baseTreino$DURACAO_PLANO)
baseTreino$VALOR_MENSAL <- as.numeric(baseTreino$VALOR_MENSAL)
baseTreino$flag_primeiro_contrato <- as.factor(baseTreino$flag_primeiro_contrato)
baseTreino$flag_primeiro_contrato <- as.factor(baseTreino$flag_primeiro_contrato)
baseTreino$TIPO_PESSOA <- as.factor(baseTreino$TIPO_PESSOA)
baseTreino$sexo <- as.factor(baseTreino$sexo)
baseTreino$estado_civil <- as.factor(baseTreino$estado_civil)
baseTreino$FLAG_JA_CANCELOU <- as.factor(baseTreino$FLAG_JA_CANCELOU)
baseTreino$FLAG_UPGRADE <- as.factor(baseTreino$FLAG_UPGRADE)
baseTreino$FLAG_NUNCA_SAIU <- as.factor(baseTreino$FLAG_NUNCA_SAIU)
baseTreino$RENOVOU <- as.factor(baseTreino$RENOVOU)

baseTeste$DATA_POSSIVEL_SAIDA <- as.Date(baseTeste$DATA_POSSIVEL_SAIDA)
baseTeste$UF <- as.factor(baseTeste$UF)
baseTeste$UF_GRUPO <- as.factor(baseTeste$UF_GRUPO)
baseTeste$PLANO <- as.factor(baseTeste$PLANO)
baseTeste$FORMA_PAGAMENTO <- as.factor(baseTeste$FORMA_PAGAMENTO)
baseTeste$PERIODICIDADE <- as.factor(baseTeste$PERIODICIDADE)
baseTeste$flag_recorrente <- as.factor(baseTeste$flag_recorrente)
baseTeste$RENOVACAO_AUTOMATICA <- as.factor(baseTeste$RENOVACAO_AUTOMATICA)
baseTeste$DURACAO_PLANO <- as.factor(baseTeste$DURACAO_PLANO)
baseTeste$VALOR_MENSAL <- as.numeric(baseTeste$VALOR_MENSAL)
baseTeste$flag_primeiro_contrato <- as.factor(baseTeste$flag_primeiro_contrato)
baseTeste$flag_primeiro_contrato <- as.factor(baseTeste$flag_primeiro_contrato)
baseTeste$TIPO_PESSOA <- as.factor(baseTeste$TIPO_PESSOA)
baseTeste$sexo <- as.factor(baseTeste$sexo)
baseTeste$estado_civil <- as.factor(baseTeste$estado_civil)
baseTeste$FLAG_JA_CANCELOU <- as.factor(baseTeste$FLAG_JA_CANCELOU)
baseTeste$FLAG_UPGRADE <- as.factor(baseTeste$FLAG_UPGRADE)
baseTeste$FLAG_NUNCA_SAIU <- as.factor(baseTeste$FLAG_NUNCA_SAIU)
baseTeste$RENOVOU <- as.factor(baseTeste$RENOVOU)

str (baseTreino)

#Adicionando level no conjunto de treino

levels(baseTreino$PLANO) <- union(levels(baseTreino$PLANO), levels(baseTeste$PLANO))

#Plotando

boxplot(VALOR_MENSAL ~ RENOVOU,
        data = baseTreino,
        col = "red",
        ylab = 'Valor Mensal',
        xlab = 'Renovou',
        names = unique(baseTreino$RENOVOU))

boxplot(qtd_ingressos ~ RENOVOU,
        data = baseTreino,
        col = "red",
        ylab = 'Quantidade Ingressos',
        xlab = 'Renovou',
        names = unique(baseTreino$RENOVOU))

boxplot(idade ~ RENOVOU,
        data = baseTreino,
        col = "red",
        ylab = 'Idade',
        xlab = 'Renovou',
        names = unique(baseTreino$RENOVOU))

boxplot(TENURE ~ RENOVOU,
        data = baseTreino,
        col = "red",
        ylab = 'Tenure',
        xlab = 'Renovou',
        names = unique(baseTreino$RENOVOU))

boxplot(TOTAL_MESES_ATIVOS ~ RENOVOU,
        data = baseTreino,
        col = "red",
        ylab = 'Total Meses Ativos',
        xlab = 'Renovou',
        names = unique(baseTreino$RENOVOU))

ggplot(baseTreino, aes(DATA_POSSIVEL_SAIDA, fill = RENOVOU)) + 
       geom_bar() + 
       xlab(label = "Data Possivel Saida") + 
       ylab(label = "Renovou")

table(baseTreino$PLANO)

table(baseTreino$UF)

table(baseTreino$estado_civil)

table(baseTreino$sexo)

# Tratando alguns outliers

boxplot.stats(baseTreino$VALOR_MENSAL)$out

boxplot.stats(baseTreino$idade)$out

boxplot.stats(baseTreino$TENURE)$out

boxplot.stats(baseTreino$TOTAL_MESES_ATIVOS)$out

baseTreino <- sqldf('select * from baseTreino where idade < 71')

str(baseTreino)

# Definindo importância das variáveis para o modelo

boruta_output <- Boruta(RENOVOU ~ . - IDCONTRATO - DATA_POSSIVEL_SAIDA - TIPO_PESSOA,
                        data = baseTreino, doTrace = 0)  

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])

boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)

print(boruta_signif)  

plot(boruta_output, cex.axis=.7, las=2, xlab="", main = "Variable Importance")  

# Treinando modelos e vendo quais features tem em comum

variableImportanceRpart <- train(RENOVOU ~ . - IDCONTRATO - DATA_POSSIVEL_SAIDA - TIPO_PESSOA, 
                                 data = baseTreino, method = "rpart")

rpartImp <- varImp(variableImportanceRpart)

print(rpartImp)

plot(rpartImp)

variableImportanceGlm <- train(RENOVOU ~ . - IDCONTRATO - DATA_POSSIVEL_SAIDA - TIPO_PESSOA,
                               data = baseTreino, method = "glm")

glmImp <- varImp(variableImportanceGlm)

print(glmImp)

plot(glmImp)

variableImportanceEarth <- train(RENOVOU ~ . - IDCONTRATO - DATA_POSSIVEL_SAIDA - TIPO_PESSOA,
                               data = baseTreino, method = "earth")

earthImp <- varImp(variableImportanceEarth)

print(earthImp)

plot(earthImp)

variableImportancerandomForest <- randomForest (RENOVOU ~ . - IDCONTRATO - DATA_POSSIVEL_SAIDA - TIPO_PESSOA,
                                                data = baseTreino)

randomForestImp <- varImp(variableImportancerandomForest)

print(randomForestImp)

sqldf('select * from randomForestImp order by overall desc')

# Label encoding

names(baseTreino)

str(baseTreino)

mapUf <- LabelEncoder.fit(baseTreino$UF)
encodingUF <- transform(mapUf, baseTreino$UF)
mapUF_GRUPO <- LabelEncoder.fit(baseTreino$UF_GRUPO)
encodingUF_GRUPO <- transform(mapUF_GRUPO, baseTreino$UF_GRUPO)
mapPLANO <- LabelEncoder.fit(baseTreino$PLANO)
encodingPLANO <- transform(mapPLANO, baseTreino$PLANO)
mapFORMA_PAGAMENTO <- LabelEncoder.fit(baseTreino$FORMA_PAGAMENTO)
encodingFORMA_PAGAMENTO <- transform(mapFORMA_PAGAMENTO, baseTreino$FORMA_PAGAMENTO)
mapPERIODICIDADE <- LabelEncoder.fit(baseTreino$PERIODICIDADE)
encodingPERIODICIDADE <- transform(mapPERIODICIDADE, baseTreino$PERIODICIDADE)
mapflag_recorrente <- LabelEncoder.fit(baseTreino$flag_recorrente)
encodingflag_recorrente <- transform(mapflag_recorrente, baseTreino$flag_recorrente)
mapRENOVACAO_AUTOMATICA <- LabelEncoder.fit(baseTreino$RENOVACAO_AUTOMATICA)
encodingRENOVACAO_AUTOMATICA <- transform(mapRENOVACAO_AUTOMATICA, baseTreino$RENOVACAO_AUTOMATICA)
mapDURACAO_PLANO <- LabelEncoder.fit(baseTreino$DURACAO_PLANO)
encodingDURACAO_PLANO <- transform(mapDURACAO_PLANO, baseTreino$DURACAO_PLANO)
mapflag_primeiro_contrato <- LabelEncoder.fit(baseTreino$flag_primeiro_contrato)
encodingflag_primeiro_contrato <- transform(mapflag_primeiro_contrato, baseTreino$flag_primeiro_contrato)
mapTIPO_PESSOA <- LabelEncoder.fit(baseTreino$TIPO_PESSOA)
encodingTIPO_PESSOA <- transform(mapTIPO_PESSOA, baseTreino$TIPO_PESSOA)
mapsexo <- LabelEncoder.fit(baseTreino$sexo)
encodingsexo <- transform(mapsexo, baseTreino$sexo)
mapestado_civil <- LabelEncoder.fit(baseTreino$estado_civil)
encodingestado_civil <- transform(mapestado_civil, baseTreino$estado_civil)
mapFLAG_JA_CANCELOU <- LabelEncoder.fit(baseTreino$FLAG_JA_CANCELOU)
encodingFLAG_JA_CANCELOU <- transform(mapFLAG_JA_CANCELOU, baseTreino$FLAG_JA_CANCELOU)
mapFLAG_UPGRADE <- LabelEncoder.fit(baseTreino$FLAG_UPGRADE)
encodingFLAG_UPGRADE <- transform(mapFLAG_UPGRADE, baseTreino$FLAG_UPGRADE)
mapFLAG_NUNCA_SAIU <- LabelEncoder.fit(baseTreino$FLAG_NUNCA_SAIU)
encodingFLAG_NUNCA_SAIU <- transform(mapFLAG_NUNCA_SAIU, baseTreino$FLAG_NUNCA_SAIU)
mapRENOVOU <- LabelEncoder.fit(baseTreino$RENOVOU)
encodingRENOVOU <- transform(mapRENOVOU, baseTreino$RENOVOU)

baseTreinoEncoded <- data.frame(encodingUF, encodingUF_GRUPO,
                                encodingPLANO, encodingFORMA_PAGAMENTO, encodingPERIODICIDADE, 
                                encodingflag_recorrente, encodingRENOVACAO_AUTOMATICA, encodingDURACAO_PLANO, 
                                baseTreino$VALOR_MENSAL, baseTreino$qtd_ingressos, encodingflag_primeiro_contrato,
                                encodingTIPO_PESSOA, encodingsexo, baseTreino$idade, encodingestado_civil, 
                                encodingFLAG_JA_CANCELOU, baseTreino$TENURE, baseTreino$TOTAL_MESES_ATIVOS,
                                encodingFLAG_UPGRADE, encodingFLAG_NUNCA_SAIU, encodingRENOVOU)

View(baseTreinoEncoded)

# Checando correlações

correlationMatrix <- signif(cor(baseTreinoEncoded[,c(1:21)]))

findCorrelation(correlationMatrix, cutoff = 0.75)

str (baseTreinoEncoded)

# VIF - Descartar variáveis com vif muito grande

vif(glm(encodingRENOVOU ~ encodingUF + encodingUF_GRUPO + encodingPLANO + encodingFORMA_PAGAMENTO 
        + encodingPERIODICIDADE + encodingflag_recorrente + encodingRENOVACAO_AUTOMATICA 
        + encodingDURACAO_PLANO + baseTreino.VALOR_MENSAL + baseTreino.qtd_ingressos + encodingflag_primeiro_contrato
        + encodingsexo + baseTreino.idade + encodingFLAG_JA_CANCELOU + baseTreino.TENURE 
        + baseTreino.TOTAL_MESES_ATIVOS + encodingFLAG_UPGRADE + encodingFLAG_NUNCA_SAIU,
        data = baseTreinoEncoded))

# VIF - Sem duracao plano

vif(glm(encodingRENOVOU ~ encodingUF + encodingUF_GRUPO + encodingPLANO + encodingFORMA_PAGAMENTO 
        + encodingPERIODICIDADE + encodingflag_recorrente + encodingRENOVACAO_AUTOMATICA 
        + baseTreino.VALOR_MENSAL + baseTreino.qtd_ingressos + encodingflag_primeiro_contrato
        + encodingsexo + baseTreino.idade + encodingFLAG_JA_CANCELOU + baseTreino.TENURE 
        + baseTreino.TOTAL_MESES_ATIVOS + encodingFLAG_UPGRADE + encodingFLAG_NUNCA_SAIU,
        data = baseTreinoEncoded))

# VIF - Sem TENURE e sem duracao plano

vif(glm(encodingRENOVOU ~ encodingUF + encodingUF_GRUPO + encodingPLANO + encodingFORMA_PAGAMENTO 
        + encodingPERIODICIDADE + encodingflag_recorrente + encodingRENOVACAO_AUTOMATICA 
        + baseTreino.VALOR_MENSAL + baseTreino.qtd_ingressos + encodingflag_primeiro_contrato
        + encodingsexo + baseTreino.idade + encodingFLAG_JA_CANCELOU  
        + baseTreino.TOTAL_MESES_ATIVOS + encodingFLAG_UPGRADE + encodingFLAG_NUNCA_SAIU,
        data = baseTreinoEncoded))

# Modelos univariados
 
model1 <- rpart(RENOVOU~UF, data = baseTreino, method = "class")
model2 <- rpart(RENOVOU~UF_GRUPO, data = baseTreino, method = "class")
model3 <- rpart(RENOVOU~PLANO, data = baseTreino, method = "class")
model4 <- rpart(RENOVOU~FORMA_PAGAMENTO, data = baseTreino, method = "class")
model5 <- rpart(RENOVOU~PERIODICIDADE, data = baseTreino, method = "class")
model6 <- rpart(RENOVOU~flag_recorrente, data = baseTreino, method = "class")
model7 <- rpart(RENOVOU~RENOVACAO_AUTOMATICA, data = baseTreino, method = "class")
model8 <- rpart(RENOVOU~DURACAO_PLANO, data = baseTreino, method = "class")
model9 <- rpart(RENOVOU~VALOR_MENSAL, data = baseTreino, method = "class")
model10 <- rpart(RENOVOU~qtd_ingressos, data = baseTreino, method = "class")
model11 <- rpart(RENOVOU~flag_primeiro_contrato, data = baseTreino, method = "class")
model12 <- rpart(RENOVOU~sexo, data = baseTreino, method = "class")
model13 <- rpart(RENOVOU~idade, data = baseTreino, method = "class")
model14 <- rpart(RENOVOU~estado_civil, data = baseTreino, method = "class")
model15 <- rpart(RENOVOU~TENURE, data = baseTreino, method = "class")
model16 <- rpart(RENOVOU~TOTAL_MESES_ATIVOS, dat = baseTreino, method = "class")
model17 <- rpart(RENOVOU~FLAG_JA_CANCELOU, data = baseTreino, method = "class")
model18 <- rpart(RENOVOU~FLAG_UPGRADE, data = baseTreino, method = "class")
model19 <- rpart(RENOVOU~FLAG_NUNCA_SAIU, data = baseTreino, method = "class")
model20 <- rpart (RENOVOU ~ UF + UF_GRUPO + PLANO + FORMA_PAGAMENTO + PERIODICIDADE 
                  + flag_recorrente + RENOVACAO_AUTOMATICA + DURACAO_PLANO + VALOR_MENSAL 
                  + qtd_ingressos + flag_primeiro_contrato+ sexo + idade + estado_civil
                  + TOTAL_MESES_ATIVOS + FLAG_JA_CANCELOU + FLAG_UPGRADE + FLAG_NUNCA_SAIU,
                  data = baseTreino, method = "class")

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
rpart.plot(model17)
rpart.plot(model18)
rpart.plot(model19)
rpart.plot(model20)

#Randomizando a ordem dos dados

set.seed(182)

baseTreinoRandomizada <- baseTreino[sample(nrow(baseTreino)),]

View(baseTreinoRandomizada) 

# Divisão do dataset em treino e teste

indiceLinhas = round(0.80 * nrow(baseTreinoRandomizada))

baseTreinoV2 <- baseTreinoRandomizada[1:indiceLinhas,]
baseTeste <- baseTreinoRandomizada[(indiceLinhas+1):nrow(baseTreinoRandomizada) ,]

table(baseTreinoV2$RENOVOU)

renovouN <- baseTreinoV2[baseTreinoV2$RENOVOU == "N", ]
renovouS <- baseTreinoV2[baseTreinoV2$RENOVOU == "S", ][order(runif(nrow(
                         baseTreinoV2[baseTreinoV2$RENOVOU == "N", ]))),]

baseTreinoV3 <- rbind(renovouS, renovouN)

remove(renovouN)
remove(renovouS)

str(baseTreinoV3)

# Modelo v1 com todas variáveis

modeloV1 <- glm(RENOVOU ~ UF + UF_GRUPO + PLANO + FORMA_PAGAMENTO + PERIODICIDADE + 
                flag_recorrente + RENOVACAO_AUTOMATICA + DURACAO_PLANO + 
                VALOR_MENSAL + estado_civil + qtd_ingressos + flag_primeiro_contrato + 
                sexo + idade + TOTAL_MESES_ATIVOS + TENURE + FLAG_JA_CANCELOU + 
                FLAG_UPGRADE + FLAG_NUNCA_SAIU, 
                data = baseTreinoV3, family = "binomial")

summary(modeloV1)

ggplot2::ggplot(varImp(modeloV1))

#Efetuando previsões

probabilidadesV1 <- predict(modeloV1, newdata = baseTeste, type = "response")

View(baseTeste)

# Curva ROC

roc.curve(baseTeste$RENOVOU, probabilidadesV1) #0.802

# ks stat (QUanto maior melhor)

ks_stat(actuals= baseTeste$RENOVOU,  predictedScores = probabilidadesV1) #0.4581

# AIC (quanto menor melhor)

AIC(modeloV1, k = 2) #56858.4


predictionV1 <- prediction(predictions = probabilidadesV1, labels = baseTeste$RENOVOU)

perf <- performance(predictionV1,"tpr","fpr")

costPerfV1 = performance(predictionV1, "cost")

cutOffV1 <- predictionV1@cutoffs[[1]][which.min(costPerfV1@y.values[[1]])]

previsoesClassesV1 <- as.factor(ifelse(probabilidadesV1 >= 0.5, "S", "N"))

previsoesClassesCutOffV1 <- as.factor(ifelse(probabilidadesV1 >= cutOffV1, "S", "N"))

# Confusion matrix

matrizConfusaoV1 <- 
        caret::confusionMatrix(data = previsoesClassesV1, reference = baseTeste$RENOVOU)

matrizConfusaoCutOffV1 <- 
        caret::confusionMatrix(data = previsoesClassesCutOffV1, reference = baseTeste$RENOVOU)

# StepAIC escolhendo variáveis

stepAIC(object = modeloV1, direction = "both")

# Modelo v2 Utilizando somente as variáveis indicadas pelo stepAIC
# Sairam UF_GRUPO, sexo 

modeloV2 <- glm(RENOVOU ~ UF + PLANO + FORMA_PAGAMENTO + PERIODICIDADE + 
                flag_recorrente + RENOVACAO_AUTOMATICA + DURACAO_PLANO + 
                VALOR_MENSAL + estado_civil + qtd_ingressos + flag_primeiro_contrato + 
                idade + TOTAL_MESES_ATIVOS + TENURE + FLAG_JA_CANCELOU + 
                FLAG_UPGRADE + FLAG_NUNCA_SAIU, 
                family = "binomial", data = baseTreinoV3)

summary(modeloV2)

ggplot2::ggplot(varImp(modeloV2))

#Efetuando previsões

probabilidadesV2 <- predict(modeloV2, newdata = baseTeste, type = "response")

View(baseTeste)

# Curva ROC modelo v2

roc.curve(baseTeste$RENOVOU, probabilidadesV2) #0.802

# ks stat (QUanto maior melhor) modelo v2

ks_stat(actuals= baseTeste$RENOVOU,  predictedScores = probabilidadesV2) #0.4577

# AIC (QUanto menor melhor)

AIC(modeloV2, k = 2) #56856.38

predictionV2 <- prediction(predictions = probabilidadesV2, labels = baseTeste$RENOVOU)

costPerfV2 = performance(predictionV2, "cost")

cutOffV2 <- predictionV2@cutoffs[[1]][which.min(costPerfV2@y.values[[1]])]

previsoesClassesV2 <- as.factor(ifelse(probabilidadesV2 >= 0.5, "S", "N"))

previsoesClassesCutOffV2 <- as.factor(ifelse(probabilidadesV2 >=  cutOffV2, "S", "N"))

# Confusion matrix
# Sensitivy = TP RATE ou Recall maior melhor (TP/ TP + FN)
# Specifity = TN RATE (TN / TN + FP)
# Pos Pred = Precision maior melhor (TP/ TP + FP) 
# Detection rate mais próximo do prevalence possível (TP/ALL)

table(baseTeste$RENOVOU)

matrizConfusaoV1 <- 
        caret::confusionMatrix(data = previsoesClassesV2, reference = baseTeste$RENOVOU)

matrizConfusaoCutOffV2 <- 
        caret::confusionMatrix(data = previsoesClassesCutOffV2, reference = baseTeste$RENOVOU)

# Modelo v3 Retirando somente a variavel sexo

modeloV3 <- glm(RENOVOU ~ UF + UF_GRUPO + PLANO + FORMA_PAGAMENTO + PERIODICIDADE + 
                flag_recorrente + RENOVACAO_AUTOMATICA + DURACAO_PLANO + 
                VALOR_MENSAL + estado_civil + qtd_ingressos + flag_primeiro_contrato + 
                + idade + TOTAL_MESES_ATIVOS + TENURE + FLAG_JA_CANCELOU + 
                FLAG_UPGRADE + FLAG_NUNCA_SAIU, 
                data = baseTreinoV3, family = "binomial")

summary(modeloV3)

ggplot2::ggplot(varImp(modeloV3))

#Efetuando previsões

probabilidadesV3 <- predict(modeloV3, newdata = baseTeste, type = "response")

View(baseTeste)

# Curva ROC modelo v3

roc.curve(baseTeste$RENOVOU, probabilidadesV3) #0.805

# ks stat (QUanto maior melhor) modelo v2

ks_stat(actuals= baseTeste$RENOVOU,  predictedScores = probabilidadesV3) #0.4655

# AIC (QUanto menor melhor)

AIC(modeloV3, k = 2) #57866.56

predictionV3 <- prediction(predictions = probabilidadesV3, labels = baseTeste$RENOVOU)

costPerfV3 = performance(predictionV3, "cost")

cutOffV3 <- prediction@cutoffs[[1]][which.min(costPerfV3@y.values[[1]])]

previsoesClassesV3 <- as.factor(ifelse(probabilidadesV3 >= 0.5, "S", "N"))

previsoesClassesCutOffV3 <- as.factor(ifelse(probabilidadesV3 >= cutOffV3, "S", "N"))

# Confusion matrix

matrizConfusaoV3 <- caret::confusionMatrix(data = previsoesClassesV3, reference = baseTeste$RENOVOU)

matrizConfusaoCutOffV3 <- 
        caret::confusionMatrix(data = previsoesClassesCutOffV3, reference = baseTeste$RENOVOU)


# Modelo v4 Retirando a variável duracao_plano

modeloV4 <- glm(RENOVOU ~ UF + UF_GRUPO + PLANO + FORMA_PAGAMENTO + PERIODICIDADE + 
                flag_recorrente + RENOVACAO_AUTOMATICA + 
                VALOR_MENSAL + estado_civil + qtd_ingressos + flag_primeiro_contrato + 
                idade + TOTAL_MESES_ATIVOS + TENURE + FLAG_JA_CANCELOU + 
                FLAG_UPGRADE + FLAG_NUNCA_SAIU, 
                family = "binomial", data = baseTreinoV3)

summary(modeloV4)

ggplot2::ggplot(varImp(modeloV4))

#Efetuando previsões

probabilidadesV4 <- predict(modeloV4, newdata = baseTeste, type = "response")

View(baseTeste)

# Curva ROC modelo v4

roc.curve(baseTeste$RENOVOU, probabilidadesV4) #0.802

# ks stat (QUanto maior melhor) modelo v4

ks_stat(actuals= baseTeste$RENOVOU,  predictedScores = probabilidadesV4) #0.4575

# AIC (QUanto menor melhor)

AIC(modeloV4, k = 2) #57859.41

predictionV4 <- prediction(predictions = probabilidadesV4, labels = baseTeste$RENOVOU)

costPerfV4 = performance(predictionV4, "cost")

cutOffV4 <- prediction@cutoffs[[1]][which.min(costPerfV4@y.values[[1]])]

previsoesClassesCutOffV4 <- as.factor(ifelse(probabilidadesV4 >= cutOffV4, "S", "N"))

# Confusion matrix

matrizConfusaoV4 <- caret::confusionMatrix(data = previsoesClassesV4, reference = baseTeste$RENOVOU)

matrizConfusaoCutOffV4 <- 
        caret::confusionMatrix(data = previsoesClassesCutOffV4, reference = baseTeste$RENOVOU)

# Comparando resultados dos modelos v2 e v3

roc.curve(baseTeste$RENOVOU, probabilidadesV2)

roc.curve(baseTeste$RENOVOU, probabilidadesV3)

ks_stat(actuals= baseTeste$RENOVOU,  predictedScores = probabilidadesV2) 

ks_stat(actuals= baseTeste$RENOVOU,  predictedScores = probabilidadesV3) 

AIC(modeloV2, k = 2)

AIC(modeloV3, k = 2)

matrizConfusaoCutOffV2

matrizConfusaoCutOffV3


# Modelo v4 Retirando a variável uf e voltando com a duracao_plano

modeloV4 <- glm(RENOVOU ~ + PLANO + FORMA_PAGAMENTO + PERIODICIDADE + 
                        flag_recorrente + RENOVACAO_AUTOMATICA + DURACAO_PLANO +
                        VALOR_MENSAL + estado_civil + qtd_ingressos + flag_primeiro_contrato + 
                        idade + TOTAL_MESES_ATIVOS + TENURE + FLAG_JA_CANCELOU + 
                        FLAG_UPGRADE + FLAG_NUNCA_SAIU, 
                family = "binomial", data = baseTreinoV3)

summary(modeloV4)

ggplot2::ggplot(varImp(modeloV4))

#Efetuando previsões

probabilidadesV4 <- predict(modeloV4, newdata = baseTeste, type = "response")

View(baseTeste)

# Curva ROC modelo v3

roc.curve(baseTeste$RENOVOU, probabilidadesV4) #0.805

# ks stat (QUanto maior melhor) modelo v2

ks_stat(actuals= baseTeste$RENOVOU,  predictedScores = probabilidadesV4) #0.4655

# AIC (QUanto menor melhor)

AIC(modeloV4, k = 2) #57866.56

predictionV4 <- prediction(predictions = probabilidadesV4, labels = baseTeste$RENOVOU)

costPerfV4 = performance(predictionV4, "cost")

cutOffV4 <- prediction@cutoffs[[1]][which.min(costPerfV4@y.values[[1]])]

previsoesClassesV4 <- as.factor(ifelse(probabilidadesV4 >= 0.5, "S", "N"))

previsoesClassesCutOffV4 <- as.factor(ifelse(probabilidadesV4 >= cutOffV3, "S", "N"))

# Confusion matrix

matrizConfusaoV4 <- caret::confusionMatrix(data = previsoesClassesV4, reference = baseTeste$RENOVOU, positive = "S")

matrizConfusaoCutOffV4 <- 
        caret::confusionMatrix(data = previsoesClassesCutOffV4, reference = baseTeste$RENOVOU, positive = "S")

# Comparando resultados dos modelos v2 e v4

roc.curve(baseTeste$RENOVOU, probabilidadesV2)

roc.curve(baseTeste$RENOVOU, probabilidadesV4)

ks_stat(actuals= baseTeste$RENOVOU,  predictedScores = probabilidadesV2) 

ks_stat(actuals= baseTeste$RENOVOU,  predictedScores = probabilidadesV4) 

AIC(modeloV2, k = 2)

AIC(modeloV4, k = 2)

matrizConfusaoCutOffV2

matrizConfusaoCutOffV4
