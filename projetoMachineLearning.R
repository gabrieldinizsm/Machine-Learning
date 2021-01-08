# Fonte de dados https://catalog.data.gov/dataset/traffic-data

memory.limit (9999999999)

setwd("C:/Users/user/Desktop/Scripts R/VT LP")

baseDados <- read.csv2("C:/Users/user/Desktop/Scripts R/VT LP/acidentes.csv")

View (baseDados)

unique(baseDados$Injury.Type)

# Definição do problema de negócio: Prever a gravidade do ferimento de um acidente
# Técnica escolhida: Classificação múltipla


nomesColunas <- c("idAcidente", "ano", "mes", "diaSemana", "finalSemana", 
                   "hora", "tipoColisao", "gravidadeFerimento", "fatorPrimario", 
                   "localidade", "latitude", "longitude")

colnames(baseDados) <- nomesColunas

View (baseDados)

dim(baseDados)

library(ggplot2)


# Distribuição de acidentes Por Gravidade do Ferimento (Expressa em %)

AgregadoAcidenteFerimento = aggregate(idAcidente ~ gravidadeFerimento, 
                                      data = baseDados, 
                                      FUN = length)

# Gráfico 1

ggplot(AgregadoAcidenteFerimento, aes(x = gravidadeFerimento, width = 0.5)) + 
geom_bar(stat = "identity", 
         aes(y = idAcidente / sum(idAcidente) * 100)) + 
geom_text(aes(label = paste(round(idAcidente / sum(idAcidente) * 100, 2),"%"),
              y = idAcidente / sum(idAcidente) * 100), 
          hjust = -0.2, 
          check_overlap = TRUE, 
          size = 3) + 
ylim(0, ceiling(max(AgregadoAcidenteFerimento$idAcidente) / sum(AgregadoAcidenteFerimento$idAcidente)) * 100) + 
coord_flip() + 
xlab(label = "Gravidade Ferimento") + 
ylab(label = "Percentual de Acidentes")

# Gráfico 2

attach(baseDados)

boxplot(hora ~ gravidadeFerimento,
        col = "red",
        ylab = 'Hora do acidente',
        xlab = 'Gravidade do Ferimento',
        names = unique(gravidadeFerimento))

# Gráfico 3

ggplot(baseDados, aes(ano, fill = gravidadeFerimento)) + 
geom_bar(position = "dodge") + 
xlab(label = "Ano do Acidente") + 
ylab(label = "Total de Acidentes")

# Pré processamento

names(baseDados)

baseDadosV2 <- baseDados[, c(-1, -10, -11, -12)]

dim(baseDadosV2)

# Excluindo valores nulos e na

baseDadosV2 <- baseDadosV2[!(baseDadosV2$finalSemana==""), ] 
baseDadosV2 <- baseDadosV2[!(baseDadosV2$hora==""), ] 
baseDadosV2 <- baseDadosV2[!(baseDadosV2$tipoColisao==""), ] 
baseDadosV2 <- baseDadosV2[!(baseDadosV2$fatorPrimario==""), ] 
baseDadosV2 <- na.omit(baseDadosV2)

any(is.null(baseDadosV2))
any(is.na(baseDadosV2))

dim(baseDadosV2)

# Randomizando a ordem dos dados

baseDadosV3 <- baseDadosV2[order(runif(dim(baseDadosV2)[1])),] 

View(baseDadosV3)

# Divisão do dataset em treino e teste

indiceLinhas = 0.75 * nrow(baseDadosV3) 

baseTreino <- baseDadosV3[1:indiceLinhas,]
baseTeste <- baseDadosV3[(indiceLinhas+1):nrow(baseDadosV3) ,]

# Convertendo para o tipo fator

baseTreino$gravidadeFerimento <- as.factor(baseTreino$gravidadeFerimento)
baseTreino$ano <- as.factor(baseTreino$ano)
baseTreino$finalSemana <- as.factor(baseTreino$finalSemana)
baseTreino$mes <- as.factor(baseTreino$mes)
baseTreino$hora <- as.factor(baseTreino$hora)
baseTreino$diaSemana <- as.factor(baseTreino$diaSemana)
baseTreino$fatorPrimario <- as.factor(baseTreino$fatorPrimario)
baseTreino$tipoColisao <- as.factor(baseTreino$tipoColisao)

baseTeste$gravidadeFerimento <- as.factor(baseTeste$gravidadeFerimento)
baseTeste$ano <- as.factor(baseTeste$ano)
baseTeste$finalSemana <- as.factor(baseTeste$finalSemana)
baseTeste$mes <- as.factor(baseTeste$mes)
baseTeste$hora <- as.factor(baseTeste$hora)
baseTeste$diaSemana <- as.factor(baseTeste$diaSemana)
baseTeste$fatorPrimario <- as.factor(baseTeste$fatorPrimario)
baseTeste$tipoColisao <- as.factor(baseTeste$tipoColisao)

str(baseTreino)

table(baseTeste$gravidadeFerimento)

library(UBL)

# Balanceando as classes através do oversample

?AdasynClassif
baseTreinoBalanceada <- AdasynClassif(gravidadeFerimento ~ ., baseTreino, dist = 'Overlap')

table(baseTreinoBalanceada$gravidadeFerimento)

# Construindo o modelo de regressão

library(nnet)

modeloRegressao <- multinom(gravidadeFerimento ~ ., 
                            data = baseTreinoBalanceada,
                            maxit = 1000,
                            MaxNWts = 500)

# Previsões de classe nos dados de treino

baseTreino$previsoesClasses <- predict(modeloRegressao, newdata = baseTreino, "class")

View(baseTreino)

# Tabela de classificação

tabelaClassificacaoTreino <- table(baseTreino$gravidadeFerimento, baseTreino$previsoesClasses)

# Calculando acurácia em treino

round((sum(diag(tabelaClassificacaoTreino)) / sum(tabelaClassificacaoTreino)) * 100, 2)

# Previsões de classe nos dados de teste

baseTeste$previsoesClasses <- predict(modeloRegressao, newdata = baseTeste, "class")
View(baseTeste)

# Tabela de classificação

tabelaClassificacaoTeste <- table(baseTeste$gravidadeFerimento, baseTeste$previsoesClasses)

# Calculando acurácia em teste

round((sum(diag(tabelaClassificacaoTeste)) / sum(tabelaClassificacaoTeste)) * 100, 2)

# Conclusões

# Tabela - Qual gravidade de ferimento apresenta a maior probabilidade média de acontecer?

probabilidadeGravidadeFerimento <- predict(modeloRegressao, newdata = baseTeste, "probs") 

View(probabilidadeGravidadeFerimento)

# Calculamos a média por coluna

probabilidadeMedia <- apply(probabilidadeGravidadeFerimento, 2, mean)
View(probabilidadeMedia)



