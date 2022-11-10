# Carregando pacotes

library("gtools")
library("partykit")
library("CHAID")
library(readxl)
library(expss)
library(rpart.plot)

#**********************************************************
#Leitura da base

banco = read.csv("base_final.csv", encoding="UTF-8")

# Transformar variaveis em fator
names(banco)

banco[sapply(banco, is.character)] <- lapply(banco[sapply(banco, is.character)], as.factor)
banco$nota.insatisfatória <- as.factor(banco$nota.insatisfatória)
banco$Tempo.Resposta <- as.factor(banco$Tempo.Resposta)


#definindo o tamanho da amostra de treino
tam_amostra <- floor(0.75 * nrow(banco))

## garantindo a reproducibilidade
set.seed(123)

train_ind <- sample(seq_len(nrow(banco)), size = tam_amostra)
#Exclusão de variáveis que não são relevantes para o modelo e das variáveis 
#cidade e Problema em razão da quantidade de itens. 
base_modelo = subset(banco, select = -c(Cidade, Problema,Respondida, Data.Finalização, Situação, Nota.do.Consumidor))
head(base_modelo)

#divisão das bases de treino e teste, sendo que 0 tem a proporção de cerca de 60% e 1 tem cerca de 40%
train <- base_modelo[train_ind, ]
prop.table(table(train$nota.insatisfatória))
test <- base_modelo[-train_ind, ]
prop.table(table(test$nota.insatisfatória))
dim(train)
dim(test)

# Árvore de decisao de acordo com o algoritmo CHAID

controle <- chaid_control(maxheight = 4,
                          minsplit = 200)


arvore <- chaid(nota.insatisfatória ~
                  Região +
                  UF  +
                  Sexo +
                  Faixa.Etária +
                  Tempo.Resposta +
                  Assunto +
                  Grupo.Problema +
                  Como.Comprou.Contratou +
                  Procurou.Empresa,
                data = train,
                #control=controle)
)

plot(arvore, gp = gpar(cex = 0.5))



# Frequencia de cada no final

train$no <- predict(arvore, train, type = "node")
table(train$no)

# propensão a atribuir nota insatisfatória

prob_geral = sum(train$nota.insatisfatória == "1") / nrow(train)
cro_rpct(train$no, train$nota.insatisfatória)

# h. Avalie o desempenho do modelo, por meio dos indices de sensibilidade, especificidade e acuracia.

train$prob <- predict(arvore, train, type = "p")[,2]
train$predito <- ifelse(train$prob >= prob_geral, "1", "0")

cro(train$nota.insatisfatória, train$predito)
#Modelo com 0.5 de ponto de corte
(5211 + 3302)/(5211 + 3336 + 2316 + 3302) #Acurácia 60,09% - (VP + VN)/(VP +VN + FP + FN)
3302/(3302+2316) #Sensibilidade 58,77% (VP)/(VP+FN)
5211/(5211+3336) #Especificidade 60,96% (VN)/(VN+FP)

#teste
arvore <- chaid(nota.insatisfatória ~
                  Região +
                  UF  +
                  Sexo +
                  Faixa.Etária +
                  Tempo.Resposta +
                  Assunto +
                  Grupo.Problema +
                  Como.Comprou.Contratou +
                  Procurou.Empresa,
                data = test,
                #control=controle
                )


plot(arvore, gp = gpar(cex = 0.5))



# Frequencia de cada no final

test$no <- predict(arvore, test, type = "node")
table(test$no)

# propensão a atribuir nota insatisfatória

prob_geral = sum(test$nota.insatisfatória == "1") / nrow(test)
cro_rpct(test$no, test$nota.insatisfatória)

# Avaliação do desempenho do modelo

test$prob <- predict(arvore, test, type = "p")[,2]
test$predito <- ifelse(test$prob >= prob_geral, "1", "0")

#Modelo com 0.5 de ponto de corte
(1732 + 1114)/(1732 + 1108 + 768 + 1114) #Acurácia 60,27% - (VP + VN)/(VP +VN + FP + FN)
1114/(1114+768) #Sensibilidade 59,19% (VP)/(VP+FN)
1732/(1732+1108) #Especificidade 60,98% (VN)/(VN+FP)
