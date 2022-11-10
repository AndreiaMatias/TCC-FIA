library(dplyr)
library(writexl)
options(scipen = 999)
banco = read.csv("base_final.csv", encoding="UTF-8")
head(banco)
#Variáveis Explicativas Qualitativas x Resposta
library(descr)
tabela <-CrossTable(banco$Região, banco$nota.insatisfatória,prop.r=TRUE, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)
ct_mtrx <- descr:::CreateNewTab(tabela)
write.csv(ct_mtrx,"regiao_nota.csv")
tabela
sexo_nota <- CrossTable(banco$Sexo, banco$nota.insatisfatória,prop.r=TRUE, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)
ct_mtrx <- descr:::CreateNewTab(sexo_nota)
write.csv(ct_mtrx,"sexo_nota.csv")
sexo_nota
Faixa_nota <- CrossTable(banco$Faixa.Etária, banco$nota.insatisfatória,prop.r=TRUE, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)
ct_mtrx <- descr:::CreateNewTab(Faixa_nota)
write.csv(ct_mtrx,"Faixa_nota.csv")
Faixa_nota
Tempo.Resposta <- CrossTable(banco$Tempo.Resposta, banco$nota.insatisfatória,prop.r=TRUE, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)
ct_mtrx <- descr:::CreateNewTab(Tempo.Resposta)
write.csv(ct_mtrx,"Tempo.Resposta.csv")
Tempo.Resposta
assunto <- CrossTable(banco$Tempo.Resposta, banco$nota.insatisfatória,prop.r=TRUE, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)
ct_mtrx <- descr:::CreateNewTab(assunto)
write.csv(ct_mtrx,"assunto.csv")
assunto
grupoProblema <- CrossTable(banco$Grupo.Problema, banco$nota.insatisfatória,prop.r=TRUE, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)
ct_mtrx <- descr:::CreateNewTab(grupoProblema)
write.csv(ct_mtrx,"grupoProblema.csv")
grupoProblema
problema <- CrossTable(banco$Problema, banco$nota.insatisfatória,prop.r=TRUE, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)
ct_mtrx <- descr:::CreateNewTab(problema)
write.csv(ct_mtrx,"problema.csv")
problema
comoComprou <- CrossTable(banco$Como.Comprou.Contratou, banco$nota.insatisfatória,prop.r=TRUE, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)
ct_mtrx <- descr:::CreateNewTab(comoComprou)
write.csv(ct_mtrx,"comoComprou.csv")
comoComprou
procurouEmpresa <- CrossTable(banco$Procurou.Empresa, banco$nota.insatisfatória,prop.r=TRUE, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)
ct_mtrx <- descr:::CreateNewTab(procurouEmpresa)
write.csv(ct_mtrx,"procurouEmpresa.csv")
procurouEmpresa
respondida <- CrossTable(banco$Respondida, banco$nota.insatisfatória,prop.r=TRUE, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)
ct_mtrx <- descr:::CreateNewTab(respondida)
write.csv(ct_mtrx,"respondida.csv")
respondida
situacao <- CrossTable(banco$Situação, banco$nota.insatisfatória,prop.r=TRUE, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)
ct_mtrx <- descr:::CreateNewTab(situacao)
write.csv(ct_mtrx,"situacao.csv")
situacao
avaliacao <- CrossTable(banco$Avaliação.Reclamação, banco$nota.insatisfatória,prop.r=TRUE, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)
ct_mtrx <- descr:::CreateNewTab(avaliacao)
write.csv(ct_mtrx,"avaliacao.csv")
avaliacao
#definindo o tamanho da amostra de treino
tam_amostra <- floor(0.75 * nrow(banco))

## garantindo a reproducibilidade
set.seed(123)

train_ind <- sample(seq_len(nrow(banco)), size = tam_amostra)
#Exclusão de Problema e de UF por terem gerado NA e demais variáveis que não são relevantes para o modelo. 
base_modelo = subset(banco, select = -c(Respondida, Data.Finalização, Situação, Nota.do.Consumidor, Problema, UF))
head(base_modelo)

#IV das variáveis
library(Information) 
IV <- create_infotables(data = base_modelo, y = "nota.insatisfatória")
IV$Summary
#Avaliação.Reclamação teve IV de 2,93, o que a torna suspeita (geralmente
# as pessoas atribuem nota mais alta quando a reclamação é considerada resolvida).
#Faixa Etária teve IV de 0,11, o que indica valor preditivo médio. Demais variáveis
# têm valor preditivo baixo.


#divisão das bases de treino e teste, sendo que 0 tem a proporção de cerca de 60% e 1 tem cerca de 40%
train <- base_modelo[train_ind, ]
prop.table(table(train$nota.insatisfatória))
test <- base_modelo[-train_ind, ]
prop.table(table(test$nota.insatisfatória))
dim(train)
dim(test)
modelo_reclamacao <- glm(nota.insatisfatória ~.,family = binomial(link = "logit"), data = train)
summary(modelo_reclamacao)



#Exclusão da variável "Cidade" em razão de estar acima do nível de significância de 10%



modelo_reclamacao2 <- glm(nota.insatisfatória ~ Região +
                           Sexo +
                           Faixa.Etária +
                           Tempo.Resposta +
                           Assunto +
                           Grupo.Problema +
                           Procurou.Empresa,
                         family = binomial(link = "logit"), data = train)
summary(modelo_reclamacao2)


#Retirada da variável Assunto (há diversos assuntos com nível de significância acima de 10%)

modelo_reclamacao3 <- glm(nota.insatisfatória ~ Região +
                            Sexo +
                            Faixa.Etária +
                            Tempo.Resposta +
                            Grupo.Problema +
                            Procurou.Empresa,
                          family = binomial(link = "logit"), data = train)
summary(modelo_reclamacao3)

#Retirada de Região (acima de 10% de significância)

modelo_reclamacao4 <- glm(nota.insatisfatória ~ 
                            Sexo +
                            Faixa.Etária +
                            Tempo.Resposta +
                            Grupo.Problema +
                            Procurou.Empresa,
                          family = binomial(link = "logit"), data = train)
summary(modelo_reclamacao4)

#Retirada de Grupo.Problema

modelo_reclamacao4 <- glm(nota.insatisfatória ~ 
                            Sexo +
                            Faixa.Etária +
                            Tempo.Resposta +
                            Procurou.Empresa,
                          family = binomial(link = "logit"), data = train)
summary(modelo_reclamacao4)

#O modelo final indica que as variáveis mais importantes são Sexo, Faixa Etária,
#Tempo de resposta e se procurou a empresa.

train$reg_log_p1 <- predict(modelo_reclamacao4, newdata = train, type = "response")


#Criar a resposta final usando o ponto de corte
train$predito1 <- as.factor(ifelse(train$reg_log_p1>0.5, 1, 0))
View(train)

library(expss)
cro(train$nota.insatisfatória,train$predito1)


base_modelo$reg_log_p1 <- predict(modelo_reclamacao, newdata = train, type = "response")

#Modelo com 0.5 de ponto de corte
(7190 + 1671)/(7190 + 3947+ 1357 + 1671) #Acurácia 62,55% - (VP + VN)/(VP +VN + FP + FN)
1671/(1671+3947) #Sensibilidade 29,74% (VP)/(VP+FN)
7190/(7190+1347) #Especificidade 84,22% (VN)/(VN+FP)

#Com o ponto de corte definido em 0,5, o modelo não consegue
#prever as notas insatisfatórias bem.

#Predição com conjunto de teste
test$reg_log_p1 <- predict(modelo_reclamacao4, newdata = test, type = "response")


#Criar a resposta final usando o ponto de corte
test$predito1 <- as.factor(ifelse(test$reg_log_p1>0.5, 1, 0))
View(test)


cro(test$nota.insatisfatória,test$predito1)


#Modelo de teste com 0.5 de ponto de corte
(2385 + 550)/(2385 + 1332+ 455 + 550) #Acurácia 62,15% - (VP + VN)/(VP +VN + FP + FN)
550/(550+1332) #Sensibilidade 29,22 (VP)/(VP+FN)
2385/(2385+455) #Especificidade 83,97% (VN)/(VN+FP)


library(Rcpp)
library(cutpointr)
#Maximizar acurácia
ponto <- cutpointr(train, reg_log_p1, nota.insatisfatória,
                   method = maximize_metric, metric = accuracy)
summary(ponto)
# O ponto de corte para maximizar a acurácia é de 0,4914, próximo do ponto já
# definido

#Minimizar a diferença entre especificidade e sensibilidade
ponto <- cutpointr(train, reg_log_p1, nota.insatisfatória,
                   method = minimize_metric, metric = abs_d_sens_spec)
summary(ponto)
#Como o interesse é predizer as notas insatisfatórios, buscou-se
#o equilíbrio entre sensibilidade e especificidade. Para verificar 
#se a sensibilidade aumenta.

#Criar a resposta final usando o ponto de corte 0,4095 para minimizar
#a diferença entre sensibilidade e especificidade
train$predito2 <- as.factor(ifelse(train$reg_log_p1>0.4095, 1, 0))
View(train)

cro(train$nota.insatisfatória,train$predito2)


#modelo com ponto de corte otimizado 0.4095
(5222 + 3318)/(5222 + 2300 + 3325 + 3318) #Acurácia 60,28% - (VP + VN)/(VP +VN + FP + FN)
3318/(3318+2300) #Sensibilidade 59,06 (VP)/(VP+FN)
5222/(5222+3325) #Especificidade 61,09%

test$predito2 <- as.factor(ifelse(test$reg_log_p1>0.4095, 1, 0))
View(test)

cro(test$nota.insatisfatória,test$predito2)

#modelo com ponto de corte otimizado 0.4095
(1748 + 1107)/(1748 + 1092 + 775 + 1107) #Acurácia 60,46% - (VP + VN)/(VP +VN + FP + FN)
1107/(1107+775) #Sensibilidade 58,82 (VP)/(VP+FN)
1748/(1748+1092) #Especificidade 61,54%


library(InformationValue)
#KS e ROC
ks_stat(actuals=train$nota.insatisfatória, predictedScores=train$reg_log_p1)
plotROC(actuals=train$nota.insatisfatória, predictedScores=train$reg_log_p1)

ks_stat(actuals=test$nota.insatisfatória, predictedScores=test$reg_log_p1)
plotROC(actuals=test$nota.insatisfatória, predictedScores=test$reg_log_p1)

