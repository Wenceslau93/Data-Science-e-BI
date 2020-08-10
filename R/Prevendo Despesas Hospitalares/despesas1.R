#Prevendo despesas Hospitalares


#Definindo local de trabalho

setwd("C:/FCD/ReAzure/Cap11/Regressao")
getwd()

#Carregando o arquivo

despesas <- read.csv("despesas.csv")
View(despesas)

#Resumo

summary(despesas)
str(despesas)

#Visualizando em um gráfico

?hist
hist(despesas$gastos, labels = T ,xlab = 'Gastos', main = 'Prevendo despesas Hospitalares')

#Visualizando a quantidade de dados de regiões

table(despesas$regiao)

#Visualizando correlação de váriaveis númericas

library("corrplot")
correlacao <- cor(despesas[c("idade", "bmi", "filhos", "gastos")])
corrplot(correlacao, method="color", tl.cex = 1, type="full", addCoef.col = "black")

#Treinando o modelo

?lm
modelo <- lm(gastos ~ ., data = despesas)
modelo

#Realizando previsão do modelo

despesas2 <- despesas
despesas2$previsao1 <- predict(modelo)
View(despesas2)

#Avaliando o modelo

summary(modelo)


#Realizando segunda previsão com várias mais importantes

despesas3 <- despesas
despesas3$fumanteSim <- ifelse(despesas3$fumante == "sim", 1, 0)
despesas3$bmi30 <- ifelse(despesas$bmi >= 30, 1, 0)

modelo2 <- lm(gastos ~ idade + filhos + bmi30 * fumanteSim, data = despesas3)

#Avaliando segundo modelo

summary(modelo2)

#Previsao com os dados de teste

?read.csv
teste <- read.csv("despesas-teste2.csv", header = TRUE, sep = ";")
View(teste)

prev <- predict(modelo2, teste)
View(prev)

# Criando nova variável e arredondando a coluna preditora 

teste$prev <- prev
teste$prev <- ceiling(teste$prev)
View(teste)
typeof(teste$prev)

#Salvando o arquivo csv

write.csv(teste, "teste.csv")
