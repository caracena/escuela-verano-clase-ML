library(data.table)
library(C50)
library(gmodels)
library(caret)

## carga de datos
dataset <- fread('dataset_empresas_adscripciones.csv', stringsAsFactors = T)
dataset$ventas <- as.factor(dataset$ventas)
dataset$Target <- as.numeric(dataset$Target)

dataset <- dataset[,c(1,2,3,4,5,6,9,11)]

## random train y test
set.seed(123)
vector_entrenamiento <- sample(1:nrow(dataset),0.7*nrow(dataset))
data_train <- dataset[vector_entrenamiento,] 
data_test <- dataset[-vector_entrenamiento,]

## revisamos proporciones
prop.table(table(data_train$Target))
prop.table(table(data_test$Target))

## generamos el modelo
modelo = C5.0(x = data_train[, -c(1,8)], 
              y = as.factor(data_train$Target),
              control = C5.0Control(minCases = 100))

## exploramos el modelo
summary(modelo)

## predecimos
predicciones <- predict(modelo, 
                        newdata = data_test[, -c(1,8)])
CrossTable(data_test$Target, predicciones,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

## accuracy
print(100*sum(data_test$Target==predicciones)/length(predicciones))

## dibujar el arbol
plot(modelo)
