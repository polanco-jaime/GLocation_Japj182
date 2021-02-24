# Store the project id
projectid = "analitica-302215"
#install.packages("bigrquery")
library(bigrquery)
library(caTools)
library(tidyverse)
library(stringr)
library(dplyr)
# Set your query

sql <- "
 
SELECT
CIENCIAS_AGRICOLAS,
AGROPECUARI, ANIMAL,  PLAGAS, AGRICULTURA,CULTIVO, SUELOS, CAFE, AGROINDUSTRIAL, VETERINARIA, AGRICOLA, TROPICAL, AGROECOSISTEMAS, PALMA, AGRARIAS, VEGETALES, ACUICULTURA, AGRONOMICAS, CRIOLLO,  
GANADER, ARROZ,  CULTIV, AGRONEGOCIO, PECES, CONURBANO, AGROFORESTAL, GIES, CARIBE, SALUD_ANIMAL,  GIDOCA, BIOTOX, GINPAS, GRITOX, GIPSA, APICULTURA,AGRARIO, CIENCIAS_ANIMALES, PECUARIA,BANANO, AGUACATE, 
PAPA, CANA_DE_AZUCAR, BIOTECNOLOGIA,  VACUNO, 

from 
  `analitica-302215.GRUPOS_AUTORES.traini1`

 "
variable_dependiente = 'CIENCIAS_AGRICOLAS'
###########################  read query and clean  ##################################################

# Run the query and store the data in a dataframe
df <- query_exec(sql, projectid, use_legacy_sql = FALSE, max_pages = Inf)

#library(dplyr)
n = ncol(df) 
for (i in 1:n) { 
  df[[i]] = as.factor(df[[i]])
}

mylist=data.frame()
for (i in 1:n) { 
  a = table(df[[i]])/nrow(df)
  listtmp = data.frame(((colnames(df)[i])),(a<0.9995))
  #barplot(table(df[[i]])/nrow(df))
  #listtmp[] = lapply(listtmp[], gsub, pattern=' TRUE', replacement='')
  mylist <- rbind(mylist, listtmp)
  mylist <- subset(mylist, str_detect(paste0(mylist[[1]],' ' ,mylist[[2]] ), "FALSE") ==T) 
}

df = df %>% dplyr::select(-(mylist[[1]]))

################### subseting in train and test

set.seed(4123)
split = caTools::sample.split(df[variable_dependiente], SplitRatio = 0.8)
training_set = subset(df, split == TRUE)
testing_set = subset(df, split == FALSE)

# Escalado de valores
#training_set[,1] = scale(training_set[,1])
#testing_set[,1] = scale(testing_set[,1])


####################
x = df

backwardElimination_aic <- function(x, sl, variable_dependiente){
  vardep <- x %>% dplyr::select(variable_dependiente)
  colnames(vardep)[1] = 'Y'
  varind <- x %>% dplyr::select(-variable_dependiente)
  numVars = ncol(varind)
  temp = matrix(data = 0, ncol = ncol(varind), nrow = nrow(varind))
  temp <- as.data.frame(temp)
  colnames(temp) <- colnames(varind)
  for (i in c(1:numVars)){
    regressor = glm(data = cbind(vardep, varind), formula = Y ~ ., family = "binomial") 
    maxVar = max(coef(summary(regressor))[c(2:numVars),4])
    adjR_before = summary(regressor)$aic
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), 4] == maxVar)
      temp[, j] = varind[, j]
      varind = varind[, -j]
      tmp_regressor = glm(data = cbind(vardep, varind), formula = Y ~ ., family = "binomial") 
      adjR_after = summary(tmp_regressor)$aic
      if (adjR_before >= adjR_after){
        x_rollback <- cbind(varind = varind[,], temp[,j])
        colnames(x_rollback) <- c(names(varind), names(temp)[j])
        varind <-as.data.frame(x_rollback)
      }
      else{
        numVars = numVars - 1
      }
    }
  }
  return(summary(regressor))
} 

distinct_(data.frame('unicos' = c(names(varind), names(temp)[j])))
backwardElimination_aic(df, sl, variable_dependiente)
backwardElimination <- function(x, sl, variable_dependiente) {
  vardep <- x %>% dplyr::select(variable_dependiente)
  colnames(vardep)[1] = 'Y'
  varind <- x %>% dplyr::select(-variable_dependiente)
  numVars = ncol(x)
  for (i in c(1:numVars)){
    regressor = glm(data = cbind(vardep, varind), formula = Y ~ ., family = "binomial") 
    maxVar = max(coef(summary(regressor))[c(2:numVars), 4])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), 4] == maxVar)
      #x = x[, -j]
      varind = varind[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
  #return(data = cbind(vardep, varind))
}

################################################
#library(MASS)
#modback <- stepAIC(modelo, trace=TRUE, direction="backward")
# Ajustar el clasificador con el conjunto de entrenamiento.
# Crear el modelo de clasificacion
sl = 0.1
backwardElimination(df, sl, variable_dependiente)
backwardElimination_aic(df, sl, variable_dependiente)
#####################################################
modelo = glm(data=training_set, CIENCIAS_AGRICOLAS ~ ., family = "binomial") 
summary(modelo)


# Prediccion de los resultados con el conjunto de testing
prob_pred = predict(modelo, type = 'response', testing_set)

y_pred = ifelse(prob_pred>=0.5,1,0)
# Crear la matriz de confusion

cm = table(testing_set[[1]], y_pred)
cm

humanidades_test  = ifelse(testing_set[[1]] == 0, 'No', 'Yes')
humanidades  = ifelse(y_pred == 0, 'No', 'Yes')
cm = table(humanidades_test, humanidades)
cm
precision = sum(diag(cm))/sum(cm)
precision

