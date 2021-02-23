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
--CIENCIAS_SOCIALES AREA,
--CIENCIAS_NATURALES AREA,
--CIENCIAS_AGRICOLAS AREA,
--INGENIERIA_Y_TECNOLOGIA AREA,
--CIENCIAS_MEDICAS_Y_DE_LA_SALUD AREA,

--SOCIAL,CULTURA,SOCIEDAD,CONFLICTO,ECONOMICO,ECONOMIA,POLITICA,DERECHO,FORMACION,EDUCACION,ECONOM,SOCIALES,ESTUDIOS,COMUNIDAD,PAZ,JURIDICO,PEDAGOGICO,GLOBALIZACION,INDEXADAS,DEMOCRACIA,RELIGACION,RELACIONES_INTERNACIONALES,MERCADEO,FINANZA,SOCIOECONOMICO,CAPITALISMO,LEYES,TRABAJO_SOCIAL,SOCIOLOGIA,RELIGION,VIOLENCIA,CORRUPCION,CRIMEN,POBREZA,CURRICULO,INTERCULTURALIDAD,SOCIO,
--MATERIALES,CONSERVACION,QUIMICA,BIOLOGIA,FISICA,ESPECIES,BIODIVERSIDAD,ECOSISTEMAS,AMBIENTE,BIOLOGICA,MICROBIOLOGIA,FAUNA,ACUATICOS,OPTICA,LASER,GEOLOGIA,BIOTECNOLOGIA,BIOINGENIERIA,SOSTENIBLE,BIOCALORIMETRIA,FORESTAL,MATEMATICAS,RECURSOS_NATURALES,MEDIO_AMBIENTE,TERMICAS,CAMBIO_CLIMATICO,FISICOQUIMICA,NEOTROPICALES,MORFOLOGIA OCEANOLOGIA,COSMOLOGIA,ASTROFISICA,ASTRONOMIA,ECOLOGIA,CONTAMINACION,METALURGIA,MATEMATICA,MICROALGAS,IMPACTO_AMBIENTAL,VIDA_SILVESTRE,RENOVABLES,ECUACIONES_DIFERENCIALES,CIENCIAS_BASICAS,
----AGROPECUARI,ANIMAL,PLANTAS,PLAGAS,AGRICULTURA,TROPICALES,CULTIVO,SUELOS,CAFE,AGROINDUSTRIAL,VETERINARIA,AGRICOLA,AGROINDUSTRIALES,TROPICAL,AGROECOSISTEMAS,PALMA,AGRARIAS,VEGETALES,ACUICULTURA,AGRONOMICAS,CRIOLLO,GANADER,ARROZ,CULTIV,AGRONEGOCIO,PECES,CONURBANO,AGROFORESTAL,GIES,CARIBE,SALUD_ANIMAL,GIDOCA,BIOTOX,GINPAS,GRITOX,GIPSA,APICULTURA,AGRARIO,CIENCIAS_ANIMALES,PECUARIA,BANANO,AGUACATE,PAPA,CANA_DE_AZUCAR,VACUNO,HIDROLOGIA,HIDRO,HIDRICO,
--DISENO, TECNOLOGICO,TECNOLOGIA,PROGRAMAS,TECNOLOGICA,HERRAMIENTAS,APLICACIONES,FOMENTAR,SOFTWARE,TELECOMUNICACIONES,PROGRAMA,AUTOMATIZACION,LABORATORIO,INGENIERIA,ENERGETICA,BIG_DATA,MACHINE_LEARNING,ARTIFICIAL_INTELLIGENCE,DEEP,INGENIERIA_AMBIENTAL,AUTOMATICA,INFRAESTRUCTU,ROBOTICA,INFORMATICA,ENGINEERING,CIENCIAS_TERMICAS,NANOESTRUCTURA,CIENCIAINGENIERIA,
--SALUD,MEDICINA,CLINICA,VIDA,EPIDEMIOLOGIA,ENFERMEDAD,PACIENTE,ENFERMERIA,REHABILITACION,INFECCIOS,CANCER,DISCAPACIDAD,MEDICAMENTO,INMUNOLOGIA,CLINICO,PATOLOGIA,BIOMEDICA,MEDICA,TERAPEUTICA,CIRUGIA,EPIDEMIOLOGICO,ODONTOLOGIA,HOSPITAL,FARMACEUTICA,VIRUS,SALUD_NFERMEDAD,CARDIOVASCULAR,FISIOTERAPIA,ARTRITIS,ESCLEOLORISIS,ANTIBIOTICO,NEUMONIA,MUSCULO,QUIRURGICA,TUMOR,METABOLISMO,GINECOLO

--from 
--  `analitica-302215.GRUPOS_AUTORES.traini1` 
 
SELECT 
CIENCIAS_SOCIALES AREA,
JURIDICO, PEDAGOGICO, GLOBALIZACION, INDEXADAS,  DEMOCRACIA, RELIGACION, RELACIONES_INTERNACIONALES, MERCADEO, FINANZA, SOCIOECONOMICO,  CAPITALISMO, LEYES, TRABAJO_SOCIAL,  SOCIOLOGIA, RELIGION,VIOLENCIA, CORRUPCION ,SOCIALES, ESTUDIOS, EDUCACION,  ECONOMIA, POLITICA, DERECHO, SOCIAL, CULTURA , CONFLICTO,  CRIMEN, POBREZA, CURRICULO, INTERCULTURALIDAD,
from 
  `analitica-302215.GRUPOS_AUTORES.traini1`

 "



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
mylist[] <- lapply(mylist[], gsub, pattern=' FALSE', replacement='')
df = df %>% dplyr::select(-(mylist[[1]]))

################### subseting in train and test

set.seed(4123)
split = caTools::sample.split(df$AREA, SplitRatio = 0.8)
training_set = subset(df, split == TRUE)
testing_set = subset(df, split == FALSE)

# Escalado de valores
#training_set[,1] = scale(training_set[,1])
#testing_set[,1] = scale(testing_set[,1])


####################

backwardElimination <- function(x, sl) {
  vardep <- x %>% dplyr::select(AREA)
  varind <- x %>% dplyr::select(-AREA)
  numVars = ncol(x)
  for (i in c(1:numVars)){
    regressor = glm(data = cbind(vardep, varind), formula = AREA ~ ., family = "binomial") 
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

####################
#library(MASS)
#modback <- stepAIC(modelo, trace=TRUE, direction="backward")
# Ajustar el clasificador con el conjunto de entrenamiento.
# Crear el modelo de clasificacion
sl = 0.05
backwardElimination(df, sl)
modelo = glm(data=training_set, AREA ~ ., family = "binomial") 
summary(modelo)

coef(summary(regressor))[ , 4]
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

