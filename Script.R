
library(readxl)
library(tidyverse)
library(dplyr)
library(MASS)
library(effects)
library(caret)
library(gtsummary)
library(pROC)
set.seed(1234)
read_excel('Input File/BASE_OF.xlsx')->datos

datos%>%
  mutate(SEXO=factor(SEXO,levels=c('1','2')),
         GREDAD=factor(GREDAD,levels=c('1','2','3')),
         EMESES=as.numeric(EMESES),
         EDI=factor(EDI,levels=c('1','2','3')),
         OBESIDAD=factor(OBESIDAD,levels=c('0','1')),
         ANEMIA=factor(ANEMIA,levels=c('0','1')),
         AFISICA=factor(AFISICA,levels=c('0','1')),
         CDIETA=factor(CDIETA,levels=c('1','2','3')),
         EDADMADRE=as.numeric(EDADMADRE),
         GINTRUC=factor(GINTRUC,levels=c('1','2','3','4','5'))
         )->datos


###SUMMARY

datos%>%
  tbl_summary(      
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    include = c(colnames(datos)),
    #by = Grupo, # split table by group
    missing = "no" # don't list missing data separately
  ) |> 
  #add_overall()|> 
  add_n() |> # add column with total number of non-missing observations
  #add_p() |> # test for a difference between groups
  modify_header(label = "**Variable**") |> # update the column header
  bold_labels()


datos%>%
  tbl_summary(      
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    include = c("IDENTIFICADOR","SEXO","GREDAD" ,"EMESES","OBESIDAD",
                "ANEMIA","AFISICA","CDIETA","EDADMADRE","GINTRUC" ),
    by = EDI, # split table by group
    missing = "no" # don't list missing data separately
  ) |> 
  add_overall()|> 
  add_n() |> # add column with total number of non-missing observations
  add_p(test = all_categorical() ~ "chisq.test") |> # test for a difference between groups
  #modify_header(label = "**Variable**") |> # update the column header
  bold_labels()



# 4. Ajustar modelo de regresión ordinal
modelo_ordinal <- polr(EDI ~ SEXO+GREDAD+EMESES+OBESIDAD+
                              ANEMIA+AFISICA+CDIETA+EDADMADRE+GINTRUC,
                       data = datos, Hess = TRUE)

# 5. Ver el resumen del modelo
summary(modelo_ordinal)

# 6. Obtener p-valores
ctable <- coef(summary(modelo_ordinal))
pvalores <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = round(pvalores, 4))
print(ctable)

# 7. Interpretación gráfica de los efectos
plot(allEffects(modelo_ordinal))

### ENTRENAMIENTO

# Número de repeticiones
n_reps <- 1000

# Guardamos las métricas
accuracy_vect <- numeric(n_reps)
kappa_vect <- numeric(n_reps)
labels_acumulados <- c()
pred_acumulados <- c()
probs_acumuladas <- data.frame()

for (i in 1:n_reps) {
  print(i)
  # 1. División aleatoria entrenamiento/validación (70/30)
  trainIndex <- createDataPartition(datos$EDI, p = 0.8, list = FALSE)
  datos_entrenamiento <- datos[trainIndex, ]
  datos_validacion <- datos[-trainIndex, ]
  
  # 2. Entrenamiento
  modelo_ordinal <- polr(EDI ~ SEXO + GREDAD + OBESIDAD + AFISICA + CDIETA,
                         data = datos_entrenamiento, Hess = TRUE)
  
  # 3. Predicción
  predicciones <- predict(modelo_ordinal, newdata = datos_validacion)
  probas <- predict(modelo_ordinal, newdata = datos_validacion, type = "probs")
  # 4. Evaluación
  cm <- confusionMatrix(predicciones, datos_validacion$EDI)
  accuracy_vect[i] <- cm$overall["Accuracy"]
  kappa_vect[i] <- cm$overall["Kappa"]
  
  # Guardar resultados
  labels_acumulados <- c(labels_acumulados, as.character(datos_validacion$EDI))
  pred_acumulados <- c(pred_acumulados, as.character(predicciones))
  probs_acumuladas <- rbind(probs_acumuladas, probas)
  
}

# Resultados promedio
mean(accuracy_vect)
mean(kappa_vect)


##NOMOGRAMA
library(MASS)     # para polr
library(rms)
dd <- datadist(datos)
options(datadist = "dd")
# Reajustar el modelo con lrm (regresión logística ordinal compatible con nomogramas)
modelo_rms <- lrm(EDI ~ SEXO + GREDAD + OBESIDAD + AFISICA + CDIETA,
                  data = datos)
nom <- nomogram(modelo_rms,
                fun = function(x) plogis(x),  # para convertir logits a probabilidades
                funlabel = "Probabilidad")
par(mfrow = c(1, 1))
plot(nom)

png('nomogram.png',width=4500,height = 2800,res=600)
plot(nom)
dev.off()


library(DynNom)   # para crear la calculadora interactiva

  
  
DynNom::DynNom(modelo_rms, data = datos)

# Distribución de resultados
summary(accuracy_vect)
summary(kappa_vect)
par(mfrow = c(1, 2))  # 3 clases
hist(accuracy_vect, main = "Distribución de Acuracia", col = "lightblue")
hist(kappa_vect, main = "Distribución de Kappa", col = "lightgreen")

png('acuracia y kappa.png',width=3800,height = 2200,res=600)
par(mfrow = c(1, 2))  # 3 clases
hist(accuracy_vect, main = "Distribución de Acurácia", col = "lightblue")
hist(kappa_vect, main = "Distribución de Kappa", col = "lightgreen")
dev.off()

## AUC


# Asegurarse que clases sean factor con niveles correctos
labels_factor <- factor(labels_acumulados, levels = levels(datos$EDI))

# Usar pROC para multiclase AUC
auc_multi <- multiclass.roc(response = labels_factor, predictor = as.matrix(probs_acumuladas))
auc_multi$auc



par(mfrow = c(1, 3))  # 3 clases

niveles <- levels(datos$EDI)

for (clase in niveles) {
  
  # Etiqueta binaria: 1 si es la clase actual, 0 si no lo es
  respuesta_binaria <- ifelse(labels_factor == clase, 1, 0)
  
  if (length(unique(respuesta_binaria)) == 2) {
    
    # Curva ROC
    roc_clase <- roc(respuesta_binaria, probs_acumuladas[[clase]])
    auc_valor <- round(auc(roc_clase), 3)
    
    # Graficar curva ROC
    plot(roc_clase,
         main = paste("ROC - Clase", clase),
         col = "blue",
         lwd = 2)
    
    # Agregar valor de AUC al gráfico
    text(x = 0.4, y = 0.2, labels = paste("AUC =", round(auc_valor,2)), col = "red", cex = 1.2)
    
    # También se puede imprimir por consola
    cat("AUC Clase", clase, ":", auc_valor, "\n")
    
  } else {
    cat("Clase", clase, "no tiene suficientes casos positivos/negativos.\n")
  }
}

png('AUC.png',width=5000,height = 2000,res=600)
par(mfrow = c(1, 3))
for (clase in niveles) {
  
  # Etiqueta binaria: 1 si es la clase actual, 0 si no lo es
  respuesta_binaria <- ifelse(labels_factor == clase, 1, 0)
  
  if (length(unique(respuesta_binaria)) == 2) {
    
    # Curva ROC
    roc_clase <- roc(respuesta_binaria, probs_acumuladas[[clase]])
    auc_valor <- round(auc(roc_clase), 3)
    
    # Graficar curva ROC
    plot(roc_clase,
         main = paste("ROC - Clase", clase),
         col = "blue",
         lwd = 2)
    
    # Agregar valor de AUC al gráfico
    text(x = 0.4, y = 0.2, labels = paste("AUC =", round(auc_valor,2)), col = "red", cex = 1.2)
    
    # También se puede imprimir por consola
    cat("AUC Clase", clase, ":", auc_valor, "\n")
    
  } else {
    cat("Clase", clase, "no tiene suficientes casos positivos/negativos.\n")
  }
}
dev.off()
