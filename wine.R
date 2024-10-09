# Bibliotecas usadas:
library(class)
library(corrplot)
library(plotly)
library(tidyverse)
library(caret)
library(MLmetrics)
library(xgboost)
library(keras)
library(randomForest)
library(rpart)
library(pROC)
library(gridExtra)
library(pdp)
library(vip)
library(iml)
library(stats)
library(factoextra)
library(cluster)
library(kerastuneR)
library(e1071)
library(gbm)
library(naivebayes)
library(mclust)
library(ROCR)
library(bnlearn)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                        PREPROCESAMIENTO DE LOS DATOS
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Lectura de datos:
data <- read.csv("~/R/winequality-red.csv")
names(data)
head(data)
View(data)

# Revisión previa de los datos:
summary(data)
str(data)

# ------------------------------------------------------------------------------
#                         ESTUDIO DE CORRELACIONES
# ------------------------------------------------------------------------------

# Estudio de la correlación entre las variables:
correlations <- cor(data)
pairs(data)
corrplot(correlations,
         method = "color",
         type = "upper",
         tl.col = "black")

# Identificación de las mayores correlaciones:
num_vals <- 12
top_corr_values <- sort(correlations[upper.tri(correlations,diag = TRUE)],
                        decreasing = TRUE)[1:num_vals]

# Impresión de las variables más correlacionadas con la calidad del vino:
for (i in 1:num_vals) {
  max_value <- top_corr_values[i]
  max_position <- which(correlations == max_value,arr.ind = TRUE)
  
  row_index <- max_position[1]
  col_index <- max_position[2]
  
  cat("Valor de correlación:",max_value, "\n")
  cat("Variables correspondientes:",colnames(correlations)[row_index],"y",
                                    colnames(correlations)[col_index],"\n\n")
}

# Mayores correlaciones con respecto a la calidad del vino:
cor_with_quality <- correlations[,"quality"]
sorted_cor <- sort(cor_with_quality,decreasing = TRUE)

top_correlation_vars <- names(sorted_cor) 
print(top_correlation_vars)

# Cálculo de las correlaciones con la variable "quality"
correlations_with_quality <- cor(data$quality,data[-12])

# Imprimir las correlaciones en la terminal
cat("Correlaciones con la variable 'quality':\n")
for (i in 1:length(correlations_with_quality)) {
  cat("Variable:",names(data)[-12][i], "\n")
  cat("Correlación:", correlations_with_quality[i], "\n\n")
}

# ------------------------------------------------------------------------------
#                    DIVISIÓN DE CONJUNTOS DE DATOS
# ------------------------------------------------------------------------------

# Crear el vector objetivo y la clase binaria:
y <- data$quality
y <- ifelse(y<=5,0,1)

# Dividir los datos en conjuntos de entrenamiento y prueba:
set.seed(33)
split <- createDataPartition(y,
                             p = 0.7,
                             list = FALSE)
X_train <- data[split,-12]
X_test <- data[-split,-12]
y_train <- y[split]
y_test <- y[-split]

# Convertir y_test en factor:
y_test_factor <- factor(y_test,
                        levels = c(0,1))

# Convertir y_test_factor a un vector numérico:
y_test_numeric <- as.numeric(y_test_factor)-1

# Escalado de variables:
scaled_data <- as.data.frame(scale(data))
X_train_sc <- as.data.frame(scaled_data[split,-12])
X_test_sc <- as.data.frame(scaled_data[-split,-12])
y_train_sc <- as.data.frame(y[split])
y_test_sc <- as.data.frame(y[-split])



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                       REGRESIÓN LINEAL MULTIVARIABLE
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# División de datos en entrenamiento y prueba
set.seed(33)
train_idx <- sample(nrow(data),
                    0.7*nrow(data))
train_data <- data[train_idx,]
test_data <- data[-train_idx,]

# Modelo de regresión lineal multivariable
reg_l <- lm(quality ~ fixed.acidity +
                      citric.acid +
                      density +
                      free.sulfur.dioxide +
                      total.sulfur.dioxide +
                      alcohol +
                      chlorides +
                      sulphates,
            data = train_data)

# Resumen del modelo
summary(reg_l)

# Predicciones en el conjunto de prueba
predict_reg <- predict(reg_l,
                       newdata = test_data)

# ------------------------------------------------------------------------------
#                          EVALUACIÓN DEL MODELO
# ------------------------------------------------------------------------------

# Evaluación del modelo
mse_reg <- mean((test_data$quality-predict_reg)^2)
accuracy_reg <- 1-mse_reg/var(test_data$quality)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#               REGRESIÓN LINEAL MULTIVARIABLE (ESCALADAS)
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# División de datos en entrenamiento y prueba
set.seed(33)
train_idx_sc <- sample(nrow(scaled_data),
                    0.7*nrow(scaled_data))
train_data_sc <- scaled_data[train_idx_sc,]
test_data_sc <- scaled_data[-train_idx_sc,]

# Modelo de regresión lineal multivariable
reg_l_sc <- lm(quality ~ fixed.acidity +
              citric.acid +
              density +
              free.sulfur.dioxide +
              total.sulfur.dioxide +
              alcohol +
              chlorides +
              sulphates,
            data = train_data_sc)

# Resumen del modelo
summary(reg_l_sc)

# Predicciones en el conjunto de prueba
predict_reg_sc <- predict(reg_l_sc,
                       newdata = test_data_sc)

# ------------------------------------------------------------------------------
#                          EVALUACIÓN DEL MODELO
# ------------------------------------------------------------------------------

# Evaluación del modelo
mse_reg_sc <- mean((test_data_sc$quality-predict_reg_sc)^2)
accuracy_reg_sc <- 1-mse_reg_sc/var(test_data_sc$quality)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                             REGRESIÓN LOGÍSTICA
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Entrenar un modelo de regresión logística
model_logistic <- glm(y_train ~ .,
                      data = X_train,
                      family = binomial(link = "logit"))

# Realizar predicciones en los datos de prueba
logistic_predictions <- predict(model_logistic,
                                newdata = X_test,
                                type = "response")

# Convertir las predicciones en clases binarias
logistic_predictions <- ifelse(logistic_predictions>0.5,1,0)

# ------------------------------------------------------------------------------
#                          VISUALIZACIÓN DEL MODELO
# ------------------------------------------------------------------------------

# Coeficientes del modelo de regresión logística:
coeficientes <- coef(model_logistic)

# Representación gráfica de los datos predichos:
plot(logistic_predictions)



# ------------------------------------------------------------------------------
#                          EVALUACIÓN DEL MODELO
# ------------------------------------------------------------------------------

# Evaluar el rendimiento del modelo
accuracy_logistic <- sum(logistic_predictions == y_test)/length(y_test)
cat("Precisión del modelo de regresión logística:",
    accuracy_logistic,"\n")

# Obtener las probabilidades predichas por el modelo
logistic_probabilities <- predict(model_logistic,
                                  newdata = X_test,
                                  type = "response")

# Calcular el AUC y crear la curva ROC
roc_logistic <- roc(response = y_test,
                    predictor = logistic_probabilities)

# Imprimir el AUC score
cat("AUC score:", auc(roc_logistic), "\n")

# Crear un data frame para los valores de la curva ROC
roc_data_logistic <- data.frame(specificity = 1 - roc_logistic$specificities,
                                sensitivity = roc_logistic$sensitivities)

# Elaboración de la curva
roc_plot_log <- ggplot(data = roc_data_logistic,
                       aes(x = specificity,
                           y = sensitivity)) +
                geom_line() +
                geom_abline(intercept = 0,
                            slope = 1,
                            linetype = "dashed",
                            color = "blue") +
                labs(title = "Curva ROC - Modelo Regresión Logística",
                     x = "Especificidad",
                     y = "Sensibilidad") +
                theme_minimal() +
                geom_text(aes(label = paste("AUC = ",round(auc(roc_logistic),3))),
                          x = 0.75,
                          y = 0.25,
                          color = "black",
                          hjust = 0.5,
                          vjust = 0.5,
                          size = 4)
print(roc_plot_log)

# ------------------------------------------------------------------------------
#                            VALIDACIÓN CRUZADA
# ------------------------------------------------------------------------------

# Configurar validación cruzada
folds <- createFolds(y_train,
                     k = 100,
                     list = TRUE,
                     returnTrain = FALSE)
cv_auc <- numeric(length(folds))

# Entrenar y evaluar modelo con validación cruzada
for (i in 1:length(folds)) {
  train_indices <- unlist(folds[-i])
  test_indices <- folds[[i]]
  
  model <- glm(y_train[train_indices] ~ .,
               data = X_train[train_indices,],
               family = binomial(link = "logit"))
  
  prob_predictions <- predict(model,
                              newdata = X_train[test_indices,],
                              type = "response")
  roc_obj <- roc(response = y_train[test_indices],
                 predictor = prob_predictions)
  cv_auc[i] <- auc(roc_obj)
}

AUC_logistica_ajustada <- mean(cv_auc)

# Imprimir resultados
cat("AUC score:", auc(roc_logistic), "\n")
cat("AUC score (mean):",mean(cv_auc), "\n")
cat("AUC score (std):",sd(cv_auc), "\n")



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                                 DECISION TREE
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Entrenamiento del modelo
model_tree <- rpart(y_train ~ .,
                    data = X_train,
                    method = "class")

# Realizar predicciones en los datos de prueba:
tree_predictions <- predict(model_tree,
                            newdata = X_test,
                            type = "class")

# ------------------------------------------------------------------------------
#                          EVALUACIÓN DEL MODELO
# ------------------------------------------------------------------------------

# Calcular la precisión del modelo:
accuracy_tree <- sum(tree_predictions == y_test)/length(y_test)
print(paste("Precisión del modelo de Árbol de Decisión:",accuracy_tree))

# Matriz de confusión:
tree_pred_factor <- factor(tree_predictions,
                           levels = c(0,1))
cm_tree <- confusionMatrix(tree_pred_factor,y_test_factor)
cm_tree

tree_probabilities <- predict(model_tree,
                              newdata = X_test,
                              type = "prob")[,2]

# Calcular la curva ROC
roc_curve_tree <- roc(response = y_test_numeric, 
                      predictor = tree_probabilities)

# Crear un data frame para los valores de la curva ROC
roc_tree <- data.frame(specificity = 1-roc_curve_tree$specificities,
                       sensitivity = roc_curve_tree$sensitivities)

# Crear la gráfica de la curva ROC con ggplot2
roc_plot_tree <- ggplot(data = roc_tree,
                        aes(x = specificity,
                            y = sensitivity)) +
                 geom_line() +
                 geom_abline(intercept = 0,
                             slope = 1,
                             linetype = "dashed",
                             color = "blue") +
                 labs(title = "Curva ROC - Modelo DecisionTree",
                      x = "Especificidad",
                      y = "Sensibilidad") +
                 theme_minimal() +
                 geom_text(aes(label = paste("AUC =",round(auc(roc_curve_tree),3))),
                           x = 0.75,
                           y = 0.25,
                           color = "black",
                           hjust = 0.5,
                           vjust = 0.5,
                           size = 4)
print(roc_plot_tree)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                                 XGBOOST
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Entrenar el modelo XGBoost:
model_xgb <- xgboost(data = as.matrix(X_train),
                     label = y_train,
                     objective = "binary:logistic",
                     nrounds = 250,
                     max_depth = 6,
                     eta = 0.3,
                     nthread = -1)

# Realizar predicciones en los datos de prueba:
xgb_predictions <- predict(model_xgb,
                           newdata = as.matrix(X_test))

# Convertir las predicciones en clases binarias:
xgb_predictions <- ifelse(xgb_predictions>0.5,1,0)

# ------------------------------------------------------------------------------
#                          EVALUACIÓN DEL MODELO
# ------------------------------------------------------------------------------

# Evaluar el rendimiento del modelo:
accuracy_xgb <- sum(xgb_predictions == y_test)/length(y_test)

# Obtener puntuaciones de importancia y graficarlas:
importance_scores <- xgb.importance(feature_names = colnames(X_train),
                                    model = model_xgb)
xgb.plot.importance(importance_matrix = importance_scores)

# Matriz de confusión:
xgb_pred_factor <- factor(xgb_predictions,
                          levels = c(0,1))
cm_xgb <- confusionMatrix(xgb_pred_factor,y_test_factor)
cm_xgb

# Calcular las probabilidades predichas por el modelo XGBoost:
xgb_probabilities <- predict(model_xgb,
                             newdata = as.matrix(X_test),
                             type = "prob")

# Calcular la curva ROC
roc_curve_xgb <- roc(response = y_test_numeric, 
                     predictor = xgb_probabilities)

# Crear un data frame para los valores de la curva ROC
roc_xgb <- data.frame(specificity = 1-roc_curve_xgb$specificities,
                      sensitivity = roc_curve_xgb$sensitivities)

# Crear la gráfica de la curva ROC con ggplot2
roc_plot_xgb <- ggplot(data = roc_xgb,
                       aes(x = specificity,y = sensitivity)) +
                geom_line() +
                geom_abline(intercept = 0,
                            slope = 1,
                            linetype = "dashed",
                            color = "blue") +
                labs(title = "Curva ROC - Modelo XGBoost",
                     x = "Especificidad",
                     y = "Sensibilidad") +
                theme_minimal() +
                geom_text(aes(label = paste("AUC =",round(auc(roc_curve_xgb),3))),
                          x = 0.75,
                          y = 0.25,
                          color = "black",
                          hjust = 0.5,
                          vjust = 0.5,
                          size = 4)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                          RED NEURONAL ARTIFICIAL
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Creación de un modelo de red neuronal:
model <- keras_model_sequential() %>%
          layer_dense(units = 64,
                      activation = "relu",
                      input_shape = ncol(X_train)) %>%
            layer_dense(units = 32,
                        activation = "relu") %>%
              layer_dense(units = 1,
                          activation = "sigmoid")

# Compilación del modelo:
model_rn <- model %>% 
              compile(loss = "binary_crossentropy",
              optimizer = optimizer_adam(),
              metrics = c("accuracy"))

# Definición de un early stopping para la red neuronal:
early_stopping <- callback_early_stopping(patience = 50)

# Entrenamiento del modelo:
set.seed(33)
history <- model %>% 
            fit(x = as.matrix(X_train),
                y = y_train,
                validation_split = 0.30,            # porcentaje de datos de test
                epochs = 350,                       # iteraciones del modelo
                batch_size = 64,                    # número de lotes para entrenar el modelo
                callbacks = list(early_stopping),   # introducción del early stopping en el modelo
                verbose = 2)                        # mostrar toda la información de los epochs

# ------------------------------------------------------------------------------
#                          EVALUACIÓN DEL MODELO
# ------------------------------------------------------------------------------

# Evaluación del rendimiento del modelo:
scores <- model %>% 
            evaluate(x = as.matrix(X_test),
                     y = y_test,
                     verbose = 0)
accuracy_rna <- scores[[2]]
cat("Precisión del modelo de red neuronal:",accuracy_rna,"\n")

# Predicciones con este modelo:
rna_predictions <- predict(model_rn,as.matrix(X_test))
rna_predictions <- as.integer(rna_predictions>0.5)

# Matriz de confusión:
rna_pred_factor <- factor(rna_predictions,
                          levels = c(0,1))
cm_rna <- confusionMatrix(rna_pred_factor,y_test_factor)
cm_rna

# Calcular las probabilidades predichas por el modelo RNA:
rna_probabilities <- as.vector(predict(model_rn, as.matrix(X_test)))

# Calcular la curva ROC
roc_curve_rna <- roc(response = y_test_numeric, 
                     predictor = rna_probabilities)

# Crear un data frame para los valores de la curva ROC
roc_rna <- data.frame(specificity = 1-roc_curve_rna$specificities,
                      sensitivity = roc_curve_rna$sensitivities)

# Crear la gráfica de la curva ROC con ggplot2
roc_plot_rna <- ggplot(data = roc_rna,
                       aes(x = specificity,
                           y = sensitivity)) +
                geom_line() +
                geom_abline(intercept = 0,
                            slope = 1,
                            linetype = "dashed",
                            color = "blue") +
                labs(title = "Curva ROC - Modelo de RNA",
                     x = "Especificidad",
                     y = "Sensibilidad") +
                theme_minimal() +
                geom_text(aes(label = paste("AUC = ",round(auc(roc_curve_rna),3))),
                          x = 0.75,
                          y = 0.25,
                          color = "black",
                          hjust = 0.5,
                          vjust = 0.5,
                          size = 4)
print(roc_plot_rna)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                              RANDOM FOREST
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Definir la cuadrícula de hiperparámetros a explorar
param_grid <- expand.grid(mtry = c(2,3,4,5),
                          splitrule = c("gini","extratrees"),
                          min.node.size = c(1,5,10))

# Configurar la validación cruzada
ctrl <- trainControl(method = "cv",
                     number = 10)

# Realizar la búsqueda de hiperparámetros con validación cruzada
rf_grid <- train(x = X_train,
                 y = y_train,
                 method = "ranger",
                 trControl = ctrl,
                 tuneGrid = param_grid)

# Imprimir los resultados de la búsqueda de hiperparámetros
print(rf_grid)

# Mejor modelo con los hiperparámetros óptimos
best_rf_model <- rf_grid$bestTune
best_rf_model$method <- "ranger"
best_rf_model$predictor <- rf_grid$pred$library[1]
print(best_rf_model)

# ------------------------------------------------------------------------------
#                           VALIDACIÓN CRUZADA
# ------------------------------------------------------------------------------

# Realizar validación cruzada
rf_cv <- train(y = y_train,
               x = X_train,
               method = "rf",
               trControl = trainControl(method = "cv",number = 10))

# Imprimir los resultados de la validación cruzada
print(rf_cv)

# ------------------------------------------------------------------------------
#                        ENTRENAMIENTO DEL MODELO
# ------------------------------------------------------------------------------

# Entrenar un modelo de Random Forest
model_rf <- randomForest(y_train ~ .,
                         data = X_train,
                         mtry = 2,
                         splitrule = "extratrees",
                         nodesize = 1)

# ------------------------------------------------------------------------------
#                          EVALUACIÓN DEL MODELO
# ------------------------------------------------------------------------------

# Predicciones con este modelo:
rf_predictions <- predict(model_rf,as.matrix(X_test))
rf_predictions <- as.integer(rf_predictions>0.5)

# Matriz de confusión:
rf_pred_factor <- factor(rf_predictions,
                         levels = c(0,1))
cm_rf <- confusionMatrix(rf_pred_factor,y_test_factor)
cm_rf

# Obtener las probabilidades predichas por el modelo
rf_probabilities <- as.numeric(predict(model_rf,
                                       newdata = X_test,
                                       type = "response"))

# Calcular el AUC
roc_rf <- roc(response = y_test,
              predictor = rf_probabilities)

# Imprimir el AUC score
cat("AUC score RandomForest:",auc(roc_rf),"\n")

# Calcular la curva ROC
roc_curve_rf <- roc(response = y_test,
                    predictor = rf_probabilities)

# Crear un data frame para los valores de la curva ROC
roc_rf_df <- data.frame(specificity = 1-roc_curve_rf$specificities,
                        sensitivity = roc_curve_rf$sensitivities)

# Crear la gráfica de la curva ROC con ggplot2
roc_plot_rf <- ggplot(data = roc_rf_df,
                      aes(x = specificity,y = sensitivity)) +
               geom_line() +
               geom_abline(intercept = 0,
                           slope = 1,
                           linetype = "dashed",
                           color = "blue") +
               labs(title = "Curva ROC - Modelo RandomForest",
                    x = "Especificidad",
                    y = "Sensibilidad") +
               theme_minimal() +
               geom_text(aes(label = paste("AUC = ",round(auc(roc_curve_rf),3))),
                         x = 0.75,
                         y = 0.25,
                         color = "black",
                         hjust = 0.5,
                         vjust = 0.5,
                         size = 4)

# ------------------------------------------------------------------------------
#                       AJUSTE DE UMBRAL DE PROBABILIDAD
# ------------------------------------------------------------------------------

#nuevo_umbral <- 0.6

# Convertir las predicciones ajustadas en un factor;
#rf_predictions_adjusted <- as.integer(rf_predictions>nuevo_umbral)
#rf_predictions_adjusted_factor <- factor(rf_predictions_adjusted,
#                                         levels = levels(y_test_factor))

# Calcular la matriz de confusión con el umbral ajustado
#cm_rf_adjusted <- confusionMatrix(rf_predictions_adjusted_factor,y_test_factor)
#cm_rf_adjusted

# ------------------------------------------------------------------------------
#                       ANÁLISIS DE INTERPRETABILIDAD
# ------------------------------------------------------------------------------

# Importancias de las características:
importance_rf <- importance(model_rf)

# Visualización de árboles individuales:
tree <- getTree(model_rf,
                k = 1)
plot(tree)

# Identificación de las variables predictoras:
predictor_names <- colnames(X_train)
print(predictor_names)

# Partial Difference Plots:
pdp_pH <- partial(model_rf,
                  pred.var = c("pH"))
pdp_alcohol <- partial(model_rf,
                       pred.var = c("alcohol"))
pdp_sulphates <- partial(model_rf,
                         pred.var = c("sulphates"))
pdp_total.sulfur.dioxide <- partial(model_rf,
                                    pred.var = c("total.sulfur.dioxide"))  # Gráfica aparte
pdp_fixed.acidity  <- partial(model_rf,
                       pred.var = c("fixed.acidity"))
pdp_density <- partial(model_rf,
                       pred.var = c("density"))
pdp_volatile.acidity <- partial(model_rf,
                                pred.var = c("volatile.acidity"))
pdp_citric.acid <- partial(model_rf,
                           pred.var = c("citric.acid"))
pdp_residual.sugar <- partial(model_rf,
                                     pred.var = c("residual.sugar"))
pdp_chlorides <- partial(model_rf,
                                pred.var = c("chlorides"))

plot(pdp_total.sulfur.dioxide)

# Superponer los gráficos en la misma figura
pdp_combined_plot <- ggplot() +
                     geom_line(data = pdp_pH,aes(x = pH,y = yhat,color = "pH")) +
                     geom_line(data = pdp_alcohol,aes(x = alcohol,y = yhat,color = "Alcohol")) +
                     geom_line(data = pdp_sulphates,aes(x = sulphates,y = yhat,color = "Sulphates")) +
                     geom_line(data = pdp_residual.sugar,aes(x = residual.sugar,y = yhat,color = "Residual sugar")) +
                     geom_line(data = pdp_density,aes(x = density,y = yhat,color = "Density")) +
                     geom_line(data = pdp_chlorides,aes(x = chlorides,y = yhat,color = "Chlorides")) +
                     geom_line(data = pdp_fixed.acidity,aes(x = fixed.acidity,y = yhat,color = "Fixed acidity")) +
                     geom_line(data = pdp_citric.acid,aes(x = citric.acid,y = yhat,color = "Citric acid")) +
                     geom_line(data = pdp_sulphates,aes(x = sulphates,y = yhat,color = "Sulphates"))+
                     labs(title = "Partial Dependence Plot",
                          x = "Variable Predictora",
                          y = "yhat",
                          color = "Variable") +
                     scale_color_manual(values = c("blue","red","purple","black","orange",
                                                   "cyan","pink","yellow","green"))
          
# Mostrar la figura combinada
print(pdp_combined_plot)

# Análisis de permutación:
perm_rf <- vip(model_rf,num_features = 11)
plot(perm_rf)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                                   KNN
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#                           OPTIMIZACIÓN DEL MODELO
# ------------------------------------------------------------------------------

# Definir la función para calcular la distancia de Mahalanobis
mahalanobis_distance <- function(X,center,cov_matrix) {
  diffs <- t(t(X) - center)
  inv_cov_matrix <- solve(cov_matrix)
  distances <- sqrt(rowSums(diffs %*% inv_cov_matrix*diffs))
  return(distances)
}

# Definir la función para entrenar y evaluar el modelo KNN con una métrica de distancia específica
set.seed(33)
train_and_evaluate_distance_knn <- function(X_train,y_train,X_test,y_test,k,distance_metric) {
  if (distance_metric == "mahalanobis") {
    cov_matrix <- cov(X_train)
    center <- colMeans(X_train)
    distances <- mahalanobis_distance(X_test,center,cov_matrix)
    nearest_indices <- order(distances)[1:k]
    y_pred <- y_train[nearest_indices]
  } else {
    y_pred <- knn(train = X_train,
                  test = X_test,
                  cl = y_train,
                  k = k,
                  prob = TRUE,
                  use.all = TRUE)
  }
  
  accuracy <- sum(y_pred == y_test)/length(y_test)
  return(accuracy)
}

# Definir las métricas de distancia a probar
distance_metrics <- c("euclidean",
                      "manhattan",
                      "minkowski",
                      "cosine",
                      "mahalanobis")

# Definir los valores de k a probar
k_values <- c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35)

# Crear un vector para almacenar las combinaciones de k y métrica junto con su precisión
combination_results <- vector("list", length = length(k_values) * length(distance_metrics))
names(combination_results) <- rep("", length(k_values) * length(distance_metrics))

# Índice para acceder al vector de combinaciones
index <- 1

# Evaluación del modelo KNN con diferentes combinaciones de k y métricas de distancia en los datos escalados
set.seed(33)
for (k in k_values) {
  for (metric in distance_metrics) {
    accuracy <- train_and_evaluate_distance_knn(X_train_sc, y_train, X_test_sc, y_test, k, metric)
    combination_name <- paste("k =", k, ", métrica =", metric)
    
    # Almacenar la combinación y su precisión en el vector
    combination_results[[index]] <- list(combination = combination_name, accuracy = accuracy)
    names(combination_results)[index] <- combination_name
    
    index <- index + 1
  }
}

# Ordenar el vector combination_results por precisión en orden descendente
sorted_combinations <- combination_results[order(sapply(combination_results,
                                                        function(x) x$accuracy),
                                                 decreasing = TRUE)]

# Obtener las combinaciones con las 5 mejores precisiones:
top_combinations <- sorted_combinations[1:5]

# Mostrar las combinaciones de k y métrica con las 5 mejores precisiones:
for (i in 1:length(top_combinations)) {
  cat(paste(top_combinations[[i]]$combination," - Precisión:",
            top_combinations[[i]]$accuracy,"\n"))
}

# ------------------------------------------------------------------------------
#                             CREACIÓN DEL MODELO
# ------------------------------------------------------------------------------

train_data <- cbind(X_train,y_train)
train_data$y_train <- factor(train_data$y_train,
                             levels = c(0, 1))
set.seed(33)
model_knn <- train(y_train ~ .,
                       method = "knn",
                       data = train_data,
                       tuneGrid = expand.grid(k = c(5,7,9,11,13,15,17,19,
                                                    21,23,25,27,29,31,33,35,
                                                    37,39,41,43,45,47,49)))
model_knn

# ------------------------------------------------------------------------------
#                           EVALUACIÓN DEL MODELO
# ------------------------------------------------------------------------------

# Matriz de confusión y parámetros estadísticos:
cm_knn <- confusionMatrix(predict(model_knn,newdata = X_test),
                          y_test_factor)
cm_knn

# Obtener las probabilidades predichas por el modelo knn
knn_probabilities <- as.numeric(predict(model_knn,
                                        newdata = X_test))

# Calcular la curva ROC
roc_knn <- roc(response = y_test,
               predictor = knn_probabilities)

# Crear un data frame para los valores de la curva ROC
roc_knn_df <- data.frame(specificity = 1 - roc_knn$specificities,
                         sensitivity = roc_knn$sensitivities)

# Crear la gráfica de la curva ROC con ggplot2
roc_plot_knn <- ggplot(data = roc_knn_df,
                       aes(x = specificity,y = sensitivity)) +
                geom_line() +
                geom_abline(intercept = 0,
                            slope = 1,
                            linetype = "dashed",
                            color = "blue") +
                labs(title = "Curva ROC - Modelo k-Nearest Neighbors",
                     x = "Especificidad",
                     y = "Sensibilidad") +
                theme_minimal() +
                geom_text(aes(label = paste("AUC = ",round(auc(roc_knn),3))),
                          x = 0.75,
                          y = 0.25,
                          color = "black",
                          hjust = 0.5,
                          vjust = 0.5,
                          size = 4)

# Mostrar la gráfica
print(roc_plot_knn)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                                   SVM
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Crear el modelo SVM:
set.seed(33)
model_svm <- svm(y_train ~ .,
                 data = X_train,
                 kernel = "radial")

# Predicciones usando el modelo:
svm_predictions <- predict(model_svm,X_test)
svm_predictions <- factor(svm_predictions,
                          levels = c(0,1))

# ------------------------------------------------------------------------------
#                            EVALUACIÓN DEL MODELO
# ------------------------------------------------------------------------------

# Matriz de confusión:
#cm_svm <- confusionMatrix(predict(model_svm,newdata = X_test),
#                          y_test_factor)
#cm_svm

# ------------------------------------------------------------------------------
#                       OPTIMIZACIÓN DE PARÁMETROS
# ------------------------------------------------------------------------------

# Define la cuadrícula de hiperparámetros
param_grid <- expand.grid(C = c(0.1,1,10,100,125,130,140,150),
                          sigma = c(0.01,0.02,0.03,0.04,0.05,0.1,1))

# Realiza la validación cruzada con diferentes valores de C y sigma:
svm_model <- train(y_train ~ .,
                   data = train_data, 
                   method = "svmRadial",
                   trControl = trainControl(method = "cv"),
                   tuneGrid = param_grid)

# Imprime los resultados de la validación cruzada:
print(svm_model$results)

# Encuentra los valores de C y sigma que resultaron en el mejor modelo:
best_C <- svm_model$bestTune$C
best_sigma <- svm_model$bestTune$sigma

cat("El mejor valor de C es",best_C,"y el mejor valor de sigma es",best_sigma,"\n")

# Entrenamiento del modelo mejorado:
model_svm <- svm(y_train ~ .,
                 data = train_data,
                 kernel = "radial",
                 cost = best_C,
                 gamma = 1/(2*best_sigma^2))

# ------------------------------------------------------------------------------
#                             CURVA ROC Y AUC
# ------------------------------------------------------------------------------

# Obtener las probabilidades predichas por el modelo SVM
svm_probabilities <- predict(model_svm,
                             newdata = X_test,
                             decision.values = TRUE)

# Calcular la curva ROC
roc_svm <- roc(response = y_test_factor,
               predictor = svm_probabilities)

# Crear un data frame para los valores de la curva ROC
roc_svm_df <- data.frame(specificity = 1-roc_svm$specificities,
                         sensitivity = roc_svm$sensitivities)

# Calcular el AUC
svm_auc <- auc(roc_svm)

# Gráfica de la curva:
roc_plot_svm <- ggplot(data = roc_svm_df,
                       aes(x = specificity,y = sensitivity)) +
                geom_line() +
                geom_abline(intercept = 0,
                            slope = 1,
                            linetype = "dashed",
                            color = "blue") +
                labs(title = paste("Curva ROC - Modelo SVM"),
                     x = "Especificidad",
                     y = "Sensibilidad") +
                theme_minimal() +
                geom_text(aes(label = paste("AUC = ",round(auc(roc_svm),3))),
                          x = 0.75,
                          y = 0.25,
                          color = "black",
                          hjust = 0.5,
                          vjust = 0.5,
                          size = 4)

# Mostrar la gráfica
print(roc_plot_svm)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                             MODELO GBM
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Definir la cuadrícula de hiperparámetros
param_grid <- expand.grid(
  n.trees = c(300,350,400,500),
  interaction.depth = c(3,4,5,6,7),
  shrinkage = c(0.2,0.22,0.25,0.26,0.27,0.28,0.29,0.3),
  n.minobsinnode = c(10,20,30)
)


# Realizar la validación cruzada con diferentes combinaciones de hiperparámetros
gbm_model <- train(
  y_train ~ ., 
  data = train_data, 
  method = "gbm",
  trControl = trainControl(method = "cv"),
  tuneGrid = param_grid
)

# Mostrar los resultados
print(gbm_model$results)
max_accuracy_row <- gbm_model$results[which.max(gbm_model$results$Accuracy),]
max_accuracy_row

# Definir los hiperparámetros
shrinkage <- 0.22
interaction_depth <- 6
n_minobsinnode <- 10
n_trees <- 300

# Crear el modelo GBM
model_gbm <- train(
  y_train ~ .,
  data = train_data,
  method = "gbm",
  trControl = trainControl(method = "cv"),
  tuneGrid = expand.grid(shrinkage = shrinkage,
                         interaction.depth = interaction_depth,
                         n.minobsinnode = n_minobsinnode,
                         n.trees = n_trees)
)

# ------------------------------------------------------------------------------
#                             CURVA ROC Y AUC
# ------------------------------------------------------------------------------

# Obtener las probabilidades predichas por el modelo SVM
gbm_probabilities <- predict(model_gbm,
                             newdata = X_test,
                             type = "prob")

roc_gbm <- roc(response = as.numeric(y_test_factor) - 1,
               predictor = gbm_probabilities[, "1"])

# Crear un data frame para los valores de la curva ROC
roc_gbm_df <- data.frame(specificity = 1-roc_gbm$specificities,
                         sensitivity = roc_gbm$sensitivities)

# Calcular el AUC
gbm_auc <- auc(roc_gbm)

# Gráfica de la curva:
roc_plot_gbm <- ggplot(data = roc_gbm_df,
                       aes(x = specificity,y = sensitivity)) +
  geom_line() +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed",
              color = "blue") +
  labs(title = paste("Curva ROC - Modelo GBM"),
       x = "Especificidad",
       y = "Sensibilidad") +
  theme_minimal() +
  geom_text(aes(label = paste("AUC = ",round(auc(roc_gbm),3))),
            x = 0.75,
            y = 0.25,
            color = "black",
            hjust = 0.5,
            vjust = 0.5,
            size = 4)

# Mostrar la gráfica
print(roc_plot_gbm)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                              NAIVE-BAYES
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Crear un modelo Naive Bayes
model_nb <- naiveBayes(y_train ~ .,
                       data = train_data,
                       laplace = 0.25,      # Parámetro de suavizado Laplace
                       type = "raw") 

# Realizar predicciones en el conjunto de prueba
predictions_nb <- predict(model_nb,X_test)

# Evaluar el rendimiento del modelo
cm_nb <- table(predictions_nb,y_test)
accuracy_nb <- sum(diag(cm_nb))/sum(cm_nb)

# Imprimir la matriz de confusión y la precisión
print(cm_nb)
cat("Precisión:",accuracy_nb)

# Obtener las probabilidades predichas por el modelo Naive-Bayes
nb_probabilities <- predict(model_nb,
                            newdata = X_test,
                            type = "raw")

roc_nb <- roc(response = as.numeric(y_test_factor)-1,
              predictor = nb_probabilities[,"1"])

# Crear un data frame para los valores de la curva ROC
roc_nb_df <- data.frame(specificity = 1-roc_nb$specificities,
                        sensitivity = roc_nb$sensitivities)

# Calcular el AUC
nb_auc <- auc(roc_nb)

# Gráfica de la curva ROC
roc_plot_nb <- ggplot(data = roc_nb_df,
                      aes(x = specificity,
                          y = sensitivity)) +
               geom_line() +
               geom_abline(intercept = 0,
                           slope = 1,
                           linetype = "dashed",
                           color = "blue") +
               labs(title = paste("Curva ROC - Modelo Naive-Bayes"),
                    x = "Especificidad",
                    y = "Sensibilidad") +
               theme_minimal() +
               geom_text(aes(label = paste("AUC = ",round(nb_auc,3))),
                         x = 0.75,
                         y = 0.25,
                         color = "black",
                         hjust = 0.5,
                         vjust = 0.5,
                         size = 4)

# Mostrar la gráfica
print(roc_plot_nb)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                       BAYESIAN NETWORK CLASSIFIER
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

#library(bnlearn)

# Crear el modelo de red bayesiana
#model_bnc <- bn("quality ~ .", data = cbind(X_train, y_train))

# Ajustar el modelo a los datos de entrenamiento
#fitted_model <- bn.fit(model_bnc, data = cbind(X_train,y_train))


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                         COMPARACIÓN DE MODELOS
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

grid.arrange(roc_plot_rf,roc_plot_xgb,roc_plot_gbm,
             roc_plot_svm,roc_plot_log,roc_plot_tree,
             roc_plot_rna,roc_plot_nb,roc_plot_knn,
             ncol = 3)

