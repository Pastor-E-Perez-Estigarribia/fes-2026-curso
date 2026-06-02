
# Contenido ----
# 0. Conceptos Fundamentales 
# 1. Analisis discriminante Lineal 
# 2. Análisis discriminante cuadrático 


# 0. Conceptos Fundamentales ----

# 1. **Análisis Discriminante Lineal (LDA):**
# - **Objetivo:** Clasificar observaciones en grupos predefinidos basado en 
# variables predictoras.
# - **Funcionamiento:** Encuentra una combinación lineal de las variables 
# predictoras que maximiza la separación entre las clases.
# - **Aplicación:** Utilizado para problemas de clasificación en diversas
# áreas, incluyendo la ciencia de los alimentos.
# 
# 2. **Matriz de Confusión:**
#   - **Descripción:** Tabla que muestra las predicciones del modelo contra 
# las clases verdaderas.
# - **Uso:** Evaluar la precisión de un modelo de clasificación.
# 
# 3. **Métricas de Precisión y Exactitud:**
# - **Precisión:** Proporción de predicciones correctas para una clase específica.
# - **Exactitud:** Proporción de todas las predicciones correctas.
# 
# 4. **Métricas de Rendimiento:**
# - **Sensibilidad (Recall):** Proporción de verdaderos positivos 
# correctamente identificados.
# - **Especificidad:** Proporción de verdaderos negativos correctamente 
# identificados.
# - **Valor Predictivo Positivo (Precisión):** Proporción de verdaderos 
# positivos sobre todos los positivos predichos.

# Metricas de rendimiento
browseURL("https://es.wikipedia.org/wiki/Curva_ROC")

# 1. Analisis discriminante ----

# Vamos a realizar un análisis discriminante en el contexto de la ciencia de 
# los alimentos. Supongamos que queremos clasificar diferentes tipos de vinos
# (Vino Rojo y Vino Blanco) basado en variables de calidad. Utilizaremos 
# análisis discriminante lineal (LDA), crearemos una matriz de confusión, 
# calcularemos métricas de precisión y exactitud, y visualizaremos los resultados.

### **1. Simulación de Datos**

# Simulamos los datos para nuestro análisis.


# Instalar y cargar los paquetes necesarios
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(MASS)
library(caret)
library(ggplot2)

# Simulación de datos
set.seed(123)
n <- 100  # Número de observaciones por grupo
tipo_vino <- factor(rep(c("Rojo", "Blanco"), each = n))
acidez <- c(rnorm(n, mean = 6, sd = 1), rnorm(n, mean = 3.5, sd = 1))
alcohol <- c(rnorm(n, mean = 12, sd = 1), rnorm(n, mean = 10, sd = 1))
sulfitos <- c(rnorm(n, mean = 50, sd = 10), rnorm(n, mean = 30, sd = 10))

# Crear data frame
datos <- data.frame(Tipo = tipo_vino, Acidez = acidez, Alcohol = alcohol, Sulfitos = sulfitos)

# Mostrar los primeros datos
head(datos)


### **2. Análisis Discriminante Lineal (LDA)**

# Ajustamos el modelo LDA y predecimos las clases.


# Ajustar el modelo LDA
modelo_lda <- lda(Tipo ~ Acidez + Alcohol + Sulfitos, data = datos)

# Predicción
predicciones <- predict(modelo_lda)


# Matriz de Confusión
conf_matrix <- table(datos$Tipo, predicciones$class)
print(conf_matrix)


### **3. Métricas de Precisión y Exactitud**

# Calculamos las métricas a partir de la matriz de confusión.


# Calcular las métricas de precisión y exactitud
precision <- conf_matrix[1,1] / sum(conf_matrix[,1])
exactitud <- sum(diag(conf_matrix)) / sum(conf_matrix)

cat("Precisión:", precision, "\n")
cat("Exactitud:", exactitud, "\n")


### **4. Métricas de Rendimiento**

# Calculamos otras métricas de rendimiento como sensibilidad, especificidad 
# y valor predictivo positivo.


# Calcular métricas de rendimiento
sensibilidad <- conf_matrix[1,1] / sum(conf_matrix[1,])
especificidad <- conf_matrix[2,2] / sum(conf_matrix[2,])
vpp <- conf_matrix[1,1] / sum(conf_matrix[,1])

cat("Sensibilidad:", sensibilidad, "\n")
cat("Especificidad:", especificidad, "\n")
cat("Valor Predictivo Positivo:", vpp, "\n")


### **5. Visualización de Resultados**

# Visualizamos los resultados del LDA.


# Extraer las puntuaciones discriminantes
datos$LD1 <- predicciones$x[,1]

# Crear gráfico LDA
ggplot(datos, aes(x = LD1, fill = Tipo)) +
  geom_histogram(binwidth = 0.5, position = "dodge", alpha = 0.7) +
  labs(title = "Distribución de las Puntuaciones Discriminantes (LDA)",
       x = "Puntuación Discriminante 1 (LD1)",
       y = "Frecuencia") +
  theme_minimal()

### **Interpretación de Resultados**

# El análisis discriminante lineal nos permite clasificar los tipos de vinos 
# basados en las variables de calidad, y las métricas de rendimiento nos
# proporcionan información sobre la efectividad del modelo. Las visualizaciones
# ayudan a comprender mejor cómo el modelo separa las clases.

# Este ejemplo ilustra cómo realizar un análisis discriminante con R, evaluar 
# su rendimiento y visualizar los resultados en el contexto de la ciencia 
# de los alimentos. 

# 2. Análisis discriminante cuadrático ----

# Vamos a realizar un análisis discriminante cuadrático (QDA) en el contexto 
# de la ciencia de los alimentos y crear visualizaciones avanzadas
# usando `ggplot2` y otras librerías. Supongamos que queremos clasificar
# diferentes tipos de vinos (Vino Rojo y Vino Blanco) basado 
# en variables de calidad.


### **Conceptos Fundamentales**
# 
# **Análisis Discriminante Cuadrático (QDA):**
# - **Objetivo:** Clasificar observaciones en grupos predefinidos basado en
# variables predictoras.
# - **Funcionamiento:** Similar al análisis discriminante lineal (LDA),
# pero permite que las matrices de covarianza sean diferentes para cada grupo.
# - **Aplicación:** Utilizado cuando las suposiciones de LDA (matrices de
# covarianza iguales entre grupos) no se cumplen.



### **1. Simulación de Datos**

# Primero, simulamos los datos para nuestro análisis.


# Instalar y cargar los paquetes necesarios
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(MASS)
library(caret)
library(ggplot2)

# Simulación de datos
set.seed(123)
n <- 100  # Número de observaciones por grupo
tipo_vino <- factor(rep(c("Rojo", "Blanco"), each = n))
acidez <- c(rnorm(n, mean = 6, sd = 1), rnorm(n, mean = 3.5, sd = 1))
alcohol <- c(rnorm(n, mean = 12, sd = 1), rnorm(n, mean = 10, sd = 1))
sulfitos <- c(rnorm(n, mean = 50, sd = 10), rnorm(n, mean = 30, sd = 10))

# Crear data frame
datos <- data.frame(Tipo = tipo_vino, Acidez = acidez, Alcohol = alcohol, Sulfitos = sulfitos)

# Mostrar los primeros datos
head(datos)


### **2. Análisis Discriminante Cuadrático (QDA)**

#Ajustamos el modelo QDA y predecimos las clases.


# Ajustar el modelo QDA
modelo_qda <- qda(Tipo ~ Acidez + Alcohol + Sulfitos, data = datos)

# Predicción
predicciones_qda <- predict(modelo_qda)

# Matriz de Confusión
conf_matrix_qda <- table(datos$Tipo, predicciones_qda$class)
print(conf_matrix_qda)


### **3. Métricas de Precisión y Exactitud**

# Calculamos las métricas a partir de la matriz de confusión.


# Calcular las métricas de precisión y exactitud
precision_qda <- conf_matrix_qda[1,1] / sum(conf_matrix_qda[,1])
exactitud_qda <- sum(diag(conf_matrix_qda)) / sum(conf_matrix_qda)

cat("Precisión (QDA):", precision_qda, "\n")
cat("Exactitud (QDA):", exactitud_qda, "\n")


### **4. Métricas de Rendimiento**

# Calculamos otras métricas de rendimiento como sensibilidad, especificidad
# y valor predictivo positivo.


# Calcular métricas de rendimiento
sensibilidad_qda <- conf_matrix_qda[1,1] / sum(conf_matrix_qda[1,])
especificidad_qda <- conf_matrix_qda[2,2] / sum(conf_matrix_qda[2,])
vpp_qda <- conf_matrix_qda[1,1] / sum(conf_matrix_qda[,1])

cat("Sensibilidad (QDA):", sensibilidad_qda, "\n")
cat("Especificidad (QDA):", especificidad_qda, "\n")
cat("Valor Predictivo Positivo (QDA):", vpp_qda, "\n")


### **5. Visualización de Resultados**

# Visualizamos los resultados del QDA utilizando `ggplot2`.

#### **Visualización de la Clasificación**


# Crear un data frame con las puntuaciones discriminantes
datos$LD1 <- predicciones_qda$posterior[,1]
datos$Predicción <- predicciones_qda$class

# Gráfico de puntos con clasificación
ggplot(datos, aes(x = Acidez, y = Alcohol, color = Predicción, shape = Tipo)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Clasificación del QDA de Vinos",
       x = "Acidez",
       y = "Alcohol") +
  theme_minimal() +
  scale_color_manual(values = c("Rojo" = "red", "Blanco" = "blue"))


#### **Visualización de las Probabilidades Posteriores**


# Gráfico de densidad de las probabilidades posteriores
ggplot(datos, aes(x = LD1, fill = Tipo)) +
  geom_density(alpha = 0.7) +
  labs(title = "Distribución de las Probabilidades Posteriores (QDA)",
       x = "Probabilidad Posterior (Vino Rojo)",
       y = "Densidad") +
  theme_minimal() +
  scale_fill_manual(values = c("Rojo" = "red", "Blanco" = "blue"))

### **Interpretación de Resultados**

# El análisis discriminante cuadrático nos permite clasificar los tipos de
# vinos basados en las variables de calidad, y las métricas de rendimiento 
# nos proporcionan información sobre la efectividad del modelo. 
# Las visualizaciones ayudan a comprender mejor cómo el modelo separa 
# las clases y distribuye las probabilidades posteriores.

# Este ejemplo ilustra cómo realizar un análisis discriminante
# cuadrático con R, evaluar su rendimiento y visualizar los resultados en 
# el contexto de la ciencia de los alimentos. 