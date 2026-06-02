
# Contenido ----
# 1. One wey MANOVA 
# 2. Two wey MANOVA 
# 3. perMANOVA (no paramétrico)


# 1. One wey MANOVA ---- 

# Análisis multivariado de la varianza (MANOVA) 

### **1. Simulación de Datos**

# Primero, simulamos algunos datos para el ejemplo:
  
  
# Instalar y cargar paquetes necesarios
if (!requireNamespace("stats", quietly = TRUE)) {
  install.packages("stats")
}
library(stats)

# Simular datos
set.seed(123)
grupo <- factor(rep(1:3, each = 20))
y1 <- c(rnorm(20, mean = 5, sd = 2), 
        rnorm(20, mean = 7, sd = 2), 
        rnorm(20, mean = 6, sd = 2))
y2 <- c(rnorm(20, mean = 10, sd = 2), 
        rnorm(20, mean = 12, sd = 2), 
        rnorm(20, mean = 11, sd = 2))

# Crear un data frame con los datos
datos <- data.frame(grupo, y1, y2)

# Mostrar las primeras filas del data frame
head(datos)


### **2. Realizar el MANOVA**

# Usaremos la función `manova()` para realizar el análisis:
  
  
# Ajustar el modelo MANOVA
modelo <- manova(cbind(y1, y2) ~ grupo, data = datos)

# Mostrar los resultados del MANOVA
summary(modelo)


### **3. Interpretación de Resultados**

# El resumen del modelo MANOVA proporcionará los valores de las pruebas
# estadísticas para evaluar si existen diferencias significativas en las 
# variables de respuesta (y1 y y2) en función del grupo.

### **4. Pruebas Post-Hoc (opcional)**

# Si se encuentran diferencias significativas, es posible realizar 
# pruebas post-hoc para identificar cuáles grupos difieren entre sí.


# Pruebas post-hoc usando el paquete heplots
if (!requireNamespace("heplots", quietly = TRUE)) {
  install.packages("heplots")
}
library(heplots)

# Prueba post-hoc para y1
pairwise.t.test(datos$y1, datos$grupo, p.adjust.method = "bonferroni")

# Prueba post-hoc para y2
pairwise.t.test(datos$y2, datos$grupo, p.adjust.method = "bonferroni")


# Este ejemplo te muestra cómo realizar un MANOVA de un factor en R, incluyendo
# la simulación de datos, el ajuste del modelo y la interpretación 
# de los resultados. 😊

# 1.1. One wey MANOVA ----

# Vamos a realizar un análisis multivariado de la varianza (MANOVA) de una vía 
# con cuatro variables de respuesta en el contexto de ciencias de los alimentos. 

# Supongamos que estamos evaluando el efecto de tres diferentes tratamientos 
# de almacenamiento (frío, ambiente, calor) sobre cuatro variables de calidad 
# de un producto alimenticio: pH, textura, color y sabor.

### **1. Simulación de Datos**

# Primero, simulamos los datos para los tratamientos y 
# las variables de respuesta.


# Instalar y cargar paquetes necesarios
if (!requireNamespace("stats", quietly = TRUE)) {
  install.packages("stats")
}
library(stats)

# Simular datos
set.seed(123)
n <- 30  # Número de observaciones por grupo
tratamiento <- factor(rep(c("Frío", "Ambiente", "Calor"), each = n))
pH <- c(rnorm(n, mean = 5, sd = 0.5), 
        rnorm(n, mean = 6, sd = 0.5), 
        rnorm(n, mean = 7, sd = 0.5))
textura <- c(rnorm(n, mean = 8, sd = 1), 
             rnorm(n, mean = 6, sd = 1), 
             rnorm(n, mean = 4, sd = 1))
color <- c(rnorm(n, mean = 3, sd = 0.5), 
           rnorm(n, mean = 4, sd = 0.5), 
           rnorm(n, mean = 5, sd = 0.5))
sabor <- c(rnorm(n, mean = 9, sd = 1), 
           rnorm(n, mean = 7, sd = 1), 
           rnorm(n, mean = 5, sd = 1))

# Crear un data frame con los datos
datos <- data.frame(Tratamiento = tratamiento, pH = pH, Textura = textura, Color = color, Sabor = sabor)

# Mostrar las primeras filas del data frame
head(datos)


### **2. Realizar el MANOVA**

# Usaremos la función `manova()` para realizar el análisis de varianza multivariado:
  
  
# Ajustar el modelo MANOVA
modelo <- manova(cbind(pH, Textura, Color, Sabor) ~ Tratamiento, data = datos)

# Mostrar los resultados del MANOVA
summary(modelo)



### **3. Interpretación de Resultados**

# El resumen del modelo MANOVA proporcionará los valores de las pruebas
# estadísticas para evaluar si existen diferencias significativas en las 
# variables de respuesta (pH, textura, color, y sabor) en función del 
# tratamiento de almacenamiento.

### **4. Pruebas Post-Hoc (opcional)**

# Si se encuentran diferencias significativas, es posible realizar 
# pruebas post-hoc para identificar qué tratamientos difieren entre sí 
# en cada variable de respuesta.


# Pruebas post-hoc para pH
pairwise.t.test(datos$pH, datos$Tratamiento, p.adjust.method = "bonferroni")

# Pruebas post-hoc para Textura
pairwise.t.test(datos$Textura, datos$Tratamiento, p.adjust.method = "bonferroni")

# Pruebas post-hoc para Color
pairwise.t.test(datos$Color, datos$Tratamiento, p.adjust.method = "bonferroni")

# Pruebas post-hoc para Sabor
pairwise.t.test(datos$Sabor, datos$Tratamiento, p.adjust.method = "bonferroni")


### **Interpretación de Resultados**
# 
# - **MANOVA:** Permite evaluar si el conjunto de variables de respuesta 
# difiere significativamente entre los tratamientos.
# - **Pruebas Post-Hoc:** Identifican específicamente entre qué tratamientos 
# existen diferencias para cada variable de respuesta.
# 
# Este ejemplo te muestra cómo realizar un MANOVA de una vía con cuatro 
# variables de respuesta en R, incluyendo la simulación de datos, el ajuste
# del modelo y la interpretación de los resultados. 

# BiplotPCA ----


### **3. Realizar el PCA**

# Realizamos el análisis de componentes principales (PCA) y preparamos 
# los datos para el biplot.


# Instalar y cargar los paquetes necesarios
if (!requireNamespace("FactoMineR", quietly = TRUE)) {
  install.packages("FactoMineR")
}
if (!requireNamespace("factoextra", quietly = TRUE)) {
  install.packages("factoextra")
}
library(FactoMineR)
library(factoextra)

# Realizar el PCA
pca <- PCA(datos[, c("pH", "Textura", "Color", "Sabor")], scale.unit = TRUE, graph = FALSE)

# Resumen del PCA
summary(pca)


### **4. Crear un Biplot con `ggplot2`**

# Usamos `factoextra` y `ggplot2` para crear el biplot.

Vamos a crear un biplot PCA y agrupar las observaciones por tratamiento para facilitar la interpretación de los resultados.

### **1. Realizar el PCA**

Realizamos el análisis de componentes principales (PCA).


# Instalar y cargar los paquetes necesarios
if (!requireNamespace("FactoMineR", quietly = TRUE)) {
  install.packages("FactoMineR")
}
if (!requireNamespace("factoextra", quietly = TRUE)) {
  install.packages("factoextra")
}
library(FactoMineR)
library(factoextra)

# Realizar el PCA
pca <- PCA(datos[, c("pH", "Textura", "Color", "Sabor")], scale.unit = TRUE, graph = FALSE)

# Resumen del PCA
summary(pca)


### **2. Crear un Biplot con `factoextra` agrupado por tratamiento**

# Usamos `factoextra` para crear el biplot y agrupar las observaciones por tratamiento.


# Crear un biplot con factoextra agrupado por tratamiento

fviz_pca_biplot(pca, 
                geom.ind = "point", 
                pointshape = 21, 
                pointsize = 2.5, 
                fill.ind = datos$Tratamiento, 
                col.ind = "black", 
                palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                addEllipses = TRUE, 
                label = "var", 
                col.var = "black",
                repel = TRUE,
                legend.title = "Tratamiento") +
  labs(title = "Biplot del PCA Agrupado por Tratamiento",
       x = "Componente Principal 1",
       y = "Componente Principal 2")


### **Interpretación de Resultados**
# 
# - **Puntos de Observación:** Cada punto representa una observación, coloreada
# según el tratamiento.
# - **Ellipses:** Las ellipses alrededor de los grupos muestran la dispersión 
# de cada tratamiento.
# - **Variables:** Las flechas indican las variables originales 
# (pH, textura, color, sabor) y su contribución a los componentes principales.

# Esta visualización te permite ver cómo se agrupan las observaciones según 
# los tratamientos y cómo cada variable contribuye a la separación de los 
# grupos. 

### **Otras Visualizaciones Alternativas**

# Además del biplot, aquí hay otras visualizaciones que pueden ser útiles:
  
  #### Gráficos de Caja y Bigote por Tratamiento
  
  
library(ggplot2)

# Gráfico de caja para pH
ggplot(datos, aes(x = Tratamiento, y = pH, color = Tratamiento)) +
  geom_boxplot() +
  labs(title = "Distribución del pH por Tratamiento",
       x = "Tratamiento",
       y = "pH") +
  theme_minimal()

# Gráfico de caja para Textura
ggplot(datos, aes(x = Tratamiento, y = Textura, color = Tratamiento)) +
  geom_boxplot() +
  labs(title = "Distribución de la Textura por Tratamiento",
       x = "Tratamiento",
       y = "Textura") +
  theme_minimal()

# Gráfico de caja para Color
ggplot(datos, aes(x = Tratamiento, y = Color, color = Tratamiento)) +
  geom_boxplot() +
  labs(title = "Distribución del Color por Tratamiento",
       x = "Tratamiento",
       y = "Color") +
  theme_minimal()

# Gráfico de caja para Sabor
ggplot(datos, aes(x = Tratamiento, y = Sabor, color = Tratamiento)) +
  geom_violin() +
  geom_jitter(height = 0, width = 0.1) +
  labs(title = "Distribución del Sabor por Tratamiento",
       x = "Tratamiento",
       y = "Sabor") +
  theme_minimal()

?geom_violin

# Gráfico de caja para Sabor
ggplot(datos, aes(x = Tratamiento, y = Sabor, color = Tratamiento)) +
  geom_violin() +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  labs(title = "Distribución del Sabor por Tratamiento",
       x = "Tratamiento",
       y = "Sabor") +
  theme_minimal()

# 2. Two wey MANOVA ----

# Vamos a realizar un ejemplo de MANOVA de dos vías 
# en el contexto de ciencias de los alimentos. 

# Supongamos que estamos evaluando el efecto de dos factores: 
# tipo de tratamiento y tipo de envase, sobre cuatro variables 
# de respuesta de calidad de un producto alimenticio: pH, textura, 
# color y sabor.

### **1. Simulación de Datos**

# Primero, simulamos los datos.


# Instalar y cargar los paquetes necesarios
if (!requireNamespace("stats", quietly = TRUE)) {
  install.packages("stats")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}
if (!requireNamespace("heplots", quietly = TRUE)) {
  install.packages("heplots")
}

library(stats)
library(ggplot2)
library(car)
library(heplots)

# Simular datos
set.seed(123)
n <- 30  # Número de observaciones por grupo
tratamiento <- factor(rep(c("Frío", "Ambiente", "Calor"), each = n * 2))
envase <- factor(rep(c("Vidrio", "Plástico"), times = n * 3))
pH <- c(rnorm(n, mean = 5, sd = 0.5), rnorm(n, mean = 5.5, sd = 0.5),
        rnorm(n, mean = 6, sd = 0.5), rnorm(n, mean = 6.5, sd = 0.5),
        rnorm(n, mean = 7, sd = 0.5), rnorm(n, mean = 7.5, sd = 0.5))
textura <- c(rnorm(n, mean = 8, sd = 1), rnorm(n, mean = 7.5, sd = 1),
             rnorm(n, mean = 6, sd = 1), rnorm(n, mean = 5.5, sd = 1),
             rnorm(n, mean = 4, sd = 1), rnorm(n, mean = 3.5, sd = 1))
color <- c(rnorm(n, mean = 3, sd = 0.5), rnorm(n, mean = 3.5, sd = 0.5),
           rnorm(n, mean = 4, sd = 0.5), rnorm(n, mean = 4.5, sd = 0.5),
           rnorm(n, mean = 5, sd = 0.5), rnorm(n, mean = 5.5, sd = 0.5))
sabor <- c(rnorm(n, mean = 9, sd = 1), rnorm(n, mean = 8.5, sd = 1),
           rnorm(n, mean = 7, sd = 1), rnorm(n, mean = 6.5, sd = 1),
           rnorm(n, mean = 5, sd = 1), rnorm(n, mean = 4.5, sd = 1))

# Crear un data frame con los datos
datos <- data.frame(Tratamiento = tratamiento, Envase = envase, pH = pH, Textura = textura, Color = color, Sabor = sabor)

# Mostrar las primeras filas del data frame
head(datos)


### **2. Realizar el MANOVA**

# Ajustamos el modelo MANOVA con los datos simulados.


# Ajustar el modelo MANOVA
modelo <- manova(cbind(pH, Textura, Color, Sabor) ~ Tratamiento * Envase, data = datos)

# Mostrar los resultados del MANOVA
summary(modelo)
summary.aov(modelo)


### **3. Evaluación de Supuestos**

#### **Supuesto de Normalidad Multivariada**

# Podemos evaluar la normalidad multivariada usando 
# el test de Shapiro-Wilk para cada variable.


# Evaluación de la normalidad para cada variable
shapiro.test(datos$pH)
shapiro.test(datos$Textura)
shapiro.test(datos$Color)
shapiro.test(datos$Sabor)


#### **Homogeneidad de Covarianza**

# Podemos usar la prueba de Box para evaluar la homogeneidad de 
# las matrices de covarianza.


# Prueba de Box para homogeneidad de covarianza
boxM(cbind(pH, Textura, Color, Sabor) ~ Tratamiento * Envase, data = datos)

?boxM

### **4. Visualización de Resultados**

#### **Gráficos de Caja y Bigote por Factor**


# Gráfico de caja para pH
ggplot(datos, aes(x = Tratamiento, y = pH, color = Envase)) +
  geom_boxplot() +
  labs(title = "Distribución del pH por Tratamiento y Envase",
       x = "Tratamiento",
       y = "pH") +
  theme_minimal()

# Gráfico de caja para Textura
ggplot(datos, aes(x = Tratamiento, y = Textura, color = Envase)) +
  geom_boxplot() +
  labs(title = "Distribución de la Textura por Tratamiento y Envase",
       x = "Tratamiento",
       y = "Textura") +
  theme_minimal()

# Gráfico de caja para Color
ggplot(datos, aes(x = Tratamiento, y = Color, color = Envase)) +
  geom_boxplot() +
  labs(title = "Distribución del Color por Tratamiento y Envase",
       x = "Tratamiento",
       y = "Color") +
  theme_minimal()

# Gráfico de caja para Sabor
ggplot(datos, aes(x = Tratamiento, y = Sabor, color = Envase)) +
  geom_boxplot() +
  labs(title = "Distribución del Sabor por Tratamiento y Envase",
       x = "Tratamiento",
       y = "Sabor") +
  theme_minimal()


### **Interpretación de Resultados**

# - **MANOVA:** Permite evaluar si el conjunto de variables de 
# respuesta difiere significativamente entre los tratamientos y 
# envases, así como sus interacciones.
# - **Evaluación de Supuestos:** Asegura que los datos cumplen con 
# los supuestos necesarios para realizar el MANOVA.
# - **Visualización:** Facilita la interpretación de los efectos de 
# los tratamientos y envases en cada variable de respuesta.
# 
# Este ejemplo muestra cómo realizar un MANOVA de dos vías, 
# evaluar los supuestos necesarios y visualizar los resultados en
# el contexto de ciencias de los alimentos. 

# 3. perMANOVA (no paramétrico) ----

# Vamos a realizar un MANOVA basado en permutaciones, que es una alternativa
# no paramétrica útil cuando no se cumplen los supuestos tradicionales de MANOVA, 
# como la normalidad multivariada.

### **1. Simulación de Datos**

# Primero, simulamos los datos como en el ejemplo anterior.


# Instalar y cargar los paquetes necesarios
if (!requireNamespace("vegan", quietly = TRUE)) {
  install.packages("vegan")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}
if (!requireNamespace("heplots", quietly = TRUE)) {
  install.packages("heplots")
}

library(vegan)
library(ggplot2)
library(car)
library(heplots)

# Simular datos
#set.seed(123)
n <- 30  # Número de observaciones por grupo
tratamiento <- factor(rep(c("Frío", "Ambiente", "Calor"), each = n * 2))
envase <- factor(rep(c("Vidrio", "Plástico"), times = n * 3))
pH <- c(rnorm(n, mean = 5, sd = 0.5), rnorm(n, mean = 5.5, sd = 0.5),
        rnorm(n, mean = 6, sd = 0.5), rnorm(n, mean = 6.5, sd = 0.5),
        rnorm(n, mean = 7, sd = 0.5), rnorm(n, mean = 7.5, sd = 0.5))
textura <- c(rnorm(n, mean = 8, sd = 1), rnorm(n, mean = 7.5, sd = 1),
             rnorm(n, mean = 6, sd = 1), rnorm(n, mean = 5.5, sd = 1),
             rnorm(n, mean = 4, sd = 1), rnorm(n, mean = 3.5, sd = 1))
color <- c(rnorm(n, mean = 3, sd = 0.5), rnorm(n, mean = 3.5, sd = 0.5),
           rnorm(n, mean = 4, sd = 0.5), rnorm(n, mean = 4.5, sd = 0.5),
           rnorm(n, mean = 5, sd = 0.5), rnorm(n, mean = 5.5, sd = 0.5))
sabor <- c(rnorm(n, mean = 9, sd = 1), rnorm(n, mean = 8.5, sd = 1),
           rnorm(n, mean = 7, sd = 1), rnorm(n, mean = 6.5, sd = 1),
           rnorm(n, mean = 5, sd = 1), rnorm(n, mean = 4.5, sd = 1))

# Crear un data frame con los datos
datos <- data.frame(Tratamiento = tratamiento, Envase = envase, pH = pH, Textura = textura, Color = color, Sabor = sabor)

# Mostrar las primeras filas del data frame
head(datos)


### **2. Realizar el MANOVA basado en Permutaciones**

# Usaremos la función `adonis2()` del paquete `vegan` para realizar el MANOVA
# basado en permutaciones.


# Ajustar el modelo MANOVA basado en permutaciones



manova_perm <-
  adonis2(
    cbind(datos$pH, datos$Textura, datos$Color, datos$Sabor) ~ Tratamiento * Envase,
    data = datos,
    permutations = 999
  )

# Mostrar los resultados del MANOVA basado en permutaciones
print(manova_perm)


### **3. Evaluación de Supuestos**

#### **Homogeneidad de Covarianza**
# Podemos usar la prueba de Box para evaluar la homogeneidad de las matrices 
# de covarianza.


# Prueba de Box para homogeneidad de covarianza
boxM(cbind(pH, Textura, Color, Sabor) ~ Tratamiento * Envase, data = datos)


### **4. Visualización de Resultados**

#### **Gráficos de Caja y Bigote por Factor**


# Gráfico de caja para pH
ggplot(datos, aes(x = Tratamiento, y = pH, color = Envase)) +
  geom_boxplot() +
  labs(title = "Distribución del pH por Tratamiento y Envase",
       x = "Tratamiento",
       y = "pH") +
  theme_minimal()

# Gráfico de caja para Textura
ggplot(datos, aes(x = Tratamiento, y = Textura, color = Envase)) +
  geom_boxplot() +
  labs(title = "Distribución de la Textura por Tratamiento y Envase",
       x = "Tratamiento",
       y = "Textura") +
  theme_minimal()

# Gráfico de caja para Color
ggplot(datos, aes(x = Tratamiento, y = Color, color = Envase)) +
  geom_boxplot() +
  labs(title = "Distribución del Color por Tratamiento y Envase",
       x = "Tratamiento",
       y = "Color") +
  theme_minimal()

# Gráfico de caja para Sabor
ggplot(datos, aes(x = Tratamiento, y = Sabor, color = Envase)) +
  geom_boxplot() +
  labs(title = "Distribución del Sabor por Tratamiento y Envase",
       x = "Tratamiento",
       y = "Sabor") +
  theme_minimal()


### **Interpretación de Resultados**

# - **MANOVA basado en Permutaciones:** Esta técnica no paramétrica evalúa si
# el conjunto de variables de respuesta difiere significativamente entre los 
# tratamientos y envases, así como sus interacciones, mediante permutaciones.
# - **Evaluación de Supuestos:** La prueba de Box asegura la homogeneidad de 
# las matrices de covarianza.
# - **Visualización:** Facilita la interpretación de los efectos de los 
# tratamientos y envases en cada variable de respuesta.
# 
# Este ejemplo muestra cómo realizar un MANOVA de dos vías basado en 
# permutaciones, evaluar los supuestos necesarios y visualizar los resultados
# en el contexto de ciencias de los alimentos. 

# Visualizaciónes multivariadas ----

#  Vamos a realizar visualizaciones usando PCA (análisis de componentes 
# principales) y PCoA (análisis de coordenadas principales) tanto
# métrico como no métrico, y agruparemos las observaciones por 
# factores de tratamiento. También explicaré las diferencias entre 
# estos enfoques.

### **1. Simulación de Datos**

# Usamos los mismos datos simulados en el ejemplo anterior.


# Instalar y cargar los paquetes necesarios
if (!requireNamespace("vegan", quietly = TRUE)) {
  install.packages("vegan")
}
if (!requireNamespace("FactoMineR", quietly = TRUE)) {
  install.packages("FactoMineR")
}
if (!requireNamespace("factoextra", quietly = TRUE)) {
  install.packages("factoextra")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(vegan)
library(FactoMineR)
library(factoextra)
library(ggplot2)

# Simular datos
set.seed(123)
n <- 30  # Número de observaciones por grupo
tratamiento <- factor(rep(c("Frío", "Ambiente", "Calor"), each = n * 2))
envase <- factor(rep(c("Vidrio", "Plástico"), times = n * 3))
pH <- c(rnorm(n, mean = 5, sd = 0.5), rnorm(n, mean = 5.5, sd = 0.5),
        rnorm(n, mean = 6, sd = 0.5), rnorm(n, mean = 6.5, sd = 0.5),
        rnorm(n, mean = 7, sd = 0.5), rnorm(n, mean = 7.5, sd = 0.5))
textura <- c(rnorm(n, mean = 8, sd = 1), rnorm(n, mean = 7.5, sd = 1),
             rnorm(n, mean = 6, sd = 1), rnorm(n, mean = 5.5, sd = 1),
             rnorm(n, mean = 4, sd = 1), rnorm(n, mean = 3.5, sd = 1))
color <- c(rnorm(n, mean = 3, sd = 0.5), rnorm(n, mean = 3.5, sd = 0.5),
           rnorm(n, mean = 4, sd = 0.5), rnorm(n, mean = 4.5, sd = 0.5),
           rnorm(n, mean = 5, sd = 0.5), rnorm(n, mean = 5.5, sd = 0.5))
sabor <- c(rnorm(n, mean = 9, sd = 1), rnorm(n, mean = 8.5, sd = 1),
           rnorm(n, mean = 7, sd = 1), rnorm(n, mean = 6.5, sd = 1),
           rnorm(n, mean = 5, sd = 1), rnorm(n, mean = 4.5, sd = 1))

# Crear un data frame con los datos
datos <- data.frame(Tratamiento = tratamiento, Envase = envase, pH = pH, Textura = textura, Color = color, Sabor = sabor)


### **2. Visualización con PCA**

# Realizamos el PCA y creamos un biplot agrupando por tratamiento.


# Realizar el PCA
pca <- PCA(datos[, c("pH", "Textura", "Color", "Sabor")], scale.unit = TRUE, graph = FALSE)

# Crear un biplot con factoextra agrupado por tratamiento
fviz_pca_biplot(pca, 
                geom.ind = "point", 
                pointshape = 21, 
                pointsize = 2.5, 
                fill.ind = datos$Tratamiento, 
                col.ind = "black", 
                palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                addEllipses = TRUE, 
                label = "var", 
                col.var = "black",
                repel = TRUE,
                legend.title = "Tratamiento") +
  labs(title = "Biplot del PCA Agrupado por Tratamiento",
       x = "Componente Principal 1",
       y = "Componente Principal 2")


### **3. Visualización con PCoA (métrico)**

# Realizamos el PCoA métrico y creamos la visualización.


# Calcular la matriz de distancias euclídeas
distancias <- dist(datos[, c("pH", "Textura", "Color", "Sabor")])

# Realizar el PCoA
pcoa <- cmdscale(distancias, k = 2, eig = TRUE)

# Crear un data frame para ggplot2
pcoa_df <- as.data.frame(pcoa$points)
pcoa_df$Tratamiento <- datos$Tratamiento

# Crear el gráfico de PCoA
ggplot(pcoa_df, aes(x = V1, y = V2, color = Tratamiento)) +
  geom_point(size = 3) +
  labs(title = "PCoA Métrico Agrupado por Tratamiento",
       x = "Dimensión 1",
       y = "Dimensión 2") +
  theme_minimal()


### **4. Visualización con PCoA (no métrico)**

# Realizamos el PCoA no métrico y creamos la visualización.


# Realizar el PCoA no métrico
nmds <- metaMDS(datos[, c("pH", "Textura", "Color", "Sabor")], distance = "bray", k = 2, trymax = 100)

# Crear un data frame para ggplot2
nmds_df <- as.data.frame(nmds$points)
nmds_df$Tratamiento <- datos$Tratamiento

# Crear el gráfico de NMDS
ggplot(nmds_df, aes(x = MDS1, y = MDS2, color = Tratamiento)) +
  geom_point(size = 3) +
  labs(title = "PCoA No Métrico Agrupado por Tratamiento",
       x = "Dimensión 1",
       y = "Dimensión 2") +
  theme_minimal()


### **Diferencias entre los Enfoques**
# 
# 1. **PCA (Análisis de Componentes Principales):**
# - **Tipo de Análisis:** Análisis de reducción de dimensiones basado 
# en la varianza.
# - **Datos:** Requiere datos continuos y escalados.
# - **Aplicación:** Utilizado para identificar las combinaciones lineales de 
# las variables originales que explican la máxima varianza.
# - **Visualización:** Biplot muestra las variables originales y las 
# observaciones en el espacio de los componentes principales.
# 
# 2. **PCoA (Análisis de Coordenadas Principales - Métrico):**
#   - **Tipo de Análisis:** Análisis de reducción de dimensiones basado en 
# una matriz de distancias métricas.
# - **Datos:** Requiere una matriz de distancias métricas (e.g., distancia euclídea).
# - **Aplicación:** Utilizado para explorar y visualizar similitudes/diferencias 
# entre las observaciones.
# - **Visualización:** Gráfico de las observaciones en el espacio de las 
# coordenadas principales.
# 
# 3. **PCoA (Análisis de Coordenadas Principales - No Métrico / NMDS):**
#   - **Tipo de Análisis:** Análisis de reducción de dimensiones basado en 
# una matriz de distancias no métricas.
# - **Datos:** Puede utilizar diferentes medidas de distancia 
# (e.g., distancia de Bray-Curtis).
# - **Aplicación:** Mejor para datos ecológicos y biológicos donde las
# relaciones no son lineales.
# - **Visualización:** Gráfico de las observaciones en el espacio de 
# las dimensiones NMDS, con el objetivo de preservar las relaciones de orden.
# 
# Estos enfoques ofrecen diferentes perspectivas para la reducción dimensional
# y visualización de datos multivariados, permitiendo una mejor comprensión de
# la estructura y relaciones en los datos. 