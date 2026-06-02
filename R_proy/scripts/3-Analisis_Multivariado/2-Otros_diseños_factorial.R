
# Contenido ----
#A. Planes jerárquicos
#B. Planes jérarquicos puros y factorial jerárquico 
#C. Diseño para sistemas continuos (EVOP)
#D. Diseño de mezclas y métodos de optimización 


#A. Planes jerárquicos ----


# análisis de varianza para un diseño jerárquico

### **Ejemplo: Efecto de diferentes tratamientos en la calidad de un 
# producto alimenticio, considerando efectos jerárquicos (anidados)**

# Imaginemos que estamos evaluando la calidad de un producto alimenticio 
# tratado con diferentes métodos de procesamiento (nivel superior). 
# Además, cada método de procesamiento se aplica en diferentes lotes 
# (nivel inferior).

### **1. Simular datos jerárquicos**

# Primero, simularemos los datos:
  
  
# Simular datos para tratamientos y lotes anidados dentro de tratamientos
set.seed(123)
tratamientos <- factor(rep(c("A", "B", "C"), each = 10))
lotes <- factor(rep(1:10, times = 3))
respuesta <- c(rnorm(10, mean = 10, sd = 2),  # Tratamiento A
               rnorm(10, mean = 12, sd = 2),  # Tratamiento B
               rnorm(10, mean = 14, sd = 2))  # Tratamiento C

# Crear un data frame con los datos
datos <- data.frame(Tratamiento = tratamientos, Lote = lotes, Respuesta = respuesta)

# Mostrar los datos
print(datos)


### **2. Realizar el ANOVA jerárquico**

# Usaremos la función `lme()` del paquete `nlme` para realizar el
# análisis jerárquico:
  
  
# Instalar y cargar el paquete nlme

pacman::p_load(nlme)

# Ajustar el modelo jerárquico (anidado)
modelo <- lme(Respuesta ~ Tratamiento, random = ~1 | Tratamiento/Lote, data = datos)

# Mostrar los resultados del modelo
summary(modelo)


### **Interpretación de resultados**

# El resumen del modelo proporcionará estimaciones de los efectos fijos (tratamientos) 
# y de los componentes de varianza para los efectos aleatorios 
# (lotes anidados dentro de tratamientos).

### **3. Comparaciones pareadas**

# Para realizar comparaciones pareadas, podemos usar la función `emmeans` 
# del paquete `emmeans`:
  
  
# Instalar y cargar el paquete emmeans

pacman::p_load(emmeans)

# Comparaciones pareadas
pares <- emmeans(modelo, pairwise ~ Tratamiento)

# Mostrar los resultados de las comparaciones pareadas
print(pares)

plot(pares)


### **Interpretación de comparaciones pareadas**

# Las comparaciones pareadas proporcionarán estimaciones ajustadas de
# las diferencias entre tratamientos y sus respectivos intervalos de 
# confianza y valores p.

# Lecturas: 

# https://en.wikipedia.org/wiki/Mixed_model
# https://bookdown.org/pingapang9/linear_models_bookdown/mixed.html 


#B. Planes jérarquicos puros y factorial jerárquico  ----

# ejemplos de análisis de planes jerárquicos puros y factorial jerárquico 

### **1. Análisis de Planes Jerárquicos Puros**

# Un diseño jerárquico puro es una estructura experimental en la que 
# los factores están anidados unos dentro de otros, en lugar de estar cruzados.
# Este tipo de diseño es útil cuando se desea evaluar la variabilidad a 
# diferentes niveles jerárquicos

# El diseño jerárquico puro es especialmente útil en contextos donde hay
# "una estructura natural de anidación", como la evaluación de productos en 
# diferentes lotes y fábricas, o estudiantes dentro de clases y escuelas.
# Su aplicación adecuada puede proporcionar insights detallados y controlados
# sobre las fuentes de variabilidad en los datos.

# Supongamos que estamos evaluando la calidad de un producto alimenticio 
# en diferentes tiendas y dentro de cada tienda, diferentes lotes.
# Este es un ejemplo de un diseño jerárquico puro.

#### **Simulación de datos jerárquicos puros**


# Simular datos para un diseño jerárquico puro
set.seed(123)
tiendas <- factor(rep(1:3, each = 10))
lotes <- factor(rep(1:10, times = 3))
respuesta <- c(rnorm(10, mean = 10, sd = 2),  # Tienda 1
               rnorm(10, mean = 12, sd = 2),  # Tienda 2
               rnorm(10, mean = 14, sd = 2))  # Tienda 3

# Crear un data frame con los datos
datos_jerarquico_puro <- data.frame(Tienda = tiendas, Lote = lotes, Respuesta = respuesta)

# Mostrar los datos
print(datos_jerarquico_puro)


#### **Análisis de varianza jerárquico puro**

# Usaremos la función `lme()` del paquete `nlme` para realizar
# el análisis jerárquico puro:
  
  
# Instalar y cargar el paquete nlme
require(nlme)

# Ajustar el modelo jerárquico puro
modelo_jerarquico_puro <- lme(Respuesta ~ 1, random = ~1 | Tienda/Lote, data = datos_jerarquico_puro)

# Mostrar los resultados del modelo
summary(modelo_jerarquico_puro)


### **2. Análisis de Planes Factorial Jerárquico**

# Supongamos que estamos evaluando el efecto de dos factores (A y B) en 
# la calidad de un producto alimenticio, y además tenemos diferentes lotes 
# dentro de cada combinación de estos factores.

#### **Simulación de datos factorial jerárquico**


# Simular datos para un diseño factorial jerárquico
set.seed(123)
factor_a <- factor(rep(c("A1", "A2"), each = 20))
factor_b <- factor(rep(c("B1", "B2"), times = 10))
lotes <- factor(rep(1:10, each = 2, times = 2))
respuesta <- c(rnorm(10, mean = 10, sd = 2),  # Combinación A1B1
               rnorm(10, mean = 12, sd = 2),  # Combinación A1B2
               rnorm(10, mean = 14, sd = 2),  # Combinación A2B1
               rnorm(10, mean = 16, sd = 2))  # Combinación A2B2

# Crear un data frame con los datos
datos_factorial_jerarquico <- data.frame(FactorA = factor_a, FactorB = factor_b, Lote = lotes, Respuesta = respuesta)

# Mostrar los datos
print(datos_factorial_jerarquico)


#### **Análisis de varianza factorial jerárquico**

# Usaremos la función `lme()` del paquete `nlme` para realizar el 
# análisis factorial jerárquico:
  
  
# Ajustar el modelo factorial jerárquico
modelo_factorial_jerarquico <- lme(Respuesta ~ FactorA * FactorB, random = ~1 | FactorA/FactorB/Lote, data = datos_factorial_jerarquico)

# Mostrar los resultados del modelo
summary(modelo_factorial_jerarquico)


### **Interpretación de resultados**

# El resumen del modelo proporcionará estimaciones de los efectos fijos 
# (factores A y B, y su interacción) y de los componentes de varianza para 
# los efectos aleatorios (lotes anidados dentro de las combinaciones de factores).

### **Comparaciones pareadas**

# Para realizar comparaciones pareadas, podemos usar
# la función `emmeans` del paquete `emmeans`:
  
  
# Instalar y cargar el paquete emmeans
if (!requireNamespace("emmeans", quietly = TRUE)) {
  install.packages("emmeans")
}
library(emmeans)

# Comparaciones pareadas
pares <- emmeans(modelo_factorial_jerarquico, pairwise ~ FactorA * FactorB)

# Mostrar los resultados de las comparaciones pareadas
print(pares)

plot(pares)

# Este ejemplo te muestra cómo realizar un análisis de varianza 
# para diseños jerárquicos puros y factoriales jerárquicos. 

#B.2 Planes jérarquicos puros y factorial jerárquico  ----

# ANOVA para dos tipos de diseños experimentales: jerárquicos puros y 
# factoriales jerárquicos.

### **1. Análisis ANOVA para Diseños Jerárquicos Puros**

# En un diseño jerárquico puro, los niveles de un factor están completamente anidados
# dentro de los niveles de otro factor. Esto es útil para analizar la variabilidad
# a diferentes niveles jerárquicos.

#### **Ejemplo: Evaluación de la calidad de un producto en diferentes fábricas y lotes**

##### **Simulación de datos jerárquicos puros**


# Simular datos para un diseño jerárquico puro
set.seed(123)
fabricas <- factor(rep(1:3, each = 10))
lotes <- factor(rep(1:10, times = 3))
respuesta <- c(rnorm(10, mean = 10, sd = 2),  # Fábrica 1
               rnorm(10, mean = 12, sd = 2),  # Fábrica 2
               rnorm(10, mean = 14, sd = 2))  # Fábrica 3

# Crear un data frame con los datos
datos_jerarquico_puro <- data.frame(Fabrica = fabricas, Lote = lotes, Respuesta = respuesta)

# Mostrar los datos
print(datos_jerarquico_puro)


##### **Análisis de varianza jerárquico puro**

# Usaremos la función `lme()` del paquete `nlme` para realizar el análisis:
  
  
# Instalar y cargar el paquete nlme
if (!requireNamespace("nlme", quietly = TRUE)) {
  install.packages("nlme")
}
library(nlme)

# Ajustar el modelo jerárquico puro
modelo_jerarquico_puro <- lme(Respuesta ~ 1, random = ~1 | Fabrica/Lote, data = datos_jerarquico_puro)

# Mostrar los resultados del modelo
summary(modelo_jerarquico_puro)

### **Interpretación de Resultados**

# - **ANOVA Jerárquico Puro:** El resumen del modelo proporcionará estimaciones
# de los componentes de varianza para los efectos aleatorios (fábricas y 
# lotes dentro de fábricas), permitiendo evaluar la variabilidad a diferentes 
# niveles jerárquicos.


### **2. Análisis ANOVA para Diseños Factorial Jerárquico**

# En un diseño factorial jerárquico, un factor está anidado dentro 
# de otro factor, pero también se evalúan múltiples factores de manera cruzada.

#### **Ejemplo: Evaluación de la calidad de un producto en diferentes 
# combinaciones de tratamientos y lotes**

##### **Simulación de datos factorial jerárquico**


# Simular datos para un diseño factorial jerárquico
set.seed(123)
factor_a <- factor(rep(c("A1", "A2"), each = 20))
factor_b <- factor(rep(c("B1", "B2"), times = 10))
lotes <- factor(rep(1:10, each = 2, times = 2))
respuesta <- c(rnorm(10, mean = 10, sd = 2),  # Combinación A1B1
               rnorm(10, mean = 12, sd = 2),  # Combinación A1B2
               rnorm(10, mean = 14, sd = 2),  # Combinación A2B1
               rnorm(10, mean = 16, sd = 2))  # Combinación A2B2

# Crear un data frame con los datos
datos_factorial_jerarquico <- data.frame(FactorA = factor_a, FactorB = factor_b, Lote = lotes, Respuesta = respuesta)

# Mostrar los datos
print(datos_factorial_jerarquico)


##### **Análisis de varianza factorial jerárquico**

# Usaremos la función `lme()` del paquete `nlme` para realizar el análisis:
  
  
# Ajustar el modelo factorial jerárquico
modelo_factorial_jerarquico <- lme(Respuesta ~ FactorA * FactorB, random = ~1 | FactorA/FactorB/Lote, data = datos_factorial_jerarquico)

# Mostrar los resultados del modelo
summary(modelo_factorial_jerarquico)


### **Interpretación de Resultados**

# - **ANOVA Factorial Jerárquico:** El resumen del modelo proporcionará
# estimaciones de los efectos fijos (factores A y B, y su interacción) y de
# los componentes de varianza para los efectos aleatorios (lotes anidados 
# dentro de las combinaciones de factores).


#C. Diseño para sistemas continuos (EVOP) ----

# El diseño para sistemas continuos (EVOP) es una metodología utilizada para
# la mejora continua de procesos industriales. 
# EVOP se centra en realizar pequeñas modificaciones en los parámetros
# del proceso y evaluar su impacto, permitiendo ajustes incrementales 
# que optimizan el rendimiento y la calidad del producto.

# Ejemplo en Ciencia de Alimentos: Optimización de un Proceso de Fermentación

# 1. Selección de Variables

# Identificar las variables clave que afectan el proceso de fermentación,
# como la temperatura, pH, concentración de nutrientes, y tiempo de fermentación.

# 2. Diseño del Experimento

# Planificar un experimento que varíe estas variables en pequeños incrementos.
# Por ejemplo, ajustar la temperatura en incrementos de 1°C y 
# evaluar el impacto en la calidad del producto fermentado.

# 3. Realización del Experimento

# Conducir el experimento en un entorno controlado, asegurando que cada cambio 
# en las variables se registre y se analice.

# 4. Análisis de Resultados

# Evaluar los datos obtenidos para determinar qué combinaciones de variables 
# producen los mejores resultados en términos de calidad, sabor, y seguridad 
# del producto fermentado.

# Ejemplo 

# diseño para sistemas continuos (EVOP) en R para optimizar un proceso de 
# fermentación en la industria alimentaria.

### **Ejemplo: Optimización de la Fermentación de Yogur**

#### **1. Definir las Variables**
# Vamos a optimizar tres variables clave: temperatura, pH y tiempo de fermentación.

#### **2. Simular Datos**
# Simulamos datos para representar diferentes combinaciones de estas variables y
# sus efectos en la calidad del yogur.


# Instalar y cargar paquetes necesarios
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("lme4", quietly = TRUE)) {
  install.packages("lme4")
}
library(dplyr)
library(ggplot2)
library(lme4)

# Simular datos

n <- 100  # Número de observaciones
temperatura <- runif(n, 37, 43)  # Temperatura en °C
ph <- runif(n, 4, 5)  # pH
tiempo <- runif(n, 6, 10)  # Tiempo en horas
calidad <- 15 + 0.5 * temperatura - 0.8 * ph + 0.3 * tiempo + rnorm(n, 0, 2)  # Modelo simulado de calidad

# Crear data frame
datos <- data.frame(Temperatura = temperatura, pH = ph, Tiempo = tiempo, Calidad = calidad)

# Mostrar los datos
head(datos)



# 2. Ajustar el Modelo
# Ahora, ajustaremos un modelo lineal simple sin anidación múltiple.


# Ajustar el modelo lineal
modelo <- lm(Calidad ~ Temperatura + pH + Tiempo, data = datos)

# Mostrar los resultados del modelo
summary(modelo)

# 3. Visualización y Análisis de Resultados
# Visualizamos los datos para comprender mejor los efectos de las variables.


# Visualización de la calidad del yogur en función de la temperatura, pH y tiempo
library(ggplot2)
ggplot(datos, aes(x = Temperatura, y = Calidad, color = pH, size = Tiempo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Calidad del Yogur en función de la Temperatura, pH y Tiempo",
       x = "Temperatura (°C)",
       y = "Calidad del Yogur") +
  theme_minimal()

# 4. Interpretación de Resultados

# El resumen del modelo proporcionará los coeficientes estimados para
# cada variable, lo que permitirá identificar su impacto en la calidad del yogur. 
# La visualización ayudará a observar las tendencias y patrones en los datos.

# 5. Implementación de Cambios
# Basándonos en los resultados del análisis, identificamos los valores óptimos
# para las variables y los implementamos en el proceso de producción.

# 6. Monitoreo Continuo
# Continuamos monitoreando el proceso para asegurarnos de que los cambios 
# sean efectivos y realicemos ajustes adicionales según sea necesario.

#D. Diseño de mezclas y métodos de optimización ----

### **Diseño de Mezclas**

# El **diseño de mezclas** es una metodología experimental utilizada 
# para encontrar la mejor combinación de ingredientes o componentes 
# que conforman una mezcla. Este tipo de diseño se aplica cuando 
# la respuesta del experimento depende de las proporciones relativas de
# los componentes en lugar de sus cantidades absolutas. Es especialmente 
# útil en la industria alimentaria, química, y farmacéutica.

#### **Conceptos Clave del Diseño de Mezclas:**

# 1. **Componentes:** Son los diferentes ingredientes o materiales que se mezclan. 
# Por ejemplo, en una bebida podrían ser azúcar, agua y jugo.
# 2. **Proporciones:** La respuesta del experimento depende de la proporción 
# de cada componente en la mezcla, no de la cantidad total.
# 3. **Restricciones:** Las proporciones de los componentes deben sumar 1 (o 100%).
# Por ejemplo, si tenemos tres componentes A, B, y C, entonces A + B + C = 1.

#### **Pasos en el Diseño de Mezclas:**

# 1. **Definir el Problema:** Determinar los objetivos y las restricciones 
# del experimento.

# 2. **Seleccionar Componentes y Niveles:** Elegir los ingredientes y sus
# rangos de proporción.

# 3. **Diseñar el Experimento:** Usar métodos estadísticos para planificar
# las combinaciones de las mezclas a evaluar.

# 4. **Realizar Experimentos:** Preparar las mezclas y medir la respuesta.

# 5. **Analizar Datos:** Utilizar herramientas estadísticas para analizar 
# los resultados.

# 6. **Optimizar:** Aplicar técnicas de optimización para encontrar 
# la mejor combinación de ingredientes.

# 7. **Validar:** Confirmar los resultados con experimentos adicionales.

### **Métodos de Optimización**

# Los **métodos de optimización** buscan encontrar la mejor 
# combinación de factores (en este caso, las proporciones de los componentes) 
# que maximicen o minimicen una respuesta de interés. 

# Los métodos comunes incluyen:
  
# 1. **Método de Superficie de Respuesta (RSM):** Una técnica estadística 
# que utiliza un modelo polinómico para aproximar la relación entre los
# factores y la respuesta. Permite encontrar el punto óptimo mediante 
# la creación de una superficie de respuesta.

# 2. **Método de Mezclas Simplex:** Un método iterativo que ajusta las
# proporciones de los componentes para mejorar la respuesta. 
# Es útil para problemas con restricciones complicadas.

# 3. **Algoritmos de Optimización (e.g., Nelder-Mead, Algoritmos Genéticos):**
# Algoritmos que buscan sistemáticamente el óptimo en el espacio de las
# combinaciones de los componentes.

### **Ejemplo en R: Optimización de una Mezcla para una Bebida**


# Instalar y cargar paquetes necesarios
if (!requireNamespace("mixexp", quietly = TRUE)) {
  install.packages("mixexp")
}
library(mixexp)

# Definir los ingredientes y las mezclas
ingredientes <- c("A", "B", "C")
mezclas <- expand.grid(A = seq(0, 1, by = 0.1), B = seq(0, 1, by = 0.1), C = seq(0, 1, by = 0.1))
mezclas <- mezclas[rowSums(mezclas) == 1, ]

# Simular la respuesta
set.seed(123)
mezclas$Sabor <- 10 + 5 * mezclas$A + 7 * mezclas$B + 3 * mezclas$C + rnorm(nrow(mezclas), 0, 0.5)

# Ajustar el modelo de mezclas
modelo <- lm(Sabor ~ A + B + C, data = mezclas)

# Optimización usando el método de Nelder-Mead
optimizacion <- optim(c(0.3, 0.3, 0.4), function(x) -predict(modelo, newdata = data.frame(A = x[1], B = x[2], C = x[3])), 
                      method = "Nelder-Mead", control = list(fnscale = -1), 
                      lower = c(0, 0, 0), upper = c(1, 1, 1), gr = NULL)

# Mostrar los resultados de la optimización
optimizacion


### **Interpretación de Resultados**

# - **Modelo de Mezclas:** Evalúa cómo cada ingrediente (A, B, C) afecta
# la respuesta (sabor) de manera individual y combinada.

# - **Optimización:** Identifica la combinación de ingredientes que 
# maximiza la respuesta.

