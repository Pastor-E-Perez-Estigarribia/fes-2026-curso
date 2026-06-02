

# Contenido ----
#0. Diseño Completamente Aleatorizado (DCA)
#A. Diseño de bloques completos al azar (DBCA)
#B. Diseño en cuadro latino (DCL) 
#C. Diseño en cuadro grecolatino (DCGL)
#D. Diseño en cuadro grecolatino (DCGL) 


#0. Análisis de varianza (ANOVA) para un Diseño Completamente Aleatorizado (DCA) ----
  
  ### 1. Simular los datos
#  Primero, simularemos algunos datos para los tratamientos y sus respuestas:
  
  
# Simular datos para tres tratamientos con cuatro repeticiones cada uno
set.seed(123)
tratamientos <- factor(rep(c("A", "B", "C"), each = 4))
respuesta <- c(rnorm(4, mean = 10, sd = 2),  # Tratamiento A
               rnorm(4, mean = 12, sd = 2),  # Tratamiento B
               rnorm(4, mean = 14, sd = 2))  # Tratamiento C

# Crear un data frame con los datos
datos <- data.frame(Tratamiento = tratamientos, Respuesta = respuesta)

# Mostrar los datos
print(datos)

# Realizar el análisis de varianza
modelo <- aov(Respuesta ~ Tratamiento, data = datos)

# Mostrar los resultados del ANOVA
summary(modelo)

plot(modelo)


#A. Diseño de bloques completos al azar (DBCA) ----

#Análisis de varianza (ANOVA) para un Diseño de Bloques Completos Aleatorizados (DBCA) 

### 1. Simulación de datos
# Simularemos datos para tres tratamientos aplicados a bloques 
# (lotes de producción) con cuatro repeticiones cada uno.


# Simular datos para tres tratamientos en cuatro bloques
set.seed(123)
tratamientos <- factor(rep(c("A", "B", "C"), each = 4))
bloques <- factor(rep(1:4, times = 3))
respuesta <- c(rnorm(4, mean = 10, sd = 2),  # Tratamiento A
               rnorm(4, mean = 12, sd = 2),  # Tratamiento B
               rnorm(4, mean = 14, sd = 2))  # Tratamiento C

# Crear un data frame con los datos
datos <- data.frame(Bloque = bloques, Tratamiento = tratamientos, Respuesta = respuesta)

# Mostrar los datos
print(datos)


### 2. Realizar el ANOVA
# Usaremos la función `aov()` para realizar el análisis de varianza, 
# incluyendo el efecto de los bloques.


# Realizar el análisis de varianza
modelo <- aov(Respuesta ~ Tratamiento + Bloque, data = datos)

# Mostrar los resultados del ANOVA
summary(modelo)


### 3. Interpretar los resultados
# El resumen del modelo ANOVA proporciona los valores F y los niveles 
# de significancia para los tratamientos y los bloques. Un valor p significativo
# indicaría que hay diferencias significativas entre los
# tratamientos y/o bloques.

### 4. Diagnóstico del modelo
# Es importante visualizar los residuos para verificar las suposiciones
# del ANOVA:
  
  
# Graficar los residuos
par(mfrow = c(2, 2))
plot(modelo)


# Esto genera varias gráficas de diagnóstico que te ayudan a evaluar 
# la normalidad de los residuos, la homogeneidad de las varianzas, entre otros.


### 2. Realizar el ANOVA
#Usaremos la función `aov()` para realizar el análisis de varianza:
  
  
# Realizar el análisis de varianza
modelo <- aov(Respuesta ~ Tratamiento, data = datos)

# Mostrar los resultados del ANOVA
summary(modelo)


### 3. Interpretar los resultados
# El resumen del modelo ANOVA proporciona los valores F y los 
# niveles de significancia para los tratamientos. Un valor p significativo 
# indicaría que hay diferencias significativas entre los tratamientos.

### 4. Diagnóstico del modelo
# Es una buena práctica visualizar los residuos para verificar las 
# suposiciones del ANOVA:
  
  
# Graficar los residuos
par(mfrow = c(2, 2))
plot(modelo)


# Esto genera varias gráficas de diagnóstico que te ayudan a evaluar la 
# normalidad de los residuos, la homogeneidad de las varianzas, etc.

# Ejemplo DBCA

# En un estudio se asignan tres dietas por un período de tres días a 
# cada uno de seis sujetos en un diseño de bloques completos al azar.
# A los sujetos, que juegan el papel de bloques, se les asignan las 
# siguientes tres dietas en orden aleatorio.

# Dieta 1: mezcla de grasa y carbohidratos,

# Dieta 2: alta en grasa

# Dieta 3: alta en carbohidratos.

# Al final del período de tres días cada sujeto se coloca un 
# aparato para caminata y se mide el tiempo de agotamiento en segundos. 
# Se registraron los siguientes datos:

require(pacman)
pacman::p_load(tidyverse)

# Crear un data frame con los datos
diet <- data.frame('dieta'=c('d1','d2','d3'),
                    's1'=c(84,91,122),
                    's2'=c(35,48,53),
                    's3'=c(91,71,110),
                    's4'=c(57,45,71),
                    's5'=c(56,61,91),
                    's6'=c(45,61,122))  %>%
  pivot_longer(!dieta,  names_to = "sujeto", values_to = "agotamiento")


View(diet)

# Hipótesis 

# Ho: Las dietas producen el mismo tiempo promedio de agotamiento en los sujetos.

# H1: Al menos una de las dietas produce tiempos promedios de agotamiento diferentes.

# Nivel de significación= 0.05

modiet=lm(agotamiento~sujeto+dieta,diet)
anova(modiet)

# Conclusión: Al nivel de significación del 5% se puede concluir que el
# tipo de dieta afecta al tiempo de agotamiento de una persona. También podemos
# decir que, al menos una de las dietas producirán un tiempo promedio 
# diferente de agotamiento.

# Verificación de Supuestos

par(mfrow=c(2,2))
plot(modiet)

# Normalidad? 

error=rstandard(modiet)
head(error)
shapiro.test(error)

# La hipótesis nula de esta prueba es que la población está 
# distribuida normalmente.

# Homogeneidad en la varianza? 

# Prueba De Puntaje Para Varianza De Error No Constante

pacman::p_load(car)

ncvTest(modiet)

# Autocorrelacion (Independencia en las observaciones)

pacman::p_load(zoo)
pacman::p_load(lmtest)
dwtest(modiet)

# Comparaciones Múltiples

diet$dieta = as.factor(diet$dieta)

pacman::p_load(multcomp)
pacman::p_load(RcmdrMisc)
modg<-aov(agotamiento~dieta+sujeto,diet)
summary(modg)

with(diet, 
     numSummary(agotamiento, 
     groups=diet$dieta, 
     statistics=c("mean", "sd")))

local({
  .Pairs <- glht(modg, linfct = mcp(dieta = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs)) # confidence intervals
  print(cld(.Pairs)) # compact letter display
  old.oma <- par(oma=c(0,5,0,0))
  plot(confint(.Pairs))
  par(old.oma)
})

numSummary(diet[,"agotamiento"], groups=diet$dieta, statistics=c("mean", "sd"))

# Con un nivel de significación del 5%, la dieta 3 es
# la que produce mayor agotamiento.

# Fuente: https://rpubs.com/misael892/DCAyBCA

#B. Diseño en cuadro latino (DCL) ----

# Análisis de varianza por permutación para un Diseño en Cuadro Latino (DCL) 
# con comparaciones pareadas. 
# Supongamos que estamos evaluando tres tratamientos 
# en un experimento de ciencias de los alimentos, 
# controlando dos factores de bloqueo (por ejemplo, días y lotes de producción).

### 1. Simulación de datos
# Primero, simulamos datos para los tratamientos, bloques y respuestas.


# Simular datos para un DCL con tres tratamientos, tres filas (bloques) y tres columnas (bloques)
set.seed(123)
tratamientos <- factor(rep(c("A", "B", "C"), 3))
bloques_filas <- factor(rep(1:3, each = 3))
bloques_columnas <- factor(rep(1:3, times = 3))
respuesta <- c(10, 12, 14, 11, 13, 15, 12, 10, 11) + rnorm(9, mean = 0, sd = 1)

# Crear un data frame con los datos
datos <- data.frame(Bloques_Filas = bloques_filas, Bloques_Columnas = bloques_columnas, Tratamiento = tratamientos, Respuesta = respuesta)

# Mostrar los datos
print(datos)



### 2. Realizar el ANOVA
# Usaremos la función `aov()` para realizar el análisis de varianza, 
# incluyendo el efecto de los bloques.


# Realizar el análisis de varianza
modelo <- aov(Respuesta ~ Tratamiento + Bloques_Filas + Bloques_Columnas, data = datos)

# Mostrar los resultados del ANOVA
summary(modelo)


### 3. Comparaciones pareadas
# Para realizar comparaciones pareadas, podemos utilizar
# la función `TukeyHSD()` para la prueba de Tukey.


# Realizar comparaciones pareadas con Tukey
comparaciones_pareadas <- TukeyHSD(modelo, "Tratamiento")

# Mostrar los resultados de las comparaciones pareadas
print(comparaciones_pareadas)


### Interpretación de resultados

# - El **ANOVA** te dará los valores F y los niveles de significancia
# para los tratamientos y bloques, indicando si hay diferencias 
# significativas entre los tratamientos o los bloques.

# - Las **comparaciones pareadas de Tukey** proporcionarán intervalos 
# de confianza y valores p ajustados para cada par de tratamientos, 
# indicando qué pares tienen diferencias significativas.

#D. Diseño en cuadro grecolatino (DCGL) ----

# Diseño en Cuadro Grecolatino (DCGL) es una extensión del
# Diseño en Cuadro Latino, añadiendo un tercer factor de bloqueo. 
# En el contexto de las ciencias de los alimentos, podemos utilizar 
# este diseño para evaluar tratamientos de conservación en 
# diferentes lotes, días y métodos de almacenamiento. 

### 1. Simulación de datos
# Simularemos datos para cuatro tratamientos, cuatro filas (bloques), 
# cuatro columnas (bloques) y cuatro bloques adicionales.


# Simular datos para un DCGL con cuatro tratamientos, cuatro filas,
# cuatro columnas y cuatro bloques adicionales
set.seed(123)
tratamientos <- factor(rep(c("A", "B", "C", "D"), 4))
bloques_filas <- factor(rep(1:4, each = 4))
bloques_columnas <- factor(rep(1:4, times = 4))
bloques_adicionales <- factor(rep(rep(1:4, each = 4)))

respuesta <- c(rnorm(4, mean = 10, sd = 2),  # Tratamiento A
               rnorm(4, mean = 12, sd = 2),  # Tratamiento B
               rnorm(4, mean = 14, sd = 2),  # Tratamiento C
               rnorm(4, mean = 16, sd = 2))  # Tratamiento D

# Crear un data frame con los datos
datos <- data.frame(Bloques_Filas = bloques_filas, Bloques_Columnas = bloques_columnas, Bloques_Adicionales = bloques_adicionales, Tratamiento = tratamientos, Respuesta = respuesta)

# Mostrar los datos
print(datos)


### 2. Realizar el ANOVA
# Usaremos la función `aov()` para realizar el análisis de varianza, 
# incluyendo los tres factores de bloqueo.


# Realizar el análisis de varianza
modelo <- aov(Respuesta ~ Tratamiento + Bloques_Filas + Bloques_Columnas + Bloques_Adicionales, data = datos)

# Mostrar los resultados del ANOVA
summary(modelo)


### 3. Interpretación de resultados
# El resumen del modelo ANOVA proporciona los valores F y los niveles de 
# significancia para los tratamientos y los bloques. Un valor p significativo
# indicaría que hay diferencias significativas entre los tratamientos o 
# los bloques.

### 4. Comparaciones pareadas
# Para realizar comparaciones pareadas, podemos utilizar 
# la función `TukeyHSD()` para la prueba de Tukey.


# Realizar comparaciones pareadas con Tukey
comparaciones_pareadas <- TukeyHSD(modelo, "Tratamiento")

# Mostrar los resultados de las comparaciones pareadas
print(comparaciones_pareadas)

plot(comparaciones_pareadas)

comparaciones_pareadas_Bloques_Filas <- TukeyHSD(modelo, "Bloques_Filas")

plot(comparaciones_pareadas_Bloques_Filas)

### Interpretación de comparaciones pareadas
# Las comparaciones pareadas de Tukey proporcionarán intervalos de 
# confianza y valores p ajustados para cada par de tratamientos, indicando
# qué pares tienen diferencias significativas.


