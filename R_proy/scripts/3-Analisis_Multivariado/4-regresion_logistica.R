# Titulo: Modelos lineales generalizados, regresión logistica  

# Objetivo: introducir y aplicar conceptos basicos de la regresión logistica 

# Autor: Pastor E. Pérez Estigarribia
# e-mail: peperez.estigarribia@pol.una.py

Sys.time()

# Fecha de complementación del script: "2022-11-30 16:08:02 -03" ----
# Fecha de ultima modificación: "2022-11-30 16:08:02 -03" ----

# Paquetes 

library(tidyverse)
library(gapminder) # dataset
library(finalfit)
library(broom)

# 1. Modelado lineal generalizado ####


# 1. El modelado lineal generalizado es una extensión del modelado lineal 

# 2. Permite aplicar los principios de la regresión lineal cuando
# los resultados no son variables numéricas continuas.

browseURL("https://argoshare.is.ed.ac.uk/healthyr_book/binary-logistic-regression.html")

# Nos centraremos en la regresión logística binaria, donde la variable 
## dependiente tiene dos niveles, por ejemplo: 

# - sí o no,
# - 0 o 1, 
# vivo o muerto 

# Aunque en la regresión logística binaria el resultado debe tener dos niveles,
# recuerde que los predictores (variables explicativas) pueden ser continuos 
# o categóricos.

# 2. Formulación del problema ####

# Consideraremos nuestro resultado como la ocurrencia de un evento 
# cardiovascular (CV) durante un período de 10 años.

# Trabajaremos con los datos que son  simulados y 
# no se basan en nada del mundo real.

# Odds y Probabilidad

browseURL("https://argoshare.is.ed.ac.uk/healthyr_book/binary-logistic-regression.html#odds-and-probabilities")

#  Ajuste de una línea de regresión

# Una escala log-odds suena increíblemente desagradable para los no matemáticos,
# pero es la solución perfecta.

# es ideal por que: 

# Va desde −∞ a +∞;

# Un odds of 1 es log-odds de 0

log(1)

# una duplicación y una reducción a la mitad de las odds
# representan la misma distancia en la escala.

log(2)

log(0.5)

browseURL("https://images.squarespace-cdn.com/content/v1/58cde3fcdb29d633eb688e9e/ced965ff-992a-49ec-b417-8cefc788f4dd/Logitstic+regression+formula.jpg?format=750w")


browseURL("https://argoshare.is.ed.ac.uk/healthyr_book/images/chapter09/2_prob_logodds.png")


# La recta ajustada y la ecuación de regresión logística

browseURL("https://argoshare.is.ed.ac.uk/healthyr_book/binary-logistic-regression.html#the-fitted-line-and-the-logistic-regression-equation")

# Veamos un ejemplo aplicado 

# Continuaremos explorando el boot::melanoma

#  3. Obtener los datos ####

melanoma <- boot::melanoma

# Comprobar los datos 

library(tidyverse)
library(finalfit)
melanoma %>% glimpse()
melanoma %>% ff_glimpse()

# Decodificar los datos

library(tidyverse)
library(finalfit)
melanoma <- melanoma %>% 
  mutate(sex.factor = factor(sex) %>%          
           fct_recode("Female" = "0",
                      "Male"   = "1") %>% 
           ff_label("Sex"),   
         
         ulcer.factor = factor(ulcer) %>% 
           fct_recode("Present" = "1",
                      "Absent"  = "0") %>% 
           ff_label("Ulcerated tumour"),
         
         age  = ff_label(age,  "Age (years)"),
         year = ff_label(year, "Year"),
         
         status.factor = factor(status) %>% 
           fct_recode("Died melanoma"  = "1",
                      "Alive" = "2",
                      "Died - other" = "3") %>% 
           fct_relevel("Alive") %>% 
           ff_label("Status"),
         
         t_stage.factor = 
           thickness %>% 
           cut(breaks = c(0, 1.0, 2.0, 4.0, 
                          max(thickness, na.rm=TRUE)),
               include.lowest = TRUE)
  )

melanoma$t_stage.factor %>% levels()

# Recodificar para facilitar.

melanoma <- melanoma %>% 
  mutate(
    t_stage.factor = 
      fct_recode(t_stage.factor,
                 "T1" = "[0,1]",
                 "T2" = "(1,2]",
                 "T3" = "(2,4]",
                 "T4" = "(4,17.4]") %>% 
      ff_label("T-stage")
  )

# También podemos ver que el estado de la mayoría de los que 
# no fallecieron se conoce más allá de los 5 años.

library(ggplot2)
melanoma %>% 
  ggplot(aes(x = time/365)) + 
  geom_histogram() + 
  facet_grid(. ~ status.factor)

# Entonces, decidamos observar la mortalidad a 5 años por melanoma. 

# 5-year mortality
melanoma <- melanoma %>% 
  mutate(
    mort_5yr = 
      if_else((time/365) < 5 & 
                (status == 1), 
              "Yes",          # then
              "No") %>%       # else
      fct_relevel("No") %>% 
      ff_label("5-year survival")
  )

# Visualicemos los datos 

# Estamos interesados en la asociación entre la ulceración del 
# tumor y el resultado

p1 <- melanoma %>% 
  ggplot(aes(x = ulcer.factor, fill = mort_5yr)) + 
  geom_bar() + 
  theme(legend.position = "none")

p2 <- melanoma %>% 
  ggplot(aes(x = ulcer.factor, fill = mort_5yr)) + 
  geom_bar(position = "fill") + 
  ylab("proportion")

library(patchwork)
p1 + p2

# Sumario de datos

library(finalfit)
dependent <- "ulcer.factor"
explanatory <- c("age", "sex.factor", "year", "t_stage.factor")
melanoma %>% 
  summary_factorlist(dependent, explanatory, p = TRUE,
                     add_dependent_label = TRUE)

# Supuestos del modelo 

browseURL("https://argoshare.is.ed.ac.uk/healthyr_book/model-assumptions.html")

# Linealidad de las variables continuas a la respuesta

# Se puede realizar una verificación gráfica de la linealidad 
# utilizando una línea de "loess" de mejor ajuste.

melanoma %>% 
  mutate(
    mort_5yr.num = as.numeric(mort_5yr) - 1
  ) %>% 
  select(mort_5yr.num, age, year) %>% 
  pivot_longer(all_of(c("age", "year")), names_to = "predictors") %>% 
  ggplot(aes(x = value, y = mort_5yr.num)) + 
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  facet_wrap(~predictors, scales = "free_x")

#  Multicolinealidad

# La presencia de dos o más variables altamente correlacionadas 
# en un análisis de regresión puede causar problemas en los resultados 
# que se generan. Las pendientes de las líneas (coeficientes, OR) pueden
# volverse inestables, lo que significa grandes cambios en su tamaño 
# con cambios mínimos en el modelo o los datos subyacentes.

# La ggpairs()función from library(GGally)es una buena manera de
# visualizar todas las asociaciones bidireccionales 

library(GGally)
explanatory <- c("ulcer.factor", "age", "sex.factor", 
                 "year", "t_stage.factor")
melanoma %>% 
  remove_labels() %>%  # ggpairs doesn't work well with labels
  ggpairs(columns = explanatory)

# Continuo con continuo

select_explanatory <- c("age", "year")
melanoma %>% 
  remove_labels() %>% 
  ggpairs(columns = select_explanatory)

# Continuo a categórico

select_explanatory <- c("age", "ulcer.factor", 
                        "sex.factor", "t_stage.factor")

melanoma %>% 
  select(all_of(select_explanatory)) %>% 
  pivot_longer(-age) %>% # pivots all but age into two columns: name and value
  ggplot(aes(value, age)) + 
  geom_boxplot() +
  facet_wrap(~name, scale = "free", ncol = 3) +
  coord_flip()

# De categórico a categórico

select_explanatory <- c("ulcer.factor", "sex.factor", "t_stage.factor")

melanoma %>% 
  select(one_of(select_explanatory)) %>% 
  pivot_longer(-sex.factor) %>% 
  ggplot(aes(value, fill = sex.factor)) + 
  geom_bar(position = "fill") +
  ylab("proportion") +
  facet_wrap(~name, scale = "free", ncol = 2) +
  coord_flip()

# Factor de inflación de la varianza

# como verificación final de la presencia de correlaciones de 
# orden superior, se puede calcular el factor de inflación de 
# la varianza para cada uno de los términos en un modelo final.
# En un lenguaje sencillo, esta es una medida de cuánto aumenta
# la varianza de un coeficiente de regresión particular debido 
# a la presencia de multicolinealidad en el modelo.


dependent <- "mort_5yr"
explanatory <- c("ulcer.factor", "age", "sex.factor", 
                 "year", "t_stage.factor")
melanoma %>% 
  glmmulti(dependent, explanatory) %>%
  car::vif()

# Una regla general común es que si esto es mayor que 5-10 para 
# cualquier variable, entonces puede existir multicolinealidad.
# El modelo debe explorarse más y los términos deben eliminarse o reducirse.

# Estrategia de modelado para resultados binarios

browseURL("https://argoshare.is.ed.ac.uk/healthyr_book/modelling-strategy-for-binary-outcomes.html")

# 4. Ajuste de modelos de regresión logística en base R ####

fit1 <- glm(mort_5yr ~ ulcer.factor, data = melanoma, family = binomial)
summary(fit1)

# Los coeficientes y sus intervalos de confianza del 95 % 
# se pueden extraer y exponenciar así.

coef(fit1) %>% exp()

confint(fit1) %>% exp()

# Una buena alternativa es la tidy() función del paquete broom.

# computar el OR 

library(broom)
fit1 %>% 
  tidy(conf.int = TRUE, exp = TRUE)

# Podemos ver a partir de estos resultados que existe una 
# fuerte asociación entre la ulceración del tumor y la mortalidad
# a los 5 años (OR 6,68; IC del 95 %: 3,18; 15,18).

# Las métricas del modelo se pueden extraer usando la función glance() 

fit1 %>% 
  glance()

# Ajuste de modelos de regresión logística con finalfit

library(finalfit)
dependent <- "mort_5yr"
explanatory <- "ulcer.factor"
melanoma %>% 
  finalfit(dependent, explanatory, metrics = TRUE)

# Vamos a empezar por incluir las variables anteriores 
# que creemos que son relevantes.

library(finalfit)
dependent <- "mort_5yr"
explanatory <- c("ulcer.factor", "age", "sex.factor", "t_stage.factor")
fit2 = melanoma %>% 
  finalfit(dependent, explanatory, metrics = TRUE)

fit2 

# Podemos convertir la edad en una variable categórica o 
# incluirla con un término cuadrático 

melanoma <- melanoma %>% 
  mutate(
    age.factor = cut(age,
                     breaks = c(0, 25, 50, 75, 100)) %>% 
      ff_label("Age (years)"))

# Add this to relevel:
# fct_relevel("(50,75]")

melanoma %>% 
  finalfit(dependent, c("ulcer.factor", "age.factor"), metrics = TRUE)
# No existe una fuerte relación entre la representación categórica de 
# la edad y el resultado. 

# En base R, se agrega un término cuadrático así: 

glm(mort_5yr ~ ulcer.factor  +I(age^2) + age, 
    data = melanoma, family = binomial) %>% 
  summary()

# Se puede hacer con Finalfit demanera similar. 

library(finalfit)
dependent <- "mort_5yr"
explanatory <- c("ulcer.factor", "I(age^2)", "age")
melanoma %>% 
  finalfit(dependent, explanatory, metrics = TRUE)
# El AIC es peor cuando se agrega la edad ya sea como un factor o 
# con un término cuadrático al modelo base.


# Un método final para visualizar la contribución de una variable
# en particular es eliminarla del modelo completo. 

library(finalfit)
dependent <- "mort_5yr"
explanatory <- c("ulcer.factor", "age.factor", "sex.factor", "t_stage.factor")
explanatory_multi <- c("ulcer.factor", "sex.factor", "t_stage.factor")

melanoma %>% 
  finalfit(dependent, explanatory, explanatory_multi, 
           keep_models = TRUE, metrics = TRUE)

# Comparemos las metricas de rendimiento y criterios ahora 

# Ahora, ¿qué pasa con la variable sexo?  ¿Está aportando mucho al modelo?

library(finalfit)
dependent <- "mort_5yr"
explanatory <- c("ulcer.factor", "sex.factor", "t_stage.factor")
explanatory_multi <- c("ulcer.factor", "t_stage.factor")

melanoma %>% 
  finalfit(dependent, explanatory, explanatory_multi, 
           keep_models = TRUE, metrics = TRUE)

# Por último, podemos comprobar si existe una interacción de primer 
# orden entre la ulceración y el estadio T.

# Solo para recordarnos lo que esto significa, una interacción significativa
# significaría que el efecto de, digamos, la ulceración en la
# mortalidad a 5 años diferiría según el estadio T.

library(finalfit)
dependent <- "mort_5yr"
explanatory <- c("ulcer.factor", "t_stage.factor")
explanatory_multi <- c("ulcer.factor*t_stage.factor") #Ojo!
melanoma %>% 
  finalfit(dependent, explanatory, explanatory_multi, 
           keep_models = TRUE, metrics = TRUE)
# No hay términos de interacción significativos.

# Nuestra tabla modelo final es por lo tanto:

library(finalfit)
dependent <- "mort_5yr"
explanatory <- c("ulcer.factor", "age.factor", 
                 "sex.factor", "t_stage.factor")
explanatory_multi <- c("ulcer.factor", "t_stage.factor")
melanoma %>% 
  finalfit(dependent, explanatory, explanatory_multi, metrics = TRUE)

# Gráfica de razón de probabilidades

dependent <- "mort_5yr"
explanatory_multi <- c("ulcer.factor", "t_stage.factor")
melanoma %>% 
  or_plot(dependent, explanatory_multi,
          breaks = c(0.5, 1, 2, 5, 10, 25),
          table_text_size = 3.5,
          title_text_size = 16)

# Podemos concluir que existe evidencia de una asociación entre la 
# ulceración del tumor y la supervivencia a los 5 años,
# que es independiente de la profundidad del tumor según lo
# captado por el estadio T.

# Regresión logistica como una capa de una red neuronal 

# La regresión logística es una herramienta estadística muy conocida para 
# problemas de clasificación. Puede expresarse como una red neuronal de una 
# sola capa (de algún modo).

# La regresión logística, en su forma simple, sigue siendo un modelo lineal.
# Por lo tanto, todavía requiere soluciones para parámetros como 
# β0,β1,… 𝛽 0 , 𝛽 1 , … , de modo que se pueda minimizar una función de costo.

# A continuación se muestra un gráfico de la función sigmoidea. Tenga en cuenta 
# que, independientemente del valor de entrada, z 𝑧 , la solución siempre estará entre 0 0 y 1 1 .

pacman::p_load(readr,plotly,DT)

x <- seq(-5, 5, 0.01)
y = 1 / (1 + exp(-x))

p <- plot_ly(x = x,
             y = y,
             name = "Sigmoid function",
             type = "scatter",
             mode = "lines") %>% 
  layout(title = "Sigmoid function")

p

# Aquí hay algunas funciones alternativas para la regresión logística en el 
# contexto de su código y el panorama más amplio del aprendizaje automático

# Ver 

browseURL("https://rpubs.com/juanhklopper/logistc_regression_as_a_network")

browseURL("https://glmnet.stanford.edu/articles/glmnetFamily.html")

browseURL("https://glmnet.stanford.edu/articles/glmnet.html")


pacman::p_load("glmnet")

data(QuickStartExample)
x <- QuickStartExample$x
y <- QuickStartExample$y

fit <- glmnet(x, y)

plot(fit)

coef(fit, s = 0.1)

set.seed(29)
nx <- matrix(rnorm(5 * 20), 5, 20)
predict(fit, newx = nx, s = c(0.1, 0.05))


# Gaussian
x = matrix(rnorm(100 * 20), 100, 20)
y = rnorm(100)
fit1 = glmnet(x, y)
print(fit1)
coef(fit1, s = 0.01)  # extract coefficients at a single value of lambda
predict(fit1, newx = x[1:10, ], s = c(0.01, 0.005))  # make predictions

# Relaxed
fit1r = glmnet(x, y, relax = TRUE)  # can be used with any model

# multivariate gaussian
y = matrix(rnorm(100 * 3), 100, 3)
fit1m = glmnet(x, y, family = "mgaussian")
plot(fit1m, type.coef = "2norm")

# binomial
g2 = sample(c(0,1), 100, replace = TRUE)
fit2 = glmnet(x, g2, family = "binomial")
fit2n = glmnet(x, g2, family = binomial(link=cloglog))
fit2r = glmnet(x,g2, family = "binomial", relax=TRUE)
fit2rp = glmnet(x,g2, family = "binomial", relax=TRUE, path=TRUE)

# multinomial
g4 = sample(1:4, 100, replace = TRUE)
fit3 = glmnet(x, g4, family = "multinomial")
fit3a = glmnet(x, g4, family = "multinomial", type.multinomial = "grouped")
# poisson
N = 500
p = 20
nzc = 5
x = matrix(rnorm(N * p), N, p)
beta = rnorm(nzc)
f = x[, seq(nzc)] %*% beta
mu = exp(f)
y = rpois(N, mu)
fit = glmnet(x, y, family = "poisson")
plot(fit)
pfit = predict(fit, x, s = 0.001, type = "response")
plot(pfit, y)

# Lecturas ---- 

# Regression analysis (OLS method)
browseURL("https://mgimond.github.io/Stats-in-R/regression.html")

# Logistic regression 
browseURL("https://mgimond.github.io/Stats-in-R/Logistic.html")

# Logistic regression as a shallow network
browseURL("https://rpubs.com/juanhklopper/logistc_regression_as_a_network")

# Logistic Regression Type Neural Networks
browseURL("https://deeplearningmath.org/logistic-regression-type-neural-networks")


