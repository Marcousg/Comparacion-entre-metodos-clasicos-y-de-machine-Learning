#Librerias
library(nycflights13)
library(mice)
library(VIM)
library(ggplot2)
library(dplyr)

set.seed(123)
flights <- flights
flights <- flights[,-c(5,8,11,12,17,18,19)]

#Analisis descriptivo de datos y datos faltantes
summary(flights)

#datos faltantes
md.pattern(flights,plot = TRUE, rotate.names = TRUE)

#porcentaje de datos faltantes
porcentajeMiss <- function(x) {sum(is.na(x))/length(x)*100}

#por columna
apply(flights, 2, porcentajeMiss)

#Se encontraron NA`s en las columnas dep_time, dep_delay, arr_time, arr_delay, air_time

# Imputación con la media
Imput_mean <- mice(flights,method="mean",
                   print=T)

Complete_mean <- mice::complete(Imput_mean)  

#Imputacion con la mediana
Complete_median <- flights

#dep_time
Complete_median$dep_time[is.na(Complete_median$dep_time)] <- median(Complete_median$dep_time, 
                                                                      na.rm = TRUE)
#dep_delay
Complete_median$dep_delay[is.na(Complete_median$dep_delay)] <- median(Complete_median$dep_delay, 
                                                                      na.rm = TRUE)
#arr_time
Complete_median$arr_time[is.na(Complete_median$arr_time)] <- median(Complete_median$arr_time, 
                                                                        na.rm = TRUE)
#arr_delay
Complete_median$arr_delay[is.na(Complete_median$arr_delay)] <- median(Complete_median$arr_delay, 
                                                                      na.rm = TRUE)
#air_time
Complete_median$air_time[is.na(Complete_median$air_time)] <- median(Complete_median$air_time, 
                                                                        na.rm = TRUE)

# Imputación con la moda 
Imput_mode <- mice(flights,
                   method="cart",
                   print=T,
                   m = 1,
                   maxit = 2) 

Complete_mode <- mice::complete(Imput_mode)


#Impute los datos faltantes via regresion lineal.
Imput_reg <- mice(flights, 
                   method = "norm.predict", 
                   print = FALSE)

Complete_reg <- mice::complete(Imput_reg)

#knn
flights$carrier <- as.factor(flights$carrier)
flights$origin  <- as.factor(flights$origin)
flights$dest    <- as.factor(flights$dest)

na_rows <- which(!complete.cases(flights))

subset_flights <- flights[unique(c(
  na_rows,
  sample(which(complete.cases(flights)), 10000)
)), ]

Complete_knn <- kNN(subset_flights,
    variable = c(
      "dep_time",
      "dep_delay",
      "arr_time",
      "arr_delay",
      "air_time"),
    dist_var = c(
      "month",
      "day",
      "origin",
      "dest",
      "distance"),
    k = 1,
    numFun = median,
    imp_var = FALSE)

#mice
imp_mice <- mice(flights, 
                 m = 3, 
                 maxit = 2, 
                 method = 'pmm')

Complete_mice <- complete(imp_mice, 1)

#Random_forest
imp_rf <- mice(flights, m = 1, 
               maxit = 2, 
               method = 'rf', 
               ntree = 10)
Complete_rf <- mice::complete(imp_rf)

#comparacion en dep_time

dep_time <- bind_rows(
  data.frame(dep_time = na.omit(flights$dep_time),
             Metodo = "Original"),
  data.frame(dep_time = Complete_mean$dep_time,
             Metodo = "Media"),
  data.frame(dep_time = Complete_median$dep_time,
             Metodo = "Mediana"),
  data.frame(dep_time = Complete_mode$dep_time,
             Metodo = "Moda"),
  data.frame(dep_time = Complete_reg$dep_time,
             Metodo = "Regresión"),
  data.frame(dep_time = Complete_knn$dep_time,
             Metodo = "kNN"),
  data.frame(dep_time = Complete_mice$dep_time,
             Metodo = "MICE"),
  data.frame(dep_time = Complete_rf$dep_time,
             Metodo = "Random Forest")
)

# Gráfica
ggplot(dep_time, aes(x = dep_time, color = Metodo, linetype = Metodo)) +
  geom_density(linewidth = 1.1, alpha = 0.8) +
  labs(
    title = "Distribuciones - dep_time",
    x = "dep_time",
    y = "Densidad",
    color = "Método de imputación"
  ) +
  scale_color_manual(
    values = c(
      "Original" = "steelblue3",
      "Media" = "red3",
      "Mediana" = "orange3",
      "Moda" = "green3",
      "Regresión" = "purple3",
      "kNN" = "pink3",
      "MICE" = "yellow3",
      "Random Forest" = "gray50"
    )) +
  guides(linetype = "none") +
  theme_minimal(base_size = 14)

#comparacion en dep_delay

dep_delay <- bind_rows(
  data.frame(dep_delay = na.omit(flights$dep_delay),
             Metodo = "Original"),
  data.frame(dep_delay = Complete_mean$dep_delay,
             Metodo = "Media"),
  data.frame(dep_delay = Complete_median$dep_delay,
             Metodo = "Mediana"),
  data.frame(dep_delay = Complete_mode$dep_delay,
             Metodo = "Moda"),
  data.frame(dep_delay = Complete_reg$dep_delay,
             Metodo = "Regresión"),
  data.frame(dep_delay = Complete_knn$dep_delay,
             Metodo = "kNN"),
  data.frame(dep_delay = Complete_mice$dep_delay,
             Metodo = "MICE"),
  data.frame(dep_delay = Complete_rf$dep_delay,
             Metodo = "Random Forest")
)

ggplot(dep_delay, aes(x = dep_delay, color = Metodo, linetype = Metodo)) +
  geom_density(linewidth = 1.1, alpha = 0.8) +
  coord_cartesian(xlim = c(-30, 50)) +
  labs(
    title = "Distribuciones - dep_delay",
    x = "dep_delay",
    y = "Densidad",
    color = "Método de imputación"
  ) +
  scale_color_manual(
    values = c(
      "Original" = "steelblue3",
      "Media" = "red3",
      "Mediana" = "orange3",
      "Moda" = "green3",
      "Regresión" = "purple3",
      "kNN" = "pink3",
      "MICE" = "yellow3",
      "Random Forest" = "gray50"
    )) +
  guides(linetype = "none") +
  theme_minimal(base_size = 14)

#comparación en arr_time

arr_time <- as.data.frame(bind_rows(
  data.frame(arr_time = na.omit(flights$arr_time),
             Metodo = "Original"),
  data.frame(arr_time = Complete_mean$arr_time,
             Metodo = "Media"),
  data.frame(arr_time = Complete_median$arr_time,
             Metodo = "Mediana"),
  data.frame(arr_time = Complete_mode$arr_time,
             Metodo = "Moda"),
  data.frame(arr_time = Complete_reg$arr_time,
             Metodo = "Regresión"),
  data.frame(arr_time = Complete_knn$arr_time,
             Metodo = "kNN"),
  data.frame(arr_time = Complete_mice$arr_time,
             Metodo = "MICE"),
  data.frame(arr_time = Complete_rf$arr_time,
             Metodo = "Random Forest")
))

ggplot(arr_time, aes(x = arr_time, color = Metodo, linetype = Metodo)) +
  geom_density(linewidth = 1.1, alpha = 0.8) +
  labs(
    title = "Distribuciones - arr_time",
    x = "arr_time",
    y = "Densidad",
    color = "Método de imputación"
  ) +
  scale_color_manual(
    values = c(
      "Original" = "steelblue3",
      "Media" = "red3",
      "Mediana" = "orange3",
      "Moda" = "green3",
      "Regresión" = "purple3",
      "kNN" = "pink3",
      "MICE" = "yellow3",
      "Random Forest" = "gray50"
    )) +
  guides(linetype = "none") +
  theme_minimal(base_size = 14)

#comparación de arr_delay
arr_delay <- as.data.frame(bind_rows(
  data.frame(arr_delay = na.omit(flights$arr_delay),
             Metodo = "Original"),
  data.frame(arr_delay = Complete_mean$arr_delay,
             Metodo = "Media"),
  data.frame(arr_delay = Complete_median$arr_delay,
             Metodo = "Mediana"),
  data.frame(arr_delay = Complete_mode$arr_delay,
             Metodo = "Moda"),
  data.frame(arr_delay = Complete_reg$arr_delay,
             Metodo = "Regresión"),
  data.frame(arr_delay = Complete_knn$arr_delay,
             Metodo = "kNN"),
  data.frame(arr_delay = Complete_mice$arr_delay,
             Metodo = "MICE"),
  data.frame(arr_delay = Complete_rf$arr_delay,
             Metodo = "Random Forest")
))

ggplot(arr_delay, aes(x = arr_delay, color = Metodo, linetype = Metodo)) +
  geom_density(linewidth = 1.1, alpha = 0.8) +
  coord_cartesian(xlim = c(-50, 70)) +
  labs(
    title = "Distribuciones - arr_delay",
    x = "arr_delay",
    y = "Densidad",
    color = "Método de imputación"
  ) +
  scale_color_manual(
    values = c(
      "Original" = "steelblue3",
      "Media" = "red3",
      "Mediana" = "orange3",
      "Moda" = "green3",
      "Regresión" = "purple3",
      "kNN" = "pink3",
      "MICE" = "yellow3",
      "Random Forest" = "gray50"
    )) +
  guides(linetype = "none") +
  theme_minimal(base_size = 14)

#Comparación en air_time
air_time <- as.data.frame(bind_rows(
  data.frame(air_time = na.omit(flights$air_time),
             Metodo = "Original"),
  data.frame(air_time = Complete_mean$air_time,
             Metodo = "Media"),
  data.frame(air_time = Complete_median$air_time,
             Metodo = "Mediana"),
  data.frame(air_time = Complete_mode$air_time,
             Metodo = "Moda"),
  data.frame(air_time = Complete_reg$air_time,
             Metodo = "Regresión"),
  data.frame(air_time = Complete_knn$air_time,
             Metodo = "kNN"),
  data.frame(air_time = Complete_mice$air_time,
             Metodo = "MICE"),
  data.frame(air_time = Complete_rf$air_time,
             Metodo = "Random Forest")
))

ggplot(air_time, aes(x = air_time, color = Metodo, linetype = Metodo)) +
  geom_density(linewidth = 1.1, alpha = 0.8) +
  labs(
    title = "Distribuciones - air_time",
    x = "air_time",
    y = "Densidad",
    color = "Método de imputación"
  ) +
  scale_color_manual(
    values = c(
      "Original" = "steelblue3",
      "Media" = "red3",
      "Mediana" = "orange3",
      "Moda" = "green3",
      "Regresión" = "purple3",
      "kNN" = "pink3",
      "MICE" = "yellow3",
      "Random Forest" = "gray50"
    )) +
  guides(linetype = "none") +
  theme_minimal(base_size = 14)