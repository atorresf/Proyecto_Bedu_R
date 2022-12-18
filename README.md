# Análisis de patrones de gasto en alimentos saludables y no saludables en hogares mexicanos
**Por _Antonio Torres Flores_ del equipo 11 (aclarar que es proyecto _individual_, desarrollado solo por quien lo subió)**
## 1. Descripción del problema
El problema planteado en este caso es determinar cómo el nivel socioeconómico, la disponibilidad de recursos financieros extras y la inseguridad alimentaria están relacionados con los patrones de gasto en alimentos saludables y no saludables en hogares mexicanos. Además, el objetivo es desarrollar un modelo que permita identificar los determinantes socioeconómicos de la inseguridad alimentaria.
Para abordar este problema, se pueden realizar diferentes análisis estadísticos y probabilísticos utilizando la base de datos de la Encuesta Nacional de Salud y Nutrición. 
### Descripción de la base de datos
La base de datos utilizada es un extracto de la Encuesta Nacional de Salud y Nutrición (2012) levantada por el Instituto Nacional de Salud Pública en México. La base de datos se encuentra en el siguiente [link](https://raw.githubusercontent.com/atorresf/BEDU_Sesion8/main/inseguridad_alimentaria_bedu.csv) y contiene información sobre el nivel socioeconómico, los recursos financieros extra al ingreso, el gasto en alimentos saludables y el gasto en alimentos no saludables de los hogares, así como la inseguridad alimentaria del hogar.

## 2. Análisis descriptivo de la información
Para realizar un análisis exploratorio de los datos, es importante seguir algunos pasos clave, que pueden variar según el objetivo del análisis y el tipo de datos que se esté utilizando. Algunos pasos que podrían ser útiles en el contexto de la información proporcionada podrían incluir:

### Revisión de la estructura de los datos
```R
# Revisar formato y estructura de la base de datos
str(df)
glimpse(df)
head(df)
View(df)
```
Para conocer los campos, sus tipos y ver como viene la información.
### Limpieza de datos
Es muy importante tener en cuenta la calidad de los datos cuando se realiza cualquier análisis estadístico o probabilístico. Por lo tanto, es importante realizar una revisión y limpieza de los datos antes de realizar cualquier análisis. Esto puede incluir eliminar registros con datos faltantes o erróneos, corregir errores de entrada de datos.
```R
# Eliminar filas con valores faltantes
df <- na.omit(df)
```
O podrías reemplazar los valores faltantes por el valor medio de la variable utilizando la función `is.na()` y `mean()`:
```R
# Reemplazar valores faltantes por el valor medio
df.completar <- df$Log_Natural_Gasto_Alimentos_Saludables[is.na(df$Log_Natural_Gasto_Alimentos_Saludables)] <- mean(df$Log_Natural_Gasto_Alimentos_Saludables, na.rm = TRUE)
```
### Generar resúmenes estadísticos de las variables
Medidas de tendencia central (por ejemplo, media, mediana) y dispersión (por ejemplo, desviación estándar, rango). Esto permite entender mejor la distribución y el comportamiento de las variables.
```R
# Medidas de tendencia central
mean(df.orig$Numero_Personas_Hogar)
mean(df.orig$Edad_Jefe)
mean(df.orig$Anios_Educ_Jefe)

median(df.orig$Numero_Personas_Hogar)
median(df.orig$Edad_Jefe)
median(df.orig$Anios_Educ_Jefe)

Mode(df.orig$Numero_Personas_Hogar)
Mode(df.orig$Anios_Educ_Jefe)

sd(df.orig$Numero_Personas_Hogar)
sd(df.orig$Edad_Jefe)
sd(df.orig$Anios_Educ_Jefe)
```

### Realizar gráficos
Para visualizar las variables y comprender mejor cómo se relacionan entre sí. Por ejemplo, se pueden utilizar gráficos de barras para comparar la distribución de las variables entre diferentes grupos (por ejemplo, por nivel socioeconómico). Aquí algunos ejemplos:

![image](https://user-images.githubusercontent.com/118469608/208286948-fc51d06e-82d7-426a-8ad2-ea036aa3178d.png)

![image](https://user-images.githubusercontent.com/118469608/208287001-0cb5662a-209a-4e11-b36c-8b6f996ff523.png)

## 3.	Calcula probabilidades que nos permitan entender el problema en México
Para calcular algunas probabilidades que podrían ser de interés son las siguientes:
### - Probabilidad de que un hogar tenga un nivel socioeconómico determinado:
```R
  prob_nse <- table(df.orig$Nivel_socioeconomico_hogar)/nrow(df.orig)
  names(prob_nse)=c("Bajo","MedioBajo","Medio","MedioAlto","Alto")
  prob_nse
```
Con esto obtenemos la probabilidad de que un hogar tenga cada uno de los niveles socioeconómicos disponibles.

### -	Probabilidad de que un hogar tenga recursos financieros extra (1) o no tenga (0)
```R
  prob_extra <- sum(df.orig$Recursos_Financieros_Distintos_Ingreso_Laboral == 1)/nrow(df.orig)
  prob_extra
```

### -	Probabilidad de que un hogar tenga inseguridad alimentaria por Nivel socioeconomico:
```R 
  prob_ins_b <- sum(df.orig$Inseguridad_Alimentaria == 1 & df.orig$Nivel_socioeconomico_hogar == 1)/table(df.orig$Nivel_socioeconomico_hogar)[1]
  prob_ins_mb <- sum(df.orig$Inseguridad_Alimentaria == 1 & df.orig$Nivel_socioeconomico_hogar == 2)/table(df.orig$Nivel_socioeconomico_hogar)[2]
  prob_ins_m <- sum(df.orig$Inseguridad_Alimentaria == 1 & df.orig$Nivel_socioeconomico_hogar == 3)/table(df.orig$Nivel_socioeconomico_hogar)[3]
  prob_ins_ma <- sum(df.orig$Inseguridad_Alimentaria == 1 & df.orig$Nivel_socioeconomico_hogar == 4)/table(df.orig$Nivel_socioeconomico_hogar)[4]
  prob_ins_a <- sum(df.orig$Inseguridad_Alimentaria == 1 & df.orig$Nivel_socioeconomico_hogar == 5)/table(df.orig$Nivel_socioeconomico_hogar)[5]
  prob_ins <- table(c(prob_ins_b,prob_ins_mb,prob_ins_m,prob_ins_ma,prob_ins_a))
  prob_ins <- matrix(c(prob_ins_b,prob_ins_mb,prob_ins_m,prob_ins_ma,prob_ins_a), ncol=5, byrow=TRUE)
  colnames(prob_ins) <- c('Bajo','MedioBajo','Medio','MedioAlto','Alto')
  rownames(prob_ins) <- c('Inseguridad Alimentaria')
  prob_ins <- as.table(prob_ins)
  prob_ins
```

|TIPO|Bajo |MedioBajo     |Medio |MedioAlto	|Alto|
|---|---|---|---|---|---|
|Inseguridad Alimentaria |0.8709641 |0.8189252 |0.7744804 |0.6753132	|0.4912770|

## 4.	Plantea hipótesis estadísticas y concluye sobre ellas para entender el problema en México
### a) En promedio el gasto de alimentos no saludables es mayor cuando el jefe de familia es hombre que cuando el jefe de familia es mujer a un NC 95%
#### Ho: Log_Natural_Gasto_Alimentos_No_Saludables_Hombre <= Log_Natural_Gasto_Alimentos_No_Saludables_Mujer
#### Ha: Log_Natural_Gasto_Alimentos_No_Saludables_Hombre > Log_Natural_Gasto_Alimentos_No_Saludables_Mujer
```R
# Cargar la base de datos
datos <- read.csv("https://raw.githubusercontent.com/atorresf/BEDU_Sesion8/main/inseguridad_alimentaria_bedu.csv")

# Seleccionar solo hogares con nivel socioeconómico conocido
datos <- subset(datos, !is.na(Sexo_Jefe))

# Se calculan las varianzas
var.test(datos[datos$Sexo_Jefe == 0, "Log_Natural_Gasto_Alimentos_No_Saludables"],
           datos[datos$Sexo_Jefe == 1, "Log_Natural_Gasto_Alimentos_No_Saludables"],
           ratio = 1, alternative = "two.sided")
           
 t.test(x = datos[datos$Sexo_Jefe == 0, "Log_Natural_Gasto_Alimentos_No_Saludables"],
         y = datos[datos$Sexo_Jefe == 1, "Log_Natural_Gasto_Alimentos_No_Saludables"],
         alternative = "greater", mu = 0, var.equal = TRUE)
         
#p-value = 5.607187e-31

#pvalue es menor a 0.05 por lo que se rechaza la hipótesis nula.
```
Una vez realizada la prueba t, se puede consultar el resultado de la misma para determinar si se rechaza o no la hipótesis nula. Para interpretar el resultado, se puede consultar el valor del p-valor Si el p-valor es menor que el nivel de significancia especificado (0.05), se rechaza la hipótesis nula y se acepta la hipótesis alterna. Si el p-valor es mayor o igual que el nivel de significancia, no se puede rechazar la hipótesis nula y se concluye que no hay evidencia suficiente para aceptar la hipótesis alterna. 

Para nuestro ejemplo el p-valor es menor que 0.05, se rechaza la hipotesis Ho, y se concluye que en promedio el gasto es mayor en alimentos no saludables cuando el jefe de familia es hombre que mujer.

### b) Queremos comparar los gastos en alimentos no saludables entre hogares de diferentes zonas geográficas. 
#### Ho: No existe una diferencia significativa en los gastos en alimentos no saludables entre hogares de diferentes zonas geográficas.
#### Ha: Los hogares ubicados en zonas urbanas tienen gastos significativamente mayores en alimentos no saludables que los hogares ubicados en zonas rurales.
```R
# Cargar la base de datos
datos <- read.csv("https://raw.githubusercontent.com/atorresf/BEDU_Sesion8/main/inseguridad_alimentaria_bedu.csv")

# Formular hipótesis nula y alterna
Ho: No existe una diferencia significativa en los gastos en alimentos no saludables entre hogares de diferentes zonas geográficas.
Ha: Los hogares ubicados en zonas urbanas tienen gastos significativamente mayores en alimentos no saludables que los hogares ubicados en zonas rurales.

# Realizar prueba t para comparar gastos en alimentos no saludables entre hogares de diferentes zonas geográficas
resultado <- t.test(Log_Natural_Gasto_Alimentos_No_Saludables ~ Zona_geografica, data = datos, var.equal = TRUE)

# Consultar p-valor para determinar si se rechaza o no la hipótesis nula
resultado$p.value

# pvalue = 1.858575e-175

```
En el ejemplo anterior, el p-value es menor que 0.05, se concluye que existe una diferencia significativa en los gastos en alimentos no saludables entre hogares de diferentes zonas geográficas. Los hogares ubicados en zonas urbanas tienen gastos significativamente mayores en alimentos no saludables que los hogares ubicados en zonas rurales. 


## 5.	Estima un modelo de regresión, lineal o logístico, para identificar los determinantes de la inseguridad alimentaria en México
utilizar un modelo de regresión logística, en el que la inseguridad alimentaria sea la variable dependiente y las variables independientes sean el nivel socioeconómico, la zona geográfica, el número de personas en el hogar, los recursos financieros distintos al ingreso laboral, la edad y el género del jefe de hogar, y los años de educación del jefe de hogar. Con este modelo, se pueden obtener los coeficientes de regresión y sus intervalos de confianza para cada variable independiente, lo que permitirá evaluar la relación entre estas variables y la inseguridad alimentaria.

El modelo de regresión logística es una técnica estadística que se utiliza para predecir una variable binaria (es decir, una variable que solo puede tomar dos valores, como "sí" o "no") a partir de un conjunto de variables independientes. En este caso, la variable binaria es la inseguridad alimentaria (que puede tomar los valores 0 o 1) y las variables independientes son el nivel socioeconómico, la zona geográfica, el número de personas en el hogar, los recursos financieros distintos al ingreso laboral, la edad y el género del jefe de hogar, y los años de educación del jefe de hogar.

Para construir el modelo de regresión logística en R, primero se debe cargar la base de datos y seleccionar las variables que se van a utilizar en el modelo. Luego, se puede utilizar la función `glm()` de la siguiente manera:
```R
modelo <- glm(Inseguridad_Alimentaria ~ Nivel_socioeconomico_hogar + Zona_geografica + Numero_Personas_Hogar + Recursos_Financieros_Distintos_Ingreso_Laboral + Edad_Jefe + Sexo_Jefe + Anios_Educ_Jefe, data = datos, family = binomial)
```
Donde datos es el nombre de la base de datos y Inseguridad_Alimentaria es la variable dependiente. Las variables independientes se especifican después del símbolo ~, separadas por +. La opción family = binomial indica que se trata de un modelo de regresión logística.

Una vez construido el modelo, se pueden obtener los coeficientes de regresión y sus intervalos de confianza con la función summary():
```R
summary(modelo)
```

Se tiene la siguiente salida:
```
Call:
glm(formula = Inseguridad_Alimentaria ~ Nivel_socioeconomico_hogar + 
    Zona_geografica + Numero_Personas_Hogar + Recursos_Financieros_Distintos_Ingreso_Laboral + 
    Edad_Jefe + Sexo_Jefe + Anios_Educ_Jefe, family = binomial, 
    data = datos)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.1891  -0.6708  -0.6708   1.3292   2.3109  

Coefficients:
                                     Estimate Std. Error z value Pr(>|z|)    
(Intercept)                         -1.564e
```

Los coeficientes de regresión son los parámetros del modelo que indican la relación entre cada variable independiente y la variable dependiente. Por ejemplo, si el coeficiente de regresión para la variable "Nivel_socioeconomico_hogar" es positivo, significa que a medida que el nivel socioeconómico del hogar aumenta, la probabilidad de presentar inseguridad alimentaria también aumenta. Si el coeficiente es negativo, significa que a medida que el nivel socioeconómico del hogar aumenta, la probabilidad de presentar inseguridad alimentaria disminuye.

Los intervalos de confianza son una medida de la incertidumbre en los coeficientes de regresión. Si el intervalo de confianza de un coeficiente de regresión no incluye el valor cero, significa que es estadísticamente significativo al nivel de confianza especificado (por ejemplo, al 95%). Si el intervalo de confianza incluye el valor cero, significa que no se puede concluir con certeza si hay una relación significativa entre la variable independiente y la variable dependiente.

Es importante tener en cuenta que el modelo de regresión logística solo proporciona información sobre la relación entre las variables y no necesariamente sobre causalidad. Además, es importante validar el modelo para asegurar que se ajusta adecuadamente a los datos y tiene buena capacidad predictiva. Para ello, se pueden utilizar técnicas como la validación cruzada o el análisis de residuos.


