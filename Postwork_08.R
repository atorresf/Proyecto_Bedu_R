"DESARROLLO
Un centro de salud nutricional está interesado en analizar estadísticamente y 
probabilísticamente los patrones de gasto en alimentos saludables y no saludables 
en los hogares mexicanos con base en su nivel socioeconómico, en si el hogar tiene 
recursos financieros extrar al ingreso y en si presenta o no inseguridad alimentaria. 
Además, está interesado en un modelo que le permita identificar los determinantes 
socioeconómicos de la inseguridad alimentaria.

La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012) 
levantada por el Instituto Nacional de Salud Pública en México. La mayoría de las 
personas afirman que los hogares con menor nivel socioeconómico tienden a gastar 
más en productos no saludables que las personas con mayores niveles socioeconómicos 
y que esto, entre otros determinantes, lleva a que un hogar presente cierta inseguridad 
alimentaria.

La base de datos contiene las siguientes variables:"
  
#- Nivel_socioeconomico_hogar (valores: 1 - "Bajo", 2 - "Medio bajo", 3 - "Medio", 4 - "Medio alto", 5 - "Alto")
#- Zona_geografica (valores: 0 "Zona urbana", 1 "Zona rural")
#- Numero_Personas_Hogar
#- Recursos_Financieros_Distintos_Ingreso_Laboral (valores: 0 - "no", 1 - "sí")
#- Edad_Jefe
#- Sexo_Jefe (valores: 0 - "Hombre", 1 - "Mujer")
#- Anios_Educ_Jefe 
#- Inseguridad_Alimentaria (valores: 0 - "No presenta Inseguridad Alimentaria", 1 - "Presenta Inseguridad Alimentaria")
#- Log_Natural_Gasto_Alimentos_Saludables
#- Log_Natural_Gasto_Alimentos_No_Saludables


#2. Realiza un análisis descriptivo de la información

# Cargar librerías
library(tidyverse)
library(DescTools)
# Cargar base de datos
df <- read.csv("https://raw.githubusercontent.com/atorresf/BEDU_Sesion8/main/inseguridad_alimentaria_bedu.csv")
df.orig <- df
# Revisar formato y estructura de la base de datos
str(df)
glimpse(df)
head(df)
View(df)
# Limpiar y preparar la base de datos
# Eliminar filas con valores faltantes
df <- na.omit(df)
df.limp <- df

# Reemplazar valores faltantes por el valor medio
df.completar <- df$Log_Natural_Gasto_Alimentos_Saludables[is.na(df$Log_Natural_Gasto_Alimentos_Saludables)] <- mean(df$Log_Natural_Gasto_Alimentos_Saludables, na.rm = TRUE)


# (ejemplo: eliminar duplicados, corregir valores faltantes, recodificar variables)
df <- df %>%
  distinct() %>%
  mutate(Nivel_socioeconomico_hogar = factor(Nivel_socioeconomico_hogar,levels=1:5,labels = c("Bajo","Medio bajo","Medio","Medio alto","Alto"),ordered = TRUE),
         Zona_geografica = factor(Zona_geografica, labels = c("Zona urbana","Zona rural")),
         Recursos_Financieros_Distintos_Ingreso_Laboral = factor(Recursos_Financieros_Distintos_Ingreso_Laboral,labels = c("no","sí")),
         Sexo_Jefe = factor(Sexo_Jefe, labels = c("Hombre","Mujer"),exclude = NA),
         Inseguridad_Alimentaria = factor(Inseguridad_Alimentaria, labels = c("No presenta Inseguridad Alimentaria","Presenta Inseguridad Alimentaria")))
View(df)

# Resumen de estadísticas básicas
summary(df)

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

# Contar ocurrencias de cada valor de la variable Nivel_socioeconomico_hogar
table(df$Nivel_socioeconomico_hogar)

#Mostrar Gráficas
# Histograma de la variable Log_Natural_Gasto_Alimentos_Saludables
hist(df$Log_Natural_Gasto_Alimentos_Saludables)

# Visualización de resultados
barplot(table(df$Nivel_socioeconomico_hogar), main = "Nivel socioeconómico de los hogares")

# Generar gráfico de barras para comparar la proporción de hogares con inseguridad alimentaria según el nivel socioeconómico
ggplot(df, aes(x = Nivel_socioeconomico_hogar, fill = Inseguridad_Alimentaria)) +
  geom_bar() +
  xlab("Nivel socioeconómico del hogar") +
  ylab("Proporción de hogares") +
  scale_fill_discrete(name = "Inseguridad alimentaria", labels = c("No presenta inseguridad alimentaria", "Presenta inseguridad alimentaria"))

# Generar gráfico de barras para comparar la proporción de hogares con inseguridad alimentaria según el nivel socioeconómico y la zona geográfica
ggplot(df, aes(x = Zona_geografica, fill = Inseguridad_Alimentaria)) +
  geom_bar(position = "dodge") +
  facet_grid(Nivel_socioeconomico_hogar ~ .) +
  xlab("Zona geográfica") +
  ylab("Proporción de hogares") +
  scale_fill_discrete(name = "Inseguridad alimentaria", labels = c("No presenta inseguridad alimentaria", "Presenta inseguridad alimentaria"))


#3. Calcula probabilidades que nos permitan entender el problema en México
#- Probabilidad de que un hogar tenga un nivel socioeconómico determinado:
  prob_nse <- table(df.orig$Nivel_socioeconomico_hogar)/nrow(df.orig)
  names(prob_nse)=c("Bajo","MedioBajo","Medio","MedioAlto","Alto")
  prob_nse
#Con esto obtenemos la probabilidad de que un hogar tenga cada uno de los niveles socioeconómicos disponibles.

#-	Probabilidad de que un hogar tenga recursos financieros extra:
  prob_extra <- sum(df.orig$Recursos_Financieros_Distintos_Ingreso_Laboral == 1)/nrow(df.orig)
  prob_extra
#Con esto obtenemos la probabilidad de que un hogar tenga recursos financieros extra (1) o no (0).

#-	Probabilidad de que un hogar tenga inseguridad alimentaria por Nivel socioeconomico:
  
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
#Con esto obtenemos la probabilidad de que un hogar tenga inseguridad alimentaria (1) o no (0).


#4. Plantea hipótesis estadísticas y concluye sobre ellas para entender el problema en México
#En promedio el gasto de alimentos no saludables es mayor cuando el jefe de familia es hombre que cuando el jefe de familia es mujer a un NC 95%
#Ho: Log_Natural_Gasto_Alimentos_No_Saludables_Hombre <= Log_Natural_Gasto_Alimentos_No_Saludables_Mujer
#Ha: Log_Natural_Gasto_Alimentos_No_Saludables_Hombre > Log_Natural_Gasto_Alimentos_No_Saludables_Mujer
  
  datos <- read.csv("https://raw.githubusercontent.com/atorresf/BEDU_Sesion8/main/inseguridad_alimentaria_bedu.csv")
  datos <- subset(datos, !is.na(Sexo_Jefe))
  var.test(datos[datos$Sexo_Jefe == 0, "Log_Natural_Gasto_Alimentos_No_Saludables"],
           datos[datos$Sexo_Jefe == 1, "Log_Natural_Gasto_Alimentos_No_Saludables"],
           ratio = 1, alternative = "two.sided")
  
 t.test(x = datos[datos$Sexo_Jefe == 0, "Log_Natural_Gasto_Alimentos_No_Saludables"],
         y = datos[datos$Sexo_Jefe == 1, "Log_Natural_Gasto_Alimentos_No_Saludables"],
         alternative = "greater", mu = 0, var.equal = TRUE)
  
# A un nivel de confianza de 95% el p-value < 2.2e-16 por lo que se rechaza la hipotesis Ho, y se concluye que en promedio el   
  #el gasto es mayor en alimentos no saludables cuando el jefe de familia es hombre que mujer
  
 
#Segunda hipótesis
# Cargar la base de datos
  datos <- read.csv("https://raw.githubusercontent.com/atorresf/BEDU_Sesion8/main/inseguridad_alimentaria_bedu.csv")
  
# Formular hipótesis nula y alterna
# Ho: No existe una diferencia significativa en los gastos en alimentos no saludables entre hogares de diferentes zonas geográficas."
# Ha: Los hogares ubicados en zonas urbanas tienen gastos significativamente mayores en alimentos no saludables que los hogares ubicados en zonas rurales.
  
# Realizar prueba t para comparar gastos en alimentos no saludables entre hogares de diferentes zonas geográficas
  resultado <- t.test(Log_Natural_Gasto_Alimentos_No_Saludables ~ Zona_geografica, data = datos, var.equal = TRUE)
  
# Consultar p-valor para determinar si se rechaza o no la hipótesis nula
  resultado$p.value
  
# Si el p-valor es menor que 0.05, se rechaza la hipótesis nula y se acepta la hipótesis alterna
# Si el p-valor es mayor o igual que 0.05, no se puede rechazar la hipótesis nula y se concluye que no hay evidencia suficiente para aceptar la hipótesis alterna
  
# Interpretar resultado de acuerdo a p-valor
# pvalue = 1.858575e-175
"En el ejemplo anterior, el p-value es menor que 0.05, se concluye que existe una 
  diferencia significativa en los gastos en alimentos no saludables entre hogares 
  de diferentes zonas geográficas. Los hogares ubicados en zonas urbanas tienen 
  gastos significativamente mayores en alimentos no saludables que los hogares 
  ubicados en zonas rurales."

--
  
#5. Estima un modelo de regresión, lineal o logístico, para identificiar los determinanres de la inseguridad alimentaria en México
#Se cargan los datos
datos <- read.csv("https://raw.githubusercontent.com/atorresf/BEDU_Sesion8/main/inseguridad_alimentaria_bedu.csv")

#Se construye el modelo de regresión logística con la función glm
modelo <- glm(Inseguridad_Alimentaria ~ Nivel_socioeconomico_hogar + Zona_geografica + Numero_Personas_Hogar + Recursos_Financieros_Distintos_Ingreso_Laboral + Edad_Jefe + Sexo_Jefe + Anios_Educ_Jefe, data = datos, family = binomial)

#Podemos revisar la salida del modelo y concluir.
summary(modelo)


