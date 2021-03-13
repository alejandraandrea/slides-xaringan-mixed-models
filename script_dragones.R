#Cargar librerías 
library(tidyverse) #Conjunto de paquetes para data science: ggplot2 tibble tidyr readr dplyr
library(readr)
library(dplyr)

#Leer y mirar los datos 
dragones <- read_tsv("dragons.tsv") #{readr} .tsv archivo de valores separados por tabulaciones 
glimpse(dragones) #{dplyr}. En {base} lo más similar es str().

#Limpieza y transformación de datos (¡ok!)

#Resumen preliminar de los datos
library(skimr)
skim(dragones) #Resúmenes de datos compactos y flexibles. En {base} lo más similar es summary()



#Análisis exploratorio de datos 
#Con operador pipe %>% de {dplyr} y ggplot
dragones %>% ggplot(aes(x=bodyLength, 
             y=testScore)) + 
             geom_jitter(alpha=.2) +
             theme_bw()

#Con más de una covariable continua, se recomienda estandarizar las covariables antes de continuar
dragons$bodyLength2 <- scale(dragons$bodyLength) #scale() de {base}

#Cargar paquetes
library(tidymodels) #Recomendado - Colección de paquetes para el modelado y análisis estadístico, que comparte la filosofía de diseño subyacente, la gramática y las estructuras de datos del tidyverse 
library(broom) #Alternativa -Resume información clave sobre objetos estadísticos en tidy tibbles (df ordenados) 

#Ajustar un modelo lineal 
ajuste_lm <- lm(testScore ~ bodyLength, data=dragones) #lm() de {stats}

#Resumir información sobre las estimaciones del modelo
tidy(ajuste_lm) #Con summary() en {base} 

#Resumir información sobre el modelo ajustado
glance(ajuste_lm)

#Graficar el ajuste del modelo
info_ajuste_lm<-augment_columns(ajuste_lm,dragones) #augment_columns() de {broom} agrega columnas con valores ajustados, residuos y otros resultados comunes 

info_ajuste_lm %>% 
            ggplot(aes(x=bodyLength, y=testScore)) + 
            geom_jitter(alpha=.2) + 
            geom_line(aes(x=bodyLength,y=.fitted))+
            theme_bw()


#Verificando los supuestos

#Linealidad
plot(dragons_lm, which=1) 
#Se refiere a la forma funcional lineal entre las variables explicativas y 
#la variable respuesta. Un gráfico de los residuos versus los valores ajustados será utilizado. 
#Si los residuos se dispersan al azar alrededor de la línea cero se sugiere que la suposición 
#de que la relación es lineal es razonable.

#Normalidad
plot(dragons_lm, which=2) #stats
#Independencia y normalidad: La independencia se refiere a que los errores son 
#independientes o no están correlacionados. La normalidad a que los residuos 
#siguen la distribución normal. Un gráfico Q-Q normal será utilizado. 
#Si los residuos se desvían de la línea recta, 
#entonces los residuos tienen colas más pesadas que la distribución normal.

#Homocedasticidad (varianzas iguales)
plot(dragons_lm, which=3)
#Homocedasticidad: Se refiere a que si los residuos tienen varianza constante a
#través de las variables explicativas (errores homocedásticos). 
#Un gráfico de localización-escala será usado. 
#Si la varianza de los residuos no es constante en función del valor ajustado, 
#hay evidencias de heterocedasticidad.


#Análisis exploratorio de datos
dragones %>% ggplot(aes(x = bodyLength, y = testScore, colour = mountainRange)) +
            geom_jitter(alpha=2) +
            theme_bw() 

dragones %>% ggplot(aes(bodyLength, testScore,colour = mountainRange))+
            geom_jitter(alpha=2) + 
            facet_wrap(~ mountainRange) +
            theme_bw()+
            theme(strip.background = element_rect(fill="white"))

dragones %>% 
  ggplot(aes(x=mountainRange, y=testScore, colour=mountainRange)) + 
  geom_boxplot(alpha=.5) +
  coord_flip()+
  theme_bw()


#Cargar paquetes
library(tidymodels) #Colección de paquetes para el modelado y análisis estadístico, que comparte la filosofía de diseño subyacente, la gramática y las estructuras de datos del tidyverse 
library(broom.mixed) #Sigue la línea del paquete broom para modelos mixtos, sin embargo, también aplica para modelos lineales
library(lme4) #Paquete para modelamiento con efectos mixtos

#Ajustar  un modelo lineal mixto
ajuste_lmer<- lmer(testScore ~ bodyLength + (1|mountainRange), data = dragones)


#Resumir información sobre las estimaciones del modelo
tidy(ajuste_lmer) #¡Sin p-valor! 

#Resumir información sobre el modelo ajustado
glance(ajuste_lmer)

#Adicionando p-valor
#Cargar paquete
library(lmerTest)

#Ajustar el modelo
ajuste_lmer<- lmer(testScore ~ bodyLength + (1|mountainRange), data = dragones)

#Resumir información sobre las estimaciones del modelo
tidy(ajuste_lmer) #¡Con p-valor! 


#Graficar el ajuste del modelo
info_ajuste_lmer <- augment_columns(ajuste_lmer, dragones)

info_ajuste_lmer %>% ggplot(aes(x=bodyLength,y=testScore,colour=mountainRange))+ 
          geom_jitter(alpha=2)+ 
          facet_wrap(~ mountainRange)+
          geom_line(aes(x=bodyLength,y=.fitted),colour="black")+
          theme_bw()


#Verificando los supuestos

#Normalidad del error
y.fit <- fitted(ajuste_lmer)
res.fit <- residuals(ajuste_lmer)
qqnorm(res.fit) 
qqline(res.fit)

#Normalidad del efecto aleatorio
pred.fit <- ranef(ajuste_lmer)[[1]][[1]]
qqnorm(pred.fit) 
qqline(pred.fit)

#Linealidad y homocedasticidad
y.fit <- fitted(ajuste_lmer)
res.fit <- residuals(ajuste_lmer)
plot(y.fit, res.fit) 
abline(h=0, lty=2,col="red")

#Incorporación de los sitios (efectos anidados)

ajuste_lmer_2<- lmer(testScore ~ bodyLength + (1|mountainRange/site), data = dragones)

ajuste_lmer_2<- lmer(testScore ~ bodyLength + (1|mountainRange) + (1|mountainRange:site), data = dragones)


