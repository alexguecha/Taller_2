library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(fBasics)
install.packages("fBasics")

library(MASS)
install.packages("MASS")

library(ggplot2)
install.packages("ggplot2")

install.packages("corrgram")
library(corrgram)
install.packages("gclus")
library(gclus)


## Resolucion punto 1 Taller 

BDD1 = read.table(file.choose(),sep = ";", head = T)


str(BDD1)


Ventas_ = BDD1$Ventas
Rtabla=data.frame(table(Ventas_))
porcentaje=prop.table(Rtabla[,2])
Rtabla2= cbind(Rtabla, porcentaje)
cum_frequencia=cumsum(Rtabla2[,2])
Rtabla3= cbind(Rtabla2, cum_frequencia)
cum_porcentaje=cumsum(Rtabla3[,3])
Rtabla4= cbind(Rtabla3, cum_porcentaje)
Rtabla4



#######

NumCuentas = BDD1$Numero.de.Cuentas
Rtabla=data.frame(table(NumCuentas))
porcentaje=prop.table(Rtabla[,2])
Rtabla2= cbind(Rtabla, porcentaje)
cum_frequencia=cumsum(Rtabla2[,2])
Rtabla3= cbind(Rtabla2, cum_frequencia)
cum_porcentaje=cumsum(Rtabla3[,3])
Rtabla4= cbind(Rtabla3, cum_porcentaje)
Rtabla4


CrossTable(BDD1$Numero.de.Cuentas,BDD1$Número..de..Competidores)


# Gráfico Simple (X,Y)
ggplot(data = BDD1, aes(x = Numero.de.Cuentas, y = Número..de..Competidores)) + 
  geom_point(colour = "red4") +
  ggtitle("Diagrama de dispersión Numero de cuentas vs numero de competidores") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



# Midiendo el nivel de correlación:
cor(x = BDD1$Numero.de.Cuentas, y = BDD1$Número..de..Competidores, method = "pearson")


######
#reconocimiento de la base de datos

head(BDD1)

####

knitr::kable(
  round(cor(x = BDD1, method = "pearson"), 3)
)



####

corrgram(BDD1, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Matriz de Correlaciones")

###

modelo1 <- lm(Ventas ~ InvPublic + NumCuentas + NumCompetidores, data = BDD1 )

summary(modelo1)

Predicciones1<-modelo1$fitted.values

library(ggplot2)
ggplot(data = BDD1, aes(x = predict(modelo1), 
                         y = abs(rstudent(modelo1))))+
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed")+
  geom_point(aes(color = ifelse(abs(rstudent(modelo1)) > 2, "red", "black")))+
  scale_color_identity()+
  labs(title = "Distribución de los residuos estudentizados", 
       x = "Predicción modelo", 
       y = "Residuos estudentizados")+
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))



##Modelo definitivo

step(object = modelo1, direction = "both", trace = 1)

modeloDef <- lm(Ventas ~ ClasificacionMercado ,data = BDD1 )

summary(modeloDef)

PrediccionesDef<-modeloDef$fitted.values

ggplot(data = BDD1, aes(x = predict(modeloDef), 
                        y = abs(rstudent(modeloDef))))+
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed")+
  geom_point(aes(color = ifelse(abs(rstudent(modeloDef)) > 2, "red", "black")))+
  scale_color_identity()+
  labs(title = "Distribución de los residuos estudentizados", 
       x = "Predicción modelo", 
       y = "Residuos estudentizados")+
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


## Segun el resultado arrojado por la función step(object = modelo1, direction = "both", trace = 1), 
##se puede observar que el modelo con mejor R cuadrado corresponde al ejercio 
#de correlacionar unicamente la variable Y = ventas con la variable X = Claficación de mercado
# lo anterior nos permite concluir que la inversión en publicidad no es significante para el resultado de las ventas ni  las otras variables aportan informaión al modelo
## no obstante la variable que mejor explica las ventas es la calificación potencial del distrito
# dicho de otra manera el 54% de las veces la calificación del distrito predice el comportamiento de las ventas


## 


# Predicciones

Y_dataset=datos$esp_vida
Y_modelad=Predicciones
Comparativo=cbind(Y_dataset, Y_modelad)
Comparativo

Diferecias=Y_dataset-Y_modelad
Errores=cbind(Comparativo, Diferecias)
Errores


###
install.packages("GGally")
library(GGally)
ggpairs(BDD1, lower = list(continuous = "smooth"),diag = list(continuous = "bar"), axisLabels = "none")


## aqui evidenciamos una alta correlación positiva (0.744) con un alto nivel de significacia
## por otro lado, se evidencian altos niveles de correlacion entre las variables numero y inversión en publicidad y numero y ventas
# no obstante esta correlación anterior no cabe lugar a analisis porque es una variable que cumple con la función de etiqueta


