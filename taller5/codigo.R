################################################################################
####################TALLER 5 ENMT 2#############################################
################################################################################
######Profesora: Juliana Helo ##################################################
#########Integrantes ###########################################################
##########Jose Ricardo Ricardo Hernandez - 202113889 ###########################
##########Andres Serrano - 202116783 ##########################################
##########Laura Rodriguez-202110325 ########################################

#----------------- #PARTE 1#--------------------------------#

#limpiamos el ambiente
rm(list = ls())
cat("\014")
#cargamos las librerias
require(pacman)
p_load(astsa, tidyverse, forecast, ggplot2, tseries, rio, fpp2, lubricate, writexl) 
# utilizamos la libreria astsa(Time series and its aplications).
# la cual tiene datos de los ingresos trimestrales de la empresa johnson and johnson
# La serie empieza desde el primer trimestre de 1960 hasta el ultimo trimestre de 1980
# Es decir, hay 84 trimestres en total.
# Motivacion: queremos evaluar las ganancias de la empresa  Johnson & Johnson 
# para determinar el ritmo de crecimiento y saber si la serie es estacionaria.
# De ser estacionaria, se podria concluir que los ingresos de  Johnson & Johnson son constantes.


Q_earnings <- ts(jj, start = 1960, end = 1980, frequency = 4)

#2#

autoplot(Q_earnings)+
  ggtitle("Ganancias trimestrales Johnson & Johnson")+
  xlab("Año")+
  ylab("Ganancias trimestrales por accion")+
  theme_classic()

#guardamos
setwd("C:/Users/richa/OneDrive - Universidad de los Andes/Universidad/octavo/Econometria 2/talleres/talleres-econometria-2-en-r/taller5/Resultados/")
ggsave("Grafico1.png", plot  = last_plot(),width = 16, height = 9)


#Descomponemos en patrones
plot(decompose(Q_earnings))

#guardar
ggsave("Grafico2.png", plot  = last_plot(), width = 16, height = 9)


#4#

ggseasonplot(Q_earnings, polar = TRUE)+
  ggtitle("Comportamiento polar Estacional")+
  xlab("Año")+
  ylab("Ganancias trimestrales por accion")+
  theme_bw()
#
ggsave("Grafico4.png", plot  = last_plot(), width = 16, height = 9)


#5# cargar otra serie de tiempo

usaGDP <- ts(GDP, start = 1960, end = 1980, frequency =4)

# podria haber una correlacion entre el crecimiento economico general de estados\
#unidos con el alto y creciente sharevalue.
cor(Q_earnings,usaGDP)  = 0.9585944


#unimos las bases de datos
 jj_GDP <- cbind(Q_earnings, usaGDP)
#grafico de los 2
 autoplot(jj_GDP, facets = TRUE)+
   xlab("Year") + ylab("") +
   ggtitle("Series de tiempo del PIB de EE.UU y ingresos por accion de J & J")

 #guardamos
 ggsave("Grafico6.png", plot = last_plot(), width = 16, height = 9)

 
#7#
 
 as.data.frame(jj_GDP)%>%
   ggplot(aes(x = usaGDP, y = Q_earnings))+
   geom_point()+
   xlab("USA PIB") + ylab("Ganancias por accion de Johnson & Johnson") +
   ggtitle("Grafico de puntos")+
   theme_classic()
#guardamos
 
 ggsave("grafico de puntos.png", plot = last_plot(), width = 16, height = 9)
 
 
 
 
 
 #8#
 print(acf(Q_earnings, lag.max = 10, type = "correlation" ,plot = FALSE))

acfQE <- acf(Q_earnings, lag.max = 10, type = "correlation", plot = FALSE )
#guardamos

write_xlsx(as.data.frame(cbind(acfQE$lag, acfQE$acf)), "acf.xlsx")
#grafico

ggAcf(Q_earnings,lag.max = 10, type = "correlation")

 
#9#

Smooth_QEarnings5 <- ma(Q_earnings, order = 5)
Smooth_QEarnings10 <- ma(Q_earnings, order = 10)
#grafico
autoplot(Q_earnings, series = "Data")+
  autolayer(Smooth_QEarnings5, series = "5-MA")+
  autolayer(Smooth_QEarnings10, series = "10-MA")+
  xlab("Año")+
  ggtitle("Serie con Suavizamiento")+
  ylab("Ganancias trimestrales por accion")+
  scale_colour_manual( values = c("Data"="grey50","5-MA"="red", "10-MA"="blue"),
                       breaks = c("Data","5-MA", "10-MA"))
ggsave("Grafico9.png", plot=last_plot(), width = 16, height =9)    

#10#
#aditiva
Q_earnings %>%
  decompose(type = "additive")%>%
  autoplot()+xlab("Año")+
  ggtitle("Descomposición Aditiva clasica para ganancias trimestrales por accion de Johnson & Johnson")

 ggsave("AdditiveDecomposition.png", plot = last_plot(), width = 16, height = 9)
#multiplicativa
 Q_earnings %>%
   decompose(type = "multiplicative")%>%
   autoplot()+xlab("Año")+
   ggtitle("Descomposición Multiplicativa clasica para ganancias trimestrales por accion de Johnson & Johnson")
 
 
 ggsave("MultiplicativeDecomposition.png", plot=last_plot(), width = 16,height = 9)
 
   
 #--------------- parte 2-----------------------------#
 require(pacman)
 p_load(haven, tidyverse, forecast, ggplot2, tseries, readxl, writexl, dplyr, 
        fable, fpp2, lubridate, mFilter, urca, astsa)
 #11# 
 
 Q_earnings <- ts(jj, start = 1960, end = 1980, frequency = 4)
 
 diff_Q_earnings<- diff(Q_earnings)
 
 ggAcf(Q_earnings, main="ACF Serie original")
 ggAcf(diff_Q_earnings, main="ACF serie difereciada 1 vez")

 
 #test Ljung Box para serie original
 Box.test(Q_earnings, type= "Ljung-Box")

#test Ljung Box para serie diferenciada
 
 Box.test(diff_Q_earnings, type= "Ljung-Box")
 
 #12 y 13#
 #test de raiz unitaria
 diff_Q_earnings%>% ur.kpss()%>%summary()
 
 # los resultados indican que no hay evidencia estadisticamente significativa de 
 # que la serie sea estacionaria
 # diferenciamos por segunda vez
 secondDiff <- diff(diff(Q_earnings))
secondDiff %>% ur.kpss() %>% summary()
# cuando diferenciamos 2 veces el resultado del test estadistico es menor a los 
# valores criticos. Por lo tanto, la serie diferenciada 2 veces es estacionaria.
#verificamos estacionariedad para test Ljung-box
Box.test(secondDiff, type = "Ljung-Box")
# De acuerdo con el test, el p-valor menor a 0.01.Por lo cual, rechazamos la hipotesis nula.
# Entonces, existe una correlacion entre los datos. No son independientes.

#14 y 15#
ggAcf(secondDiff, main="ACF para serie diferenciada 2 veces")
ggPacf(secondDiff, main= "PACF para serie diferenciada 2 veces")

#planteamiento de modelos
m1 <- arima(secondDiff, order = c(1,2,0))
m2 <- arima(secondDiff, order = c(1,1,0))
m3 <- arima(secondDiff, order = c(1,2,2))
m4 <- arima(secondDiff, order = c(2,2,5))
m5 <- arima(secondDiff, order = c(0,2,6))
m6 <- arima(secondDiff, order = c(2,2,0))
m7 <- arima(secondDiff, order = c(0,0,3))
m8 <- arima(secondDiff, order = c(0,1,0))
m9 <- arima(secondDiff, order = c(2,0,6))
#Posible modelo que escogeriamos M4. Dado el pacf, sabemos que hay que incluir 2 rezagos
# Sabemos que la serie estacionaria cuando diferenciamos 2 veces. Entonces, d=2 (ya la tenemos diferenciada 2 veces, no es necesario incluir d=2).
# Por ultimo, dado el numero de lags fuera del intervalo de confianza, escogemos q=6.

#criterio de seleccion AIC(seleccionamos el AIC mas pequeno)
listAIC <- c(m1$aic,m2$aic,m3$aic,m4$aic,m5$aic,m6$aic, m7$aic, m8$aic, m9$aic)
which.min(listAIC) #= m9
fitauto<-auto.arima(secondDiff, seasonal = FALSE, stepwise = FALSE)
fitauto
 #16 modelo de auto.arima(0,0,3) y modelo propuesto (2,0,6)

#comprobar que se comporten como ruido blanco y test de raiz unitaria
#(0,0,3):
checkresiduals(fitauto)
autoplot(fitauto)

#(2,0,6):
checkresiduals(m9)
autoplot(m9)+title("Test de raiz unitaria para ARIMA(2,0,6)")


#17#
#para el modelo (0,0,3)
autoplot(forecast(fitauto, h=8))
forecast(fitauto, h = 8, level = c(90, 95, 99))
write_xlsx(as.data.frame(forecast(fitauto,h = 8,level = c(90, 95, 99),... = "resultadoss2(003).xlsx")))

#para el modelo (2,0,6)
autoplot(forecast(m9), h=8)
write_xlsx(as.data.frame(forecast(fitauto, h=8, level=c(90, 95, 99))), "resultados206.xlsx")








