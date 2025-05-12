################################################################################
####################TALLER 5 ENMT 2#############################################
################################################################################
######Profesora: Juliana Helo ##################################################
#########Integrantes ###########################################################
##########Jose Ricardo Ricardo Hernandez - 202113889 ###########################
##########Andres Serrano - 202116783 ##########################################
##########Laura Rodriguez-202110325 ########################################

                    #PARTE 1#

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

Smooth_QEarnings5 <- ma(Q_earnings, order = 4)
Smooth_QEarnings20 <- ma(Q_earnings, order = 10)
#grafico
autoplot(Q_earnings, series = "Data")+
  autolayer(Smooth_QEarnings, series = "5-MA")+
  autolayer(Smooth_QEarnings20, series = "10-MA")+
  xlab("Año")+
  title("Serie con Suavizamiento")+
  ylab("Ganancias trimestrales por accion")+
  scale_colour_manual( values = c("Data"="grey50","5-MA"="red", "10-MA"="blue"),
                       breaks = c("Data","5-MA", "10-MA"))
ggsave("Grafico9.png", plot=last_plot(), width = 16, height = )    

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
 
   
 
 
 
 
 
 
 
 
 

