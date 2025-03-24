################################################################################
####################TALLER 3 ENMT 2#############################################
################################################################################
######Profesora: Juliana Helo ##################################################
#########Integrantes ###########################################################
##########Jose Ricardo Ricardo Hernandez - 202113889 ###########################
##########Andres Serrano - 202116783 ##########################################


#####################PUNTO 1 #################################################


rm(list=ls())
require(pacman)
p_load(rio, stargazer, rmarkdown)

dirBases <- "C:/Users/richa/OneDrive - Universidad de los Andes/Universidad/octavo/Econometria 2/talleres/talleres-econometria-2-en-r/Taller 3/bases/Panel.dta"


paneldata <- import(dirBases)

                      #a)#

modeloPOOLEDOLS <- lm(log(rent)~y90+log(pop)+log(avginc)+ pctstu , data= paneldata)
stargazer(modeloPOOLEDOLS, type="text")   
 
    #Supuestos necesarios: 1.linealidad en los parametros
#                          2.muestreo aleatorio
#
#                          3. exogeneidad estricta. las variables explicativas no estan correlacionadas con el error y efecto fijo

#                          4. homocedasticidad. la varianza del error es constante para las variables explicativas
#                          # si se viola este supuesto, el estimador es todavia consistente pero no eficiente.

#                         5. No colinealidad perfecta


                      ##b)##

modeloPOOLEDOLS2 <- lm(log(rent)~+log(pop)+log(avginc)+ pctstu , data= paneldata)



      #exportamos el documento Rmarkdown(se usa para graficar en pdf y se encuentra en carpeta resultados panel
rmarkdown::render("punto 1 a y b.Rdm")


## ¿que captura delta?## delta captura el efecto del precio de la renta entre los años 80' y 90'.
#  en este caso, la renta en los años 90' es un 26.2% mayor que en el año 80

                          
                      ##c)##
# instalamos el paquete plm para realizar estimacion por medio de primeras diferencias
#     1) realizamos las primeras diferencias para cada variable.

   Dlnrent<- diff(paneldata$lrent)

   Dy90 <- diff(paneldata$y90)

   DlogPop <- diff(log(paneldata$pop))
   
   DlogAvgInc <- diff(log(paneldata$avginc))
  
   Dpctstu <- diff(paneldata$avginc)


#     2) juntamos todas las variables que son diferencias y estimamos la regresion
   
   dataprimerasdiferencias <- as.data.frame(cbind(Dy90, Dlnrent,DlogPop,DlogAvgInc,Dpctstu))

   modeloPrimerasDiferencias <- lm(Dlnrent~ Dy90+DlogPop+DlogAvgInc+Dpctstu, data = dataprimerasdiferencias)
   
   
               #Supuesto necesario: cov(Dlnrent,Derror)=cov(DlogPop,Derror)=cov(DlogAvgInc, Derror)= 0
   
   
 #     exportamos con Rdmarkdown(el archivo markdown esta en resultados/Panel)
   
   
   
   
   
   
   
   
   