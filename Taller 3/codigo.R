################################################################################
####################TALLER 3 ENMT 2#############################################
################################################################################
######Profesora: Juliana Helo ##################################################
#########Integrantes ###########################################################
##########Jose Ricardo Ricardo Hernandez - 202113889 ###########################
##########Andres Serrano - 202116783 ##########################################
##########Laura Rodriguez-202110325 ########################################

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
#    
modeloPrimerasDiferencias <- plm(log(rent)~ y90+log(pop)+log(avginc)+pctstu, data= paneldata, model = "fd")
   
               #Supuesto necesario: cov(Dlnrent,Derror)=cov(DlogPop,Derror)=cov(DlogAvgInc, Derror)= 0
   
   
 #     exportamos con Rdmarkdown(el archivo markdown esta en resultados/Panel)
   
   
   
                    ##d)### 
require(pacman)   
   p_load(dplyr, stargazer, rmarkdown, tinytex) # utilizamos el paquete dplyr para manipular datos eficientemente
   
#   1) creamos las variables within   
   dataWithin <- paneldata %>% 
     group_by(city) %>% # agrupamos el promedio de cada variable por individuo(municipio) 
     mutate(
       Within_logrent= log(rent)-mean(log(rent)),
       Within_logPop = log(pop)- mean(log(pop)),
       Within_pctstu = pctstu - mean(pctstu),
       Within_logAvginc = log(avginc) - mean(log(avginc)),
       Within_y90    = y90 - mean(y90)) %>% #utilizamos mutate(libreria dplyr) para crear variables deacuerdo a varias operaciones de calculo. En
      ungroup()      # en este caso restamos el promedio 

   
# 2) corremos los 2 modelos: 1 sin y90 y con y90
   modelowithin1 <- lm(Within_logrent~Within_logPop+Within_logAvginc+Within_pctstu+Within_y90, data = dataWithin)
   modelowithin2 <- lm(Within_logrent~Within_logPop+Within_logAvginc+Within_pctstu, data = dataWithin)
   
# 3) visualizamos y exportamos los archivos. exportamos con rmarkdown (el procedimiento en rmark down es el mismo que realizaamos aca.)
   stargazer(modelowithin1, modelowithin2, type="latex", header=FALSE, escape=FALSE)
rmarkdown::render("punto 1 d.Rmd")



                  ##e)## 
# dado que en r no existe el comando xtreg,vamos a utilizar la libreria plm
require(pacman)
p_load(stargazer, rio, plm, tinytex)

EF_modelo1 <- plm(log(rent)~log(pop)+log(avginc)+pctstu+y90, data = paneldata, model = "within")

EF_Modelo2 <- plm(log(rent)~log(pop)+log(avginc)+pctstu, data = paneldata, model="within")   
 # exppportamos con rmarkdown
stargazer(EF_modelo1, EF_Modelo2, type="latex", title = "Efectos fijos con plm/xtreg",
          header = FALSE,
          escape = FALSE)


    #los supuestos necesarios para que sea consistente el estimador es que no
#  exista relacion entre el efecto within de las variables explicativas con 
#   el efecto within del error.  Cov(withinlog(pop), within_error)=cov(withinlog(avginc),within_error) =cov(whihinpctstu,within_error)= cov(withiny90, withinerror)=0



                ## f)##
require(pacman)
p_load(stargazer, dplyr, plm, rio)  #usamos libreria plm, la cual nos permite realizar las 2 regresiones


EfectosFijosindividual <- plm(log(rent)~log(pop)+log(avginc)+pctstu+y90,
                              data=paneldata, 
                              model = "within",
                              effect="individual")

EfectosFijosTwoWay <- plm(log(rent)~log(pop)+log(avginc)+pctstu, 
                          data=paneldata,
                          model="within",
                          effect = "twoways")
                ##exportamos con rmarkdown

   rmarkdown::render("punto 1 f.Rmd", output_format = "pdf")
   
   #explicacion: si solo usamos efectos fijos por ciudad se mantiene en control las diferencias estructurales 
   # entre ciudades, pero no varia en el tiempo. Los dos modelos son iguales porque ambos restan el promedio por ciudad
 # En la regresion twoway-fixed effects no se estima y90 por ser una variable que es fija en el tiempo.
   
   
                  ##g##
   
  # Tenemos 4 modelos, por lo cual, vamos a indicar el estimador de el logaritmo del la poblacion respecto al logaritmo de la renta
  # para el modelo de ols: el estimador es 0.041. Sin embargo, no es estadisticamente siginifativo puesto que dos veces 0.023(error estandar) es mayor al estimador.  
  #                        por lo tanto, no se puede realizar inferencia.
   
   
  # Para el modelo de primeras diferencias: Al igual que en ols, el estimador de primeras diferencias para el logaritmo de la poblacion 
  #                                         no es estadisticamente significativo. Esto puede ser debido a que hay menor muestra, luego 
  #                                        la variabilidad de los datos aumenta.
  #
   
  # Para el modelo within manual: A diferencia de FD, aplicar effectos within no reduce la informacion. 
  #                               De acuerdo con el primer modelo, no podemos afirmar nada sobre el estimador de within_logPop.
  #                               Sin embargo, el segundo modelo within, donde no incluimos la variable y90, estima que un aumento del 1% de la poblacion
  #                               aumenta en 0.297% el precio de alquiler, manteniendo las otras variables constantes. Este estimador es significativo al 5% de significancia
   
  # para el modelo within xtreg: ningun estimador dentro de la variable logpop es significativo.                              
  #
   
  
   
   ##  BONO. Realizado en rmarkdown. No obstante se pone el codigo del proceso acontinuacion
   # nota: rmarkdown permite usar latex con la libreria tinytex, por lo cual la demostracion cumple con Latex.
   
   
<<<<<<< HEAD
  #####################PUNTO 1 #################################################  
   rm(list = ls())

   dirBaseDD <- "C:/Users/richa/OneDrive - Universidad de los Andes/Universidad/octavo/Econometria 2/talleres/talleres-econometria-2-en-r/Taller 3/bases/Base_Taller_DD.dta"   
  require(pacman)
  p_load(rio, stargazer, did) 
baseDD <- import(dirBaseDD)  


                 ##b##   
modeloDD <- lm(duration ~ highearn + after_1980+ highearn*after_1980, data=baseDD)
stargazer(modeloDD, type="text")


                ##c)###
#supuesto de tendencias paralelas
#necesitamos el contrafactual


        ###d)##
modeloDD2 <- lm(log_duration~highearn + after_1980 + highearn*after_1980, data=baseDD)
modeloDD3 <- lm(log_duration~highearn + after_1980 + highearn*after_1980 +
                  male + married + hosp + age + lprewage, data= baseDD)


stargazer(modeloDD2, modeloDD3, type="latex")
=======
   
>>>>>>> 071291ec2e49c560564eab42b7b85f86785bbdb6
