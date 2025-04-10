###############################################################################
######Taller 2 practico econometria 2##########################################
###################2025-1######################################################
###Profesora: Juliana Helo ####################################################
#Integrantes
# Jose Ricardo Ricardo Hernandez - 202113889
#Andres Serrano - 202116783
###############################################################################
###############################################################################                              
                              #PUNTO 1#
###############################################################################
#cargamos la base de de datos "BaseP1.dta"
require(pacman)
p_load(rio, gridExtra, rmarkdown, knitr)
setwd("C:/Users/richa/OneDrive - Universidad de los Andes/Universidad/octavo/Econometria 2/taller 2/taller2 practico/New folder/talleres-econometria-2-en-r/bases
      ")
base1 <- import("C:/Users/richa/OneDrive - Universidad de los Andes/Universidad/octavo/Econometria 2/taller 2/taller2 practico/New folder/talleres-econometria-2-en-r/bases/BaseP1.dta")

#########a) estadisticas descriptivas para variables continuas

varcont <- subset(base1, select = c(lwage, exper, motheduc, fatheduc, educ))
edc<-list(summary(varcont))
#creamos una base de datos de variables deseadas, guardamos en una tabla y exportamos en rmarkdown
edc<- as.data.frame(t(sapply(varcont, summary)))


kable(edc, caption="Estadísticas Descriptivas para variables Continuas")
export(edc, "resultados/estadisticas_descriptivas.xlsx")
setwd("C:/Users/richa/OneDrive - Universidad de los Andes/Universidad/octavo/Econometria 2/taller 2/taller2 practico/New folder/talleres-econometria-2-en-r/resultados")
render("edc1.Rmd", output_format = "pdf_document")
## ahora realizamos una tabla con estadisticas descriptivas en variables categoricas

#extraemos variables categoricas(debido a que sin variables categoricas, poco nos sirve utilzar summary. Por ello vamos a extraer el porcentaje)

varcat <- subset(base1, select=c(nearc4, black, married))
variablescat<- c("nearc4","black", "married")
varpercent <- list()
for (var in colnames(varcat)) {

  varpercent[[var]] <- prop.table(table(varcat[[var]]))
  
}

#exportamos
df_varpercent <- do.call(cbind, varpercent)
export(df_varpercent, "edd.xlsx")

########resultados del punto a)#######################
#Estadísticas Descriptivas para Variables Continuas
#        Min.	   1st Qu.	  median	  Mean	     3rd Qu.	Max.
#lwage	4.60517	 5.991465	6.309918	6.285423	6.580639	7.784889
#exper	0.00000	 6.000000	8.000000	8.292342	10.000000	22.000000
#motheduc0.0000	 8.000000	12.000000	10.608108	12.000000	18.000000
#fatheduc	0.000  8.000000	10.000000	10.078829	12.000000	18.000000
#educ	1.00000	  12.000000	13.000000	13.627477	16.000000	18.000000

#estadisticas descriptivas para variables discretas

#     nearc4	   black	      married
# ==0  0.311261261	0.841441441	0.279279279
# ==1  0.688738739	0.158558559	0.720720721


#descripcion y pregunta del enunciado: el minimo de experiencia son 0 años, mientras que el maximo es 22. En promedio se obtiene 8 años.
# para la eduacion del padre y la madre, su media es de 10 años y 12 años respectivamente. El padre con mayor experiencia cuenta con 18 años, mientras que el padre que cuenta con menor experiencia de cero años.
#Igualmente, tanto la madre con mayor años de experiencia como la madre con menos años de experiencia es igual a la del padre.

#el numero de personas que no son de raza negra conforman el 84.14 porciento, mientras que el 15.8 si son esta raza.
# el estado civil de casado conforman el 72.07 porciento de la muestra, mientras que el 27,9 porciento esta no casado.

#Que pasa si se omiten datos? podemos tener un problema de variable omitida, donde el error captura el efecto de la variable omitida. Considerando lo anterior, si imitimos las variables podemos tener un problema de endogeneidad.

###b)###########
#vamos a estimar el parametro para la regresion Yi= alpha + b1Si+gammaXi+ei  

p_load(stargazer, dplyr)
base1df <- as.data.frame(base1)

modelo1 <- lm(lwage ~ educ + exper + black + married + fatheduc + motheduc, data = base1df) 
resultadosm1 <- stargazer(... = modelo1, type="text")

#interpretacion#

#===============================================
#  Dependent variable:    lwage
#  ---------------------------
#             
#-----------------------------------------------
# educ                         0.075***          
#  (0.004)          

#exper                        0.040***          
#  (0.003)          

#black                        -0.172***         
#  (0.024)          

#married                      0.143***          
#  (0.019)          

#fatheduc                       0.004           
#(0.003)          

#motheduc                     0.010***          
#  (0.004)          

#Constant                     4.712***          
#  (0.076)          

-----------------------------------------------
 # Observations                   2,220           
#R2                             0.229           
#Adjusted R2                    0.227           
#Residual Std. Error      0.387 (df = 2213)     
#F Statistic          109.724*** (df = 6; 2213) 
#===============================================
#  Note:               *p<0.1; **p<0.05; ***p<0.01

# dado que hay otra(s) variables que expliquen el logaritmo del ingreso (habilidad innata), hay algo en el error que tiene relacion con los años de educacion.
#por lo tanto, hay un problema de endogeneidad. Al existir un problema de endogeneidad, la estimacion de los años de educacion esta sesgada y inconsistente.
#como probar?
# forma teorica. 1) evualuamos el modelo incorrecto en el estimador de b1.
#                2) Utilizamos propiedades y llegamos a sesgamiento y inconsistencia
#                3) Si el efecto de la habilidad inata es positiva respecto a los años de educación,entonces estamos sobreestimando. Infraestimando cuando sucede lo contrario
#

# forma empirica
#               1) sabemos que al existir endogeneidad sucede que E[u*|s]!=0, donde u*=u+b2Habilidadinnata 


#               2) obtenemos los errores del modelo


residuales <- modelo1$residuals



#               3) obtenemos si existe una relacion entre Si y el error.

                cor.test(base1df$educ, residuales)

#               4) si la correlacion es positiva o negativa(distinta de cero) entonces probablemente hay un problema de endogeneidad
                cor.test(base1df$educ, residuales) >0 ====> entonces hay endongeeidad
                
###### C)##########       
# el metodo de variables instrumentales nos permite eliminar el sesgo generado por una variable endogena.
#cuando utilizamos una variable instrumental queremos eliminar la endogeneidad por medio de una variable instrumental Zi.
#esta variable debe cumplir 2 caractaristicas fundamentales al mismo tiempo, relevancia y exogeneidad. 
# Cuando un instrumento es relevante, decimos que existe una relacion entre la variable endogena y el instrumento.
#igualmente, cuando el instrumento zi no tiene relacion con el error, entonces decimos que es exogeno. Por lo tanto,
# la interacion entre Zi solo afecta a Yi por medio de la variable endogena y garantizando exogeneidad.
                
                
#########d)############
#cuando utilizamos la variable dicotoma nearc4 podemos concluir que el instrumento cumple las 2 propiedades fundamentales.
# relevancia: Existe una relacion entre los años de educacion y si existe una universidad cerca al condado.
                cov(base1df$nearc4, base1df$educ) = 0.1507828 >0
                
#exogeneidad: En el anterior caso, concluimos que hay un endogeneidad cuando E[u*|s]!= 0, donde u*=u+b2Habilidadinnata
#             Por lo cual, se puede argumentar que si un condado cuenta con una universidad no necesariamente esto afecta la habilidad innata.
                
                
                
######## e) ###########
modelo1 <- lm(lwage ~ educ + exper + black + married + fatheduc + motheduc, data = base1df) 
resultadosm1 <- stargazer(... = modelo1, type="text")

#modelo con variable instrumental
# paquetes necesarios
require(pacman)
p_load(stargazer, AER)
modelo2 <- ivreg(lwage ~ educ  + exper + black + married + fatheduc + motheduc| nearc4 + exper + black + married + fatheduc + motheduc, data = base1df)         
                
#comparamos modelos
resultadosIv <- stargazer(modelo1, modelo2, type="text")
#exportamos con rmarkdown
setwd("C:/Users/richa/OneDrive - Universidad de los Andes/Universidad/octavo/Econometria 2/taller 2/taller2 practico/New folder/talleres-econometria-2-en-r/resultados")
rmarkdown::render("resultadosIv.Rmd",output_format = "pdf_document")

#interpretacion
#======================================================================
  #Dependent variable:    lwage        
  --------------------------------------
#                           
#                  OLS                     instrumental
#                  variable  
#                          (1)                (2)     
----------------------------------------------------------------------
# educ                 0.075***            0.330***  
#                       (0.004)            (0.090)   

#exper                 0.040***            0.128***  
#                       (0.003)            (0.031)   

#black                 -0.172***           -0.084*   
#                      (0.024)            (0.049)   

#married               0.143***             0.070*   
#                       (0.019)            (0.039)   

#fatheduc              0.004             -0.027**  
#                     (0.003)            (0.012)   

#motheduc             0.010***            -0.024*   
#                      (0.004)            (0.013)   

#Constant            4.712***             1.215    
#                     (0.076)            (1.236)   

----------------------------------------------------------------------
#  Observations                              2,220              2,220    
#R2                                        0.229              -0.948   
#Adjusted R2                               0.227              -0.953   
#Residual Std. Error (df = 2213)           0.387              0.615    
#F Statistic                     109.724*** (df = 6; 2213)             
#======================================================================
#  Note:                                      *p<0.1; **p<0.05; ***p<0.01

#  ¿Es coherente el resultado con el sentido del sesgo por variable omitida?
#es decir ¿creer que la omisi´ on de la habilidad innata sobre-estimar´ ıa o sub
#estimar´ ıa el rendimiento de la educaci´on sobre los ingresos?

#respuesta: 
#  BONO:¿Por qu´e cree entonces que los retornos a la educaci´on sean mayores
#para los compliers que para la poblaci´on en general? Relacione la respuesta
#a este punto con el estimador IV LATE.

#respuesta 


###f)### primera etapa

modelo1eraetapa <- lm(educ ~ nearc4 +  exper + black + married + fatheduc + motheduc, data= base1df)
resultados1eraetapa <- stargazer(modelo1eraetapa, type="latex", escape= FALSE, header= FALSE)
#exportamos
render("resultados1eraetapa.Rmd", output_format="pdf_document")
#interpretacion
# ===============================================
#Dependent variable:    
  ---------------------------
#                              educ            
-----------------------------------------------
#  nearc4                       0.316***          
#                             (0.086)          

#exper                        -0.341***         
#                             (0.011)          

#black                        -0.332***         
#                              (0.116)          

#married                      0.286***          
#                             (0.091)          

#fatheduc                     0.116***          
#                             (0.014)          

#motheduc                     0.133***          
#                             (0.017)          

#Constant                     13.506***         
#                             (0.228)          

-----------------------------------------------
#  Observations                   2,220           
#R2                             0.482           
#Adjusted R2                    0.481           
#Residual Std. Error      1.864 (df = 2213)     
#F Statistic          343.619*** (df = 6; 2213) 
#===============================================
#  Note:               *p<0.1; **p<0.05; ***p<0.01

#### g)##### 
#modelo en forma reducida: Yi = alpha + b1Zi+gammaXi+ei, donde Zi es el instrumento nearc4

formareducida <- lm(lwage ~ nearc4 + exper + black + married + fatheduc + motheduc, data= base1df)
resultadosfr <- stargazer(formareducida, data= base1df, header=FALSE, escape=FALSE, type="latex")
render("regresionFR.Rmd", output_format="pdf_document")                
              

#####
#estimador por medio de ivreg = 0.330 
# estimador por medio de la primera etapa = 0.316
#estimador de la forma reducida = 0.104
#de acuerdo con el enunciado; estimador de iv= estimador de forma reducida/ estimador de primera etapa

0.104/0.316 == 0.3291139


#explicacion: tal relacion se da debido a que el instrumento IV estudia el efecto entre la forma reducida y la primera etapa


##########h)##########
#primera etapa 
modelo1eraetapa <- lm(educ ~ nearc4 +  exper + black + married + fatheduc + motheduc, data= base1df)
#obtenemos la recta que mejor se ajusta a los puntos
sihat <- modelo1eraetapa$fitted.values


#segunda etapa
modelo2etapa <- lm(lwage ~ sihat + exper + black + married + fatheduc + motheduc, data= base1df)
resultados2etapa <- stargazer(modelo2etapa, type = "latex", escape= FALSE, header=FALSE)

#unimos la regresion de 1era etapa con la de 2nda etapa y con ivreg
p_load(rio, AER, dplyr, rmarkdown)
modeloiv <-modelo2
uniondereg <- stargazer(modelo1eraetapa,modelo2etapa, modeloiv, type="latex", header = FALSE, escape = FALSE)
#exportamos regresion
render("regresionUnion.Rmd", output_format = "pdf_document")

###############################################################################
#########################SEGUNDO PUNTO########################################
###############################################################################
###############################################################################


#Limpiamos el inventario
rm(list=ls())
require(pacman)
p_load(stargazer, rio, ggplot2, estimatr, modelsummary, knitr)

####### a)############
base2 <- import("C:/Users/richa/OneDrive - Universidad de los Andes/Universidad/octavo/Econometria 2/taller 2/taller2 practico/New folder/talleres-econometria-2-en-r/bases/BaseP2.dta")

modelo1 <- lm(parvio_ip4_pc~left_winner, data = base2)
resultados <- stargazer(modelo1, type = "text", header = FALSE, escape = FALSE)
#===============================================
#  Dependent variable:    
#  ---------------------------
#  parvio_ip4_pc       
#-----------------------------------------------
#  left_winner                   -0.761           
#                                (0.979)          

#Constant                     2.610***          
#                               (0.692)          

-----------------------------------------------
#Observations                    254            
#R2                             0.002           
#Adjusted R2                   -0.002           
#Residual Std. Error      7.799 (df = 252)      
#F Statistic             0.605 (df = 1; 252)    
#===============================================
#  Note:               *p<0.1; **p<0.05; ***p<0.01


#de acuerdo con nuestra pregunta clave, la cual consiste en saber si la eleccion de un presidente de izquierda
# aumentaba los ataques violentos de grupos paramilitares 4 años despues de la eleccion de dicho alcalde, podemos
# ver que hay una relacion negativa entre la eleccion de un alcalde de izquierda y ataques paramilitares.
# Es decir, un municipio que cuente con un alcalde de izquierda dismunuye el indice de violencia generado por los paramilitares

########### b)#########

ggplot(data = base2, mapping = aes(x=base2$vote_share, y = base2$left_winner)) +
  geom_line() + geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Probabilidad de recibir tratamiento dado que D=1",
       x = "proporción de votos",
       y = "Probabilidad de ser alcalde de izquierda") 


#podemos utilizar una regresion discontinua aguda dado que hay perfect compliance en el tratamiento

##########c)##########
## 1) graficar relacion existente entre vote_share y parvio_ip4_pc
ggplot(data = base2, mapping = aes(x=base2$vote_share, y=base2$parvio_ip4_pc))+
  geom_point() + labs(x="proporcion de votos", y= "indice de violencia por paramilitares")
#2) correr regresionRDD y=b0 + b1voteshare+e (sin no linealidad) y sin interacciones
p_load(rdd, AER, rddtools, stargazer) ## paquetes rdd y rddtools nos permite hacer un rd y es compatible con stargazer

    #creamos un objeto rd antes de correr la regresion

objetoRDD <- rdd_data(y= base2$parvio_ip4_pc, x= base2$vote_share, cutpoint = 0)


    #estimamos la regresion y graficamos
modeloRDD <- rdd_reg_lm(objetoRDD, order=1)## utilizamos rdd_reg_lm puesto que es una regresionrd simple no lineal



    # regresion RDD con no linealidad (cuadratica)
modeloRDDnl <- rdd_reg_lm(objetoRDD, order=2) 

#3) regresion RDD con linealidad y intereaccion con Var.interes= left_winner



modeloRDDxLW <- lm_robust(parvio_ip4_pc~vote_share+left_winner*vote_share,
                          data= base2)

wdRp2<-"C:/Users/richa/OneDrive - Universidad de los Andes/Universidad/octavo/Econometria 2/taller 2/taller2 practico/New folder/talleres-econometria-2-en-r/resultados/resultadosP2"
setwd(wdRp2)
resuladosRDDxLW <-modelsummary(modeloRDDxLW, type="pdf")

# regresion RDD cuadratica y con interaccion
modeloRDDxLWc <-  lm_robust(parvio_ip4_pc~vote_share+vote_share_sq+left_winner*vote_share,
                            data= base2)
resultadosRDDxLWc <- modelsummary(modeloRDDxLWc, modeloRDDxLW)
resultadosRDDxLWc

modelsummary(list("Modelo RDD lineal" = modeloRDD, 
                  "Modelo RDD cuadrático" = modeloRDDnl, 
                  "Modelo RDD con interacción"=modeloRDDxLW,
                  "Modelo RDD con interacción y cuadrático"=modeloRDDxLWc), output ="C:/Users/richa/OneDrive - Universidad de los Andes/Universidad/octavo/Econometria 2/taller 2/taller2 practico/New folder/talleres-econometria-2-en-r/resultados/resultadosP2/regTOTALES.docx")

###############d)###############
    #filtramos los datos con el ancho de banda 0.5
anchoDeBanda <- 0.5
dataConABanda <- subset(base2, vote_share>=-anchoDeBanda & vote_share<=anchoDeBanda)


  objetoRDDh <-rdd_data(y=parvio_ip4_pc,x=vote_share, cutpoint = 0, data=dataConABanda)
  modeloRDDh0.5 <- rdd_reg_lm(objetoRDDh,order=1)
  modeloRDDnlh0.5 <- rdd_reg_lm(objetoRDD, order=2)
  modeloRDDxLWh0.5 <- lm_robust(parvio_ip4_pc~vote_share+left_winner*vote_share,
                            data= dataConABanda)
  modeloRDDxLWch0.5 <-  lm_robust(parvio_ip4_pc~vote_share+vote_share_sq+left_winner*vote_share,
                              data= dataConABanda)

  modelsummary(list("Modelo RDD lineal" = modeloRDDh0.5, 
                    "Modelo RDD cuadrático" = modeloRDDnlh0.5, 
                    "Modelo RDD con interacción"=modeloRDDxLWh0.5,
                    "Modelo RDD con interacción y cuadrático"=modeloRDDxLWch0.5), output ="C:/Users/richa/OneDrive - Universidad de los Andes/Universidad/octavo/Econometria 2/taller 2/taller2 practico/New folder/talleres-econometria-2-en-r/resultados/resultadosP2/regpuntoD.docx")


  
#########  e) #############
  
  anchoDeBanda <- 0.1
  dataConABanda <- subset(base2, vote_share>=-anchoDeBanda & vote_share<=anchoDeBanda)
  
  
  objetoRDDh0.1 <-rdd_data(y=parvio_ip4_pc,x=vote_share, cutpoint = 0, data=dataConABanda)
  modeloRDDh0.1 <- rdd_reg_lm(objetoRDDh,order=1)
  modeloRDDnlh0.1 <- rdd_reg_lm(objetoRDD, order=2)
  modeloRDDxLWh0.1 <- lm_robust(parvio_ip4_pc~vote_share+left_winner*vote_share,
                                data= dataConABanda)
  modeloRDDxLWch0.1 <-  lm_robust(parvio_ip4_pc~vote_share+vote_share_sq+left_winner*vote_share,
                                  data= dataConABanda)
  
  modelsummary(list("Modelo RDD lineal" = modeloRDDh0.1, 
                    "Modelo RDD cuadrático" = modeloRDDnlh0.1, 
                    "Modelo RDD con interacción"=modeloRDDxLWh0.1,
                    "Modelo RDD con interacción y cuadrático"=modeloRDDxLWch0.1), output ="C:/Users/richa/OneDrive - Universidad de los Andes/Universidad/octavo/Econometria 2/taller 2/taller2 practico/New folder/talleres-econometria-2-en-r/resultados/resultadosP2/regpuntoE.docx")
  
  
  
  
#################################################################################
#
#
 
##grafico de dispersion con puntos y RDD
ggplot(mapping= aes(x= vote_share,y=parvio_ip4_pc), data= base2)+ geom_point(alpha=0.5)+
  geom_smooth(method="loess", data = subset(base2, vote_share<0), se=FALSE)+
  geom_smooth(method="loess", data=subset(base2, vote_share >0), se= FALSE)+
  labs(title= "regresion discontinua con no linealidad y con puntos",
       x="proporcion de votos de la izquiera",
       y= "indice de violencia por paramilitares")

#graficp de dispercion sin puntos
ggplot(mapping= aes(x= vote_share,y=parvio_ip4_pc), data= base2)+
  geom_smooth(method="loess", data = subset(base2, vote_share<0), se=FALSE)+
  geom_smooth(method="loess", data=subset(base2, vote_share >0), se= FALSE)+
  labs(title= "regresion discontinua con no linealidad",
       x="proporcion de votos de la izquiera",
       y= "indice de violencia por paramilitares")


###############################################################################
#########################TERCER PUNTO########################################
###############################################################################
###############################################################################
#
# a) 
rm(list=ls())
require(pacman)
p_load(rio, stargazer, ggplot2)
direccion <- "C:/Users/richa/OneDrive - Universidad de los Andes/Universidad/octavo/Econometria 2/taller 2/taller2 practico/New folder/talleres-econometria-2-en-r/bases/BaseP3.dta"
base3 <- import(direccion)
setwd("C:/Users/richa/OneDrive - Universidad de los Andes/Universidad/octavo/Econometria 2/taller 2/taller2 practico/New folder/talleres-econometria-2-en-r/resultados/resultadosP3")

politicalquizSTD <- scale(base3$politicalquiz)
base3STD<- cbind(base3, politicalquizSTD)

#b dado que la edad es un factor fundamental a la hora de votar, puesto que al cumplir 18 años es 
# obligatorio, la edad nos sirve de running variable para estimar el salto en probabilidad de votar
# a medida que aumenta la edad. 


#c) 
ggplot(data = base3STD, mapping= aes(x=base3STD$age, y=base3STD$vote))+ 
  geom_smooth(data = subset(base3STD, age<18), method="loess", mapping = aes(x=age, y=vote))+
  geom_smooth(data=subset(base3STD, age>=18), method = "loess", mapping=aes(x=age, y=vote))+
 geom_vline(xintercept = 18, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Probabilidad de votar",
       x = "edad",
       y = "probabilidad de votar") 

#d) 

compulsoryvote <- ifelse(base3STD$age>=18,1,0)
compulsoryvote <- as.data.frame(compulsoryvote)
base3STDc <- cbind(base3STD, compulsoryvote)
# por que tendria sentido utilizar esta variable como instrumento en una regresion discontinua borrosa?
# los individuos no pueden manipular su edad, es una variable continua y ademas existe una discontinuidad entre votar y si cumple con 18 años


#e)
base3STDc6M <- subset(base3STDc, sample6m==1)

  # En la vecindad del punto de corte hay discontinuidad en la probabilidad de votar
      
p_load(stargazer, rio, ggplot2, rddensity, rdd)
ggplot(data = base3STD, mapping= aes(x=base3STD$age, y=base3STD$vote))+ 
  geom_smooth(data = subset(base3STD, age<18), method="loess", mapping = aes(x=age, y=vote))+
  geom_smooth(data=subset(base3STD, age>=18), method = "loess", mapping=aes(x=age, y=vote))+
  geom_vline(xintercept = 18, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Probabilidad de votar",
       x = "edad",
       y = "probabilidad de votar") 


  # Hay continuidad local para las siguientes covariables: white, female, college
  #mother, livewithparents y work. Utilice la variable categ´ orica del nivel escolar
  #como control para todos los casos.(realzado con ayuda de chatgpt)
covariables <- c("white", "female", "college", "mother", "livewithparents", "work")

for (var in covariables) {
  model <- lm(base3STDc6M[[var]] ~ I(age - 18) * (age >= 18), data = base3STDc6M)
  print(summary(model))
}


  # No hay manipulaci´on en la variable de focalizaci´ on
p_load(rddensity)
a<-rddensity(base3STDc6M$diasalt_1)
summary(object = a)
#resultado: el pvalor que se obtiene al realizar la hipotesis nula donde se asume que no existe manipulacion del
#           instrumento, es de 0,1674,el cual, es mayor al nuvel de significancia 0,05.Por lo cual, no hay evidencia
#           estadisticamente significativa que afirme que hay manipulacion del instrumento.


rdplotdensity(a, X = base3STDc6M$diasalt_1)


#f)
#mco
p_load(rdrobust, stargazer, estimatr, AER,officer,flextable, dplyr)
mco <- lm(politicalquizSTD ~ vote+escola+votedbefore2010+female+collegemother_y,base3STDc)
#sharp

controls <- "escola + votedbefore2010 + white + female + collegemother_y"
sharp_formula <- as.formula(paste("politicalquizSTD ~ vote + diasalt_1 + diasalt_1:vote +", controls))
sharp_model <- lm(sharp_formula, data = base3STDc6M)

#borroza

fuzzy_model <- ivreg(politicalquizSTD ~ diasalt_1 + diasalt_1:vote + escola + votedbefore2010 + white + female + collegemother_y | vote, data = base3STDc6M)

all_vars <- unique(c(names(coef(mco)), names(coef(sharp_model)), names(coef(fuzzy_model))))
summary_table <- data.frame(
  Variable = all_vars,
  MCO = NA,
  Sharp_RD = NA,
  Fuzzy_RD = NA
)

# Rellenar con coeficientes de cada modelo
summary_table$MCO[match(names(coef(mco)), all_vars)] <- coef(mco)
summary_table$Sharp_RD[match(names(coef(sharp_model)), all_vars)] <- coef(sharp_model)
summary_table$Fuzzy_RD[match(names(coef(fuzzy_model)), all_vars)] <- coef(fuzzy_model)

print(summary_table)
  
ft <- flextable(summary_table) %>%
  autofit()

# Exportar a Word
doc <- read_docx()
doc <- body_add_flextable(doc, ft)
print(doc, target = "resultados_regresion.docx")

print("Archivo Word exportado correctamente.")



