###############################################################################
######Taller 2 practico econometria 2##########################################
###################2025-1######################################################
###Profesora: Juliana Helo ####################################################
#Integrantes
# Jose Ricardo Ricardo Hernandez - 202113889
#
#
###############################################################################
                              
                              #PUNTO 1#

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
              
