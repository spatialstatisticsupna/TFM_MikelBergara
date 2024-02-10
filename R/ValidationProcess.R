##############################################################################
### CAPITULO 4: PROCESO DE VALIDACION DE LOS MODELOS Y PREDICCION A FUTURO ###
##############################################################################

rm(list=ls())
library(sf)   
library(dplyr)
library(INLA) 
library(RColorBrewer) 
library(tmap) 

# Fijar el directorio de trabajo
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# Cargar la base de datos correspondiente al periodo 2001_2017: data2001_2017.RData
load("../Datos/data2001_2017.RData")
# Cargar la base de datos correspondiente al periodo completo 2001_2020: Periodo_Completo.RData
load("../Datos/Periodo_Completo.RData")
# Cargar la funcion "validate_model" de Auxiliares.R
# Cargar la funcion "validation_subsets" de Auxiliares.R
source("Auxiliares.R")
# Cargar la matriz de estructura espacial: R_s.RData
load("../Datos/R_s.RData")
# Cargar cartografia inglesa: Carto_England.RData
load("../Datos/Carto_England.Rdata")
# Cargar los modelos espaciotemporales ajustados en todo el periodo 2001_2017
load("../Datos/Modelos2001_2017.RData")

###########################################################
### SECCION 4.1: CREACION DE SUBCONJUNTOS DE VALIDACION ###
###########################################################
# Datos correspondientes al periodo 2001-2017
Datos <- data.frame(O = Data.INLA_2$O, E = Data.INLA_2$E, Area = Data.INLA_2$Area, Year = Data.INLA_2$Year)
# Crear vector con todos los intervalos de tiempo (en este caso son anos)
vec_temporal <- levels(as.factor(Datos$Year))
# Vector de observaciones y offsets (poblacion para las tasas)
y <- Datos$O
e <- Datos$E
# Definir "inicio", "final" y "prediccion" utilizados en la funcion validate_subsets
inicio <- "2001"; final <- "2010" ; prediccion <- 3
indice_inicial <- which(inicio == vec_temporal)
indice_final <- which(final == vec_temporal)
# Utilizar la funcion de validation_subsets para crear todos los subconjuntos
validation_subsets(inicio, final, prediccion, vec_temporal, Datos, y)

##############################################################
### SECCION 4.4: AJUSTE DE LOS MODELOS A CADA SUBCONJUNTO  ###
##############################################################

# Validar con cada subconjunto generado anteriormente los modelos seleccionados
R_s <- Rs1

# Modelos utilizados (4):
# Espacial: BYM; Espacio_temporal: I/II/III/IV
nombres_esptemporal <- c("I","II","III","IV")
Espacial <- "BYM"; Temporal_iid <- "No"; Temporal_estruc <- "RW1" 

# Los bucles que vienen a continuacion tienen un coste computacional considerable
for (s in 1:4) {
  Espacio_temporal <- nombres_esptemporal[s]
  indice_inicial <- which(inicio == vec_temporal)
  indice_final <- which(final == vec_temporal)
  # Guardar DIC y WAIC 
  medidas <- NULL
  data.comparar1 <- NULL
  data.comparar2 <- NULL
    
  # Ajustar el modelo seleccionado para cada subconjunto de validacion: 
  while ((indice_final+prediccion) <= length(vec_temporal)) {
      
    Subconj_validacion <- read.table(file=paste("PeriodoValidacionNA",vec_temporal[indice_inicial],vec_temporal[indice_final+prediccion],".txt",sep="_"))  
      
    # Llamar a las observaciones, offset, variable espacial y variable temporal
    O <- Subconj_validacion$O; E <- Subconj_validacion$E; Var_espacial <- Subconj_validacion$Area; Var_temporal <- Subconj_validacion$Year
    predic_model <- validate_model(Subconj_validacion, O, E, R_s, Var_espacial, Var_temporal, Espacial, Temporal_estruc, Temporal_iid, Espacio_temporal,modelos[[s]],2)
      
    # Guardar los resultados de los anos a predecir: valores observados (los tenemos guardados como PeriodoValidacionObservados) vs predicciones
    valores_observados <- read.table(file=paste("PeriodoValidacionObservados",vec_temporal[indice_inicial],vec_temporal[indice_final+prediccion],".txt",sep="_")) 
    valores_observados <- valores_observados$O[which(valores_observados$Year %in% vec_temporal[c((indice_final+1):(indice_final+prediccion))])]
      
    # Para obtener las predicciones de los casos, multiplicar el offset E a los valores esperados de tasas dadas por el modelo
    tasas <- predic_model$summary.fitted.values$mean[which(Subconj_validacion$Year %in% vec_temporal[c((indice_final+1):(indice_final+prediccion))])] 
    offset <- Subconj_validacion$E[which(Subconj_validacion$Year %in% vec_temporal[c((indice_final+1):(indice_final+prediccion))])]
    predicciones <- tasas*offset # valor esperado casos predichos
    # Por otro lado, extraer tambien los limites del intervalo de credibilidad del 95%
    # Muestreo aleatorio para calcular funcion de densidad empirica de los casos predichos
    set.seed(1)
    size_muestra <- 3000
    size_datos <- length(which(Subconj_validacion$Year %in% vec_temporal[c((indice_final+1):(indice_final+prediccion))]))
    muestra = matrix(NA, nrow=size_muestra, ncol=size_datos)
    marginales_pred <- predic_model$marginals.fitted.values[which(Subconj_validacion$Year %in% vec_temporal[c((indice_final+1):(indice_final+prediccion))])]
    for(i in 1:size_datos){
      x = inla.rmarginal(size_muestra, marginales_pred[[i]]) # generacion de muestra aleatoria
      pred_x = x*offset[i]  
      muestra[,i]= rpois(size_muestra, pred_x) # generar valores de casos predichos
    }
    l <- apply(muestra, 2, quantile, 0.025)
    u <- apply(muestra, 2, quantile, 0.975)
      
    # Guardar en un dataframe las observaciones y las predicciones de cada subperiodo
    data.comparar1 <- cbind(data.comparar1,cbind(valores_observados,predicciones))
    # Guardar en un dataframe las observaciones y los intervalos de credibilidad de cada subperiodo
    data.comparar2 <- rbind(data.comparar2,cbind(valores_observados,l,u))
      
    # Guardar los resultados del modelo para cada subconjunto respecto a su DIC y WAIC 
    DIC <- predic_model$dic$dic; WAIC <- predic_model$waic$waic
    medidas <- rbind(medidas,cbind(DIC,WAIC,Periodo.Validacion = paste("PeriodoValidacion",vec_temporal[indice_inicial],vec_temporal[indice_final+prediccion],sep="_"), Modelo = paste(Espacial,Temporal_estruc,Temporal_iid,Espacio_temporal,sep="+")))
      
    indice_inicial <- indice_inicial + 1 ; indice_final<- indice_final + 1
  } 
  
  ##################
  ### APENDICE C ###
  ################## 
  write.table(medidas, file = paste("Medidas_predictivas_Modelo",Espacial,Temporal_estruc,Temporal_iid,Espacio_temporal,".txt",sep="_"))  
    
  # Por ultimo, calcular las medidas de validacion del modelo ajustado,
  # MAE (mean absolute error) y RMSE (root mean square error) para cada region:
  num_configuraciones <- length(colnames(data.comparar1))/2   # numero de subconjuntos creados
  suma_MAE <- 0; suma_RMSE <- 0
  for (k in 1:num_configuraciones) {
    abs_value <- abs(data.comparar1[,2*k-1]-data.comparar1[,2*k]); suma_MAE <- suma_MAE + abs_value
    cuadrado <- (data.comparar1[,2*k-1]-data.comparar1[,2*k])^2; suma_RMSE <- suma_RMSE + cuadrado
  }
  # Errores para cada area por definicion. 
  # Como se ha seleccionado prediccion == 3, por cada region (105) habra tres indicadores del error, uno por cada ano a predecir
  MAE <- 1/num_configuraciones*suma_MAE  
  RMSE <- sqrt(1/num_configuraciones*suma_RMSE)  
    
  # IS (interval score) para cada region con alpha=0.05
  IS <- c(); alpha = 0.05
  for (i in 1:length(data.comparar2[,1])) {
    if (as.numeric(data.comparar2[i,1]) > as.numeric(data.comparar2[i,2]) & as.numeric(data.comparar2[i,1]) < as.numeric(data.comparar2[i,3])){
      score <- (data.comparar2[i,3]-data.comparar2[i,2])
      IS <- c(IS,score)
    }
    if (as.numeric(data.comparar2[i,1]) < as.numeric(data.comparar2[i,2])){
      score <- (as.numeric(data.comparar2[i,3])-as.numeric(data.comparar2[i,2])) + 2/alpha*(as.numeric(data.comparar2[i,2])-as.numeric(data.comparar2[i,1]))
      IS <- c(IS,score)
    }
    if (as.numeric(data.comparar2[i,1]) > as.numeric(data.comparar2[i,3])){
      score <- (as.numeric(data.comparar2[i,3])-as.numeric(data.comparar2[i,2])) + 2/alpha*(as.numeric(data.comparar2[i,1])-as.numeric(data.comparar2[i,3]))
      IS <- c(IS,score)
    }
  }
  medidas_validacion1 <- cbind(MAE,RMSE)
  medidas_validacion2 <- IS
    
  # Calcular la media de cada una de las medidas obtenidas, para cada ano predicho a futuro. De esta manera se compararan los modelos.
  # Los datos de MAE/RMSE estan organizados (medidas_validacion1) tal que primero aparecen los errores a un ano, luego a dos, luego a tres... para cada region
  media_medidas <- NULL
  num_regiones <- length(medidas_validacion1[,1])/prediccion
  for (m in 1:prediccion) {
    mediaMAE <- mean(medidas_validacion1[(m*num_regiones-num_regiones+1):(m*num_regiones),1])
    mediaRMSE <- mean(medidas_validacion1[(m*num_regiones-num_regiones+1):(m*num_regiones),2])
    media_medidas <- rbind(media_medidas,cbind(mediaMAE,mediaRMSE))
  }
  # Los datos de IS estan organizados (medidas_validacion2) por configuraciones (5). Las primeras 315 filas corresponden a la primera configuracion, despues la segunda, etc...
  reorganizacion_datos <- NULL
  for (k in 1:num_configuraciones) {
    reorganizacion_datos <- cbind(reorganizacion_datos,medidas_validacion2[(k*prediccion*num_regiones-prediccion*num_regiones+1):(k*prediccion*num_regiones)]) # poner cada configuracion al lado de la otra, de modo que escoger cada ano predicho se facilita
  }
  mediaIS <- c()
  for (m in 1:prediccion) {
    mediaIS <- c(mediaIS,mean(reorganizacion_datos[(m*num_regiones-num_regiones+1):(m*num_regiones)]))
  }
  # Guardar la media dada por cada medida para cada ano predicho a futuro
  media_medidas <- cbind(media_medidas,mediaIS)
  write.table(media_medidas, file = paste("MediaMedidasValidacionModelo",Espacial,Temporal_estruc,Temporal_iid,Espacio_temporal,".txt",sep="_"))
}



#########################################################
### SECCION 4.5: PREDICCIONES DE TASAS PARA 2018_2020 ###
#########################################################

# Datos correspondientes al periodo completo 2001_2020
Datos_completos <- Datos2
# Tomar la base de datos inicial y poner los anos a predecir como "NA". En este caso 2018, 2019 y 2020 
vec_predecir <- c(2018,2019,2020)
Datos_completos$O[which(Datos_completos$Year %in% vec_predecir)] = NA

# Predecir los anos 2018,2019,2020 con el mejor modelo obtenido
modelo.predictivo <- fit_model(Datos_completos, Datos_completos$O, Datos_completos$E, R_s, Datos_completos$Area, Datos_completos$Year, "BYM", "RW1", "No", "II")

# Asignar las tasas estimadas por el modelo por 100.000 habitantes
media.estimada.tasas <- modelo.predictivo$summary.fitted.values$mean
media.estimada.tasas <- media.estimada.tasas*1e+5

# Dibujar las tasas estimadas para el periodo completo
Datos_completos$Area <- as.factor(Datos_completos$Area)
Datos_completos$Year <- as.factor(Datos_completos$Year)

T <- length(unique(Datos_completos$Year))
T.from <- levels(Datos_completos$Year)[1]
T.to <- levels(Datos_completos$Year)[T]

carto.Inci <- carto
kont <- 1
for(i in seq(T.from,T.to)){
  carto.Inci$var <- media.estimada.tasas[kont:(kont+105-1)]
  names(carto.Inci)[ncol(carto.Inci)] <- paste("Year",i,sep=".")
  kont <- kont + 105
}

values <- c(7,11.8,16.7,21.5,26.3,31.2,38) 
paleta <- brewer.pal(6,"Reds")  

Map1.Rates <- tm_shape(carto.Inci) +
  tm_polygons(col=paste("Year",round(seq(T.from,T.to)),sep="."), palette=paleta, border.alpha=0.5,
              title="", legend.show=T, legend.reverse=T,
              style="fixed", breaks=values, interval.closure="left") +
  tm_layout(main.title="Tasas ajustadas cancer de estomago", main.title.position="left", panel.label.size=1.5,
            panel.labels=paste("",round(seq(T.from,T.to))),
            legend.outside=T, legend.outside.position="right", legend.frame=F,
            legend.outside.size=0.2, outer.margins=c(0.02,0.01,0.02,0.01)) + 
  tm_facets(nrow=4, ncol=5)

tmap_mode("plot")
tmap_save(Map1.Rates, file="MapaPredicciones.pdf") 
  
#######################################################################################################################################  
### Dibujar las medianas posteriores predictivas para algunas regiones, junto a su intervalo de credibilidad del 95% y tasas crudas ###
#######################################################################################################################################
S <- length(unique(Datos_completos$Area)); T <- length(unique(Datos_completos$Year))
T.from <- levels(Datos_completos$Year)[1]; T.to <- levels(Datos_completos$Year)[T]
periodo <- seq(T.from,T.to); periodo_inverso <- seq(T.to,T.from)

# Primero se calculan las poblaciones medias para cada region (105)
vec_regiones <- levels(Datos_completos$Area)
poblaciones <- NULL
for (i in 1:S) {
  pob_media <- mean(Datos_completos$E[which(Datos_completos$Area == vec_regiones[i])])
  poblaciones <- c(poblaciones,pob_media)
}
# Ordenar el vector de poblaciones medias de mayor a menor
poblaciones_orden <- sort(poblaciones,decreasing = TRUE)
# Se seleccionan regiones con tamanos de poblacion diversos, puesto que el grosor de los intervalos de credibilidad cambiara (mas o menos variabilidad)
# Region 1 (mas poblada):
reg1 <- vec_regiones[which(poblaciones_orden[1] == poblaciones)] # E38000258/E38000259 (Birmingham)
# Region 2 (medianamente poblada):
reg2 <- vec_regiones[which(poblaciones_orden[S%/%2] == poblaciones)] # E38000097 (Leicester)
# Region 3 (menos poblada):
reg3 <- vec_regiones[which(poblaciones_orden[S] == poblaciones)] # E38000200 (Lancashire)

# Utilizar la region de Bristol en vez de Leicester, por ser mas ilustrativa
reg2 <- "E38000222"
regiones <- c(reg1, reg2, reg3)
nombres_reg <- c("Birmingham","Bristol","Lancashire")

pdf(file = "ComparacionRegiones.pdf", width=10, height=3.25)
par(mfrow = c(1,3))
par(mar = c(2,2,1,0.2))
for(r in 1:length(regiones)){
  indice_reg <- which(modelo.predictivo$.args$data$Area == regiones[r]) #indices correspondientes a la region
  obs_reg <- Datos2$O[indice_reg]
  offset_reg <- Datos_completos$E[indice_reg]
  # Tasas estimadas e intervalos credibilidad 95%
  mediana <- 1e+5*modelo.predictivo$summary.fitted.values$`0.5quant`[indice_reg] #tasas estimadas
  l <- 1e+5*modelo.predictivo$summary.fitted.values$`0.025quant`[indice_reg] #limite inferior
  u <- 1e+5*modelo.predictivo$summary.fitted.values$`0.975quant`[indice_reg] # limite superior
  # Dibujar simultaneamente los graficos para las tres regiones
  plot(periodo, obs_reg, type = "n", ylab = "Tasas predichas", xlab = "", yaxt = "n", xaxt = "n", ylim = c(0,45), xlim = c(min(periodo),max(periodo)), cex.main = 0.8)
  polygon(c(periodo, periodo_inverso), c(l, rev(u)), col = "gray70", border = NA) # intervalo credibilidad
  lines(periodo, mediana, col = "black", lwd = 1, lty = 1) # mediana
  points(periodo, 1e+5*obs_reg/offset_reg, pch = 20, cex = 0.5) # tasas crudas
  abline(v = 2017, col = "black", lty = 3, lwd = 1) # ano donde comienzan las predicciones
  abline(h = 100000 * exp(modelo.predictivo$summary.fixed$`0.5quant`), lty = 3, lwd = 1, col = "blue") # tasa media como referencia
  axis(1, at = c(2001,2017,2020), cex.axis = 0.8) # agregar nuevas etiquetas para los ejes
  axis(2, at = seq(0, 50, by = 5), cex.axis = 0.8)
  title(main = nombres_reg[r], adj = 0.5, line = 0.3, cex.main = 1)
  legend("topleft", inset = 0.01, legend = c("Tasas crudas","Mediana estimada","Intervalo credibilidad 95%"),
         col = c(NA, "black", NA), lty = c(0,1,0), cex = 0.9, box.lty = 0)
  legend("topleft", inset = 0.01, legend = c("","",""), col = c("black",NA,NA), pch = 20, cex = 0.9, box.lty = 0, bty = "n")
  legend("topleft", inset = 0.01, legend = c("","","","",""), fill = c(NA,NA,"gray70"), cex = 0.9, box.lty = 0, bty = "n",border = NA)
}
dev.off()








