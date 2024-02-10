#############################################
# SECCION 3.3: MODELIZACION ESPACIOTEMPORAL #
#############################################
# Modelos espaciotemporales de ajuste de tasas crudas de incidencia para el cancer de estomago masculino
# Periodo 2001_2017

rm(list=ls())
library(sf)   
library(dplyr)
library(INLA) 
library(RColorBrewer) 
library(tmap) 

# Fijar el directorio de trabajo
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# Cargar la base de datos correspondiente al periodo 2001-2017: data2001-2017.RData
load("../Datos/data2001_2017.RData")
# Cargar la funcion "validate_model" de Auxiliares.R
source("Auxiliares.R")
# Cargar la matriz de estructura espacial: R_s.RData
load("../Datos/R_s.RData")
# Cargar cartografia inglesa: Carto_England.RData
load("../Datos/Carto_England.Rdata")

#############################################################################################
# MODELO 1 
# ESPACIAL: bym
# TEMPORAL: random walk1 
# SPACIO-TEMPORAL: interaccion I

StomachMaleInci1 <- fit_model(Data.INLA_2, Data.INLA_2$O, Data.INLA_2$E, Rs1, Data.INLA_2$Area, Data.INLA_2$Year, "BYM", "RW1","No","I")

##############################################################################################
# MODELO 2
# ESPACIAL: bym
# TEMPORAL: random walk1 
# ESPACIO-TEMPORAL: interaccion II

StomachMaleInci2 <- fit_model(Data.INLA_2, Data.INLA_2$O, Data.INLA_2$E, Rs1, Data.INLA_2$Area, Data.INLA_2$Year, "BYM", "RW1","No","II")

##############################################################################################
# MODELO 3 
# ESPACIAL: bym
# TEMPORAL: random walk1 
# SPACIO-TEMPORAL: interaccion III

StomachMaleInci3 <- fit_model(Data.INLA_2, Data.INLA_2$O, Data.INLA_2$E, Rs1, Data.INLA_2$Area, Data.INLA_2$Year, "BYM", "RW1","No","III")

##############################################################################################
# MODELO 4 
# ESPACIAL: bym
# TEMPORAL: random walk1 
# SPACIO-TEMPORAL: interaccion IV

StomachMaleInci4 <- fit_model(Data.INLA_2, Data.INLA_2$O, Data.INLA_2$E, Rs1, Data.INLA_2$Area, Data.INLA_2$Year, "BYM", "RW1","No","IV")

##############################################################################################
# MODELO 5 
# ESPACIAL: iCAR
# TEMPORAL: random walk1 
# SPACIO-TEMPORAL: interaccion I

StomachMaleInci5 <- fit_model(Data.INLA_2, Data.INLA_2$O, Data.INLA_2$E, Rs1, Data.INLA_2$Area, Data.INLA_2$Year, "iCAR", "RW1","No","I")

##############################################################################################
# MODELO 6
# ESPACIAL: iCAR
# TEMPORAL: random walk1 
# ESPACIO-TEMPORAL: interaccion II

StomachMaleInci6 <- fit_model(Data.INLA_2, Data.INLA_2$O, Data.INLA_2$E, Rs1, Data.INLA_2$Area, Data.INLA_2$Year, "iCAR", "RW1","No","II")

##############################################################################################
# MODELO 7 
# ESPACIAL: iCAR
# TEMPORAL: random walk1 
# SPACIO-TEMPORAL: interaccion III

StomachMaleInci7 <- fit_model(Data.INLA_2, Data.INLA_2$O, Data.INLA_2$E, Rs1, Data.INLA_2$Area, Data.INLA_2$Year, "iCAR", "RW1","No","III")

##############################################################################################
# MODELO 8 
# ESPACIAL: iCAR
# TEMPORAL: random walk1 
# SPACIO-TEMPORAL: interaccion IV

StomachMaleInci8 <- fit_model(Data.INLA_2, Data.INLA_2$O, Data.INLA_2$E, Rs1, Data.INLA_2$Area, Data.INLA_2$Year, "iCAR", "RW1","No","IV")

#######################################
# SECCION 3.3.1: SELECCION DE MODELOS #
#######################################
modelos <- list(StomachMaleInci1, StomachMaleInci2, StomachMaleInci3, StomachMaleInci4, StomachMaleInci5, StomachMaleInci6, StomachMaleInci7, StomachMaleInci8)
tabla_medidas <- data.frame(DIC = unlist(lapply(modelos, function(x) x$dic$dic)),
                    WAIC = unlist(lapply(modelos, function(x) x$waic$waic)))
write.table(tabla_medidas, file= "SeleccionModelos.txt")
save(modelos, file = "../Datos/Modelos2001_2017.RData")


############################################
# SECCION 3.3.2: RESULTADOS DE LOS MODELOS #
############################################

# Representacion de los dos mejores modelos (modelo 4 y modelo 8)

####################### MODELO 4 #########################

# Distribuciones a posteriori para el intercepto y los hiperparametros de precision

graphics.off()
pdf(file = "Distribuciones_modelo4.pdf", width = 10, height = 7)
par(mfrow = c(2, 3))
plot(inla.smarginal(StomachMaleInci4$marginals.fixed$`(Intercept)`), type = "l", main = "InterceptoBYM", xlab = "", ylab = "")
abline(v = StomachMaleInci4$summary.fixed$`0.5quant`, col="red") 
plot(inla.smarginal(StomachMaleInci4$marginals.hyperpar$`Precision for I.Area (iid component)`), type = "l", lwd = 1.5, main = "Hiperparametro espacial no estructurado", xlab = "", ylab = "", xlim = c(0,8000))
abline(v = StomachMaleInci4$summary.hyperpar["Precision for I.Area (iid component)","0.5quant"], col = "red")
plot(inla.smarginal(StomachMaleInci4$marginals.hyperpar$`Precision for I.Area (spatial component)`), type = "l", lwd = 1.5, main = "Hiperparametro espacial estructurado", xlab = "", ylab = "")
abline(v = StomachMaleInci4$summary.hyperpar["Precision for I.Area (spatial component)","0.5quant"], col = "red")
plot(inla.smarginal(StomachMaleInci4$marginals.hyperpar$`Precision for I.Year`), type = "l", lwd = 1.5, main = "Hiperparametro temporal estructurado", xlab = "", ylab = "", xlim = c(0,3000))
abline(v = StomachMaleInci4$summary.hyperpar["Precision for I.Year","0.5quant"], col = "red")
plot(inla.smarginal(StomachMaleInci4$marginals.hyperpar$`Precision for I.Area.Year`), type = "l", lwd = 1.5, main = "Hiperparametro espaciotemporal", xlab = "", ylab = "")
abline(v = StomachMaleInci4$summary.hyperpar["Precision for I.Area.Year","0.5quant"], col = "red")
dev.off()

# Ilustracion del patron espaciotemporal de las tasas estimadas
estimated.mean.CR <- StomachMaleInci4$summary.fitted.values$mean
estimated.mean.CR <- estimated.mean.CR*1e+5

Data.INLA_2$Area <- as.factor(Data.INLA_2$Area)
Data.INLA_2$Year <- as.factor(Data.INLA_2$Year)

T <- length(unique(Data.INLA_2$Year))
T.from <- levels(Data.INLA_2$Year)[1]
T.to <- levels(Data.INLA_2$Year)[17]

carto.Inci <- carto
kont <- 1
for(i in seq(T.from,T.to)){
  carto.Inci$var <- estimated.mean.CR[kont:(kont+105-1)]
  names(carto.Inci)[ncol(carto.Inci)] <- paste("Year",i,sep=".")
  kont <- kont + 105
}

values <- c(7,11.8,16.7,21.5,26.3,31.2,36) 
paleta <- brewer.pal(6,"Reds")  

Map1.Rates <- tm_shape(carto.Inci) +
  tm_polygons(col=paste("Year",round(seq(T.from,T.to)),sep="."), palette=paleta, border.alpha=0.5,
              title="Tasas estimadas", legend.show=T, legend.reverse=T,
              style="fixed", breaks=values, interval.closure="left") +
  tm_layout(main.title="Modelo BYM-IV", main.title.position="left", panel.label.size=1.5,
            panel.labels=paste("",round(seq(T.from,T.to))),
            legend.outside=T, legend.outside.position="right", legend.frame=F,
            legend.outside.size=0.2, outer.margins=c(0.02,0.01,0.02,0.01)) + 
  tm_facets(nrow=4, ncol=5)

tmap_mode("plot")
tmap_save(Map1.Rates, file="Modelo4.pdf")

####################### MODELO 8 #########################

# Distribuciones a posteriori para el intercepto y los hiperparametros de precision

graphics.off()
pdf(file = "Distribuciones_modelo8.pdf", width = 10, height = 7)
par(mfrow = c(2, 2))
plot(inla.smarginal(StomachMaleInci8$marginals.fixed$`(Intercept)`), type = "l", main = "InterceptoiCAR", xlab = "", ylab = "")
abline(v = StomachMaleInci8$summary.fixed$`0.5quant`, col="red") 
plot(inla.smarginal(StomachMaleInci8$marginals.hyperpar$`Precision for I.Area`), type = "l", lwd = 1.5, main = "Hiperparametro espacial estructurado", xlab = "", ylab = "")
abline(v = StomachMaleInci8$summary.hyperpar["Precision for I.Area","0.5quant"], col = "red")
plot(inla.smarginal(StomachMaleInci8$marginals.hyperpar$`Precision for I.Year`), type = "l", lwd = 1.5, main = "Hiperparametro temporal estructurado", xlab = "", ylab = "", xlim = c(0,5000))
abline(v = StomachMaleInci8$summary.hyperpar["Precision for I.Year","0.5quant"], col = "red")
plot(inla.smarginal(StomachMaleInci8$marginals.hyperpar$`Precision for I.Area.Year`), type = "l", lwd = 1.5, main = "Hiperparametro espaciotemporal", xlab = "", ylab = "")
abline(v = StomachMaleInci8$summary.hyperpar["Precision for I.Area.Year","0.5quant"], col = "red")
dev.off()

estimated.mean.CR <- StomachMaleInci8$summary.fitted.values$mean
estimated.mean.CR <- estimated.mean.CR*1e+5

Data.INLA_2$Area <- as.factor(Data.INLA_2$Area)
Data.INLA_2$Year <- as.factor(Data.INLA_2$Year)

T <- length(unique(Data.INLA_2$Year))
T.from <- levels(Data.INLA_2$Year)[1]
T.to <- levels(Data.INLA_2$Year)[17]

carto.Inci <- carto
kont <- 1
for(i in seq(T.from,T.to)){
  carto.Inci$var <- estimated.mean.CR[kont:(kont+105-1)]
  names(carto.Inci)[ncol(carto.Inci)] <- paste("Year",i,sep=".")
  kont <- kont + 105
}

values <- c(7,11.8,16.7,21.5,26.3,31.2,36) 
paleta <- brewer.pal(6,"Reds")  

Map1.Rates <- tm_shape(carto.Inci) +
  tm_polygons(col=paste("Year",round(seq(T.from,T.to)),sep="."), palette=paleta, border.alpha=0.5,
              title="Tasas estimadas", legend.show=T, legend.reverse=T,
              style="fixed", breaks=values, interval.closure="left") +
  tm_layout(main.title="Modelo iCAR-IV", main.title.position="left", panel.label.size=1.5,
            panel.labels=paste("",round(seq(T.from,T.to))),
            legend.outside=T, legend.outside.position="right", legend.frame=F,
            legend.outside.size=0.2, outer.margins=c(0.02,0.01,0.02,0.01)) + 
  tm_facets(nrow=4, ncol=5)

tmap_mode("plot")
tmap_save(Map1.Rates, file="Modelo8.pdf")
