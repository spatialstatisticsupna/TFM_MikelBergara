rm(list=ls())
library(INLA)
library(RColorBrewer) 
library(sf) 
library(tmap) 
# Fijar el directorio de trabajo
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# Cargar la base de datos correspondiente al periodo 2001_2017: data2001_2017.RData
load("../Datos/data2001_2017.RData")
# Cargar la funcion "validate_model" de Auxiliares.R
source("Auxiliares.R")
# Cargar la matriz de estructura espacial: R_s.RData
load("../Datos/R_s.RData")
# Cargar cartografia inglesa: Carto_England.RData
load("../Datos/Carto_England.Rdata")
# Cargar los modelos espaciotemporales ajustados en todo el periodo 2001_2017
load("../Datos/Modelos2001_2017.RData")

############################################################################
### SECCION 4.2: REDUCCION COSTE COMPUTACIONAL DEL PROCESO DE VALIDACION ###
############################################################################
# Recuperar el modelo BYM_II 
StomachMaleInci2 <- modelos[[2]]

# Ajustar el modelo BYM_II en los subconjuntos de validacion SIN TENER EN CUENTA el modelo del periodo completo 2001_2017
# Subperiodo 2001_2013
subp1 <- Data.INLA_2[which(!(Data.INLA_2$Year %in% c("2014","2015","2016","2017"))),]
submodel1 <- validate_model(subp1, subp1$O, subp1$E, Rs1, subp1$Area, subp1$Year, "BYM", "RW1","No","II",StomachMaleInci2,0)
# Subperiodo 2002_2014
subp2 <- Data.INLA_2[which(!(Data.INLA_2$Year %in% c("2001","2015","2016","2017"))),]
submodel2 <- validate_model(subp2, subp2$O, subp2$E, Rs1, subp2$Area, subp2$Year, "BYM", "RW1","No","II",StomachMaleInci2,0)
# Subperiodo 2003_2015
subp3 <- Data.INLA_2[which(!(Data.INLA_2$Year %in% c("2001","2002","2016","2017"))),]
submodel3 <- validate_model(subp3, subp3$O, subp3$E, Rs1, subp3$Area, subp3$Year, "BYM", "RW1","No","II",StomachMaleInci2,0)
# Subperiodo 2004_2016
subp4 <- Data.INLA_2[which(!(Data.INLA_2$Year %in% c("2001","2002","2003","2017"))),]
submodel4 <- validate_model(subp4, subp4$O, subp4$E, Rs1, subp4$Area, subp4$Year, "BYM", "RW1","No","II",StomachMaleInci2,0)
# Subperiodo 2005_2017
subp5 <- Data.INLA_2[which(!(Data.INLA_2$Year %in% c("2001","2002","2003","2004"))),]
submodel5 <- validate_model(subp5, subp5$O, subp5$E, Rs1, subp5$Area, subp5$Year, "BYM", "RW1","No","II",StomachMaleInci2,0)

# Ajustar el modelo BYM_II en los subconjuntos de validacion TENIENDO EN CUENTA el modelo del periodo completo 2001_2017
# Subperiodo 2001_2013
submodel1trick <- validate_model(subp1, subp1$O, subp1$E, Rs1, subp1$Area, subp1$Year, "BYM", "RW1","No","II", StomachMaleInci2, 1)
# Subperiodo 2002_2014
submodel2trick <- validate_model(subp2, subp2$O, subp2$E, Rs1, subp2$Area, subp2$Year, "BYM", "RW1","No","II", StomachMaleInci2, 1)
# Subperiodo 2003_2015
submodel3trick <- validate_model(subp3, subp3$O, subp3$E, Rs1, subp3$Area, subp3$Year, "BYM", "RW1","No","II", StomachMaleInci2, 1)
# Subperiodo 2004_2016
submodel4trick <- validate_model(subp4, subp4$O, subp4$E, Rs1, subp4$Area, subp4$Year, "BYM", "RW1","No","II", StomachMaleInci2, 1)
# Subperiodo 2005_2017
submodel5trick <- validate_model(subp5, subp5$O, subp5$E, Rs1, subp5$Area, subp5$Year, "BYM", "RW1","No","II", StomachMaleInci2, 1)

# Comparar las modas de los hiperparametros para todos los modelos ajustados hasta el momento:
# Modelo periodo completo 2001_2017
precision.pcompleto <- StomachMaleInci2$summary.hyperpar$mode
# Modelos sin utilizar control.mode
precision.1 <- submodel1$summary.hyperpar$mode
precision.2 <- submodel2$summary.hyperpar$mode
precision.3 <- submodel3$summary.hyperpar$mode
precision.4 <- submodel4$summary.hyperpar$mode
precision.5 <- submodel5$summary.hyperpar$mode
# Modelos utilizando control.mode(..., Restart == TRUE)
precision.1trick <- submodel1trick$summary.hyperpar$mode
precision.2trick <- submodel2trick$summary.hyperpar$mode
precision.3trick <- submodel3trick$summary.hyperpar$mode
precision.4trick <- submodel4trick$summary.hyperpar$mode
precision.5trick <- submodel5trick$summary.hyperpar$mode

modas <- rbind(precision.pcompleto,precision.1,precision.2,precision.3,precision.4,precision.5,precision.1trick,precision.2trick,precision.3trick,precision.4trick,precision.5trick)
colnames(modas) <- c("Espacial_iid", "Espacial", "Temporal", "Espacio_temporal")
####################################################################################################
# Comparar las distribuciones a posteriori de los hiperparametros junto a su mediana 
# Coger cada periodo de tiempo y comparar la distribucion del modelo ajustado "normal" vs "con truco"
# Periodo 2001_2013
graphics.off()
pdf(file = "Distribuciones_2001_2013.pdf", width = 8, height = 8)
par(mfrow = c(2, 2))
plot(inla.smarginal(submodel1$marginals.hyperpar$`Precision for I.Area (iid component)`), type = "l", lwd = 1.5, main = "Espacial no estructurado", xlab = "", ylab = "", xlim = c(0,8000))
par(new=TRUE)
plot(inla.smarginal(submodel1trick$marginals.hyperpar$`Precision for I.Area (iid component)`), type = "l", lwd = 1.5, xlab = "", ylab = "", xlim = c(0,8000), col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel1$summary.hyperpar["Precision for I.Area (iid component)","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel1trick$summary.hyperpar["Precision for I.Area (iid component)","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(submodel1$marginals.hyperpar$`Precision for I.Area (spatial component)`), type = "l", lwd = 1.5, main = "Espacial estructurado", xlab = "", ylab = "")
par(new=TRUE)
plot(inla.smarginal(submodel1trick$marginals.hyperpar$`Precision for I.Area (spatial component)`), type = "l", lwd = 1.5, xlab = "", ylab = "", col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel1$summary.hyperpar["Precision for I.Area (spatial component)","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel1trick$summary.hyperpar["Precision for I.Area (spatial component)","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(submodel1$marginals.hyperpar$`Precision for I.Year`), type = "l", lwd = 1.5, main = "Temporal", xlab = "", ylab = "", xlim = c(0,3000))
par(new=TRUE)
plot(inla.smarginal(submodel1trick$marginals.hyperpar$`Precision for I.Year`), type = "l", lwd = 1.5, xlab = "", ylab = "", xlim = c(0,3000), col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel1$summary.hyperpar["Precision for I.Year","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel1trick$summary.hyperpar["Precision for I.Year","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(submodel1$marginals.hyperpar$`Precision for I.Area.Year`), type = "l", lwd = 1.5, main = "Espaciotemporal", xlab = "", ylab = "")
par(new=TRUE)
plot(inla.smarginal(submodel1trick$marginals.hyperpar$`Precision for I.Area.Year`), type = "l", lwd = 1.5, xlab = "", ylab = "", col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel1$summary.hyperpar["Precision for I.Area.Year","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel1trick$summary.hyperpar["Precision for I.Area.Year","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

dev.off()

# Periodo 2002_2014
graphics.off()
pdf(file = "Distribuciones_2002_2014.pdf", width = 8, height = 8)
par(mfrow = c(2, 2))
plot(inla.smarginal(submodel2$marginals.hyperpar$`Precision for I.Area (iid component)`), type = "l", lwd = 1.5, main = "Espacial no estructurado", xlab = "", ylab = "", xlim = c(0,8000))
par(new=TRUE)
plot(inla.smarginal(submodel2trick$marginals.hyperpar$`Precision for I.Area (iid component)`), type = "l", lwd = 1.5, xlab = "", ylab = "", xlim = c(0,8000), col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel2$summary.hyperpar["Precision for I.Area (iid component)","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel2trick$summary.hyperpar["Precision for I.Area (iid component)","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(submodel2$marginals.hyperpar$`Precision for I.Area (spatial component)`), type = "l", lwd = 1.5, main = "Espacial estructurado", xlab = "", ylab = "")
par(new=TRUE)
plot(inla.smarginal(submodel2trick$marginals.hyperpar$`Precision for I.Area (spatial component)`), type = "l", lwd = 1.5, xlab = "", ylab = "", col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel2$summary.hyperpar["Precision for I.Area (spatial component)","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel2trick$summary.hyperpar["Precision for I.Area (spatial component)","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(submodel2$marginals.hyperpar$`Precision for I.Year`), type = "l", lwd = 1.5, main = "Temporal", xlab = "", ylab = "", xlim = c(0,3000))
par(new=TRUE)
plot(inla.smarginal(submodel2trick$marginals.hyperpar$`Precision for I.Year`), type = "l", lwd = 1.5, xlab = "", ylab = "", xlim = c(0,3000), col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel2$summary.hyperpar["Precision for I.Year","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel2trick$summary.hyperpar["Precision for I.Year","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(submodel2$marginals.hyperpar$`Precision for I.Area.Year`), type = "l", lwd = 1.5, main = "Espaciotemporal", xlab = "", ylab = "")
par(new=TRUE)
plot(inla.smarginal(submodel2trick$marginals.hyperpar$`Precision for I.Area.Year`), type = "l", lwd = 1.5, xlab = "", ylab = "", col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel2$summary.hyperpar["Precision for I.Area.Year","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel2trick$summary.hyperpar["Precision for I.Area.Year","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

dev.off()
# Periodo 2003_2015
graphics.off()
pdf(file = "Distribuciones_2003_2015.pdf", width = 8, height = 8)
par(mfrow = c(2, 2))
plot(inla.smarginal(submodel3$marginals.hyperpar$`Precision for I.Area (iid component)`), type = "l", lwd = 1.5, main = "Espacial no estructurado", xlab = "", ylab = "", xlim = c(0,8000))
par(new=TRUE)
plot(inla.smarginal(submodel3trick$marginals.hyperpar$`Precision for I.Area (iid component)`), type = "l", lwd = 1.5, xlab = "", ylab = "", xlim = c(0,8000), col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel3$summary.hyperpar["Precision for I.Area (iid component)","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel3trick$summary.hyperpar["Precision for I.Area (iid component)","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(submodel3$marginals.hyperpar$`Precision for I.Area (spatial component)`), type = "l", lwd = 1.5, main = "Espacial estructurado", xlab = "", ylab = "")
par(new=TRUE)
plot(inla.smarginal(submodel3trick$marginals.hyperpar$`Precision for I.Area (spatial component)`), type = "l", lwd = 1.5, xlab = "", ylab = "", col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel3$summary.hyperpar["Precision for I.Area (spatial component)","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel3trick$summary.hyperpar["Precision for I.Area (spatial component)","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(submodel3$marginals.hyperpar$`Precision for I.Year`), type = "l", lwd = 1.5, main = "Temporal", xlab = "", ylab = "", xlim = c(0,3000))
par(new=TRUE)
plot(inla.smarginal(submodel3trick$marginals.hyperpar$`Precision for I.Year`), type = "l", lwd = 1.5, xlab = "", ylab = "", xlim = c(0,3000), col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel3$summary.hyperpar["Precision for I.Year","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel3trick$summary.hyperpar["Precision for I.Year","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(submodel3$marginals.hyperpar$`Precision for I.Area.Year`), type = "l", lwd = 1.5, main = "Espaciotemporal", xlab = "", ylab = "")
par(new=TRUE)
plot(inla.smarginal(submodel3trick$marginals.hyperpar$`Precision for I.Area.Year`), type = "l", lwd = 1.5, xlab = "", ylab = "", col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel3$summary.hyperpar["Precision for I.Area.Year","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel3trick$summary.hyperpar["Precision for I.Area.Year","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

dev.off()
# Periodo 2004_2016
graphics.off()
pdf(file = "Distribuciones_2004_2016.pdf", width = 8, height = 8)
par(mfrow = c(2, 2))
plot(inla.smarginal(submodel4$marginals.hyperpar$`Precision for I.Area (iid component)`), type = "l", lwd = 1.5, main = "Espacial no estructurado", xlab = "", ylab = "", xlim = c(0,8000))
par(new=TRUE)
plot(inla.smarginal(submodel4trick$marginals.hyperpar$`Precision for I.Area (iid component)`), type = "l", lwd = 1.5, xlab = "", ylab = "", xlim = c(0,8000), col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel4$summary.hyperpar["Precision for I.Area (iid component)","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel4trick$summary.hyperpar["Precision for I.Area (iid component)","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(submodel4$marginals.hyperpar$`Precision for I.Area (spatial component)`), type = "l", lwd = 1.5, main = "Espacial estructurado", xlab = "", ylab = "")
par(new=TRUE)
plot(inla.smarginal(submodel4trick$marginals.hyperpar$`Precision for I.Area (spatial component)`), type = "l", lwd = 1.5, xlab = "", ylab = "", col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel4$summary.hyperpar["Precision for I.Area (spatial component)","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel4trick$summary.hyperpar["Precision for I.Area (spatial component)","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(submodel4$marginals.hyperpar$`Precision for I.Year`), type = "l", lwd = 1.5, main = "Temporal", xlab = "", ylab = "", xlim = c(0,3000))
par(new=TRUE)
plot(inla.smarginal(submodel4trick$marginals.hyperpar$`Precision for I.Year`), type = "l", lwd = 1.5, xlab = "", ylab = "", xlim = c(0,3000), col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel4$summary.hyperpar["Precision for I.Year","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel4trick$summary.hyperpar["Precision for I.Year","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(submodel4$marginals.hyperpar$`Precision for I.Area.Year`), type = "l", lwd = 1.5, main = "Espaciotemporal", xlab = "", ylab = "")
par(new=TRUE)
plot(inla.smarginal(submodel4trick$marginals.hyperpar$`Precision for I.Area.Year`), type = "l", lwd = 1.5, xlab = "", ylab = "", col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel4$summary.hyperpar["Precision for I.Area.Year","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel4trick$summary.hyperpar["Precision for I.Area.Year","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

dev.off()
# Periodo 2005_2017
graphics.off()
pdf(file = "Distribuciones_2005_2017.pdf", width = 8, height = 8)
par(mfrow = c(2, 2))
plot(inla.smarginal(submodel5$marginals.hyperpar$`Precision for I.Area (iid component)`), type = "l", lwd = 1.5, main = "Espacial no estructurado", xlab = "", ylab = "", xlim = c(0,8000))
par(new=TRUE)
plot(inla.smarginal(submodel5trick$marginals.hyperpar$`Precision for I.Area (iid component)`), type = "l", lwd = 1.5, xlab = "", ylab = "", xlim = c(0,8000), col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel5$summary.hyperpar["Precision for I.Area (iid component)","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel5trick$summary.hyperpar["Precision for I.Area (iid component)","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(submodel5$marginals.hyperpar$`Precision for I.Area (spatial component)`), type = "l", lwd = 1.5, main = "Espacial estructurado", xlab = "", ylab = "")
par(new=TRUE)
plot(inla.smarginal(submodel5trick$marginals.hyperpar$`Precision for I.Area (spatial component)`), type = "l", lwd = 1.5, xlab = "", ylab = "", col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel5$summary.hyperpar["Precision for I.Area (spatial component)","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel5trick$summary.hyperpar["Precision for I.Area (spatial component)","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(submodel5$marginals.hyperpar$`Precision for I.Year`), type = "l", lwd = 1.5, main = "Temporal", xlab = "", ylab = "", xlim = c(0,3000))
par(new=TRUE)
plot(inla.smarginal(submodel5trick$marginals.hyperpar$`Precision for I.Year`), type = "l", lwd = 1.5, xlab = "", ylab = "", xlim = c(0,3000), col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel5$summary.hyperpar["Precision for I.Year","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel5trick$summary.hyperpar["Precision for I.Year","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(submodel5$marginals.hyperpar$`Precision for I.Area.Year`), type = "l", lwd = 1.5, main = "Espaciotemporal", xlab = "", ylab = "")
par(new=TRUE)
plot(inla.smarginal(submodel5trick$marginals.hyperpar$`Precision for I.Area.Year`), type = "l", lwd = 1.5, xlab = "", ylab = "", col="brown",yaxt = "n", xaxt = "n")
abline(v = submodel5$summary.hyperpar["Precision for I.Area.Year","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel5trick$summary.hyperpar["Precision for I.Area.Year","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.normal","Distr.mod.truco","Mediana normal","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

dev.off()

##################
### APENDICE B ###
##################
# Dibujar las distribuciones de los hiperparametros del modelo completo 2001_2017 
# Comparar el modelo del subperiodo 2001_2013 control.mode(..., Restart=TRUE) para ver si la optimizacion de los hiperparametros cambia 
graphics.off()
pdf(file = "Comparacion_2001_2013.pdf", width = 8, height = 8)
par(mfrow = c(2, 2))
plot(inla.smarginal(StomachMaleInci2$marginals.hyperpar$`Precision for I.Area (iid component)`), type = "l", lwd = 1.5, main = "Espacial no estructurado", xlab = "", ylab = "", xlim = c(0,8000))
par(new=TRUE)
plot(inla.smarginal(submodel1trick$marginals.hyperpar$`Precision for I.Area (iid component)`), type = "l", lwd = 1.5, xlab = "", ylab = "", xlim = c(0,8000), col="brown",yaxt = "n", xaxt = "n")
abline(v = StomachMaleInci2$summary.hyperpar["Precision for I.Area (iid component)","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel1trick$summary.hyperpar["Precision for I.Area (iid component)","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.entero","Distr.mod.truco","Mediana entera","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(StomachMaleInci2$marginals.hyperpar$`Precision for I.Area (spatial component)`), type = "l", lwd = 1.5, main = "Espacial estructurado", xlab = "", ylab = "")
par(new=TRUE)
plot(inla.smarginal(submodel1trick$marginals.hyperpar$`Precision for I.Area (spatial component)`), type = "l", lwd = 1.5, xlab = "", ylab = "", col="brown",yaxt = "n", xaxt = "n")
abline(v = StomachMaleInci2$summary.hyperpar["Precision for I.Area (spatial component)","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel1trick$summary.hyperpar["Precision for I.Area (spatial component)","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.entero","Distr.mod.truco","Mediana entera","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(StomachMaleInci2$marginals.hyperpar$`Precision for I.Year`), type = "l", lwd = 1.5, main = "Temporal", xlab = "", ylab = "", xlim = c(0,3000))
par(new=TRUE)
plot(inla.smarginal(submodel1trick$marginals.hyperpar$`Precision for I.Year`), type = "l", lwd = 1.5, xlab = "", ylab = "", xlim = c(0,3000), col="brown",yaxt = "n", xaxt = "n")
abline(v = StomachMaleInci2$summary.hyperpar["Precision for I.Year","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel1trick$summary.hyperpar["Precision for I.Year","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.entero","Distr.mod.truco","Mediana entera","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

plot(inla.smarginal(StomachMaleInci2$marginals.hyperpar$`Precision for I.Area.Year`), type = "l", lwd = 1.5, main = "Espaciotemporal", xlab = "", ylab = "")
par(new=TRUE)
plot(inla.smarginal(submodel1trick$marginals.hyperpar$`Precision for I.Area.Year`), type = "l", lwd = 1.5, xlab = "", ylab = "", col="brown",yaxt = "n", xaxt = "n")
abline(v = StomachMaleInci2$summary.hyperpar["Precision for I.Area.Year","0.5quant"], col = "red",lty = 3, lwd = 1)
abline(v = submodel1trick$summary.hyperpar["Precision for I.Area.Year","0.5quant"], col = "blue",lty = 3, lwd = 1)
legend("topright",legend = c("Distr.mod.entero","Distr.mod.truco","Mediana entera","Mediana truco"),lty = c(1,1,3,3), col = c("black","brown","red","blue"),cex = 0.75)

dev.off()

######################################################################################################################
# Comparar las tasas estimadas en el periodo 2001_2013 por ambos modelos (se puede hacer para todos los subperiodos)
media.estimada1 <- submodel1$summary.fitted.values$mean; media.estimada2 <- submodel1trick$summary.fitted.values$mean
media.estimada1 <- media.estimada1*1e+5; media.estimada2 <- media.estimada2*1e+5
subp1$Area <- as.factor(subp1$Area)
subp1$Year <- as.factor(subp1$Year)
T <- length(unique(subp1$Year))
T.from <- levels(subp1$Year)[1]
T.to <- levels(subp1$Year)[T]
carto.Inci1 <- carto; carto.Inci2 <- carto
kont <- 1
for(i in seq(T.from,T.to)){
  carto.Inci1$var <- media.estimada1[kont:(kont+105-1)]
  carto.Inci2$var <- media.estimada2[kont:(kont+105-1)]
  names(carto.Inci1)[ncol(carto.Inci1)] <- paste("Year",i,sep=".")
  names(carto.Inci2)[ncol(carto.Inci2)] <- paste("Year",i,sep=".")
  kont <- kont + 105
}
values <- c(7,11.8,16.7,21.5,26.3,31.2,38) 
paleta <- brewer.pal(6,"Reds")  

Mapa1 <- tm_shape(carto.Inci1) +
  tm_polygons(col=paste("Year",round(seq(T.from,T.to)),sep="."), palette=paleta, border.alpha=0.5,
              title="Tasas ajustadas", legend.show=T, legend.reverse=T,
              style="fixed", breaks=values, interval.closure="left") +
  tm_layout(main.title="Tasas estimadas cancer de estomago", main.title.position="left", panel.label.size=1.5,
            panel.labels=paste("",round(seq(T.from,T.to))),
            legend.outside=T, legend.outside.position="right", legend.frame=F,
            legend.outside.size=0.2, outer.margins=c(0.02,0.01,0.02,0.01)) + 
  tm_facets(nrow=4, ncol=4)
tmap_mode("plot")
tmap_save(Mapa1, file="EstimacionesNORMAL.pdf") 

Mapa2 <- tm_shape(carto.Inci2) +
  tm_polygons(col=paste("Year",round(seq(T.from,T.to)),sep="."), palette=paleta, border.alpha=0.5,
              title="Tasas ajustadas", legend.show=T, legend.reverse=T,
              style="fixed", breaks=values, interval.closure="left") +
  tm_layout(main.title="Tasas estimadas cancer de estomago", main.title.position="left", panel.label.size=1.5,
            panel.labels=paste("",round(seq(T.from,T.to))),
            legend.outside=T, legend.outside.position="right", legend.frame=F,
            legend.outside.size=0.2, outer.margins=c(0.02,0.01,0.02,0.01)) + 
  tm_facets(nrow=4, ncol=4)
tmap_mode("plot")
tmap_save(Mapa2, file="EstimacionesTRICK.pdf") 

##################
### APENDICE B ###
##################
# Construir diagrama de dispersion para los predictores lineales en el periodo 2001_2013
# Comparar los predictores dados por ambos modelos. 
# Mostrar por cada ano del subperiodo los valores dados en cada region
lin.predictor1 <- submodel1$summary.linear.predictor$mean; lin.predictor2 <- submodel1trick$summary.linear.predictor$mean
kont <- 1
graphics.off()
pdf(file = "DispersionesPredictores2001_2013.pdf", width = 12, height = 12)
par(mfrow = c(4, 4))
for(i in seq(2001,2013)){
  y1 <- lin.predictor1[kont:(kont+105-1)]
  plot(1:105, y1, type = "p", ylab = "", xlab = "", yaxt = "n", xaxt = "n", col = "blue", main = paste("Year",i,sep="_"), pch = 1)
  par(new=TRUE)
  y2 <- lin.predictor2[kont:(kont+105-1)]
  plot(1:105, y2, type = "p", ylab = "Predictor lineal", xlab = "CCGs Inglaterra", yaxt = "n", xaxt = "n", col = "red", pch = 4)
  legend("topright",legend = c("Pred.normal","Pred.truco"), pch = c(1,4), col = c("blue","red"),cex = 0.75, bty = "n")
  kont <- kont + 105
}
dev.off()

# Estimar las tasas (exponenciacion de los predictores lineales)
tasa1 <- submodel1$summary.fitted.values$mean; tasa2 <- submodel1trick$summary.fitted.values$mean
kont <- 1
graphics.off()
pdf(file = "DispersionesTasas2001_2013.pdf", width = 12, height = 12)
par(mfrow = c(4, 4))
for(i in seq(2001,2013)){
  y1 <- tasa1[kont:(kont+105-1)]
  plot(1:105, y1, type = "p", ylab = "", xlab = "", yaxt = "n", xaxt = "n", col = "blue", main = paste("Year",i,sep="_"))
  par(new=TRUE)
  y2 <- tasa2[kont:(kont+105-1)]
  plot(1:105, y2, type = "p", ylab = "Tasas estimadas", xlab = "CCGs Inglaterra", yaxt = "n", xaxt = "n", col = "red", pch = 4)
  legend("topright",legend = c("Tasa.normal","Tasa.truco"), pch = c(1,4), col = c("blue","red"),cex = 0.75, bty = "n")
  kont <- kont + 105
}
dev.off()

######################################################################################################
# Calcular los tiempos de ejecucion de los cuatro modelos BYM ajustados en el periodo completo 2001_2017
# BYM_I, BYM_II, BYM_III y BIM_IV
StomachMaleInci1 <- modelos[[1]]
StomachMaleInci2 <- modelos[[2]]
StomachMaleInci3 <- modelos[[3]]
StomachMaleInci4 <- modelos[[4]]
tabla_tiempos <- rbind(StomachMaleInci1$cpu.used,StomachMaleInci2$cpu.used,StomachMaleInci3$cpu.used,StomachMaleInci4$cpu.used)
rownames(tabla_tiempos) <- c("BYM_I","BYM_II","BYM_III","BYM_IV")

##################
### APENDICE A ###
##################
# Calcular los tiempos de ejecucion de los modelos anteriores para cada subperiodo
# Crear los 5 subconjuntos utilizados
Datos <- data.frame(O = Data.INLA_2$O, E = Data.INLA_2$E, Area = Data.INLA_2$Area, Year = Data.INLA_2$Year)
vec_temporal <- levels(as.factor(Datos$Year))
y <- Datos$O
e <- Datos$E
inicio <- "2001"; final <- "2010" ; prediccion <- 3
indice_inicial <- which(inicio == vec_temporal)
indice_final <- which(final == vec_temporal)
# Utilizar la funcion de "validation_subsets" para crear todos los subconjuntos
validation_subsets(inicio, final, prediccion, vec_temporal, Datos, y)

# Los modelos del subperiodo correspondiente se ajustan de tres maneras diferentes
# 1) Sin utilizar control.mode
# 2) control.mode(..., Restart == TRUE) 
# 3) control.mode(..., Restart == FALSE) 
R_s <- Rs1
nombres_esptemporal <- c("I","II","III","IV")
Espacial <- "BYM"; Temporal_iid <- "No"; Temporal_estruc <- "RW1" 
modelosBYM <- list(StomachMaleInci1,StomachMaleInci2,StomachMaleInci3,StomachMaleInci4)
tabla_tiempos_normal <- NULL; tabla_tiempos_true <- NULL; tabla_tiempos_false <- NULL
for (s in 1:4) {
  modelo_BYM <- modelosBYM[[s]]
  Espacio_temporal <- nombres_esptemporal[s]
  indice_inicial <- which(inicio == vec_temporal)
  indice_final <- which(final == vec_temporal)
  
  # Ajustar el modelo seleccionado para cada subconjunto de validacion: 
  while ((indice_final+prediccion) <= length(vec_temporal)) {
    
    Subconj_validacion <- read.table(file=paste("PeriodoValidacionNA",vec_temporal[indice_inicial],vec_temporal[indice_final+prediccion],".txt",sep="_"))  
    O <- Subconj_validacion$O; E <- Subconj_validacion$E; Var_espacial <- Subconj_validacion$Area; Var_temporal <- Subconj_validacion$Year
    predic_model_normal <- validate_model(Subconj_validacion, O, E, R_s, Var_espacial, Var_temporal, Espacial, Temporal_estruc, Temporal_iid, Espacio_temporal,modelo_BYM,0) # Sin control.mode
    predic_model_true <- validate_model(Subconj_validacion, O, E, R_s, Var_espacial, Var_temporal, Espacial, Temporal_estruc, Temporal_iid, Espacio_temporal,modelo_BYM,1) # Restart=TRUE
    predic_model_false <- validate_model(Subconj_validacion, O, E, R_s, Var_espacial, Var_temporal, Espacial, Temporal_estruc, Temporal_iid, Espacio_temporal,modelo_BYM,2) # Restart=FALSE   
    
    tabla_tiempos_normal <- rbind(tabla_tiempos_normal,cbind(predic_model_normal$cpu.used[4]))
    tabla_tiempos_true <- rbind(tabla_tiempos_true,cbind(predic_model_true$cpu.used[4]))
    tabla_tiempos_false <- rbind(tabla_tiempos_false,cbind(predic_model_false$cpu.used[4], paste(Espacial,Espacio_temporal,sep="+"), paste("PeriodoValidacion",vec_temporal[indice_inicial],vec_temporal[indice_final+prediccion],sep="_")))
    
    indice_inicial <- indice_inicial + 1 ; indice_final<- indice_final + 1
  } 
}
colnames(tabla_tiempos_normal) <- c("Tiempos.NORMAL") 
colnames(tabla_tiempos_true) <- c("Tiempos.TRUE") 
colnames(tabla_tiempos_false) <- c("Tiempos.FALSE","Modelo","Subperiodo") 
comparacion_tiempos <- cbind(tabla_tiempos_normal,tabla_tiempos_true,tabla_tiempos_false)
# Guardar los tiempos de todos los modelos
save(comparacion_tiempos, file = "TiemposEjecucionModelos.RData")
# Calcular tiempo total para los modelos de cada metodo
comparacion_tiempos2 <- data.frame(apply(comparacion_tiempos[,1:3], c(1,2), function(x) as.numeric(as.character(x))))
tiempos_totales <- data.frame(apply(comparacion_tiempos2, 2, function(x) sum(x)))
colnames(tiempos_totales) <- c("Tiempo Total")






