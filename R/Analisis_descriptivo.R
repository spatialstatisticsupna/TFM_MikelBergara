######################################
# SECCION 3.2.: ANALISIS DESCRIPTIVO #
######################################
rm(list=ls()) 
library(sf)   
library(dplyr)
library(RColorBrewer) 
library(tmap) 
# Fijar el directorio de trabajo
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# Cargar cartografia inglesa: Carto_England.RData (contiene tambien la matriz de adyacencia)
load("../Datos/Carto_England.Rdata")
# Cargar base de datos inglesa inicial: England_database.Rdata (contiene las bases de datos de incidencia/mortalidad y la poblacion)
load("../Datos/England_database.Rdata")
# Cargar funcion auxiliar "fusionar_datos" de Bases_modelos.R

##################################
# SECCION 3.2.1: PATRON ESPACIAL #
##################################

# Evolucion espacial de tasas de incidencia/mortalidad por cancer

# Eliminar los datos de 2018, 2019 y 2020
Inci <- Inci[which(!(Inci$Year %in% c(2018,2019,2020))),]
Mort <- Mort[which(!(Mort$Year %in% c(2018,2019,2020))),]

# Cancer de mama: C50, Cancer de estomago: C16, y Cancer de pulmon: C33-C34
# Sexo: Female/Male
colores_paleta <- c("Blues","Reds","Greens","Greens")
opciones <- c("C50","Female","C16","Male","C33-C34","Female","C33-C34","Male")
nombres <- c("mama","mujeres","estomago","hombres","pulmon","mujeres","pulmon","hombres")

for (i in 1:(length(opciones)/2)) {
  cancer <- fusionar_datos(ICD10=opciones[2*i-1], Gender=opciones[2*i])
  # Crear una base de datos con los datos de incidencia, otra con los datos de mortalidad
  cancer.Inci <- cancer$Datos.Inci
  cancer.Mort <- cancer$Datos.Mort
  cancer.Inci$Region <- as.factor(cancer.Inci$Region)
  cancer.Mort$Region <- as.factor(cancer.Mort$Region)
  
  # Calcular las tasas crudas para cada una de las bases teniendo en cuenta todo el periodo (2001_2017)
  lista_rates1 <- c(); lista_rates2 <- c()
  for (j in levels(cancer.Inci$Region)) {
    lista_rates1 <- c(lista_rates1, sum(cancer.Inci$Count[cancer.Inci$Region==j])/sum(cancer.Inci$Pop[cancer.Inci$Region==j])*1e+5)
    lista_rates2 <- c(lista_rates2, sum(cancer.Mort$Count[cancer.Mort$Region==j])/sum(cancer.Mort$Pop[cancer.Mort$Region==j])*1e+5)
  }
  # Anadir las tasas a las cartografias e ilustrarlas
  carto.Inci <- carto
  carto.Mort <- carto
  carto.Inci$rates1 <- lista_rates1
  carto.Mort$rates2 <- lista_rates2
  
  paleta <- brewer.pal(6,colores_paleta[i])  
  values1 <- round(seq(min(carto.Inci$rates1), max(carto.Inci$rates1), length.out = 7)) # intervalos tasas de incidencia
  values2 <- round(seq(min(carto.Mort$rates2), max(carto.Mort$rates2), length.out = 7)) # intervalos tasas de mortalidad
  
  # Dibujar un mapa con tasas de incidencia y otro con tasas de mortalidad
  Map1.Rates <- tm_shape(carto.Inci) +
    tm_polygons(col="rates1", palette=paleta, border.alpha=0.5,
                title="Tasas crudas", legend.show=T, legend.reverse=T,
                style="fixed", breaks=values1, interval.closure="left") +
    tm_layout(title = "", legend.title.size = 2, legend.text.size = 1.5, legend.position = c("left", "top")) +
    tm_facets(nrow=1, ncol=1)
  
  Map2.Rates <- tm_shape(carto.Mort) +
    tm_polygons(col="rates2", palette=paleta, border.alpha=0.5,
                title="Tasas crudas", legend.show=T, legend.reverse=T,
                style="fixed", breaks=values2, interval.closure="left") +
    tm_layout(title = "", legend.title.size = 2, legend.text.size = 1.5, legend.position = c("left", "top")) +
    tm_facets(nrow=1, ncol=1)
  
  tmap_mode("plot")
  tmap_save(Map1.Rates, file=paste("Cancer",nombres[2*i-1],nombres[2*i],"Incidencia.pdf", sep = "_"))
  tmap_save(Map2.Rates, file=paste("Cancer",nombres[2*i-1],nombres[2*i],"Mortalidad.pdf", sep = "_"))
}

##################################
# SECCION 3.2.2: PATRON TEMPORAL #
##################################

# Dibujar linea temporal de tasas
for (i in 1:(length(opciones)/2)) {
  cancer <- fusionar_datos(ICD10=opciones[2*i-1], Gender=opciones[2*i])
  # Crear una base de datos con los datos de incidencia, otra con los datos de mortalidad
  cancer.Inci <- cancer$Datos.Inci
  cancer.Mort <- cancer$Datos.Mort
  cancer.Inci$Year <- as.factor(cancer.Inci$Year)
  cancer.Mort$Year <- as.factor(cancer.Mort$Year)
  
  # Calcular las tasas crudas para todo el area
  lista_pop1 <- c(); lista_pop2 <- c()
  lista_rates1 <- c(); lista_rates2 <- c()
  for (j in levels(cancer.Inci$Year)) {
    lista_pop1 <- c(lista_pop1, sum(cancer.Inci$Pop[cancer.Inci$Year==j]))
    lista_pop2 <- c(lista_pop2, sum(cancer.Mort$Pop[cancer.Mort$Year==j]))
    lista_rates1 <- c(lista_rates1, sum(cancer.Inci$Count[cancer.Inci$Year==j])/sum(cancer.Inci$Pop[cancer.Inci$Year==j])*1e+5)
    lista_rates2 <- c(lista_rates2, sum(cancer.Mort$Count[cancer.Mort$Year==j])/sum(cancer.Mort$Pop[cancer.Mort$Year==j])*1e+5)
  }
  # Dibujar linea temporal de tasas (azul)
  x <- levels(cancer.Inci$Year)
  yInci <- lista_rates1; yMort <- lista_rates2
  graphics.off()
  pdf(file=paste("Temporal",nombres[2*i-1],nombres[2*i],"Incidencia.pdf", sep = "_"), width=4.4, height=4.4)
  plot(x,yInci,"l",main="Tasas por 100.000 habitantes",xlab="",ylab="Tasa incidencia",col="blue")
  pdf(file=paste("Temporal",nombres[2*i-1],nombres[2*i],"Mortalidad.pdf", sep = "_"), width=4.4, height=4.4)
  plot(x,yMort,"l",main="Tasas por 100.000 habitantes",xlab="",ylab="Tasa mortalidad",col="blue")
  dev.off()
}

#########################################
# SECCION 3.2.3: PATRON ESPACIOTEMPORAL #
#########################################

# Evolucion espaciotemporal de tasas de incidencia/mortalidad por cancer

# Cancer de estomago en el sexo masculino para datos de incidencia
Stomach.male <- fusionar_datos(ICD10="C16", Gender="Male")
Stomach.Inci <- Stomach.male$Datos.Inci

Stomach.Inci$Region <- as.factor(Stomach.Inci$Region)
Stomach.Inci$Age.group <- as.factor(Stomach.Inci$Age.group)
Stomach.Inci$Year <- as.factor(Stomach.Inci$Year)

# Calcular las tasas crudas respecto al espacio y tiempo
Rates <- c()
for (j in levels(Stomach.Inci$Year)) {
  for (i in levels(Stomach.Inci$Region)) {
    kont <- sum(Stomach.Inci$Count[Stomach.Inci$Region==i & Stomach.Inci$Year==j]) 
    popu <- sum(Stomach.Inci$Pop[Stomach.Inci$Region==i & Stomach.Inci$Year==j])
    rate <- kont/popu*1e+5
    Rates <- c(Rates, rate)
  }
}

# Anadir a la cartografia las columnas de las tasas por cada año
T <- length(unique(Stomach.Inci$Year))
T.from <- levels(Stomach.Inci$Year)[1]
T.to <- levels(Stomach.Inci$Year)[length(levels(Stomach.Inci$Year))]

carto.Inci <- carto
kont <- 1
for(i in seq(T.from,T.to)){
  carto.Inci$var <- Rates[kont:(kont+105-1)]
  names(carto.Inci)[ncol(carto.Inci)] <- paste("Year",i,sep=".")
  kont <- kont + 105
}

values <- c(3,9.7,16.3,23,29.7,36.3,43) # intervalos tasas de incidencia
paleta <- brewer.pal(6,"Reds")    

# Dibujar mapas de incidencia de evolucion espaciotemporal
Map1.Rates <- tm_shape(carto.Inci) +
  tm_polygons(col=paste("Year",round(seq(T.from,T.to)),sep="."), palette=paleta, border.alpha=0.5,
              title="Tasas crudas", legend.show=T, legend.reverse=T,
              style="fixed", breaks=values, interval.closure="left") +
  tm_layout(main.title="Tasas crudas cancer estomago (hombres)", main.title.position="left", panel.label.size=1.5,
            panel.labels=paste("",round(seq(T.from,T.to))),
            legend.outside=T, legend.outside.position="right", legend.frame=F,
            legend.outside.size=0.2, outer.margins=c(0.02,0.01,0.02,0.01)) + 
  tm_facets(nrow=4, ncol=5)

tmap_mode("plot")
tmap_save(Map1.Rates, file="TC_EstomagoCancer_Hombre.pdf")
