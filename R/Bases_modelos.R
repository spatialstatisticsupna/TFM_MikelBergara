rm(list=ls()) 
# Fijar el directorio de trabajo
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# Cargar cartografia inglesa: Carto_England.RData (contiene tambien matriz de adyacencia)
load("../Datos/Carto_England.Rdata")
# Cargar base de datos inglesa inicial: England_database.Rdata (contiene las bases de datos de incidencia/mortalidad y la poblacion)
load("../Datos/England_database.Rdata")

# Funcion auxiliar para juntar datos, escogiendo cancer y sexo:
fusionar_datos <- function(ICD10=NULL, Gender=NULL){
  
  Pop <- Pop[order(Pop$Year,Pop$Region),]
  
  # Fijar tipo de cancer y genero del individuo:
  Datos.Inci <- Inci[Inci$ICD10_code==ICD10 & Inci$Gender==Gender, c("Age.group","Year","Region","Gender","ICD10_code","Cancer_site","Count")]
  Datos.Inci <- Datos.Inci[order(Datos.Inci$Age.group,Datos.Inci$Year,Datos.Inci$Region),]
  Datos.Inci$Pop <- NA
  rownames(Datos.Inci) <- NULL
  
  A <- length(unique(Datos.Inci$Age.group))          ## 6 grupos de edad (Under 25, 25 to 49, 50 to 59, 60 to 69, 70 to 79, 80 and over)
  S <- length(unique(Datos.Inci$Region))             ## 105 regiones 
  T <- length(unique(Datos.Inci$Year))               
  if(A*S*T!=nrow(Datos.Inci)) stop("Dimensions of incidence data are not correct")
  
  Datos.Mort <- Mort[Mort$ICD10_code==ICD10 & Mort$Gender==Gender, c("Age.group","Year","Region","Gender","ICD10_code","Cancer_site","Count")]
  Datos.Mort <- Datos.Mort[order(Datos.Mort$Age.group,Datos.Mort$Year,Datos.Mort$Region),]
  Datos.Mort$Pop <- NA
  rownames(Datos.Mort) <- NULL
  
  A <- length(unique(Datos.Mort$Age.group))          ## 6 grupos de edad (Under 25, 25 to 49, 50 to 59, 60 to 69, 70 to 79, 80 and over)
  S <- length(unique(Datos.Mort$Region))             ## 105 regiones 
  T <- length(unique(Datos.Mort$Year))               
  if(A*S*T!=nrow(Datos.Mort)) stop("Dimensions of incidence data are not correct")
  
  # Insertar columna con datos de la poblacion en la base de incidencia/mortalidad
  
  suppressWarnings({
    ## Age.group="Under 25" ##
    if(Gender=="Female"){
      aux <- Pop[,which(substr(colnames(Pop),1,1)=="f" & as.numeric(substr(colnames(Pop),2,3))<25)]
    }
    if(Gender=="Male"){
      aux <- Pop[,which(substr(colnames(Pop),1,1)=="m" & as.numeric(substr(colnames(Pop),2,3))<25)]
    }
    Datos.Inci$Pop[Datos.Inci$Age.group=="Under 25"] <- apply(aux,1,sum)
    Datos.Mort$Pop[Datos.Mort$Age.group=="Under 25"] <- apply(aux,1,sum)
    
    
    ## Age.group="25 to 49" ##
    if(Gender=="Female"){
      aux <- Pop[,which(substr(colnames(Pop),1,1)=="f" & as.numeric(substr(colnames(Pop),2,3))>=25 & as.numeric(substr(colnames(Pop),2,3))<50)]
    }
    if(Gender=="Male"){
      aux <- Pop[,which(substr(colnames(Pop),1,1)=="m" & as.numeric(substr(colnames(Pop),2,3))>=25 & as.numeric(substr(colnames(Pop),2,3))<50)]
    }
    Datos.Inci$Pop[Datos.Inci$Age.group=="25 to 49"] <- apply(aux,1,sum)
    Datos.Mort$Pop[Datos.Mort$Age.group=="25 to 49"] <- apply(aux,1,sum)
    
    
    ## Age.group="50 to 59" ##
    if(Gender=="Female"){
      aux <- Pop[,which(substr(colnames(Pop),1,1)=="f" & as.numeric(substr(colnames(Pop),2,3))>=50 & as.numeric(substr(colnames(Pop),2,3))<60)]
    }
    if(Gender=="Male"){
      aux <- Pop[,which(substr(colnames(Pop),1,1)=="m" & as.numeric(substr(colnames(Pop),2,3))>=50 & as.numeric(substr(colnames(Pop),2,3))<60)]
    }
    Datos.Inci$Pop[Datos.Inci$Age.group=="50 to 59"] <- apply(aux,1,sum)
    Datos.Mort$Pop[Datos.Mort$Age.group=="50 to 59"] <- apply(aux,1,sum)
    
    
    ## Age.group="60 to 69" ##
    if(Gender=="Female"){
      aux <- Pop[,which(substr(colnames(Pop),1,1)=="f" & as.numeric(substr(colnames(Pop),2,3))>=60 & as.numeric(substr(colnames(Pop),2,3))<70)]
    }
    if(Gender=="Male"){
      aux <- Pop[,which(substr(colnames(Pop),1,1)=="m" & as.numeric(substr(colnames(Pop),2,3))>=60 & as.numeric(substr(colnames(Pop),2,3))<70)]
    }
    Datos.Inci$Pop[Datos.Inci$Age.group=="60 to 69"] <- apply(aux,1,sum)
    Datos.Mort$Pop[Datos.Mort$Age.group=="60 to 69"] <- apply(aux,1,sum)
    
    
    ## Age.group="70 to 79" ##
    if(Gender=="Female"){
      aux <- Pop[,which(substr(colnames(Pop),1,1)=="f" & as.numeric(substr(colnames(Pop),2,3))>=70 & as.numeric(substr(colnames(Pop),2,3))<80)]
    }
    if(Gender=="Male"){
      aux <- Pop[,which(substr(colnames(Pop),1,1)=="m" & as.numeric(substr(colnames(Pop),2,3))>=70 & as.numeric(substr(colnames(Pop),2,3))<80)]
    }
    Datos.Inci$Pop[Datos.Inci$Age.group=="70 to 79"] <- apply(aux,1,sum)
    Datos.Mort$Pop[Datos.Mort$Age.group=="70 to 79"] <- apply(aux,1,sum)
    
    
    ## Age.group="80 and over" ##
    if(Gender=="Female"){
      aux <- Pop[,which(substr(colnames(Pop),1,1)=="f" & as.numeric(substr(colnames(Pop),2,3))>=80)]
    }
    if(Gender=="Male"){
      aux <- Pop[,which(substr(colnames(Pop),1,1)=="m" & as.numeric(substr(colnames(Pop),2,3))>=80)]
    }
    Datos.Inci$Pop[Datos.Inci$Age.group=="80 and over"] <- apply(aux,1,sum)
    Datos.Mort$Pop[Datos.Mort$Age.group=="80 and over"] <- apply(aux,1,sum)
  })
  
  ## Calcular las tasas crudas (por 100,000 habitantes) ##
  Datos.Inci$Rate <- Datos.Inci$Count/Datos.Inci$Pop*1e+5
  Datos.Mort$Rate <- Datos.Mort$Count/Datos.Mort$Pop*1e+5
  
  # La lista que devuelve la funcion tiene dos bases de datos:
  # Una para datos de incidencia, la otra para datos de mortalidad (a ambas se les ha anadido la columna de poblacion, y las tasas crudas)
  return(list(Datos.Inci=Datos.Inci, Datos.Mort=Datos.Mort))
}

##############################################################
# CONSTRUCCION DE LAS BASES DE DATOS UTILIZADAS EN LOS MODELOS

# Utilizar la función auxiliar anterior para seleccionar cancer de estomago y sexo masculino:
Stomach.Cancer <- fusionar_datos(ICD10="C16", Gender="Male")
# Seleccionar la base de incidencia
Incidence <- Stomach.Cancer$Datos.Inci

# Construir el dataframe que introduciremos al modelo
base.Inci <- data.frame(Age.group =Incidence$Age.group, Year = Incidence$Year, Region = Incidence$Region, Count = Incidence$Count, Pop = Incidence$Pop)

lista_offset <- c(); lista_cont <- c(); lista_region <- c(); lista_year <- c()
base.Inci$Region <- as.factor(base.Inci$Region)
base.Inci$Year <- as.factor(base.Inci$Year)
for (j in levels(base.Inci$Year)) {
  for (i in levels(base.Inci$Region)) {
    lista_offset <- c(lista_offset, sum(base.Inci$Pop[base.Inci$Year == j & base.Inci$Region == i]))
    lista_cont <- c(lista_cont, sum(base.Inci$Count[base.Inci$Year == j & base.Inci$Region == i]))
    lista_region <- c(lista_region, i)
    lista_year <- c(lista_year, j)
  }
}

Datos2 <- data.frame(O = lista_cont, E = lista_offset, Area = lista_region, Year = lista_year)
Data.INLA_2 <- Datos2[which(!(Datos2$Year %in% c(2018,2019,2020))),]

save(Data.INLA_2, file = "../Datos/data2001_2017.RData")
save(Datos2, file = "../Datos/Periodo_Completo.RData")
#####################################################################################
# CONSTRUCCION MATRIZ DE ESTRUCTURA ESPACIAL A PARTIR DE MATRIZ DE ADYACENCIA BINARIA
ady_bin <- as.matrix(W)
R_s <- matrix(NA,length(ady_bin[,1]),length(ady_bin[1,]))
for (i in 1:length(ady_bin[,1])) {
  for (j in 1:length(ady_bin[1,])) {
    if (i==j){
      R_s[i,j] = sum(ady_bin[i,])
    }
    if (i!=j){
      if (ady_bin[i,j]==0){
        R_s[i,j] = 0
      }
      if (ady_bin[i,j]==1){
        R_s[i,j] = -1
      }
    }
  }
}
Rs1 <- as(R_s,"dgCMatrix")
save(Rs1, file = "../Datos/R_s.RData")







