# Funcion para definir un procedimiento de creacion de conjuntos de validacion (capitulo 4)
# Argumentos:
# -inicio y final: intervalo de tiempo que se quiere utilizar como primer periodo de validacion del modelo
# -prediccion: cantidad de anos que se quiere predecir (introducir numero entero)
# -vec_temporal: vector de todos los intervalos de tiempo sobre los que se ha realizado el estudio (niveles de la variable temporal)
# -datos: base de datos con los casos observados, offset, variable espacial y variable temporal
# -y: variable a predecir (casos observados)

validation_subsets <- function(inicio = NULL, final = NULL, prediccion = NULL, vec_temporal = NULL, datos = NULL, y = NULL){
  
  if (!(inicio %in% vec_temporal)==TRUE | !(final %in% vec_temporal)==TRUE) stop("Error: los años introducidos no se han analizado")
  
  indice_inicial <- which(inicio == vec_temporal) # controlar el indice del primer ano que se usa para estimar el modelo
  indice_final <- which(final == vec_temporal) # controlar el indice del ultimo ano que se usa para estimar el modelo
  
  if (indice_final + prediccion > length(vec_temporal)) stop("Error: no se va a predecir para años no analizados")
  
  # Identificar el indice de la columna de la componente temporal
  for (i in 1:length(names(datos))) {
    if (identical(levels(as.factor(datos[,i])),vec_temporal)){
      indice_temporal <- i
    }
  }    
  
  # Identificar el indice de la columna de la variable de observaciones
  for (j in 1:length(names(datos))) {
    if (identical(datos[,j],y)){
      indice_observaciones <- j
    }
  }  
  
  # Crear subconjuntos de validacion hasta que no tengamos anos disponibles
  while ((indice_final+prediccion) <= length(vec_temporal)) { 
    conj_validacion <- datos[which(datos[,indice_temporal] %in% vec_temporal[(indice_inicial:(indice_final+prediccion))]),] # se crea primer subconjunto con las observaciones
    conj_validacionNA <- conj_validacion
    conj_validacionNA[which(conj_validacionNA[,indice_temporal] %in% vec_temporal[c((indice_final+1):(indice_final+prediccion))]),indice_observaciones] = NA # para los ultimos "anos_predecir" anos, se ponen los casos como NA y se predice
    write.table(conj_validacion, file=paste("PeriodoValidacionObservados",vec_temporal[indice_inicial],vec_temporal[indice_final+prediccion],".txt",sep="_")) # guardar cada subconjunto de observaciones con el nombre adecuado
    write.table(conj_validacionNA, file=paste("PeriodoValidacionNA",vec_temporal[indice_inicial],vec_temporal[indice_final+prediccion],".txt",sep="_")) # guardar cada subconjunto de validacion con el nombre adecuado
    # Actualizar en una unidad los indices para pasar al siguiente periodo
    indice_inicial <- indice_inicial + 1
    indice_final <- indice_final + 1
  }
}

##################################################################################################################
# Funcion de ajuste de los modelos (capitulo 3)
# Argumentos:
# -Datos: base de datos disponible
# -O: casos observados
# -E: offset (poblacion)
# -R_s: matriz de estructura espacial, derivada de la matriz de adyacencia 
# -Var_espacial y Var_temporal: variables espacial y temporal de los datos
# -Espacial: modelo espacial a seleccionar entre "BYM" e "iCAR" 
# -Temporal_estruc: modelo temporal "RW1" 
# -Temporal_iid: "Yes" o "No" 
# -Espacio_temporal: modelo con interaccion "I", "II", "III" o "IV"

fit_model <- function(Datos = NULL, O = NULL, E = NULL, R_s = NULL, Var_espacial = NULL, Var_temporal = NULL, Espacial = NULL, Temporal_estruc = NULL, Temporal_iid = NULL, Espacio_temporal = NULL){
  # Dimensiones variables espacial y temporal
  S <- length(levels(as.factor(Var_espacial)))
  T <- length(levels(as.factor(Var_temporal)))
  
  # "Improper uniform priors" para todos los parametros de precision
  sdunif <- "expression:\nlogdens=-log_precision/2;\nreturn(logdens)"
  
  # Crear los identificadores para los efectos espacial, temporal y espaciotemporal
  I.Area <- c(rep(1:S,T))
  I.Year <- c(rep(1:T,each=S))
  I.Year1 <- c(rep(1:T,each=S))
  I.Area.Year <- seq(1,S*T)
  
  # Definir matriz de estructura espacial y matriz de estructura temporal
  R_s <- R_s                          # espacial
  D1 <- diff(diag(T),differences=1) 
  R_t <- t(D1)%*%D1                   # temporal RW1
  
  # Definir matriz producto de Kronecker necesaria para cada interaccion espaciotemporal
  R2 <- kronecker(R_t,diag(S)) #interaccion II
  R3 <- kronecker(diag(T),R_s) #interaccion III
  R4 <- kronecker(R_t,R_s) #interaccion IV
  
  # Casos observados y offset del subconjunto utilizado
  O <- O
  E <- E
  
  # Restricciones extra de cada modelo para RW1:
  # Interaccion II
  r.def.II <- S
  A.constr.II <- kronecker(matrix(1,1,T),diag(S))
  # Interaccion III
  r.def.III <- T
  A.constr.III <- kronecker(diag(T),matrix(1,1,S))  
  # Interaccion IV
  r.def.IV <- S+T-1
  A1 <- kronecker(matrix(1,1,T),diag(S))
  A2 <- kronecker(diag(T),matrix(1,1,S))  
  A.constr.IV <- rbind(A1,A2)
  
  # Formula para cada uno de los modelos
  if (Espacial == "BYM" & Temporal_estruc == "RW1" & Temporal_iid == "Yes" & Espacio_temporal == "I"){
    formula <- O ~ f(I.Area, model = "bym", graph = R_s, constr = TRUE, hyper = list(theta1 = list(prior = sdunif),theta2 = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year1, model = "iid", hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "iid", constr = TRUE, hyper = list(prec = list(prior = sdunif))) 
  }
  
  if (Espacial == "BYM" & Temporal_estruc == "RW1" & Temporal_iid == "Yes" & Espacio_temporal == "II"){
    formula <- O ~ f(I.Area, model = "bym", graph = R_s, constr = TRUE, hyper = list(theta1 = list(prior = sdunif),theta2 = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year1, model = "iid", hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R2, rankdef = r.def.II, constr = TRUE, extraconstr = list(A = A.constr.II, e = rep(0, S)), hyper = list(prec = list(prior = sdunif)))
  } 
  
  if (Espacial == "BYM" & Temporal_estruc == "RW1" & Temporal_iid == "Yes" & Espacio_temporal == "III"){
    formula <- O ~ f(I.Area, model = "bym", graph = R_s, constr = TRUE, hyper = list(theta1 = list(prior = sdunif),theta2 = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year1, model = "iid", hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R3, rankdef = r.def.III, constr = TRUE, extraconstr = list(A = A.constr.III, e = rep(0, T)), hyper = list(prec = list(prior = sdunif)))    
  }  
  
  if (Espacial == "BYM" & Temporal_estruc == "RW1" & Temporal_iid == "Yes" & Espacio_temporal == "IV"){
    formula <- O ~ f(I.Area, model = "bym", graph = R_s, constr = TRUE, hyper = list(theta1 = list(prior = sdunif),theta2 = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year1, model = "iid", hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R4, rankdef = r.def.IV, constr = TRUE, extraconstr = list(A = A.constr.IV, e = rep(0, S+T)), hyper = list(prec = list(prior = sdunif)))    
  }
  
  if (Espacial == "BYM" & Temporal_estruc == "RW1" & Temporal_iid == "No" & Espacio_temporal == "I"){
    formula <- O ~ f(I.Area, model = "bym", graph = R_s, constr = TRUE, hyper = list(theta1 = list(prior = sdunif),theta2 = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "iid", constr = TRUE, hyper = list(prec = list(prior = sdunif)))     
  }
  
  if (Espacial == "BYM" & Temporal_estruc == "RW1" & Temporal_iid == "No" & Espacio_temporal == "II"){
    formula <- O ~ f(I.Area, model = "bym", graph = R_s, constr = TRUE, hyper = list(theta1 = list(prior = sdunif),theta2 = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R2, rankdef = r.def.II, constr = TRUE, extraconstr = list(A = A.constr.II,e = rep(0, S)), hyper = list(prec = list(prior = sdunif)))    
  }
  
  if (Espacial == "BYM" & Temporal_estruc == "RW1" & Temporal_iid == "No" & Espacio_temporal == "III"){
    formula <- O ~ f(I.Area, model = "bym", graph = R_s, constr = TRUE, hyper = list(theta1 = list(prior = sdunif),theta2 = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R3, rankdef = r.def.III, constr = TRUE, extraconstr = list(A = A.constr.III, e = rep(0, T)), hyper = list(prec = list(prior = sdunif)))     
  }
  
  if (Espacial == "BYM" & Temporal_estruc == "RW1" & Temporal_iid == "No" & Espacio_temporal == "IV"){
    formula <- O ~ f(I.Area, model = "bym", graph = R_s, constr = TRUE, hyper = list(theta1 = list(prior = sdunif),theta2 = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R4, rankdef = r.def.IV, constr = TRUE, extraconstr = list(A = A.constr.IV, e = rep(0, S+T)), hyper = list(prec = list(prior = sdunif)))      
  }
  
  if (Espacial == "iCAR" & Temporal_estruc == "RW1" & Temporal_iid == "Yes" & Espacio_temporal == "I"){
    formula <- O ~ f(I.Area, model = "besag", graph = R_s, constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year1, model = "iid", hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "iid", constr = TRUE, hyper = list(prec = list(prior = sdunif)))     
  }
  
  if (Espacial == "iCAR" & Temporal_estruc == "RW1" & Temporal_iid == "Yes" & Espacio_temporal == "II"){
    formula <- O ~ f(I.Area, model = "besag", graph = R_s, constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year1, model = "iid", hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R2, rankdef = r.def.II, constr = TRUE, extraconstr = list(A = A.constr.II, e = rep(0, S)), hyper = list(prec = list(prior = sdunif)))    
  }
  
  if (Espacial == "iCAR" & Temporal_estruc == "RW1" & Temporal_iid == "Yes" & Espacio_temporal == "III"){
    formula <- O ~ f(I.Area, model = "besag", graph = R_s, constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year1, model = "iid", hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R3, rankdef = r.def.III, constr = TRUE, extraconstr = list(A = A.constr.III, e = rep(0, T)), hyper = list(prec = list(prior = sdunif)))      
  }
  
  if (Espacial == "iCAR" & Temporal_estruc == "RW1" & Temporal_iid == "Yes" & Espacio_temporal == "IV"){
    formula <- O ~ f(I.Area, model = "besag", graph = R_s, constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year1, model = "iid", hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R4, rankdef = r.def.IV, constr = TRUE, extraconstr = list(A = A.constr.IV, e = rep(0, S+T)), hyper = list(prec = list(prior = sdunif)))       
  }
  
  if (Espacial == "iCAR" & Temporal_estruc == "RW1" & Temporal_iid == "No" & Espacio_temporal == "I"){
    formula <- O ~ f(I.Area, model = "besag", graph = R_s, constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "iid", constr = TRUE, hyper = list(prec = list(prior = sdunif)))     
  }
  
  if (Espacial == "iCAR" & Temporal_estruc == "RW1" & Temporal_iid == "No" & Espacio_temporal == "II"){
    formula <- O ~ f(I.Area, model = "besag", graph = R_s, constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R2, rankdef = r.def.II, constr = TRUE, extraconstr = list(A = A.constr.II, e = rep(0, S)), hyper = list(prec = list(prior = sdunif)))       
  }
  
  if (Espacial == "iCAR" & Temporal_estruc == "RW1" & Temporal_iid == "No" & Espacio_temporal == "III"){
    formula <- O ~ f(I.Area, model = "besag", graph = R_s, constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R3, rankdef = r.def.III, constr = TRUE, extraconstr = list(A = A.constr.III, e = rep(0, T)), hyper = list(prec = list(prior = sdunif)))      
  }
  
  if (Espacial == "iCAR" & Temporal_estruc == "RW1" & Temporal_iid == "No" & Espacio_temporal == "IV"){
    formula <- O ~ f(I.Area, model = "besag", graph = R_s, constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R4, rankdef = r.def.IV, constr = TRUE, extraconstr = list(A = A.constr.IV, e = rep(0, S+T)), hyper = list(prec = list(prior = sdunif)))     
  }
  
  predic_model <- inla(formula, family='poisson', data=data.frame(Datos, I.Area, I.Year, I.Year1, I.Area.Year), E=E, 
                       control.predictor=list(link=1,compute=TRUE, cdf=c(log(1))), 
                       control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, return.marginals.predictor=TRUE), 
                       control.inla=list(strategy='simplified.laplace', int.strategy='auto', verbose=TRUE))
  return(predic_model)
  
}

##############################################################################################################################################

# Funcion de validacion del modelo seleccionado con el subconjunto seleccionado, similar a la funcion fit_model (capitulo 4)
# Argumentos:
# -Subconj_validacion: un subconjunto de validacion obtenido con la funcion validation_subsets
# -O: casos observados del subconjunto de validacion
# -E: offset (poblacion) del subconjunto de validacion
# -R_s: matriz de estructura espacial, derivada de la matriz de adyacencia 
# -Var_espacial y Var_temporal: variables espacial y temporal del subconjunto de validacion
# -Espacial: modelo espacial a seleccionar entre "BYM" e "iCAR" 
# -Temporal_estruc: modelo temporal "RW1" 
# -Temporal_iid: "Yes" o "No" 
# -Espacio_temporal: modelo con interaccion "I", "II", "III" o "IV"
# -Modelo_general: modelo ajustado para el periodo completo
# -Optimizar: toma los valores 0 (para continuar con el metodo clasico) o  1 y 2 (para utilizar la opcion control.mode(...,Restart=TRUE/FALSE)) 
#             en lo que respecta a la optimizacion de hiperparametros              

validate_model <- function(Subconj_validacion = NULL, O = NULL, E = NULL, R_s = NULL, Var_espacial = NULL, Var_temporal = NULL, Espacial = NULL, Temporal_estruc = NULL, Temporal_iid = NULL, Espacio_temporal = NULL, Modelo_general = NULL, Optimizar = NULL){
  # Dimensiones variables espacial y temporal
  S <- length(levels(as.factor(Var_espacial)))
  T <- length(levels(as.factor(Var_temporal)))
  
  # "Improper uniform priors" para todos los parametros de precision
  sdunif <- "expression:\nlogdens=-log_precision/2;\nreturn(logdens)"
  
  # Crear los identificadores para los efectos espacial, temporal y espaciotemporal
  I.Area <- c(rep(1:S,T))
  I.Year <- c(rep(1:T,each=S))
  I.Year1 <- c(rep(1:T,each=S))
  I.Area.Year <- seq(1,S*T)
  
  # Definir matriz de estructura espacial y matriz de estructura temporal
  R_s <- R_s                          # espacial
  D1 <- diff(diag(T),differences=1) 
  R_t <- t(D1)%*%D1                   # temporal RW1
  
  # Definir matriz producto de Kronecker necesaria para cada interaccion espaciotemporal
  R2 <- kronecker(R_t,diag(S)) #interaccion II
  R3 <- kronecker(diag(T),R_s) #interaccion III
  R4 <- kronecker(R_t,R_s) #interaccion IV
  
  # Casos observados y offset del subconjunto utilizado
  O <- O
  E <- E
  
  # Restricciones extra de cada modelo para RW1:
  # Interaccion II
  r.def.II <- S
  A.constr.II <- kronecker(matrix(1,1,T),diag(S))
  # Interaccion III
  r.def.III <- T
  A.constr.III <- kronecker(diag(T),matrix(1,1,S))  
  # Interaccion IV
  r.def.IV <- S+T-1
  A1 <- kronecker(matrix(1,1,T),diag(S))
  A2 <- kronecker(diag(T),matrix(1,1,S))  
  A.constr.IV <- rbind(A1,A2)
  
  # Formula para cada uno de los modelos
  if (Espacial == "BYM" & Temporal_estruc == "RW1" & Temporal_iid == "Yes" & Espacio_temporal == "I"){
    formula <- O ~ f(I.Area, model = "bym", graph = R_s, constr = TRUE, hyper = list(theta1 = list(prior = sdunif),theta2 = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year1, model = "iid", hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "iid", constr = TRUE, hyper = list(prec = list(prior = sdunif))) 
  }
  
  if (Espacial == "BYM" & Temporal_estruc == "RW1" & Temporal_iid == "Yes" & Espacio_temporal == "II"){
    formula <- O ~ f(I.Area, model = "bym", graph = R_s, constr = TRUE, hyper = list(theta1 = list(prior = sdunif),theta2 = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year1, model = "iid", hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R2, rankdef = r.def.II, constr = TRUE, extraconstr = list(A = A.constr.II, e = rep(0, S)), hyper = list(prec = list(prior = sdunif)))
  } 
  
  if (Espacial == "BYM" & Temporal_estruc == "RW1" & Temporal_iid == "Yes" & Espacio_temporal == "III"){
    formula <- O ~ f(I.Area, model = "bym", graph = R_s, constr = TRUE, hyper = list(theta1 = list(prior = sdunif),theta2 = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year1, model = "iid", hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R3, rankdef = r.def.III, constr = TRUE, extraconstr = list(A = A.constr.III, e = rep(0, T)), hyper = list(prec = list(prior = sdunif)))    
  }  
  
  if (Espacial == "BYM" & Temporal_estruc == "RW1" & Temporal_iid == "Yes" & Espacio_temporal == "IV"){
    formula <- O ~ f(I.Area, model = "bym", graph = R_s, constr = TRUE, hyper = list(theta1 = list(prior = sdunif),theta2 = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year1, model = "iid", hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R4, rankdef = r.def.IV, constr = TRUE, extraconstr = list(A = A.constr.IV, e = rep(0, S+T)), hyper = list(prec = list(prior = sdunif)))    
  }
  
  if (Espacial == "BYM" & Temporal_estruc == "RW1" & Temporal_iid == "No" & Espacio_temporal == "I"){
    formula <- O ~ f(I.Area, model = "bym", graph = R_s, constr = TRUE, hyper = list(theta1 = list(prior = sdunif),theta2 = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "iid", constr = TRUE, hyper = list(prec = list(prior = sdunif)))     
  }
  
  if (Espacial == "BYM" & Temporal_estruc == "RW1" & Temporal_iid == "No" & Espacio_temporal == "II"){
    formula <- O ~ f(I.Area, model = "bym", graph = R_s, constr = TRUE, hyper = list(theta1 = list(prior = sdunif),theta2 = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R2, rankdef = r.def.II, constr = TRUE, extraconstr = list(A = A.constr.II,e = rep(0, S)), hyper = list(prec = list(prior = sdunif)))    
  }
  
  if (Espacial == "BYM" & Temporal_estruc == "RW1" & Temporal_iid == "No" & Espacio_temporal == "III"){
    formula <- O ~ f(I.Area, model = "bym", graph = R_s, constr = TRUE, hyper = list(theta1 = list(prior = sdunif),theta2 = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R3, rankdef = r.def.III, constr = TRUE, extraconstr = list(A = A.constr.III, e = rep(0, T)), hyper = list(prec = list(prior = sdunif)))     
  }
  
  if (Espacial == "BYM" & Temporal_estruc == "RW1" & Temporal_iid == "No" & Espacio_temporal == "IV"){
    formula <- O ~ f(I.Area, model = "bym", graph = R_s, constr = TRUE, hyper = list(theta1 = list(prior = sdunif),theta2 = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R4, rankdef = r.def.IV, constr = TRUE, extraconstr = list(A = A.constr.IV, e = rep(0, S+T)), hyper = list(prec = list(prior = sdunif)))      
  }
  
  if (Espacial == "iCAR" & Temporal_estruc == "RW1" & Temporal_iid == "Yes" & Espacio_temporal == "I"){
    formula <- O ~ f(I.Area, model = "besag", graph = R_s, constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year1, model = "iid", hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "iid", constr = TRUE, hyper = list(prec = list(prior = sdunif)))     
  }
  
  if (Espacial == "iCAR" & Temporal_estruc == "RW1" & Temporal_iid == "Yes" & Espacio_temporal == "II"){
    formula <- O ~ f(I.Area, model = "besag", graph = R_s, constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year1, model = "iid", hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R2, rankdef = r.def.II, constr = TRUE, extraconstr = list(A = A.constr.II, e = rep(0, S)), hyper = list(prec = list(prior = sdunif)))    
  }
  
  if (Espacial == "iCAR" & Temporal_estruc == "RW1" & Temporal_iid == "Yes" & Espacio_temporal == "III"){
    formula <- O ~ f(I.Area, model = "besag", graph = R_s, constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year1, model = "iid", hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R3, rankdef = r.def.III, constr = TRUE, extraconstr = list(A = A.constr.III, e = rep(0, T)), hyper = list(prec = list(prior = sdunif)))      
  }
  
  if (Espacial == "iCAR" & Temporal_estruc == "RW1" & Temporal_iid == "Yes" & Espacio_temporal == "IV"){
    formula <- O ~ f(I.Area, model = "besag", graph = R_s, constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year1, model = "iid", hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R4, rankdef = r.def.IV, constr = TRUE, extraconstr = list(A = A.constr.IV, e = rep(0, S+T)), hyper = list(prec = list(prior = sdunif)))       
  }
  
  if (Espacial == "iCAR" & Temporal_estruc == "RW1" & Temporal_iid == "No" & Espacio_temporal == "I"){
    formula <- O ~ f(I.Area, model = "besag", graph = R_s, constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "iid", constr = TRUE, hyper = list(prec = list(prior = sdunif)))     
  }
  
  if (Espacial == "iCAR" & Temporal_estruc == "RW1" & Temporal_iid == "No" & Espacio_temporal == "II"){
    formula <- O ~ f(I.Area, model = "besag", graph = R_s, constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R2, rankdef = r.def.II, constr = TRUE, extraconstr = list(A = A.constr.II, e = rep(0, S)), hyper = list(prec = list(prior = sdunif)))       
  }
  
  if (Espacial == "iCAR" & Temporal_estruc == "RW1" & Temporal_iid == "No" & Espacio_temporal == "III"){
    formula <- O ~ f(I.Area, model = "besag", graph = R_s, constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R3, rankdef = r.def.III, constr = TRUE, extraconstr = list(A = A.constr.III, e = rep(0, T)), hyper = list(prec = list(prior = sdunif)))      
  }
  
  if (Espacial == "iCAR" & Temporal_estruc == "RW1" & Temporal_iid == "No" & Espacio_temporal == "IV"){
    formula <- O ~ f(I.Area, model = "besag", graph = R_s, constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Year, model = "rw1",constr = TRUE, hyper = list(prec = list(prior = sdunif))) + 
      f(I.Area.Year, model = "generic0", Cmatrix = R4, rankdef = r.def.IV, constr = TRUE, extraconstr = list(A = A.constr.IV, e = rep(0, S+T)), hyper = list(prec = list(prior = sdunif)))     
  }
  
  if (Optimizar == 0){
  predic_model <- inla(formula, family='poisson', data=data.frame(Subconj_validacion, I.Area, I.Year, I.Year1, I.Area.Year), E=E, 
                       control.predictor=list(link=1,compute=TRUE, cdf=c(log(1))), 
                       control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, return.marginals.predictor=TRUE), 
                       control.inla=list(strategy='simplified.laplace', int.strategy='auto', verbose=TRUE))
  }
  
  if (Optimizar == 1){
    predic_model <- inla(formula, family='poisson', data=data.frame(Subconj_validacion, I.Area, I.Year, I.Year1, I.Area.Year), E=E, 
                         control.predictor=list(link=1,compute=TRUE, cdf=c(log(1))), 
                         control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, return.marginals.predictor=TRUE), 
                         control.inla=list(strategy='simplified.laplace', int.strategy='auto', verbose=TRUE),
                         control.mode = list(theta = Modelo_general$mode$theta, restart = TRUE))  # AQUI ESTA EL TRUCO 
  } 
  if (Optimizar == 2){
    predic_model <- inla(formula, family='poisson', data=data.frame(Subconj_validacion, I.Area, I.Year, I.Year1, I.Area.Year), E=E, 
                         control.predictor=list(link=1,compute=TRUE, cdf=c(log(1))), 
                         control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE, return.marginals.predictor=TRUE), 
                         control.inla=list(strategy='simplified.laplace', int.strategy='auto', verbose=TRUE),
                         control.mode = list(theta = Modelo_general$mode$theta, restart = FALSE))  # AQUI ESTA EL TRUCO 
  } 
  return(predic_model)
}
