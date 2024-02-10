# TFM_MikelBergara
Este repositorio contiene el código R utilizado para reproducir y replicar el análisis de datos del Trabajo Fin de Máster ["Validación de modelos predictivos espacio-temporales de la incidencia y mortalidad por cáncer"](https://github.com/spatialstatisticsupna/TFM_MikelBergara/blob/main/TFM_MikelBergara.pdf) realizado por Mikel Bergara Martinez (bajo la supervisión de Aritz Adin y Jaione Etxeberria) en el [Máster de Modelización e Investigación Matemática, Estadística y Computación](https://www.unavarra.es/sites/masteres/ciencias/modelizacion-invest-matematica/presentacion.html) de la Universidad Pública de Navarra.


## Índice

- [Datos](#Datos)
- [Código R](#Código-r)
- [Agradecimientos](#Agradecimientos)

# Datos

Esta carpeta contiene los ficheros con datos reales de cáncer necesarios para realizar el análisis práctico de los capítulos 3 y 4 del trabajo, es decir, el análisis descriptivo, la modelización espacio-temporal y el desarrollo del método de validación y predicción realizado mediante el uso de datos oficiales de Inglaterra. 

- [**Carto_England.Rdata**](https://github.com/spatialstatisticsupna/TFM_MikelBergara/blob/main/Datos/Carto_England.Rdata)

Este archivo contiene dos objetos. Por un lado, la cartografía de Inglaterra "**carto**" (105 _clinical commissioning groups_) y por otro lado la matriz de adyacencia binaria "**W**" correspondiente al grafo de vecindad de las 105 regiones de estudio inglesas.

- [**Carto_England_shapefile**](https://github.com/spatialstatisticsupna/TFM_MikelBergara/tree/main/Datos/Carto_England_shapefile)

Esta carpeta contiene los archivos _shapefile_ (cartografía inglesa) utilizados en la aplicación [SSTCDApp](https://emi-sstcdapp.unavarra.es/Login/) empleada en el aprendizaje teórico de la modelización espacio-temporal.

- [**England_database.Rdata**](https://github.com/spatialstatisticsupna/TFM_MikelBergara/blob/main/Datos/England_database.Rdata)

Este archivo contiene tres objetos. Por un lado, una base de datos de la población inglesa ("**Pop**") desagregada por año de estudio (2001-2020), región (105 _clinical commissioning groups_) sexo y edad. Por otro lado, las dos bases de datos correspondientes a la incidencia y mortalidad ("**Inci**" y "**Mort**") por cáncer desagregadas tal que cada base cuenta con las siguientes columnas: 

  - **_Year_**: año de estudio (periodo 2001-2020)
  - **_Gender_**: sexo del individuo
  - **_Age.group_**: grupo de edad del individuo
  - **_Region_**: nombre de la región en el sistema inglés 
  - **_ICD10_code_**: identificador del tipo de cáncer
  - **_Cancer_site_**: nombre del cáncer 
  - **_Count_**: casos registrados

_*Fuente de datos_: Sistema nacional de salud para Inglaterra [(NHS England)](https://www.cancerdata.nhs.uk/incidence_and_mortality) y Oficina de Estadística Nacional [(ONS)](https://www.ons.gov.uk/peoplepopulationandcommunity).

# Código R

El código de R correspondiente a los capítulos 3 y 4 del trabajo, se divide en los siguientes scripts:

- [**Auxiliares.R**](https://github.com/spatialstatisticsupna/TFM_MikelBergara/blob/main/R/Auxiliares.R)

Este primer script define tres funciones auxiliares ("**validation_subsets**", "**fit_model**" y "**validate_model**") que serán de utilidad en el resto de scripts para ajustar modelos espacio-temporales, además de facilitar la creación un proceso de validación para realizar predicciones a corto plazo.

- [**Bases_modelos.R**](https://github.com/spatialstatisticsupna/TFM_MikelBergara/blob/main/R/Bases_modelos.R)

Este script nos permite crear y simplificar las bases de datos que se utilizarán para ajustar los modelos, partiendo de los conjuntos de datos iniciales presentados anteriormente. Se obtiene también la matriz de estructura espacial partiendo de la matriz de adyacencia binaria.

- [**Analisis_Descriptivo.R**](https://github.com/spatialstatisticsupna/TFM_MikelBergara/blob/main/R/Analisis_Descriptivo.R)

Este script presenta el análisis descriptivo realizado utilizando diferentes tipos de cáncer, para datos de incidencia y mortalidad. En concreto, el análisis realizado se divide en tres secciones: patrón espacial, patrón temporal y patrón espacio-temporal.

- [**ModelosEspacioTemporales.R**](https://github.com/spatialstatisticsupna/TFM_MikelBergara/blob/main/R/ModelosEspacioTemporales.R)

Este script reproduce el ajuste de los 8 modelos espacio-temporales (utilizando los datos de incidencia del cáncer de estómago en hombres) mediante el uso de INLA. Además, se crea una tabla de medidas de validación mediante la cual se seleccionan los dos mejores modelos. Finalmente, se muestran los resultados obtenidos para estos dos últimos modelos (distribuciones a posteriori para el intercepto e hiperparámetros, junto a la representación cartográfica de las tasas estimadas).

- [**ValidationProcess.R**](https://github.com/spatialstatisticsupna/TFM_MikelBergara/blob/main/R/ValidationProcess.R)

Este script simula el proceso de validación y predicción desarrollado en el capítulo 4. Primero, se crean diferentes subconjuntos de validación partiendo del periodo completo 2001-2017, y después se ajustan a cada uno de ellos los 4 modelos BYM definidos en el capítulo 3. Se utilizan diferentes medidas de validación para determinar el mejor de los modelos respecto a su capacidad predictiva, y se realizan predicciones de tasas de incidencia de cáncer de estómago a corto plazo (se predicen los años 2018, 2019 y 2020). 

- [**ReduccionCostes.R**](https://github.com/spatialstatisticsupna/TFM_MikelBergara/blob/main/R/ReduccionCostes.R)

Este script desarrolla el método de reducción de costes (en lo referente al proceso de validación) definido en la sección 4.2 del capítulo 4. Se realizan las comprobaciones necesarias para determinar que la metodología empleada es válida, y se calculan los tiempos de ejecución de todos los modelos ajustados a cada subconjunto de validación.


# Agradecimientos
Este Trabajo Fin de Máster ha sido realizado bajo la financiación de las Ayudas de Iniciación a la Investigación de la Universidad Pública de Navarra en el ámbito de sus institutos de Investigación durante el curso académico 2022-2023 ([resolución nº 2359/2022](https://www2.unavarra.es/gesadj/sede/INVESTIGACION2022/RES2359_Iniciacion22-23.pdf)).
