# TFM_MikelBergara
Este repositorio contiene el código R utilizado para reproducir y replicar el análisis de datos del Trabajo Fin de Máster ["Validación de modelos predictivos espacio-temporales de la incidencia y mortalidad por cáncer"](https://github.com/spatialstatisticsupna/TFM_MikelBergara/blob/main/TFM_MikelBergara.pdf) realizado por Mikel Bergara Martinez (bajo la supervisión de Aritz Adin y Jaione Etxeberria) en el [Máster de Modelización e Investigación Matemática, Estadística y Computación](https://www.unavarra.es/sites/masteres/ciencias/modelizacion-invest-matematica/presentacion.html) de la Universidad Pública de Navarra.


## Índice

- [Datos](#Datos)
- [Código R](#Código-r)
- [Agradecimientos](#Agradecimientos)

# Datos

Esta carpeta contiene los ficheros con datos reales de cáncer necesarios para realizar el análisis práctico de los capítulos 3 y 4 del trabajo, es decir, el análisis descriptivo, la modelización espacio-temporal y el desarrollo del método de validación y predicción realizado mediante el uso de datos oficiales de Inglaterra. 

-[**Carto_England.Rdata**](https://github.com/spatialstatisticsupna/TFM_MikelBergara/blob/main/Datos/Carto_England.Rdata)

Este archivo contiene dos objetos. Por un lado, la cartografía de Inglaterra "**carto**" (105 _clinical commissioning groups_) y por otro lado la matriz de adyacencia binaria "**W**" correspondiente al grafo de vecindad de las 105 regiones de estudio inglesas.

-[**Carto_England_shapefile**](https://github.com/spatialstatisticsupna/TFM_MikelBergara/tree/main/Datos/Carto_England_shapefile)

Esta carpeta contiene los archivos _shapefile_ (cartografía inglesa) utilizados en la aplicación [SSTCDApp](https://emi-sstcdapp.unavarra.es/Login/) empleada en el aprendizaje teórico de la modelización espacio-temporal.

-[**England_database.Rdata**](https://github.com/spatialstatisticsupna/TFM_MikelBergara/blob/main/Datos/England_database.Rdata)

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

# Agradecimientos
Este Trabajo Fin de Máster ha sido realizado bajo la financiación de las Ayudas de Iniciación a la Investigación de la Universidad Pública de Navarra en el ámbito de sus institutos de Investigación durante el curso académico 2022-2023 ([resolución 2359/2022](https://www2.unavarra.es/gesadj/sede/INVESTIGACION2022/RES2359_Iniciacion22-23.pdf)).



# Datos

Esta carpeta contiene los ficheros con los que se ha realizado el tercer capítulo del trabajo, en el que se ilustra el funcionamiento de los modelos con los datos reales de cáncer.

- [**Datos_gb_f.csv**](https://github.com/spatialstatisticsupna/TFM_AnderBodegas/blob/main/Datos/Datos_gb_f.csv) y [**Datos_gb_m.csv**](https://github.com/spatialstatisticsupna/TFM_AnderBodegas/blob/main/Datos/Datos_gb_m.csv)

  Bases de datos de incidencia y mortalidad po cáncer en la isla de Gran Bretaña para 11 causas (leucemia, mama, cérvix, melanoma, hígado, colorrectal, páncreas, estómago, pulmón, vejiga y esófago) desagregadas por área y año. Cada fichero contiene las siguientes variables:
  
    - **_Code_**: código identificador del área (S=142 regiones)
    - **_Year_**: identificador del año (periodo 2002-2019)
    - **_Population_**: población en riesgo
    - **_Count_All_**: número total de casos registrados
    - **_Deaths_All_**: número total de muertes registradas
    - **_Count_xxx_**: número total de casos registrados para cada una de las 11 causas 
    - **_Deaths_xxx_**: número total de muertes registradas para cada una de las 11 causas

  _*Fuente de datos_: Sistema nacional de salud para Inglaterra [(NHS England)](https://www.cancerdata.nhs.uk/incidence_and_mortality), Gales [(NHS Wales)](https://phw.nhs.wales/services-and-teams/welsh-cancer-intelligence-and-surveillance-unit-wcisu/) y Escocia [(NHS Scotland)](https://www.opendata.nhs.scot/dataset).
  
- [**Carto**](https://github.com/spatialstatisticsupna/TFM_AnderBodegas/blob/main/Datos/Carto/)

  Esta carpeta contiene la cartografía (archivos _shapefile_) de las regiones de Gran Bretaña (106 _clinical commissioning group0s_ para Inglaterra, 22 _local authorities_ para Gales y 14 _health boards_ para Escocia), además de la matriz de adyacencia espacial.
  
- [**adj_bg.txt**](https://github.com/spatialstatisticsupna/TFM_AnderBodegas/blob/main/Datos/Carto/adj_gb.txt)

  Este archivo contiene la matriz de adyacencia (binaria) correspondiente al grafo de vecindad de las 142 regiones bajo estudio. El grafo original ha sido modificado (se han añadido 9 conexiones extra) para conectar las islas y así obtener un grafo conexo.





