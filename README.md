# Taller-2-Economia-Urbana-

En el marco del curso **Economía Urbana (ECON 4683, 2025-2)**, este repositorio reúne código y la resolución de los distintos ejercicios propuestos, completamente replicable en **R** correspondiente al **Taller 2: Aglomeración, Segregación y Discriminación**.

Los scripts combinan **análisis econométrico** y **herramientas de análisis espacial** para abordar tres aplicaciones empíricas:

- el estudio de la **aglomeración y los precios de restaurantes en Milán**;
- el análisis de la **segregación racial y los tipping points** en la ciudad de Chicago;
- y el diseño de un **Plan de Análisis Pre-especificado (PAP)** para un experimento de correspondencia sobre **discriminación en el mercado de alquiler en Suecia**.

Si bien los códigos se desarrollaron en un entorno académico, se presentan como **ejercicios aplicados** cuyo propósito es trasladar el aprendizaje del curso a contextos prácticos y ofrecer ejemplos **reproducibles** de trabajo empírico en economía urbana.

Este repositorio se divide en dos ramas principales: **Main**, la cual es donde está el readme que estás leyendo y que contiene las principales instrucciones del ejercicio; **Taller PDF** donde está subido el taller completo en formato PDF y **Códigos**, donde se encuentran los RScripts utilizados para cada uno de los puntos. Es indispensable que, para la ejecución correcta de los códigos, se creen las carpetas **Data** y **Outputs** en la dirección de trabajo, para evitar tener problemas y una mejor organización del trabajo. La carpeta Data será donde se guarden todos las bases de datos y la carpeta Outputs será donde se depositen todas las tablas y gráficas usadas para los ejercicios que así lo requieran. 


Es indispensable que, para la ejecución correcta de los códigos, se creen las carpetas **Data** y **Outputs** en la dirección de trabajo, para evitar tener problemas y una mejor organización del trabajo. La carpeta Data será donde se guarden todos las bases de datos y la carpeta Outputs será donde se depositen todas las tablas y gráficas usadas para el ejercicio.

¡Muchas gracias!





## Ejercicio 2 – Distribución racial en Chicago

En este ejercicio analizamos la **distribución racial en la ciudad de Chicago** a lo largo del tiempo usando datos censales a nivel de *census tract*. El objetivo es documentar y cuantificar patrones de **segregación residencial** y su evolución.

El código de este ejercicio:

- Construye **mapas** de la evolución de la proporción de:
  - Afroamericanos  
  - Hispanos  
- Relaciona estos patrones espaciales con el **ingreso mediano del barrio**;
- Calcula, para varios años, los **índices de Dissimilarity e Isolation** para:
  - Afroamericanos vs. Blancos  
  - Hispanos vs. Blancos  
- Estima los **tipping points**:
  - Para minorías en general  
  - Y por grupo específico (Afroamericanos e Hispanos),
  analizando cómo cambian en el tiempo y cómo se redistribuyen los barrios a ambos lados del punto de quiebre.

En conjunto, este ejercicio ofrece un ejemplo reproducible de **medición empírica de segregación urbana**, combinando herramientas de análisis espacial, estadística descriptiva e interpretación económica de los resultados.


## Ejercicio 3 – Discriminación en el mercado de alquiler en Suecia (PAP)

Este ejercicio desarrolla un **Plan de Análisis Pre-especificado (PAP)** para un experimento de correspondencia sobre **discriminación en el mercado de alquiler de apartamentos en Suecia**, inspirado en Ahmed & Hammarstedt (2008). El documento está escrito en formato de artículo académico y pensado para ser enviado a un journal o registrado en un repositorio de PAPs.

El PAP incluye:

- **Motivación y contexto**  
  - Evidencia sobre discriminación en el acceso a vivienda y segregación urbana.  
  - Descripción del mercado inmobiliario sueco y su evolución reciente (precios, tipos de vivienda, condiciones de arriendo).

- **Hipótesis de investigación**  
  - Hipótesis principal sobre brechas de trato en las respuestas a solicitantes de distintos grupos étnicos y de género.  
  - Hipótesis secundarias de heterogeneidad por tipo de arrendador (persona natural vs. inmobiliaria) y características del inmueble.

- **Diseño experimental**  
  - Construcción de identidades ficticias (hombre sueco, hombre latino, hombre musulmán, mujer latina, etc.) a partir de nombres típicos.  
  - Selección y filtrado de anuncios en plataformas reales, reglas de inclusión/exclusión y control de duplicados.  
  - Asignación aleatoria de perfiles a anuncios y definición de la **variable de resultado** (sin respuesta, rechazo, respuesta positiva).

- **Plan de análisis estadístico**  
  - Especificación de un **modelo de probabilidad lineal (LPM)** con efectos fijos por anuncio para medir las brechas en tasas de respuesta.  
  - Extensiones no lineales (probit/logit) y modelos con interacciones para estudiar **heterogeneidad** por tipo de arrendador y características del inmueble.  
  - Discusión de variables de control, amenazas a la validez y ajustes por múltiples comparaciones.

- **Cálculos de poder estadístico**  
  - Procedimiento de simulaciones Monte Carlo basado en el generador de datos del experimento.  
  - Supuestos sobre tamaño de efecto, tasa base de respuesta y parámetros de heterogeneidad.  
  - Tablas de **poder para distintos tamaños de muestra** y para la detección de heterogeneidad en la discriminación.  
  - Discusión de **limitaciones y trade-offs del diseño** (tamaño de muestra, costos, precisión y robustez de los resultados).

En conjunto, este ejercicio muestra cómo estructurar un **PAP completo y transparente** para un experimento de campo sobre discriminación en vivienda, dejando claros desde el inicio el diseño, las hipótesis, el modelo estadístico y la capacidad del estudio para detectar los efectos de interés.



