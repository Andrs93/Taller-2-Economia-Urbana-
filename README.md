# Taller-2-Economia-Urbana-

En el marco del curso **Economía Urbana (ECON 4683, 2025-2)**, este repositorio reúne código y la resolución de los distintos ejercicios propuestos, completamente replicable en **R** correspondiente al **Taller 2: Aglomeración, Segregación y Discriminación**.

Los scripts combinan **análisis econométrico** y **herramientas de análisis espacial** para abordar tres aplicaciones empíricas:

- el estudio de la **aglomeración y los precios de restaurantes en Milán**;
- el análisis de la **segregación racial y los tipping points** en la ciudad de Chicago;
- y el diseño de un **Plan de Análisis Pre-especificado (PAP)** para un experimento de correspondencia sobre **discriminación en el mercado de alquiler en Suecia**.

Si bien los códigos se desarrollaron en un entorno académico, se presentan como **ejercicios aplicados** cuyo propósito es trasladar el aprendizaje del curso a contextos prácticos y ofrecer ejemplos **reproducibles** de trabajo empírico en economía urbana.

Es indispensable que, para la ejecución correcta de los códigos, se creen las carpetas **Data** y **Outputs** en la dirección de trabajo, para evitar tener problemas y una mejor organización del trabajo. La carpeta Data será donde se guarden todos las bases de datos y la carpeta Outputs será donde se depositen todas las tablas y gráficas usadas para el ejercicio.





## Ejercicio 2 – Distribución racial en Chicago

En este ejercicio analizamos la **distribución racial en la ciudad de Chicago** a lo largo del tiempo usando datos censales a nivel de *census tract*. El objetivo es documentar y cuantificar patrones de **segregación residencial** y su evolución.

El código de este ejercicio:

- construye **mapas** de la evolución de la proporción de:
  - Afroamericanos  
  - Hispanos  
- relaciona estos patrones espaciales con el **ingreso mediano del barrio**;
- calcula, para varios años, los **índices de Dissimilarity e Isolation** para:
  - Afroamericanos vs. Blancos  
  - Hispanos vs. Blancos  
- estima los **tipping points**:
  - para minorías en general  
  - y por grupo específico (Afroamericanos e Hispanos),
  analizando cómo cambian en el tiempo y cómo se redistribuyen los barrios a ambos lados del punto de quiebre.

En conjunto, este ejercicio ofrece un ejemplo reproducible de **medición empírica de segregación urbana**, combinando herramientas de análisis espacial, estadística descriptiva e interpretación económica de los resultados.




