# 🌍 Dashboard Gapminder: Desarrollo Mundial Interactivo

## Producto Académico Colaborativo

Este proyecto desarrolla un dashboard interactivo utilizando **R**, **Shiny** y la base de datos **Gapminder**, con el objetivo de explorar indicadores globales de desarrollo humano mediante visualizaciones dinámicas e interactivas.

El dashboard permite analizar información relacionada con:

* Esperanza de vida
* PIB per cápita
* Población
* Brechas entre países y continentes
* Índice de Desarrollo Relativo
* Evolución temporal del desarrollo mundial

---

# 👥 Integrantes

* Jose Manuel Armaza Nalvarte
* Ruth Sandra Jara Oré
* Emerson Dennis Paucar Cuipal
* Randal Manuel Unsueta Quispe
* Oscar Gary Yaringaño Pizarro

---

# 🎯 Objetivo del Proyecto

Diseñar un dashboard interactivo orientado al análisis del desarrollo mundial utilizando la base de datos Gapminder, permitiendo comparar países, continentes y tendencias históricas mediante visualizaciones dinámicas que faciliten la interpretación de datos y la toma de decisiones.

---

# 🔗 Enlaces del Proyecto

## Repositorio GitHub

[https://github.com/epaucarc/Grupo-2-ME](https://github.com/epaucarc/Grupo-2-ME)

## Dashboard publicado en shinyapps.io

[https://emerson-cuipal.shinyapps.io/grupo-2-me/](https://emerson-cuipal.shinyapps.io/grupo-2-me/)

---

# 🛠️ Herramientas Utilizadas

| Herramienta  | Descripción                                        |
| ------------ | -------------------------------------------------- |
| R            | Lenguaje de programación para análisis estadístico |
| RStudio      | Entorno de desarrollo integrado                    |
| Shiny        | Framework para dashboards interactivos             |
| ggplot2      | Visualización estadística                          |
| plotly       | Interactividad gráfica                             |
| dplyr        | Manipulación y transformación de datos             |
| DT           | Tablas dinámicas                                   |
| GitHub       | Control de versiones y trabajo colaborativo        |
| shinyapps.io | Publicación del dashboard                          |

---

# 🌐 Base de Datos Utilizada

## Dataset Gapminder

La base de datos Gapminder contiene información histórica sobre indicadores de desarrollo mundial entre 1952 y 2007.

## Variables principales

| Variable  | Descripción       |
| --------- | ----------------- |
| country   | País              |
| continent | Continente        |
| year      | Año               |
| lifeExp   | Esperanza de vida |
| pop       | Población         |
| gdpPercap | PIB per cápita    |

---

# ⚙️ Funcionalidades del Dashboard

El dashboard implementa diversas funcionalidades orientadas al análisis interactivo:

✅ Filtros dinámicos por continente, país y año.

✅ KPIs automáticos.

✅ Mapa mundial interactivo.

✅ Evolución temporal de indicadores.

✅ Comparación entre países y continentes.

✅ Ranking de países.

✅ Brechas de desarrollo.

✅ Índice de Desarrollo Relativo.

✅ Insights automáticos.

✅ Tabla interactiva.

---

# 🧭 Arquitectura del Dashboard

```text
Base de datos Gapminder
            ↓
Procesamiento y transformación en R
            ↓
Filtros dinámicos
            ↓
Visualizaciones interactivas
            ↓
Generación de insights
            ↓
Análisis y toma de decisiones
```

---

# 📊 Dashboard Principal

## Panorama Mundial del Desarrollo

El dashboard principal presenta la relación entre:

* PIB per cápita
* Esperanza de vida
* Población
* Continente

Esta visualización permite identificar patrones globales de desarrollo y diferencias estructurales entre regiones.

![Mapa Mundial](Imagenes/Mapa.png)

---

# 🌎 Mapa Mundial Interactivo

El mapa mundial permite visualizar territorialmente la esperanza de vida de cada país según el año seleccionado.

## Principales insights

* Europa y Oceanía presentan mayores niveles de esperanza de vida.
* África presenta menores indicadores relativos.
* América y Asia muestran comportamientos intermedios y heterogéneos.

El mapa facilita la identificación espacial de brechas de desarrollo.

![Mapa Mundial](Imagenes/Mapa.png)

---

# 📈 Evolución Temporal

El dashboard permite analizar la evolución histórica de la esperanza de vida.

Dependiendo de los filtros seleccionados, el sistema muestra:

* Evolución promedio por continente.
* Evolución de países de un continente.
* Comparación entre país seleccionado, promedio continental y promedio mundial.

## Insight principal

La evolución temporal permite identificar tendencias, rezagos y mejoras relativas entre países y regiones.

---

# 💰 Relación PIB vs Esperanza de Vida

La visualización compara el comportamiento económico y social de los países.

## Principales hallazgos

* Existe una relación positiva entre PIB per cápita y esperanza de vida.
* Los países con mayores ingresos presentan, en promedio, mejores condiciones de vida.
* Se observan diferencias importantes entre regiones.

---

# 🏆 Ranking de Países

El dashboard incorpora rankings dinámicos que permiten identificar:

* Países con mayor esperanza de vida.
* Países con menor esperanza de vida.

## Utilidad analítica

Facilita reconocer líderes y rezagos en indicadores de desarrollo humano.

---

# ⚠️ Brechas de Desarrollo

La sección de brechas permite analizar desigualdades:

* Entre continentes.
* Entre países de un continente.

## Insight principal

Una mayor diferencia en esperanza de vida refleja mayores desigualdades estructurales.

Esta visualización ayuda a identificar territorios con mayor disparidad interna.

---

# 🧠 Índice de Desarrollo Relativo

Se construyó un indicador propio considerando:

* 60% esperanza de vida normalizada.
* 40% PIB per cápita normalizado.

El índice clasifica países según niveles relativos de desarrollo.

## Clasificación

| Nivel    | Interpretación           |
| -------- | ------------------------ |
| Muy alto | Alto desarrollo relativo |
| Alto     | Desarrollo favorable     |
| Medio    | Desarrollo intermedio    |
| Bajo     | Rezago relativo          |

![Índice de Desarrollo](Imagenes/Índice de desarrollo.png)

---

# 📋 Tabla Interactiva

El dashboard incorpora una tabla dinámica que permite:

* Buscar países.
* Ordenar variables.
* Comparar indicadores.
* Explorar información histórica.

## Variables mostradas

* País
* Continente
* Esperanza de vida
* PIB per cápita
* Población
* Índice de desarrollo relativo

![Tabla Resumen](Imagenes/Tabla_resumen.png)

---

# 💡 Insights Automáticos

El dashboard genera insights automáticos relacionados con:

* País con mayor esperanza de vida.
* País con menor esperanza de vida.
* País con mayor PIB per cápita.
* Países por debajo del promedio mundial.
* País con mayor índice de desarrollo relativo.

## Valor agregado

La incorporación de insights fortalece la interpretación analítica y facilita la toma de decisiones.

---

# ✅ Ventajas del Dashboard

* Interactividad dinámica.
* Comparación territorial.
* Visualizaciones intuitivas.
* Análisis temporal.
* Integración de múltiples indicadores.
* Fácil interpretación visual.
* Accesibilidad web mediante shinyapps.io.

---

# ⚠️ Limitaciones

* La base de datos solo incluye información hasta 2007.
* Algunos países presentan menor disponibilidad histórica.
* El índice de desarrollo relativo es un indicador experimental.
* No incorpora variables políticas o ambientales.

---

# 🚀 Mejoras Futuras

* Incorporar mapas avanzados.
* Añadir modelos predictivos.
* Integrar machine learning.
* Incorporar más indicadores sociales.
* Generar reportes automáticos.
* Mejorar el diseño visual con dashboards ejecutivos.

---

# 📌 Conclusiones

* Shiny permite desarrollar dashboards interactivos orientados al análisis de datos.
* Gapminder facilita el estudio comparativo del desarrollo mundial.
* Existen diferencias significativas entre continentes y países.
* Las visualizaciones interactivas favorecen la interpretación de información.
* El dashboard fortalece el análisis basado en evidencia para la toma de decisiones.

---

# 📚 Referencias Bibliográficas

* Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer.
* Chang, W. (2021). *Shiny: Web Application Framework for R*. R package version.
* Gapminder Foundation. (2024). *Gapminder Data*. [https://www.gapminder.org/](https://www.gapminder.org/)
* Xie, Y. (2023). *R Markdown: The Definitive Guide*. Chapman and Hall/CRC.

---

# 🙌 Gracias

## Dashboard Gapminder: Desarrollo Mundial Interactivo

### Producto Académico Colaborativo

### Grupo 2 ME
