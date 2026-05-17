# 🌍 Dashboard Interactivo de Desarrollo Mundial- Banco Mundial - Gapminder
## Producto Académico Colaborativo – Grupo 2 ME

Este proyecto desarrolla un **dashboard interactivo en R Shiny** para analizar indicadores de desarrollo mundial a partir de datos de **Gapminder** y datos complementarios del **Banco Mundial**.

El dashboard permite explorar la evolución de indicadores como esperanza de vida, PIB per cápita, población, brechas entre países y continentes, así como un Índice de Desarrollo Relativo construido para facilitar la comparación territorial.

---

## 👥 Integrantes

- Jose Manuel Armaza Nalvarte
- Ruth Sandra Jara Oré
- Emerson Dennis Paucar Cuipal
- Randal Manuel Unsueta Quispe
- Oscar Gary Yaringaño Pizarro
- Luis Felipe Veintemilla Villacorta

---

## 🎯 Objetivo del proyecto

Diseñar un dashboard interactivo que permita analizar, comparar y visualizar indicadores de desarrollo mundial mediante herramientas de ciencia de datos, facilitando la interpretación de tendencias históricas, brechas territoriales y patrones de desarrollo entre países y continentes.

---

## 🔗 Enlaces del proyecto

### 📁 Repositorio GitHub  
https://github.com/epaucarc/Grupo-2-ME

### 🌐 Dashboard publicado en shinyapps.io  
https://emerson-cuipal.shinyapps.io/grupo-2-me/

---

# 🛠️ Herramientas utilizadas

| Herramienta | Descripción |
|---|---|
| R | Lenguaje de programación para análisis estadístico |
| RStudio | Entorno de desarrollo |
| Shiny | Framework para construir aplicaciones web interactivas |
| ggplot2 | Visualización estadística |
| plotly | Gráficos interactivos |
| dplyr | Limpieza y transformación de datos |
| leaflet | Mapas interactivos |
| DT | Tablas dinámicas |
| countrycode | Homologación de países y códigos ISO |
| WDI | Consulta de indicadores del Banco Mundial |
| GitHub | Control de versiones |
| shinyapps.io | Publicación del dashboard |

---

# 🌐 Fuentes de datos

## Gapminder

Gapminder proporciona información histórica sobre indicadores de desarrollo humano y económico de distintos países.

| Variable | Descripción |
|---|---|
| country | País |
| continent | Continente |
| year | Año |
| lifeExp | Esperanza de vida |
| pop | Población |
| gdpPercap | PIB per cápita |

---

## Banco Mundial

Se incorporan datos complementarios del Banco Mundial para actualizar y ampliar el análisis del desarrollo mundial, permitiendo superar la limitación temporal de Gapminder, cuya base histórica llega hasta 2023.

---

# 🌐 Gapminder y Banco Mundial: relación y diferencias

El dashboard integra información proveniente de **Gapminder** y del **Banco Mundial**, dos fuentes ampliamente utilizadas para el análisis de indicadores de desarrollo global.

## 🔹 ¿Qué tienen en común?

Ambas bases de datos trabajan con indicadores relacionados con:

- Desarrollo humano
- Economía
- Salud
- Población
- Calidad de vida
- Comparación entre países

Además, gran parte de la información utilizada por Gapminder proviene originalmente de organismos internacionales como el propio Banco Mundial, Naciones Unidas y la OMS.

---

## 🔹 Principales diferencias

| Aspecto | Gapminder | Banco Mundial |
|---|---|---|
| Enfoque | Visualización y divulgación educativa | Información estadística oficial |
| Cobertura temporal | Principalmente 1952–2007 | Información continuamente actualizada |
| Complejidad | Datos simplificados y listos para análisis | Gran volumen de indicadores especializados |
| Facilidad de uso | Alta | Media |
| Objetivo principal | Comprensión visual del desarrollo mundial | Monitoreo y análisis estadístico global |

---

## 🔹 ¿Por qué es importante integrar ambas fuentes?

La integración de ambas bases fortalece el análisis del dashboard debido a que:

✅ Gapminder facilita la exploración visual e histórica del desarrollo mundial.

✅ Banco Mundial permite actualizar indicadores y ampliar el análisis territorial y temporal.

✅ La combinación mejora la comparación entre países y continentes.

✅ Se fortalece el análisis basado en evidencia mediante datos internacionales reconocidos.

✅ Permite comprender tanto tendencias históricas como comportamientos recientes del desarrollo global.

---

## 🔹 Valor agregado para el proyecto

La combinación de Gapminder y Banco Mundial permite construir un dashboard más completo, dinámico y actualizado, integrando visualización interactiva con análisis estadístico internacional.

Esto mejora la interpretación de información y fortalece la toma de decisiones basada en datos.

---

# 🧭 Flujo metodológico del dashboard

```text
1. Carga de datos
   Gapminder + Banco Mundial
          ↓
2. Limpieza y homologación
   Países, continentes, subregiones y códigos ISO
          ↓
3. Transformación de variables
   Indicadores, rankings, brechas e índice relativo
          ↓
4. Filtros dinámicos
   Continente, subregión, país y año
          ↓
5. Visualizaciones interactivas
   Mapas, gráficos, KPIs, rankings y tablas
          ↓
6. Insights automáticos
   Principales hallazgos para el análisis
          ↓
7. Interpretación y toma de decisiones
   Comparación territorial y análisis basado en evidencia
```

---

# ⚙️ Funcionalidades del dashboard

El dashboard incorpora las siguientes funcionalidades:

- Filtros dinámicos por continente, subregión, país y año.
- KPIs automáticos.
- Mapa mundial interactivo.
- Evolución temporal de indicadores.
- Comparación entre países y continentes.
- Ranking de países.
- Análisis de brechas de desarrollo.
- Índice de Desarrollo Relativo.
- Insights automáticos.
- Tabla interactiva para consulta de datos.

---

# 📊 Principales módulos del dashboard

## 1. Panorama mundial del desarrollo

Presenta una visión general del desarrollo mundial mediante indicadores como:

- Esperanza de vida
- PIB per cápita
- Población
- Continente
- Año de análisis

Permite identificar patrones globales y diferencias estructurales entre regiones.

---

## 2. Mapa mundial interactivo

El mapa permite visualizar territorialmente los indicadores de desarrollo por país.

Esta visualización facilita la identificación espacial de brechas de desarrollo, diferencias regionales y comportamientos heterogéneos entre continentes.

---

## 3. Evolución temporal

Permite analizar la evolución histórica de los indicadores seleccionados.

Según los filtros aplicados, el dashboard puede mostrar:

- Evolución promedio mundial.
- Evolución por continente.
- Evolución de un país específico.
- Comparación entre país, continente y promedio mundial.

---

## 4. Relación entre PIB y esperanza de vida

Esta visualización permite analizar la relación entre desarrollo económico y bienestar social.

En términos generales, los países con mayor PIB per cápita tienden a presentar mayores niveles de esperanza de vida; sin embargo, también se observan diferencias importantes entre regiones.

---

## 5. Ranking de países

El dashboard genera rankings dinámicos para identificar países con mejores y menores desempeños según los indicadores seleccionados.

Esto permite reconocer líderes, rezagos y contrastes territoriales.

---

## 6. Brechas de desarrollo

La sección de brechas permite analizar desigualdades entre países y continentes.

Una mayor diferencia entre valores máximos y mínimos refleja mayores disparidades estructurales en los indicadores de desarrollo.

---

## 7. Índice de Desarrollo Relativo

Se construyó un indicador compuesto para comparar el nivel relativo de desarrollo entre países.

El índice considera:

- 60% esperanza de vida normalizada.
- 40% PIB per cápita normalizado.

| Nivel | Interpretación |
|---|---|
| Muy alto | Alto desarrollo relativo |
| Alto | Desarrollo favorable |
| Medio | Desarrollo intermedio |
| Bajo | Rezago relativo |

---

## 8. Insights automáticos

El dashboard genera mensajes automáticos que resumen hallazgos relevantes, tales como:

- País con mayor esperanza de vida.
- País con menor esperanza de vida.
- País con mayor PIB per cápita.
- Países por debajo del promedio mundial.
- País con mayor índice de desarrollo relativo.

Estos insights permiten que el usuario interprete rápidamente los resultados sin depender únicamente de la lectura manual de gráficos y tablas.

---

# ✅ Ventajas del dashboard

- Permite explorar datos de forma interactiva.
- Facilita la comparación entre países, continentes y subregiones.
- Integra visualizaciones gráficas, mapas y tablas.
- Mejora la interpretación de tendencias históricas.
- Incorpora datos del Banco Mundial para actualizar el análisis.
- Fortalece el análisis basado en evidencia.
- Puede ser consultado desde un navegador web mediante shinyapps.io.

---

# ⚠️ Limitaciones

- Gapminder tiene información histórica limitada hasta 2007.
- No todos los países cuentan con información completa para todos los años.
- Algunos indicadores del Banco Mundial pueden presentar valores faltantes.
- El Índice de Desarrollo Relativo es un indicador construido con fines académicos y exploratorios.
- El dashboard no reemplaza un análisis econométrico o causal.

---

# 🚀 Mejoras futuras

- Incorporar nuevos indicadores del Banco Mundial.
- Añadir modelos predictivos.
- Incorporar análisis de machine learning.
- Generar reportes automáticos.
- Mejorar el diseño ejecutivo del dashboard.
- Incluir análisis prospectivo.
- Incorporar alertas o semáforos de desempeño.
- Ampliar el análisis por subregiones.

---

# 📂 Estructura del proyecto

```bash
Grupo-2-ME/
│
├── app.R
├── global.R
├── server.R
├── ui.R
├── data/
├── www/
├── rsconnect/
├── README.md
└── .gitignore
```

---

# ▶️ Instalación y ejecución local

## 1. Clonar el repositorio

```bash
git clone https://github.com/epaucarc/Grupo-2-ME.git
```

---

## 2. Abrir el proyecto en RStudio

Abrir la carpeta del proyecto desde RStudio.

---

## 3. Instalar paquetes requeridos

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "ggplot2",
  "plotly",
  "dplyr",
  "DT",
  "leaflet",
  "countrycode",
  "WDI"
))
```

---

## 4. Ejecutar la aplicación

```r
shiny::runApp()
```

---

# ☁️ Publicación en shinyapps.io

Para publicar o actualizar la aplicación:

```r
rsconnect::deployApp()
```

---

# 📌 Conclusiones

El dashboard desarrollado permite analizar el desarrollo mundial desde una perspectiva comparativa, visual e interactiva.

La integración de Gapminder y Banco Mundial mejora la capacidad de análisis, ya que combina información histórica con datos actualizables. Asimismo, el uso de filtros, mapas, rankings e insights automáticos facilita la interpretación de patrones globales y brechas territoriales.

Este proyecto demuestra la utilidad de R Shiny como herramienta para comunicar información estadística de manera accesible, dinámica y orientada a la toma de decisiones.

---

# 📚 Referencias

- Gapminder Foundation. Gapminder Data. https://www.gapminder.org/
- World Bank. World Development Indicators. https://data.worldbank.org/
- Chang, W. Shiny: Web Application Framework for R.
- Wickham, H. ggplot2: Elegant Graphics for Data Analysis.
- Xie, Y. R Markdown: The Definitive Guide.

---

# 🙌 Producto Académico Colaborativo

## Dashboard Interactivo de Desarrollo Mundial  
### Grupo 2 ME