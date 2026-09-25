# =============================================================================
# Interfaz interactiva: Indicadores INEGI del Reto de Ciencia de Datos
# Aramberri, Doctor Arroyo, General Zaragoza y Mier y Noriega (Nuevo León)
#
# Cómo correrla:
#   1. Instalar paquetes (una sola vez):
#      install.packages(c("shiny", "bslib", "plotly", "DT", "dplyr",
#                         "ggplot2", "scales", "httr", "jsonlite"))
#   2. Abrir este archivo en RStudio y presionar "Run App"
#      (o en la consola: shiny::runApp("~/Desktop/interfaz_inegi"))
#
# Los datos se leen de datos/inegi_datos.rds. Si no existe, se descargan de la
# API de INEGI con descargar_datos.R (tarda unos minutos).
# =============================================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)

source("funciones.R", encoding = "UTF-8")
# local = TRUE para que el censo use las mismas piezas de interfaz definidas en este archivo
source("censo.R", local = TRUE, encoding = "UTF-8")
source("programas.R", local = TRUE, encoding = "UTF-8")
source("inegi_auto.R", local = TRUE, encoding = "UTF-8")

# Datos de INEGI: los guardados más recientes; se actualizan solos cada cierto tiempo
inegi$datos <- cargar_datos_inegi()
inegi$estado <- paste0("Datos descargados el ", format(attr(inegi$datos, "descargado"), "%d/%m/%Y"),
                       ". Se revisan automáticamente cada ", DIAS_ACTUALIZACION_INEGI, " días.")
datos_iniciales <- inegi$datos

# Los años que se ofrecen en los filtros son los que tienen dato en los municipios
# (Nuevo León y el país tienen series más largas y dejarían años vacíos)
# Catálogo de indicadores: de aquí sale la fuente que se muestra debajo de los filtros
CATALOGO <- read.csv("catalogo_indicadores.csv", colClasses = "character", fileEncoding = "UTF-8")

# Nota con la fuente de los indicadores de uno o varios subtemas
nota_fuente <- function(...) {
  texto <- texto_fuente(CATALOGO$fuente[CATALOGO$subtema %in% c(...)])
  if (is.null(texto)) return(NULL)
  tags$p(class = "text-muted small mb-0 mt-2", icon("database"), " ", texto)
}

anios_de <- function(filtro) {
  sort(unique(datos_iniciales$anio[filtro & datos_iniciales$lugar %in% MUNICIPIOS]))
}
A_POB <- anios_de(datos_iniciales$subtema == "Población total")
A_EMP <- anios_de(datos_iniciales$eje == "Empleo y ocupación")
A_SAL <- anios_de(datos_iniciales$subtema == "Afiliación a servicios de salud")
A_HOG <- anios_de(datos_iniciales$subtema == "Jefatura del hogar")
A_FEC <- anios_de(datos_iniciales$subtema == "Hijos por grupo de edad de la madre")
R_NAC <- range(anios_de(datos_iniciales$subtema == "Nacimientos registrados"))
R_DEF <- range(anios_de(datos_iniciales$subtema == "Defunciones registradas"))
R_VIT <- c(max(R_NAC[1], R_DEF[1]), min(R_NAC[2], R_DEF[2]))

# ------------------------------------------------------ Piezas de la UI -----

LOGO <- "logo_pal_sur_del_norte.png"

tema_psn <- bs_theme(
  version = 5, primary = MORADO, secondary = LAVANDA, success = TURQUESA_OSCURO,
  info = LAVANDA, warning = AMARILLO, "navbar-bg" = MORADO, fg = "#1E1640", bg = "#FFFFFF",
  base_font = font_collection(font_google("Nunito Sans", local = FALSE), "Segoe UI", "Arial", "sans-serif"),
  heading_font = font_collection(font_google("Montserrat", local = FALSE), "Segoe UI", "Arial", "sans-serif")
)

a_plotly <- function(p) {
  req(p)
  ggplotly(p, tooltip = "text") |>
    layout(legend = list(orientation = "h", x = 0, y = -0.18, title = list(text = "")),
           hoverlabel = list(align = "left"),
           font = list(family = "Nunito Sans, Segoe UI, Arial, sans-serif")) |>
    config(displaylogo = FALSE, locale = "es",
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

tabla_dt <- function(d) {
  datatable(tabla_datos(d), rownames = FALSE, filter = "top", extensions = "Buttons",
            options = list(pageLength = 10, scrollX = TRUE, dom = "Bfrtip",
                           buttons = c("copy", "csv", "excel"),
                           language = list(url = "https://cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")))
}

# Cuadros de indicadores con los colores de Pal Sur del Norte
TEMAS_CAJA <- list(
  primary = value_box_theme(bg = MORADO, fg = "#FFFFFF"),
  secondary = value_box_theme(bg = TURQUESA, fg = MORADO),
  info = value_box_theme(bg = LAVANDA, fg = "#FFFFFF"),
  light = value_box_theme(bg = AMARILLO, fg = MORADO)
)

caja <- function(titulo, id, icono, tema = "primary") {
  value_box(title = titulo, value = textOutput(id), showcase = icon(icono), theme = TEMAS_CAJA[[tema]])
}

cajas <- function(...) {
  n <- length(list(...))
  layout_column_wrap(width = 1 / n, fill = FALSE, ...)
}

# Tarjeta de la portada que lleva a la pestaña de un eje temático
tarjeta_eje <- function(pestana, icono, descripcion, anios) {
  tags$a(
    class = "eje-tarjeta", href = "#",
    onclick = sprintf("Shiny.setInputValue('ir_a', '%s', {priority: 'event'}); return false;", pestana),
    tags$div(class = "eje-icono", icon(icono)),
    tags$div(class = "eje-titulo", pestana),
    tags$div(class = "eje-desc", descripcion),
    tags$div(class = "eje-anios", icon("calendar"), " ", anios)
  )
}

anios_eje <- function(e) {
  a <- range(datos_iniciales$anio[datos_iniciales$eje == e])
  if (a[1] == a[2]) as.character(a[1]) else paste0(a[1], "–", a[2])
}

nota <- function(...) tags$p(class = "text-muted small mb-0", ...)

# Los cuatro municipios vienen marcados; Nuevo León y el total nacional se pueden marcar
# para comparar, salvo en las gráficas de cantidades donde no son comparables (comparar = FALSE)
filtro_municipios <- function(id, comparar = TRUE) {
  checkboxGroupInput(id, if (comparar) "Lugares" else "Municipios",
                     choices = if (comparar) LUGARES else MUNICIPIOS, selected = MUNICIPIOS)
}

filtro_anios <- function(id, anios, etiqueta = "Años") {
  checkboxGroupInput(id, etiqueta, choices = anios, selected = anios, inline = TRUE)
}

# Cada gráfica va en su propia pestaña con sus filtros al lado,
# así solo se ven los filtros que afectan a esa gráfica
grafica_con_filtros <- function(titulo, id, ..., alto = "480px") {
  nav_panel(
    titulo,
    layout_sidebar(
      sidebar = sidebar(title = "Filtros de esta gráfica", width = 270, open = "desktop", ...),
      plotlyOutput(id, height = alto),
      fillable = FALSE
    )
  )
}

pide <- function(x, que = "al menos un municipio") validate(need(length(x) > 0, paste("Selecciona", que)))

hay_datos <- function(d) validate(need(nrow(d) > 0, "No hay datos para esta combinación de filtros."))

# ------------------------------------------------------------------- UI -----

ui <- page_navbar(
  title = tags$span(tags$img(src = LOGO, class = "logo-nav", alt = ""), "Pal Sur del Norte"),
  window_title = "Pal Sur del Norte · Indicadores INEGI",
  id = "pestanas",
  theme = tema_psn,
  fillable = FALSE,
  header = tags$head(
    tags$link(rel = "stylesheet", href = "estilos.css"),
    tags$link(rel = "icon", type = "image/png", href = LOGO),
    # Al cambiar de pestaña, regresar al inicio de la página
    tags$script(HTML("$(document).on('shown.bs.tab', '.navbar a[data-bs-toggle=\"tab\"]', function() { window.scrollTo({top: 0, behavior: 'smooth'}); });"))
  ),
  footer = tags$footer(
    class = "pie",
    tags$img(src = LOGO, alt = ""),
    tags$span(tags$b("Pal Sur del Norte"), " · Datos abiertos de la API del Banco de Indicadores de INEGI")
  ),

  # ---- Portada de bienvenida ----
  nav_panel(
    "Bienvenida", icon = icon("house"),
    tags$section(
      class = "portada",
      tags$div(
        class = "portada-grid",
        tags$div(class = "portada-logo-wrap",
                 tags$img(src = LOGO, class = "portada-logo", alt = "Logo de Pal Sur del Norte")),
        tags$div(
          class = "portada-texto",
          tags$span(class = "portada-etiqueta", icon("sun"), " Reto de Ciencia de Datos · Datos abiertos de INEGI"),
          tags$h1("Te damos la bienvenida"),
          tags$p(class = "portada-sub", "Interfaz de indicadores del sur de Nuevo León"),
          tags$p(class = "portada-lead",
                 "Explora datos oficiales sobre población, empleo, natalidad y mortalidad, migración, salud, hogares y educación. ",
                 "Cada gráfica tiene sus propios filtros para comparar municipios y años."),
          tags$div(class = "portada-municipios",
                   lapply(MUNICIPIOS, function(m) tags$span(class = "chip", icon("location-dot"), m))),
          tags$div(
            class = "portada-botones",
            actionButton("empezar", "Comenzar a explorar", icon = icon("arrow-right"), class = "btn-turquesa btn-lg")
          )
        )
      )
    ),
    cajas(
      caja("Indicadores", "ini_ind", "chart-line"),
      caja("Consultas a la API", "ini_urls", "database", "secondary"),
      caja("Observaciones", "ini_obs", "table", "info"),
      caja("Última descarga", "ini_fecha", "calendar", "light")
    ),
    tags$h3(class = "seccion-titulo", "Explora por eje temático"),
    tags$div(
      class = "ejes-grid",
      tarjeta_eje("Población", "users", "Población total por sexo, pirámide de edad y grandes grupos de edad.", anios_eje("Población")),
      tarjeta_eje("Empleo", "briefcase", "Condición de actividad económica, mujeres y hombres en la población activa y población no activa.", anios_eje("Empleo y ocupación")),
      tarjeta_eje("Natalidad y mortalidad", "baby", "Nacimientos, defunciones, crecimiento natural y promedio de hijos.", anios_eje("Natalidad y mortalidad")),
      tarjeta_eje("Migración", "route", "Inmigrantes, emigrantes, saldo migratorio y sus causas.", anios_eje("Migración")),
      tarjeta_eje("Salud", "hospital", "Afiliación a servicios de salud y población con alguna limitación (discapacidad).", anios_eje("Salud")),
      tarjeta_eje("Hogares", "house-chimney", "Hogares con jefatura femenina y masculina.", anios_eje("Hogares y vivienda")),
      tarjeta_eje("Educación", "graduation-cap", "Escolaridad, alfabetización y asistencia escolar por municipio.", anios_eje("Educación"))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Ejes temáticos disponibles"), DTOutput("ini_tabla")),
      card(
        card_header("Cómo usar la interfaz"),
        tags$ul(
          class = "lista-ayuda",
          tags$li("Cada pestaña de arriba es un eje temático; dentro, cada gráfica tiene su propia pestaña."),
          tags$li("Los filtros aparecen junto a la gráfica que modifican y solo afectan a esa gráfica."),
          tags$li("Los cuadros de colores resumen a los cuatro municipios."),
          tags$li("Pasa el cursor sobre las gráficas para ver los valores; arrastra para hacer zoom y haz doble clic para regresar."),
          tags$li("Haz clic en la leyenda para ocultar o mostrar series, y usa el botón de expandir para verlas en pantalla completa.")
        ),
        actionButton("actualizar", "Actualizar datos desde la API ahora", icon = icon("arrows-rotate"), class = "btn-primary"),
        nota(textOutput("inegi_estado", inline = TRUE))
      )
    )
  ),

  # ---- Población ----
  nav_panel(
    "Población", icon = icon("users"),
    cajas(
      caja("Población 2020", "pob_total", "users"),
      caja("Cambio 2010–2020", "pob_cambio", "arrow-trend-up", "secondary"),
      caja("Población de 65 años y más (2020)", "pob_mayores", "person-cane", "info")
    ),
    navset_card_tab(
      full_screen = TRUE,
      grafica_con_filtros(
        "Evolución", "pob_evol",
        filtro_municipios("pob_evol_mun", comparar = FALSE),
        checkboxGroupInput("pob_evol_sexo", "Sexo", choices = c("Total", "Hombres", "Mujeres"), selected = "Total", inline = TRUE),
        filtro_anios("pob_evol_anios", A_POB),
        nota_fuente("Población total")
      ),
      grafica_con_filtros(
        "Pirámide de edad 2020", "pob_piramide",
        selectInput("pob_pir_lugar", "Lugar", choices = list("Los 4 municipios" = "todos",
                                                            Municipios = MUNICIPIOS, `Comparar con` = COMPARATIVOS)),
        input_switch("pob_pir_pct", "Mostrar en porcentaje", TRUE),
        nota("Censo de Población y Vivienda 2020."),
        nota_fuente("Grupos de edad"),
        alto = "600px"
      ),
      grafica_con_filtros(
        "Grandes grupos de edad", "pob_grupos",
        filtro_municipios("pob_grupos_mun", comparar = FALSE),
        nota("Censo de Población y Vivienda 2020."),
        nota_fuente("Grupos de edad")
      ),
      nav_panel("Tabla", DTOutput("pob_tabla"))
    )
  ),

  # ---- Empleo ----
  nav_panel(
    "Empleo", icon = icon("briefcase"),
    cajas(
      caja("Población económicamente activa · promedio municipal", "emp_activa", "briefcase"),
      caja("Mujeres dentro de la población activa · promedio municipal", "emp_mujeres", "person-dress", "secondary"),
      caja("Población no activa dedicada al hogar · promedio municipal", "emp_hogar", "house-user", "info")
    ),
    navset_card_tab(
      full_screen = TRUE,
      grafica_con_filtros(
        "Condición de actividad", "emp_cond",
        filtro_municipios("emp_cond_mun"),
        radioButtons("emp_cond_anio", "Año", choices = rev(A_EMP), inline = TRUE),
        radioButtons("emp_cond_tipo", "Tipo de gráfica", choices = c("Barras apiladas" = "apiladas", "Barras agrupadas" = "barras")),
        nota("Porcentaje de la población de 12 años y más."),
        nota_fuente("Condición de actividad")
      ),
      grafica_con_filtros(
        "Mujeres y hombres en la población activa", "emp_sexo",
        filtro_municipios("emp_sexo_mun"),
        filtro_anios("emp_sexo_anios", A_EMP),
        nota("Distribución por sexo de la población económicamente activa: mujeres y hombres suman 100%."),
        nota_fuente("Participación por sexo")
      ),
      grafica_con_filtros(
        "Población no económicamente activa", "emp_pnea",
        filtro_municipios("emp_pnea_mun"),
        checkboxGroupInput("emp_pnea_cat", "Motivo", choices = MOTIVOS_PNEA, selected = MOTIVOS_PNEA),
        radioButtons("emp_pnea_anio", "Año", choices = rev(A_EMP), inline = TRUE),
        nota("Porcentaje de la población no económicamente activa de 12 años y más."),
        nota_fuente("Población no económicamente activa")
      ),
      nav_panel("Tabla", DTOutput("emp_tabla"))
    )
  ),

  # ---- Natalidad y mortalidad ----
  nav_panel(
    "Natalidad y mortalidad", icon = icon("baby"),
    cajas(
      caja("Nacimientos registrados", "nat_nac_kpi", "baby"),
      caja("Defunciones registradas", "nat_def_kpi", "dove", "secondary"),
      caja("Crecimiento natural", "nat_crec_kpi", "seedling", "info"),
      caja("Promedio de hijos nacidos vivos", "nat_hijos_kpi", "children", "light")
    ),
    navset_card_tab(
      full_screen = TRUE,
      grafica_con_filtros(
        "Nacimientos", "nat_nac",
        filtro_municipios("nat_nac_mun", comparar = FALSE),
        sliderInput("nat_nac_anios", "Años", min = R_NAC[1], max = R_NAC[2], value = R_NAC, step = 1, sep = ""),
        radioButtons("nat_nac_escala", "Mostrar como", choices = c("Número" = "num", "Índice (primer año = 100)" = "indice")),
        nota_fuente("Nacimientos registrados")
      ),
      grafica_con_filtros(
        "Defunciones", "nat_def",
        filtro_municipios("nat_def_mun", comparar = FALSE),
        sliderInput("nat_def_anios", "Años", min = R_DEF[1], max = R_DEF[2], value = R_DEF, step = 1, sep = ""),
        radioButtons("nat_def_escala", "Mostrar como", choices = c("Número" = "num", "Índice (primer año = 100)" = "indice")),
        nota_fuente("Defunciones registradas")
      ),
      grafica_con_filtros(
        "Crecimiento natural", "nat_crec",
        filtro_municipios("nat_crec_mun", comparar = FALSE),
        sliderInput("nat_crec_anios", "Años", min = R_VIT[1], max = R_VIT[2], value = R_VIT, step = 1, sep = ""),
        nota("Nacimientos menos defunciones registradas en cada año. La línea punteada marca el cero."),
        nota_fuente("Nacimientos registrados", "Defunciones registradas")
      ),
      grafica_con_filtros(
        "Promedio de hijos", "nat_prom",
        filtro_municipios("nat_prom_mun"),
        nota_fuente("Promedio de hijos nacidos vivos")
      ),
      grafica_con_filtros(
        "Hijos por edad de la madre", "nat_edad",
        filtro_municipios("nat_edad_mun"),
        radioButtons("nat_edad_anio", "Año", choices = rev(A_FEC), inline = TRUE),
        nota_fuente("Hijos por grupo de edad de la madre")
      ),
      nav_panel("Tabla", DTOutput("nat_tabla"))
    )
  ),

  # ---- Migración ----
  nav_panel(
    "Migración", icon = icon("route"),
    cajas(
      caja("Inmigrantes", "mig_inm", "right-to-bracket"),
      caja("Emigrantes", "mig_emi", "right-from-bracket", "secondary"),
      caja("Saldo migratorio", "mig_saldo", "scale-balanced", "info"),
      caja("Causa principal", "mig_causa", "circle-question", "light")
    ),
    navset_card_tab(
      full_screen = TRUE,
      grafica_con_filtros(
        "Inmigrantes y emigrantes", "mig_flujos",
        filtro_municipios("mig_flu_mun", comparar = FALSE),
        checkboxGroupInput("mig_flu_tipo", "Flujo", choices = c("Inmigrantes", "Emigrantes"), selected = c("Inmigrantes", "Emigrantes")),
        nota("Censo 2020, población de 5 años y más."),
        nota_fuente("Flujos migratorios")
      ),
      grafica_con_filtros(
        "Saldo migratorio", "mig_saldo_graf",
        filtro_municipios("mig_sal_mun", comparar = FALSE),
        nota("Inmigrantes menos emigrantes, Censo 2020."),
        nota_fuente("Flujos migratorios")
      ),
      grafica_con_filtros(
        "Causas de la migración", "mig_causas_graf",
        filtro_municipios("mig_cau_mun"),
        checkboxGroupInput("mig_causas", "Causas", choices = CAUSAS, selected = CAUSAS),
        radioButtons("mig_modo", "Mostrar como",
                     choices = c("Barras agrupadas" = "barras", "Barras apiladas" = "apiladas", "Mapa de calor" = "calor")),
        nota_fuente("Causas de la migración")
      ),
      nav_panel("Tabla", DTOutput("mig_tabla"))
    )
  ),

  # ---- Salud ----
  nav_panel(
    "Salud", icon = icon("hospital"),
    cajas(
      caja("Población afiliada a servicios de salud (2020)", "sal_cob", "shield-heart"),
      caja("Institución con mayor cobertura (2020)", "sal_principal", "hospital", "secondary"),
      caja("Cambio en afiliación 2015–2020", "sal_cambio", "arrow-trend-up", "info"),
      caja("Limitación más frecuente (2020)", "sal_disc", "wheelchair", "light")
    ),
    navset_card_tab(
      full_screen = TRUE,
      grafica_con_filtros(
        "Por institución", "sal_inst_graf",
        filtro_municipios("sal_inst_mun"),
        radioButtons("sal_inst_anio", "Año", choices = rev(A_SAL), inline = TRUE),
        checkboxGroupInput("sal_inst", "Instituciones", choices = INSTITUCIONES,
                           selected = c("IMSS", "ISSSTE", "PEMEX, SDN o SM", "Seguro Popular", "IMSS-BIENESTAR", "Otra institución")),
        nota("IMSS-BIENESTAR y servicios médicos privados solo tienen dato en 2020; seguro privado solo en 2015."),
        alto = "540px",
        nota_fuente("Afiliación a servicios de salud")
      ),
      grafica_con_filtros(
        "2015 contra 2020", "sal_cambio_graf",
        filtro_municipios("sal_cam_mun"),
        selectInput("sal_inst_cambio", "Institución",
                    choices = c("Afiliada a algún servicio", "IMSS", "ISSSTE", "PEMEX, SDN o SM", "Seguro Popular", "Otra institución")),
        nota_fuente("Afiliación a servicios de salud")
      ),
      grafica_con_filtros(
        "Cobertura total", "sal_cob_graf",
        filtro_municipios("sal_cob_mun"),
        filtro_anios("sal_cob_anios", A_SAL),
        nota_fuente("Afiliación a servicios de salud")
      ),
      grafica_con_filtros(
        "Discapacidad", "sal_dis_graf",
        filtro_municipios("sal_dis_mun", comparar = FALSE),
        checkboxGroupInput("sal_dis_tipos", "Limitación en la actividad para", choices = DISCAPACIDADES, selected = DISCAPACIDADES),
        radioButtons("sal_dis_medida", "Mostrar como", choices = c("Número de personas" = "personas", "% de la población" = "pct")),
        radioButtons("sal_dis_vista", "Agrupar por", choices = c("Tipo de limitación" = "categoria", "Municipio" = "lugar")),
        nota("Censo 2020. Una persona puede tener más de una limitación."),
        alto = "520px",
        nota_fuente("Discapacidad")
      ),
      nav_panel("Tabla", DTOutput("sal_tabla"))
    )
  ),

  # ---- Hogares ----
  nav_panel(
    "Hogares", icon = icon("house-chimney"),
    cajas(
      caja("Total de hogares", "hog_total", "house-chimney"),
      caja("Hogares con jefatura femenina", "hog_fem", "person-dress", "secondary"),
      caja("Crecimiento de la jefatura femenina", "hog_crec", "arrow-trend-up", "info")
    ),
    navset_card_tab(
      full_screen = TRUE,
      grafica_con_filtros(
        "Evolución de hogares", "hog_evol",
        filtro_municipios("hog_evo_mun", comparar = FALSE),
        checkboxGroupInput("hog_evo_jef", "Tipo de jefatura", choices = c("Jefatura femenina", "Jefatura masculina"),
                           selected = c("Jefatura femenina", "Jefatura masculina")),
        filtro_anios("hog_evo_anios", A_HOG),
        nota_fuente("Jefatura del hogar")
      ),
      grafica_con_filtros(
        "% con jefatura femenina", "hog_pct",
        filtro_municipios("hog_pct_mun"),
        filtro_anios("hog_pct_anios", A_HOG),
        nota_fuente("Jefatura del hogar")
      ),
      grafica_con_filtros(
        "Composición por año", "hog_comp",
        filtro_municipios("hog_com_mun"),
        filtro_anios("hog_com_anios", A_HOG),
        nota_fuente("Jefatura del hogar")
      ),
      nav_panel("Tabla", DTOutput("hog_tabla"))
    )
  ),

  # ---- Educación ----
  nav_panel(
    "Educación", icon = icon("graduation-cap"),
    cajas(
      caja("Grado promedio de escolaridad · promedio municipal", "edu_grado", "graduation-cap"),
      caja("Población sin escolaridad · promedio municipal", "edu_sin", "book-open", "secondary"),
      caja("Población con instrucción superior · promedio municipal", "edu_sup", "user-graduate", "info")
    ),
    navset_card_tab(
      full_screen = TRUE,
      grafica_con_filtros(
        "Comparación entre municipios", "edu_comp",
        selectInput("edu_com_ind", "Indicador", choices = EDUCACION),
        selectInput("edu_com_anio", "Año", choices = NULL),
        filtro_municipios("edu_com_mun"),
        input_switch("edu_com_prom", "Línea con el promedio de los municipios", TRUE),
        nota_fuente("Educación")
      ),
      grafica_con_filtros(
        "Evolución", "edu_evol",
        selectInput("edu_evo_ind", "Indicador", choices = EDUCACION),
        filtro_municipios("edu_evo_mun"),
        nota_fuente("Educación")
      ),
      grafica_con_filtros(
        "Perfil educativo", "edu_perfil",
        radioButtons("edu_per_anio", "Año", choices = c("2020", "2015"), inline = TRUE),
        filtro_municipios("edu_per_mun"),
        nota("Distribución de la población de 15 años y más por nivel de escolaridad."),
        nota_fuente("Educación")
      ),
      nav_panel("Tabla", DTOutput("edu_tabla"))
    )
  ),

  # ---- Censo (protegido con contraseña) ----
  nav_panel("Censo", icon = icon("lock"), censo_login_ui()),

  # ---- Programas (protegido con la misma contraseña) ----
  nav_panel("Programas", icon = icon("clipboard-check"), programas_login_ui()),

  nav_spacer(),
  nav_item(tags$a(icon("database"), " API de INEGI",
                  href = "https://www.inegi.org.mx/servicios/api_indicadores.html", target = "_blank"))
)

# --------------------------------------------------------------- Server -----

server <- function(input, output, session) {

  # Todas las sesiones ven los datos más recientes de INEGI, también tras una actualización automática
  datos <- reactivePoll(10000, session, checkFunc = function() inegi$version, valueFunc = function() inegi$datos)

  # ---- Navegación desde la portada ----
  observeEvent(input$empezar, nav_select("pestanas", "Población"))
  observeEvent(input$ir_a, nav_select("pestanas", input$ir_a))

  # ---- Portada: indicadores y resumen ----
  output$ini_ind <- renderText(n_distinct(datos()$id))
  output$ini_urls <- renderText(n_distinct(paste(datos()$id, datos()$cve_area)))
  output$ini_obs <- renderText(comma(nrow(datos())))
  output$ini_fecha <- renderText({
    f <- attr(datos(), "descargado")
    if (is.null(f)) "Sin registro" else format(f, "%d/%m/%Y")
  })
  output$ini_tabla <- renderDT(
    datatable(resumen_ejes(datos()), rownames = FALSE, options = list(dom = "t", ordering = FALSE, pageLength = 10))
  )

  # ---- Actualización de INEGI ----
  output$inegi_estado <- renderText({
    invalidateLater(15000)
    inegi$estado
  })
  # Revisión automática al abrir la app y cada 6 horas (solo descarga si los datos son viejos)
  observe({
    invalidateLater(6 * 60 * 60 * 1000)
    iniciar_actualizacion_inegi()
  })
  observeEvent(input$actualizar, {
    if (iniciar_actualizacion_inegi(forzar = TRUE)) {
      showNotification("La descarga de INEGI empezó en segundo plano. Puedes seguir usando la interfaz; los datos se actualizan solos al terminar.",
                       type = "message", duration = 8)
    } else {
      showNotification("Ya hay una actualización de INEGI en curso.", type = "warning")
    }
  })

  # ---- Población ----
  pob_kpi <- reactive(kpi_poblacion(datos()))
  output$pob_total <- renderText(pob_kpi()$total)
  output$pob_cambio <- renderText(pob_kpi()$cambio)
  output$pob_mayores <- renderText(pob_kpi()$mayores)

  output$pob_evol <- renderPlotly({
    pide(input$pob_evol_mun)
    pide(input$pob_evol_sexo, "al menos una opción de sexo")
    pide(input$pob_evol_anios, "al menos un año")
    d <- datos_pob_total(datos(), input$pob_evol_mun, input$pob_evol_sexo, as.integer(input$pob_evol_anios))
    hay_datos(d)
    a_plotly(graf_generica(d, x = "anio", color = "lugar", eje_y = "Personas",
                           facet = if (length(input$pob_evol_sexo) > 1) "categoria"))
  })
  output$pob_piramide <- renderPlotly({
    d <- datos_piramide(datos(), input$pob_pir_lugar, input$pob_pir_pct)
    hay_datos(d)
    a_plotly(graf_piramide(d, input$pob_pir_pct)) |> layout(barmode = "overlay")
  })
  output$pob_grupos <- renderPlotly({
    pide(input$pob_grupos_mun)
    d <- datos_grupos_grandes(datos(), input$pob_grupos_mun)
    hay_datos(d)
    a_plotly(graf_generica(d, x = "lugar", tipo = "proporcion", color = "categoria",
                           eje_y = "Porcentaje de la población (2020)", horizontal = TRUE))
  })
  output$pob_tabla <- renderDT(tabla_dt(filter(datos(), eje == "Población")))

  # ---- Empleo ----
  emp_kpi <- reactive(kpi_empleo(datos()))
  output$emp_activa <- renderText(emp_kpi()$activa)
  output$emp_mujeres <- renderText(emp_kpi()$mujeres)
  output$emp_hogar <- renderText(emp_kpi()$hogar)

  output$emp_cond <- renderPlotly({
    pide(input$emp_cond_mun)
    d <- datos() |>
      filter(subtema == "Condición de actividad", lugar %in% input$emp_cond_mun, anio == as.integer(input$emp_cond_anio))
    hay_datos(d)
    a_plotly(graf_generica(d, x = "lugar", tipo = input$emp_cond_tipo, color = "categoria",
                           eje_y = paste("% de la población de 12 años y más,", input$emp_cond_anio)))
  })
  output$emp_sexo <- renderPlotly({
    pide(input$emp_sexo_mun)
    pide(input$emp_sexo_anios, "al menos un año")
    d <- datos() |>
      filter(subtema == "Participación por sexo", lugar %in% input$emp_sexo_mun, anio %in% as.integer(input$emp_sexo_anios))
    hay_datos(d)
    a_plotly(graf_generica(d, x = "lugar", tipo = "apiladas", color = "categoria",
                           facet = if (length(input$emp_sexo_anios) > 1) "anio",
                           eje_y = "% de la población económicamente activa"))
  })
  output$emp_pnea <- renderPlotly({
    pide(input$emp_pnea_mun)
    pide(input$emp_pnea_cat, "al menos un motivo")
    d <- datos() |>
      filter(subtema == "Población no económicamente activa", lugar %in% input$emp_pnea_mun,
             categoria %in% input$emp_pnea_cat, anio == as.integer(input$emp_pnea_anio))
    hay_datos(d)
    a_plotly(graf_generica(d, x = "categoria", tipo = "barras", color = "lugar", horizontal = TRUE,
                           eje_y = paste("% de la población no económicamente activa,", input$emp_pnea_anio)))
  })
  output$emp_tabla <- renderDT(tabla_dt(filter(datos(), eje == "Empleo y ocupación")))

  # ---- Natalidad y mortalidad ----
  nat_kpi <- reactive(kpi_natalidad(datos()))
  output$nat_nac_kpi <- renderText(nat_kpi()$nacimientos)
  output$nat_def_kpi <- renderText(nat_kpi()$defunciones)
  output$nat_crec_kpi <- renderText(nat_kpi()$crecimiento)
  output$nat_hijos_kpi <- renderText(nat_kpi()$hijos)

  graf_vital <- function(subtema_sel, municipios, anios, escala, tipo, palabra, etiqueta) {
    pide(municipios)
    d <- datos_vitales(datos(), subtema_sel, municipios, anios, escala, palabra)
    hay_datos(d)
    a_plotly(graf_generica(d, x = "anio", tipo = tipo, color = "lugar",
                           eje_y = if (escala == "indice") "Índice (primer año = 100)" else etiqueta,
                           referencia = if (escala == "indice") 100))
  }
  output$nat_nac <- renderPlotly(
    graf_vital("Nacimientos registrados", input$nat_nac_mun, input$nat_nac_anios,
               input$nat_nac_escala, "lineas", "nacimientos", "Nacimientos")
  )
  output$nat_def <- renderPlotly(
    graf_vital("Defunciones registradas", input$nat_def_mun, input$nat_def_anios,
               input$nat_def_escala, "lineas", "defunciones", "Defunciones")
  )
  output$nat_crec <- renderPlotly({
    pide(input$nat_crec_mun)
    d <- datos_crecimiento_natural(datos(), input$nat_crec_mun, input$nat_crec_anios)
    hay_datos(d)
    a_plotly(graf_generica(d, x = "anio", color = "lugar", eje_y = "Nacimientos menos defunciones", referencia = 0))
  })
  output$nat_prom <- renderPlotly({
    pide(input$nat_prom_mun)
    d <- datos() |> filter(subtema == "Promedio de hijos nacidos vivos", lugar %in% input$nat_prom_mun)
    hay_datos(d)
    a_plotly(graf_generica(d, x = "anio", tipo = "barras", color = "lugar",
                           eje_y = "Hijos nacidos vivos por mujer de 12 años y más"))
  })
  output$nat_edad <- renderPlotly({
    pide(input$nat_edad_mun)
    d <- datos() |>
      filter(subtema == "Hijos por grupo de edad de la madre", lugar %in% input$nat_edad_mun,
             anio == as.integer(input$nat_edad_anio))
    hay_datos(d)
    a_plotly(graf_generica(d, x = "grupo_edad", color = "lugar",
                           eje_x = "Edad de la madre (años)", eje_y = "Promedio de hijos nacidos vivos"))
  })
  output$nat_tabla <- renderDT(tabla_dt(filter(datos(), eje == "Natalidad y mortalidad")))

  # ---- Migración ----
  mig_kpi <- reactive(kpi_migracion(datos()))
  output$mig_inm <- renderText(mig_kpi()$inmigrantes)
  output$mig_emi <- renderText(mig_kpi()$emigrantes)
  output$mig_saldo <- renderText(mig_kpi()$saldo)
  output$mig_causa <- renderText(mig_kpi()$causa)

  output$mig_flujos <- renderPlotly({
    pide(input$mig_flu_mun)
    pide(input$mig_flu_tipo, "al menos un flujo")
    d <- datos() |> filter(subtema == "Flujos migratorios", lugar %in% input$mig_flu_mun, categoria %in% input$mig_flu_tipo)
    hay_datos(d)
    a_plotly(graf_generica(d, x = "lugar", tipo = "barras", color = "categoria", eje_y = "Personas (2020)"))
  })
  output$mig_saldo_graf <- renderPlotly({
    pide(input$mig_sal_mun)
    d <- datos_saldo(datos(), input$mig_sal_mun)
    hay_datos(d)
    a_plotly(graf_saldo(d))
  })
  output$mig_causas_graf <- renderPlotly({
    pide(input$mig_cau_mun)
    pide(input$mig_causas, "al menos una causa")
    d <- datos() |> filter(subtema == "Causas de la migración", lugar %in% input$mig_cau_mun, categoria %in% input$mig_causas)
    hay_datos(d)
    p <- switch(input$mig_modo,
                calor = graf_calor(d),
                barras = graf_generica(d, x = "categoria", tipo = "barras", color = "lugar",
                                       eje_y = "% de la población migrante", horizontal = TRUE),
                apiladas = graf_generica(d, x = "lugar", tipo = "apiladas", color = "categoria",
                                         eje_y = "% de la población migrante", horizontal = TRUE))
    a_plotly(p)
  })
  output$mig_tabla <- renderDT(tabla_dt(filter(datos(), eje == "Migración")))

  # ---- Salud ----
  sal_kpi <- reactive(kpi_salud(datos()))
  output$sal_cob <- renderText(sal_kpi()$cobertura)
  output$sal_principal <- renderText(sal_kpi()$principal)
  output$sal_cambio <- renderText(sal_kpi()$cambio)
  output$sal_disc <- renderText(sal_kpi()$discapacidad)

  output$sal_inst_graf <- renderPlotly({
    pide(input$sal_inst_mun)
    pide(input$sal_inst, "al menos una institución")
    d <- datos() |>
      filter(subtema == "Afiliación a servicios de salud", lugar %in% input$sal_inst_mun,
             categoria %in% input$sal_inst, anio == as.integer(input$sal_inst_anio))
    validate(need(nrow(d) > 0, "Las instituciones seleccionadas no tienen datos en ese año."))
    a_plotly(graf_generica(d, x = "categoria", tipo = "barras", color = "lugar",
                           eje_y = paste("% de la población,", input$sal_inst_anio), horizontal = TRUE))
  })
  output$sal_cambio_graf <- renderPlotly({
    pide(input$sal_cam_mun)
    d <- datos() |>
      filter(subtema == "Afiliación a servicios de salud", lugar %in% input$sal_cam_mun, categoria == input$sal_inst_cambio)
    p <- graf_cambio_salud(d)
    validate(need(!is.null(p), "Esta institución no tiene datos de 2015 y 2020."))
    a_plotly(p)
  })
  output$sal_cob_graf <- renderPlotly({
    pide(input$sal_cob_mun)
    pide(input$sal_cob_anios, "al menos un año")
    d <- datos() |>
      filter(categoria == "Afiliada a algún servicio", lugar %in% input$sal_cob_mun,
             anio %in% as.integer(input$sal_cob_anios))
    hay_datos(d)
    a_plotly(graf_generica(d, x = "lugar", tipo = "barras", color = "anio",
                           eje_y = "% de la población afiliada a servicios de salud"))
  })
  output$sal_dis_graf <- renderPlotly({
    pide(input$sal_dis_mun)
    pide(input$sal_dis_tipos, "al menos un tipo de limitación")
    d <- datos_discapacidad(datos(), input$sal_dis_mun, input$sal_dis_tipos, input$sal_dis_medida)
    hay_datos(d)
    eje_y <- if (input$sal_dis_medida == "pct") "% de la población (2020)" else "Personas (2020)"
    p <- if (input$sal_dis_vista == "categoria") {
      graf_generica(d, x = "categoria", tipo = "barras", color = "lugar", horizontal = TRUE, eje_y = eje_y)
    } else {
      graf_generica(d, x = "lugar", tipo = "barras", color = "categoria", eje_y = eje_y)
    }
    a_plotly(p)
  })
  output$sal_tabla <- renderDT(tabla_dt(filter(datos(), eje == "Salud")))

  # ---- Hogares ----
  hog_kpi <- reactive(kpi_hogares(datos()))
  output$hog_total <- renderText(hog_kpi()$total)
  output$hog_fem <- renderText(hog_kpi()$femenina)
  output$hog_crec <- renderText(hog_kpi()$crecimiento)

  output$hog_evol <- renderPlotly({
    pide(input$hog_evo_mun)
    pide(input$hog_evo_jef, "al menos un tipo de jefatura")
    pide(input$hog_evo_anios, "al menos un año")
    d <- datos() |>
      filter(subtema == "Jefatura del hogar", lugar %in% input$hog_evo_mun,
             categoria %in% input$hog_evo_jef, anio %in% as.integer(input$hog_evo_anios))
    hay_datos(d)
    a_plotly(graf_generica(d, x = "anio", color = "lugar", eje_y = "Hogares",
                           facet = if (length(input$hog_evo_jef) > 1) "categoria"))
  })
  output$hog_pct <- renderPlotly({
    pide(input$hog_pct_mun)
    pide(input$hog_pct_anios, "al menos un año")
    d <- datos_jefatura_femenina(datos(), input$hog_pct_mun, as.integer(input$hog_pct_anios))
    hay_datos(d)
    a_plotly(graf_generica(d, x = "anio", color = "lugar", eje_y = "% de hogares con jefatura femenina"))
  })
  output$hog_comp <- renderPlotly({
    pide(input$hog_com_mun)
    pide(input$hog_com_anios, "al menos un año")
    d <- datos() |>
      filter(subtema == "Jefatura del hogar", lugar %in% input$hog_com_mun, anio %in% as.integer(input$hog_com_anios)) |>
      group_by(lugar, anio) |>
      mutate(tip = paste0("<b>", categoria, "</b><br>", lugar, " · ", periodo, "<br>",
                          number(100 * valor / sum(valor), accuracy = 0.1), "%")) |>
      ungroup()
    hay_datos(d)
    a_plotly(graf_generica(d, x = "lugar", tipo = "proporcion", color = "categoria", horizontal = TRUE,
                           facet = if (length(input$hog_com_anios) > 1) "anio", eje_y = "% de los hogares"))
  })
  output$hog_tabla <- renderDT(tabla_dt(filter(datos(), eje == "Hogares y vivienda")))

  # ---- Educación ----
  edu_kpi <- reactive(kpi_educacion(datos()))
  output$edu_grado <- renderText(edu_kpi()$grado)
  output$edu_sin <- renderText(edu_kpi()$sin_escolaridad)
  output$edu_sup <- renderText(edu_kpi()$superior)

  observeEvent(input$edu_com_ind, {
    d <- datos()
    a <- sort(unique(d$anio[d$categoria == input$edu_com_ind & d$lugar %in% MUNICIPIOS]), decreasing = TRUE)
    updateSelectInput(session, "edu_com_anio", choices = a, selected = a[1])
  })
  # Los indicadores que son número de personas no se comparan con Nuevo León ni con el país
  lugares_del_indicador <- function(id_filtro, indicador) {
    opciones <- if (indicador %in% EDUCACION_SOLO_MUNICIPIOS) MUNICIPIOS else LUGARES
    elegidos <- isolate(input[[id_filtro]])
    if (is.null(elegidos)) elegidos <- MUNICIPIOS
    updateCheckboxGroupInput(session, id_filtro, label = if (length(opciones) > 4) "Lugares" else "Municipios",
                             choices = opciones, selected = intersect(elegidos, opciones))
  }
  observeEvent(input$edu_com_ind, lugares_del_indicador("edu_com_mun", input$edu_com_ind))
  observeEvent(input$edu_evo_ind, lugares_del_indicador("edu_evo_mun", input$edu_evo_ind))
  output$edu_comp <- renderPlotly({
    pide(input$edu_com_mun)
    req(input$edu_com_anio)
    d <- datos() |>
      filter(categoria == input$edu_com_ind, lugar %in% input$edu_com_mun, anio == as.integer(input$edu_com_anio))
    validate(need(nrow(d) > 0, "No hay datos de los municipios seleccionados en ese año."))
    promedio <- if (isTRUE(input$edu_com_prom) && nrow(d) > 1) mean(d$valor)
    a_plotly(graf_generica(d, x = "lugar", tipo = "barras", color = "lugar",
                           eje_y = paste0(input$edu_com_ind, " (", input$edu_com_anio, ")"),
                           referencia = promedio))
  })
  output$edu_evol <- renderPlotly({
    pide(input$edu_evo_mun)
    d <- datos() |> filter(categoria == input$edu_evo_ind, lugar %in% input$edu_evo_mun)
    hay_datos(d)
    a_plotly(graf_generica(d, x = "anio", color = "lugar", eje_y = input$edu_evo_ind))
  })
  output$edu_perfil <- renderPlotly({
    pide(input$edu_per_mun)
    anio_perfil <- as.integer(input$edu_per_anio)
    d <- datos() |> filter(categoria %in% NIVELES_EDUCATIVOS, lugar %in% input$edu_per_mun, anio == anio_perfil)
    hay_datos(d)
    d$categoria <- factor(d$categoria, levels = NIVELES_EDUCATIVOS)
    d <- d[order(d$categoria), ]
    a_plotly(graf_generica(d, x = "lugar", tipo = "apiladas", color = "categoria", horizontal = TRUE,
                           eje_y = paste("% de la población de 15 años y más,", anio_perfil)))
  })
  output$edu_tabla <- renderDT(tabla_dt(filter(datos(), eje == "Educación")))

  # ---- Censo ----
  censo_server(input, output, session)
}

shinyApp(ui, server)
