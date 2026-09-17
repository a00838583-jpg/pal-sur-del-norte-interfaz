# =============================================================================
# Pestaña "Programas": registro de asistencia con la base Beneficio
#
# - Usa la misma contraseña que el Censo; al entrar se abre la pestaña aparte
#   "Registro de programas".
# - La base Beneficio tiene una fila por persona (nombre_completo) y una columna
#   por programa o sesión. La celda guarda la fecha de asistencia (AAAA-MM-DD),
#   «Sí» cuando no se sabe la fecha, o vacío si no asistió.
# - También guarda Escolaridad, Promedio / Aptitudes y Lugar de origen.
# - Con Google configurado se usa la pestaña "Beneficio" de la hoja; si no, el Excel
#   de BENEFICIO_RUTA.
# =============================================================================

PESTANA_BENEFICIO <- if (nzchar(CENSO_HOJA)) "Beneficio" else NULL
RUTA_BENEFICIO <- if (nzchar(CENSO_HOJA)) CENSO_HOJA else
  Sys.getenv("BENEFICIO_RUTA", path.expand("~/Desktop/BaseNuevaMerge_v2.1_Beneficio.xlsx"))

DATOS_BENEFICIO <- c("Escolaridad", "Promedio / Aptitudes", "Lugar de origen")
# Columnas que guardan un número de asistencias y no una fecha
COLUMNAS_CONTEO <- c("Asistencia_Ingenium")
NOMBRES_PROGRAMA <- c("Asistencia_Ingenium" = "Ingenium (número de asistencias)",
                      "Entrevistado_ProSocial" = "Entrevistado por ProSocial")

leer_beneficio <- function() leer_censo(RUTA_BENEFICIO, PESTANA_BENEFICIO)

columnas_programa <- function(base) setdiff(names(base), c("nombre_completo", DATOS_BENEFICIO))

nombre_programa <- function(columnas) {
  unname(ifelse(columnas %in% names(NOMBRES_PROGRAMA), NOMBRES_PROGRAMA[columnas], columnas))
}

# Una celda cuenta como asistencia si tiene cualquier dato distinto de «No» (o de 0 en los conteos)
asistio <- function(x) !vacio(x) & !(normalizar_sino(x) %in% "No") & !(trimws(x) %in% "0")

# Fechas de Excel guardadas como número (por ejemplo 45450) a texto AAAA-MM-DD
fecha_excel_a_texto <- function(x) {
  num <- suppressWarnings(as.numeric(x))
  es_fecha <- !is.na(num) & num > 20000 & num < 80000
  x[es_fecha] <- format(as.Date(num[es_fecha], origin = "1899-12-30"), "%Y-%m-%d")
  x
}

# Cómo se ve una celda de asistencia en la página: 26/01/2024, «Sí», «4 asistencias»...
formato_asistencia <- function(x, columna = "") {
  x <- trimws(fecha_excel_a_texto(x))
  iso <- !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  x[iso] <- format(as.Date(x[iso]), "%d/%m/%Y")
  if (columna %in% COLUMNAS_CONTEO) x[!vacio(x)] <- paste0(x[!vacio(x)], " asistencia(s)")
  else x[!is.na(x) & x == "1"] <- "Sí"
  ifelse(vacio(x), "", x)
}

# Limpia la base Beneficio antes de subirla: fechas de Excel a AAAA-MM-DD y «1» a «Sí»
limpiar_beneficio <- function(base) {
  for (col in setdiff(columnas_programa(base), COLUMNAS_CONTEO)) {
    v <- fecha_excel_a_texto(base[[col]])
    v[!is.na(v) & trimws(v) == "1"] <- "Sí"
    base[[col]] <- v
  }
  base
}

# ---------------------------------------------------- Lectura y escritura -----

crear_programa_censo <- function(ruta, nombre) {
  nombre <- gsub("\\s+", " ", trimws(if (is.null(nombre)) "" else nombre))
  if (!nzchar(nombre)) stop("Escribe el nombre del programa.")
  if (nchar(nombre) > 80) stop("El nombre del programa es muy largo (máximo 80 caracteres).")
  validar_guardado(ruta)
  actual <- leer_censo(ruta, PESTANA_BENEFICIO)
  if (toupper(nombre) %in% toupper(c(names(actual), nombre_programa(names(actual))))) {
    stop("Ya existe un programa o columna llamada «", nombre, "».")
  }
  agregar_columna_censo(ruta, nombre, ncol(actual) + 1, PESTANA_BENEFICIO)
  nombre
}

# Escribe valores para varias personas; las que aún no están en Beneficio se agregan al final
escribir_personas_beneficio <- function(ruta, nombres, valores_por_columna) {
  validar_guardado(ruta)
  actual <- leer_censo(ruta, PESTANA_BENEFICIO)
  faltan <- setdiff(names(valores_por_columna), names(actual))
  if (length(faltan)) stop("La columna «", faltan[1], "» ya no está en la base. Recarga la página.")
  filas <- match(nombres, actual$nombre_completo)
  nuevas <- which(is.na(filas))
  filas[nuevas] <- nrow(actual) + seq_along(nuevas)
  cambios <- do.call(rbind, lapply(names(valores_por_columna), function(col) {
    data.frame(fila = filas + 1, columna = col, valor = valores_por_columna[[col]], stringsAsFactors = FALSE)
  }))
  if (length(nuevas)) {
    cambios <- rbind(cambios, data.frame(fila = filas[nuevas] + 1, columna = "nombre_completo",
                                         valor = nombres[nuevas], stringsAsFactors = FALSE))
  }
  escribir_celdas_lote(ruta, names(actual), cambios, PESTANA_BENEFICIO)
  length(nombres)
}

marcar_asistencia_censo <- function(ruta, programa, nombres, valor) {
  if (length(nombres) == 0) stop("Selecciona al menos a una persona en la tabla.")
  escribir_personas_beneficio(ruta, nombres, setNames(list(valor), programa))
}

resumen_programas <- function(base) {
  cols <- columnas_programa(base)
  if (length(cols) == 0) return(data.frame(Programa = character(), Asistieron = integer(), check.names = FALSE))
  data.frame(Programa = nombre_programa(cols),
             Asistieron = vapply(cols, function(col) sum(asistio(base[[col]])), integer(1)),
             check.names = FALSE, row.names = NULL)
}

# Personas del censo y de Beneficio juntas, con su asistencia al programa elegido
tabla_asistencia <- function(beneficio, censo, programa, mostrar = "todas") {
  nombres <- sort(union(censo$nombre_completo, beneficio$nombre_completo))
  i <- match(nombres, beneficio$nombre_completo)
  j <- match(nombres, censo$nombre_completo)
  valor <- beneficio[[programa]][i]
  col <- function(n) if (n %in% names(censo)) censo[[n]][j] else rep(NA_character_, length(nombres))
  t <- data.frame(`Nombre completo` = nombres, Municipio = col("MPIO"), Ejido = col("EJIDO"),
                  `Asistencia` = ifelse(asistio(valor), formato_asistencia(valor, programa), "Sin asistencia"),
                  check.names = FALSE)
  if (mostrar == "si") t <- t[t$Asistencia != "Sin asistencia", ]
  if (mostrar == "no") t <- t[t$Asistencia == "Sin asistencia", ]
  rownames(t) <- NULL
  t
}

graf_programas <- function(res) {
  res$Programa <- factor(res$Programa, levels = rev(res$Programa))
  ggplot(res, aes(x = Programa, y = Asistieron,
                  text = paste0("<b>", Programa, "</b><br>Asistieron: ", Asistieron, " personas"))) +
    geom_col(width = 0.7, fill = MORADO) +
    coord_flip() +
    labs(x = NULL, y = "Personas que asistieron") +
    tema_inegi()
}

# --------------------------------------------------------------------- UI -----

programas_contenido_ui <- function() {
  tagList(
    tags$div(
      class = "censo-aviso", icon("clipboard-check"),
      " Registra la asistencia a cada programa con la base Beneficio. ",
      if (es_hoja_google(RUTA_BENEFICIO)) "Los cambios se guardan en la pestaña «Beneficio» de la hoja de Google."
      else tagList("Los cambios se guardan en tu Excel ", tags$b(ruta_legible(RUTA_BENEFICIO)), ".")
    ),
    cajas(
      caja("Programas registrados", "prog_n", "clipboard-list"),
      caja("Personas con al menos una asistencia", "prog_personas", "users", "secondary"),
      caja("Asistencias registradas", "prog_asistencias", "circle-check", "info"),
      caja("Programa con más asistencia", "prog_top", "trophy", "light")
    ),
    navset_card_tab(
      id = "prog_tabs",
      nav_panel(
        "Registrar asistencia", value = "registrar",
        layout_sidebar(
          sidebar = sidebar(
            title = "Programa y filtros", width = 300, open = "desktop",
            selectInput("prog_programa", "Programa", choices = NULL),
            radioButtons("prog_mostrar", "Mostrar",
                         choices = c("Todas las personas" = "todas", "Asistieron" = "si", "Sin asistencia" = "no")),
            dateInput("prog_fecha", "Fecha de la asistencia", value = Sys.Date(), format = "dd/mm/yyyy", language = "es"),
            nota("Busca en la tabla y haz clic en las filas para seleccionar a las personas; después registra su asistencia."),
            tags$hr(),
            actionButton("prog_recargar", "Recargar base", icon = icon("rotate"), class = "btn-outline-primary w-100"),
            actionButton("prog_salir", "Cerrar sesión", icon = icon("right-from-bracket"), class = "btn-outline-secondary w-100 mt-2")
          ),
          tags$div(
            class = "ficha-botones mb-3 align-items-center",
            tags$span(class = "fw-bold me-2", textOutput("prog_seleccion", inline = TRUE)),
            actionButton("prog_si", "Registrar asistencia", icon = icon("check"), class = "btn-primary"),
            actionButton("prog_quitar", "Quitar asistencia", icon = icon("eraser"), class = "btn-outline-secondary"),
            actionButton("prog_deseleccionar", "Quitar selección", icon = icon("square-minus"), class = "btn-outline-secondary")
          ),
          DTOutput("prog_tabla"),
          fillable = FALSE
        )
      ),
      nav_panel(
        "Nuevo programa", value = "nuevo",
        tags$div(
          class = "p-2", style = "max-width: 620px;",
          textInput("prog_nuevo_nombre", "Nombre del programa", placeholder = "Por ejemplo: Taller de lectura 2026", width = "100%"),
          actionButton("prog_crear", "Crear programa", icon = icon("plus"), class = "btn-primary"),
          tags$div(class = "mt-3", uiOutput("prog_resultado_crear")),
          tags$h5(class = "ficha-seccion mt-4", "Programas registrados"),
          uiOutput("prog_lista")
        )
      ),
      nav_panel(
        "Datos de la persona", value = "datos",
        tags$div(
          class = "p-2", style = "max-width: 620px;",
          selectizeInput("prog_persona", "Persona", choices = NULL, width = "100%",
                         options = list(placeholder = "Escribe un nombre...")),
          uiOutput("prog_form_datos")
        )
      ),
      nav_panel("Resumen", value = "resumen", plotlyOutput("prog_grafica", height = "440px"), DTOutput("prog_tabla_resumen")),
      nav_panel("Por persona", value = "persona", DTOutput("prog_tabla_personas"))
    )
  )
}

# ----------------------------------------------------------------- Server -----
# Se llama desde censo_server(): comparte el acceso y la lista de personas del Censo.

programas_server <- function(input, output, session, censo, cargar, autorizado) {
  beneficio <- reactiveVal(NULL)
  programa_preferido <- reactiveVal(NULL)
  resultado_crear <- reactiveVal(NULL)
  idioma_dt <- list(url = "https://cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")

  cargar_beneficio <- function() {
    base <- tryCatch(leer_beneficio(), error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 8)
      NULL
    })
    beneficio(base)
  }
  observeEvent(autorizado(), if (autorizado()) cargar_beneficio() else beneficio(NULL))
  observeEvent(input$prog_recargar, {
    req(autorizado())
    cargar_beneficio()
    cargar(isolate(input$censo_buscar))
    showNotification("Bases recargadas.", type = "message")
  })

  personas <- reactive(sort(union(req(censo())$nombre_completo, req(beneficio())$nombre_completo)))

  observe({
    cols <- columnas_programa(req(beneficio()))
    actual <- isolate(input$prog_programa)
    preferido <- programa_preferido()
    seleccion <- if (!is.null(preferido) && preferido %in% cols) preferido
                 else if (!is.null(actual) && actual %in% cols) actual
                 else if (length(cols)) cols[1]
    updateSelectInput(session, "prog_programa", choices = setNames(cols, nombre_programa(cols)), selected = seleccion)
  })
  observe({
    updateSelectizeInput(session, "prog_persona", choices = personas(),
                         selected = isolate(input$prog_persona), server = TRUE)
  })

  # ---- Cuadros ----
  output$prog_n <- renderText({ length(columnas_programa(req(beneficio()))) })
  output$prog_personas <- renderText({
    base <- req(beneficio())
    cols <- columnas_programa(base)
    if (length(cols) == 0) return("0")
    comma(sum(Reduce(`|`, lapply(cols, function(c) asistio(base[[c]])))))
  })
  output$prog_asistencias <- renderText({ comma(sum(resumen_programas(req(beneficio()))$Asistieron)) })
  output$prog_top <- renderText({
    res <- resumen_programas(req(beneficio()))
    if (nrow(res) == 0 || max(res$Asistieron) == 0) return("Sin asistencias")
    i <- which.max(res$Asistieron)
    paste0(res$Programa[i], " (", res$Asistieron[i], ")")
  })

  # ---- Registrar asistencia ----
  tabla_actual <- reactive({
    req(beneficio(), censo(), input$prog_programa, input$prog_programa %in% names(beneficio()))
    tabla_asistencia(beneficio(), censo(), input$prog_programa, input$prog_mostrar)
  })
  output$prog_tabla <- renderDT({
    datatable(tabla_actual(), rownames = FALSE, selection = "multiple",
              options = list(pageLength = 15, scrollX = TRUE, language = idioma_dt)) |>
      formatStyle("Asistencia", fontWeight = "bold",
                  color = styleEqual("Sin asistencia", "#8A8AA0", default = TURQUESA_OSCURO))
  })
  output$prog_seleccion <- renderText(paste0(length(input$prog_tabla_rows_selected), " persona(s) seleccionada(s)"))
  observeEvent(input$prog_deseleccionar, selectRows(dataTableProxy("prog_tabla"), NULL))

  marcar <- function(quitar = FALSE) {
    req(autorizado(), beneficio())
    programa <- input$prog_programa
    nombres <- tabla_actual()$`Nombre completo`[input$prog_tabla_rows_selected]
    valor <- if (quitar) "" else if (programa %in% COLUMNAS_CONTEO) NA else format(input$prog_fecha, "%Y-%m-%d")
    if (!quitar && programa %in% COLUMNAS_CONTEO) {
      # En los conteos se suma una asistencia a lo que ya tenía cada persona
      previo <- suppressWarnings(as.numeric(beneficio()[[programa]][match(nombres, beneficio()$nombre_completo)]))
      valor <- as.character(ifelse(is.na(previo), 0, previo) + 1)
    }
    n <- tryCatch(marcar_asistencia_censo(RUTA_BENEFICIO, programa, nombres, valor), error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 8)
      NULL
    })
    if (!is.null(n)) {
      programa_preferido(programa)
      cargar_beneficio()
      texto <- if (quitar) "Se quitó la asistencia de " else "Se registró la asistencia de "
      showNotification(paste0(texto, n, " persona(s) en ", nombre_programa(programa), "."), type = "message", duration = 6)
    }
  }
  observeEvent(input$prog_si, marcar())
  observeEvent(input$prog_quitar, marcar(quitar = TRUE))

  # ---- Nuevo programa ----
  observeEvent(input$prog_crear, {
    req(autorizado())
    columna <- tryCatch(crear_programa_censo(RUTA_BENEFICIO, input$prog_nuevo_nombre), error = function(e) {
      resultado_crear(list(ok = FALSE, texto = conditionMessage(e)))
      NULL
    })
    if (!is.null(columna)) {
      programa_preferido(columna)
      cargar_beneficio()
      updateTextInput(session, "prog_nuevo_nombre", value = "")
      resultado_crear(list(ok = TRUE, texto = paste0("Se creó el programa «", columna,
                                                     "». Ya puedes registrar la asistencia en «Registrar asistencia».")))
    }
  })
  output$prog_resultado_crear <- renderUI({
    r <- resultado_crear()
    if (is.null(r)) return(NULL)
    tags$div(class = paste("alert", if (r$ok) "alert-success" else "alert-danger"),
             icon(if (r$ok) "circle-check" else "triangle-exclamation"), " ", r$texto)
  })
  output$prog_lista <- renderUI({
    res <- resumen_programas(req(beneficio()))
    if (nrow(res) == 0) return(nota("Todavía no hay programas."))
    tags$ul(class = "lista-ayuda", lapply(seq_len(nrow(res)), function(i) {
      tags$li(tags$b(res$Programa[i]), paste0(" · ", res$Asistieron[i], " personas asistieron"))
    }))
  })

  # ---- Datos de la persona (escolaridad, promedio y lugar de origen) ----
  output$prog_form_datos <- renderUI({
    base <- req(beneficio())
    req(input$prog_persona)
    fila <- match(input$prog_persona, base$nombre_completo)
    tagList(
      lapply(seq_along(DATOS_BENEFICIO), function(k) {
        col <- DATOS_BENEFICIO[k]
        valor <- if (is.na(fila) || !col %in% names(base) || vacio(base[fila, col])) "" else base[fila, col]
        textInput(paste0("prog_dato_", k), etiqueta_censo(col), value = valor, width = "100%", placeholder = "Escribe aquí")
      }),
      actionButton("prog_guardar_datos", "Guardar datos", icon = icon("floppy-disk"), class = "btn-primary")
    )
  })
  observeEvent(input$prog_guardar_datos, {
    req(autorizado(), beneficio(), input$prog_persona)
    cols <- intersect(DATOS_BENEFICIO, names(beneficio()))
    valores <- lapply(match(cols, DATOS_BENEFICIO), function(k) trimws(if (is.null(input[[paste0("prog_dato_", k)]])) "" else input[[paste0("prog_dato_", k)]]))
    n <- tryCatch(escribir_personas_beneficio(RUTA_BENEFICIO, input$prog_persona, setNames(valores, cols)),
                  error = function(e) {
                    showNotification(conditionMessage(e), type = "error", duration = 8)
                    NULL
                  })
    if (!is.null(n)) {
      cargar_beneficio()
      showNotification(paste0("Se guardaron los datos de ", input$prog_persona, "."), type = "message", duration = 6)
    }
  })

  # ---- Resumen y tabla por persona ----
  output$prog_grafica <- renderPlotly({
    res <- resumen_programas(req(beneficio()))
    validate(need(nrow(res) > 0, "Todavía no hay programas."))
    a_plotly(graf_programas(res))
  })
  output$prog_tabla_resumen <- renderDT({
    datatable(resumen_programas(req(beneficio())), rownames = FALSE, options = list(dom = "t", pageLength = 50, language = idioma_dt))
  })
  output$prog_tabla_personas <- renderDT({
    base <- req(beneficio())
    t <- data.frame(`Nombre completo` = base$nombre_completo, check.names = FALSE)
    for (c in intersect(DATOS_BENEFICIO, names(base))) t[[etiqueta_censo(c)]] <- ifelse(vacio(base[[c]]), "", base[[c]])
    for (c in columnas_programa(base)) t[[nombre_programa(c)]] <- formato_asistencia(base[[c]], c)
    datatable(t, rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE, language = idioma_dt))
  })
}
