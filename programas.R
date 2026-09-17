# =============================================================================
# Pestaña "Programas": registro de asistencia de las personas del censo
#
# - Usa la misma contraseña que el Censo; al entrar se abre la pestaña aparte
#   "Registro de programas".
# - Cada programa es una columna de la base del censo llamada "Programa: <nombre>"
#   con «Sí», «No» o vacío (sin dato). "Asistencia 21 de Abril" se toma como programa.
# - Los cambios se guardan en la misma base del censo (Excel u hoja de Google).
# =============================================================================

PREFIJO_PROGRAMA <- "Programa: "
PROGRAMAS_EXISTENTES <- c("Asistencia 21 de Abril" = "21 de Abril")

columnas_programa <- function(base) {
  c(intersect(names(PROGRAMAS_EXISTENTES), names(base)),
    grep(paste0("^", PREFIJO_PROGRAMA), names(base), value = TRUE))
}

nombre_programa <- function(columnas) {
  unname(ifelse(columnas %in% names(PROGRAMAS_EXISTENTES), PROGRAMAS_EXISTENTES[columnas],
                sub(paste0("^", PREFIJO_PROGRAMA), "", columnas)))
}

# ---------------------------------------------------- Lectura y escritura -----

crear_programa_censo <- function(ruta, nombre) {
  nombre <- gsub("\\s+", " ", trimws(if (is.null(nombre)) "" else nombre))
  if (!nzchar(nombre)) stop("Escribe el nombre del programa.")
  if (nchar(nombre) > 60) stop("El nombre del programa es muy largo (máximo 60 caracteres).")
  columna <- paste0(PREFIJO_PROGRAMA, nombre)
  validar_guardado(ruta)
  actual <- leer_censo(ruta)
  if (toupper(nombre) %in% toupper(nombre_programa(columnas_programa(actual))) || columna %in% names(actual)) {
    stop("Ya existe un programa llamado «", nombre, "».")
  }
  agregar_columna_censo(ruta, columna, ncol(actual) + 1)
  columna
}

marcar_asistencia_censo <- function(ruta, programa, nombres, valor) {
  if (length(nombres) == 0) stop("Selecciona al menos a una persona en la tabla.")
  if (!valor %in% c("Sí", "No", "")) stop("Valor de asistencia no válido.")
  validar_guardado(ruta)
  actual <- leer_censo(ruta)
  if (!programa %in% names(actual)) stop("El programa ya no está en la base. Recarga la página.")
  filas <- match(nombres, actual$nombre_completo)
  if (anyNA(filas)) stop("Algunas personas ya no están en la base. Recarga la página.")
  escribir_celdas_lote(ruta, names(actual),
                       data.frame(fila = filas + 1, columna = programa, valor = valor, stringsAsFactors = FALSE))
  length(filas)
}

resumen_programas <- function(base) {
  cols <- columnas_programa(base)
  if (length(cols) == 0) {
    return(data.frame(Programa = character(), Asistieron = integer(), `No asistieron` = integer(),
                      `Sin dato` = integer(), check.names = FALSE))
  }
  do.call(rbind, lapply(cols, function(col) {
    v <- normalizar_sino(base[[col]])
    data.frame(Programa = nombre_programa(col), Asistieron = sum(v %in% "Sí"), `No asistieron` = sum(v %in% "No"),
               `Sin dato` = sum(is.na(v)), check.names = FALSE)
  }))
}

tabla_asistencia <- function(base, programa, mostrar = "todas") {
  v <- normalizar_sino(base[[programa]])
  col <- function(n) if (n %in% names(base)) base[[n]] else rep(NA_character_, nrow(base))
  t <- data.frame(`Nombre completo` = base$nombre_completo, Municipio = col("MPIO"), Ejido = col("EJIDO"),
                  `Asistió` = ifelse(is.na(v), "Sin dato", v), check.names = FALSE)
  if (mostrar %in% c("Sí", "No")) t <- t[t$`Asistió` == mostrar, ]
  if (mostrar == "sin") t <- t[t$`Asistió` == "Sin dato", ]
  rownames(t) <- NULL
  t
}

graf_programas <- function(res) {
  estados <- c("Asistieron", "No asistieron", "Sin dato")
  largo <- do.call(rbind, lapply(estados, function(e) data.frame(Programa = res$Programa, Estado = e, Personas = res[[e]])))
  largo$Programa <- factor(largo$Programa, levels = rev(res$Programa))
  largo$Estado <- factor(largo$Estado, levels = rev(estados))
  ggplot(largo, aes(x = Programa, y = Personas, fill = Estado,
                    text = paste0("<b>", Programa, "</b><br>", Estado, ": ", Personas, " personas"))) +
    geom_col(width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("Asistieron" = MORADO, "No asistieron" = "#D9577E", "Sin dato" = "#D9D8E8"),
                      breaks = estados, name = NULL) +
    labs(x = NULL, y = "Personas del censo") +
    tema_inegi()
}

# --------------------------------------------------------------------- UI -----

programas_contenido_ui <- function() {
  tagList(
    tags$div(
      class = "censo-aviso", icon("clipboard-check"),
      " Registra la asistencia de las personas del censo a cada programa. ",
      if (es_hoja_google(RUTA_CENSO)) "Los cambios se guardan en la hoja de Google del censo."
      else tagList("Los cambios se guardan en tu Excel ", tags$b(ruta_legible(RUTA_CENSO)), ".")
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
            title = "Filtros de esta tabla", width = 300, open = "desktop",
            selectInput("prog_programa", "Programa", choices = NULL),
            radioButtons("prog_mostrar", "Mostrar",
                         choices = c("Todas las personas" = "todas", "Asistieron" = "Sí", "No asistieron" = "No", "Sin dato" = "sin")),
            nota("Busca en la tabla y haz clic en las filas para seleccionar a las personas; después marca su asistencia."),
            tags$hr(),
            actionButton("prog_salir", "Cerrar sesión", icon = icon("right-from-bracket"), class = "btn-outline-secondary w-100")
          ),
          tags$div(
            class = "ficha-botones mb-3 align-items-center",
            tags$span(class = "fw-bold me-2", textOutput("prog_seleccion", inline = TRUE)),
            actionButton("prog_si", "Marcar «Sí asistió»", icon = icon("check"), class = "btn-primary"),
            actionButton("prog_no", "Marcar «No asistió»", icon = icon("xmark"), class = "btn-outline-primary"),
            actionButton("prog_quitar", "Quitar dato", icon = icon("eraser"), class = "btn-outline-secondary"),
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
      nav_panel("Resumen", value = "resumen", plotlyOutput("prog_grafica", height = "440px"), DTOutput("prog_tabla_resumen")),
      nav_panel("Por persona", value = "persona", DTOutput("prog_tabla_personas"))
    )
  )
}

# ----------------------------------------------------------------- Server -----
# Se llama desde censo_server(): comparte la base, la carga y el acceso con el Censo.

programas_server <- function(input, output, session, censo, cargar, autorizado) {
  programa_preferido <- reactiveVal(NULL)
  resultado_crear <- reactiveVal(NULL)
  idioma_dt <- list(url = "https://cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")

  observe({
    req(censo())
    cols <- columnas_programa(censo())
    actual <- isolate(input$prog_programa)
    preferido <- programa_preferido()
    seleccion <- if (!is.null(preferido) && preferido %in% cols) preferido
                 else if (!is.null(actual) && actual %in% cols) actual
                 else if (length(cols)) tail(cols, 1)
    updateSelectInput(session, "prog_programa", choices = setNames(cols, nombre_programa(cols)), selected = seleccion)
  })

  # ---- Cuadros ----
  output$prog_n <- renderText({ req(censo()); length(columnas_programa(censo())) })
  output$prog_personas <- renderText({
    base <- req(censo())
    cols <- columnas_programa(base)
    if (length(cols) == 0) return("0")
    asistio <- vapply(cols, function(c) normalizar_sino(base[[c]]) %in% "Sí", logical(nrow(base)))
    comma(sum(rowSums(matrix(asistio, nrow = nrow(base))) > 0))
  })
  output$prog_asistencias <- renderText({ comma(sum(resumen_programas(req(censo()))$Asistieron)) })
  output$prog_top <- renderText({
    res <- resumen_programas(req(censo()))
    if (nrow(res) == 0 || max(res$Asistieron) == 0) return("Sin asistencias")
    i <- which.max(res$Asistieron)
    paste0(res$Programa[i], " (", res$Asistieron[i], ")")
  })

  # ---- Registrar asistencia ----
  tabla_actual <- reactive({
    req(censo(), input$prog_programa, input$prog_programa %in% names(censo()))
    tabla_asistencia(censo(), input$prog_programa, input$prog_mostrar)
  })
  output$prog_tabla <- renderDT({
    datatable(tabla_actual(), rownames = FALSE, selection = "multiple",
              options = list(pageLength = 15, scrollX = TRUE, language = idioma_dt)) |>
      formatStyle("Asistió", fontWeight = "bold",
                  color = styleEqual(c("Sí", "No", "Sin dato"), c(TURQUESA_OSCURO, "#D9577E", "#8A8AA0")))
  })
  output$prog_seleccion <- renderText(paste0(length(input$prog_tabla_rows_selected), " persona(s) seleccionada(s)"))
  observeEvent(input$prog_deseleccionar, selectRows(dataTableProxy("prog_tabla"), NULL))

  marcar <- function(valor) {
    req(autorizado(), censo())
    seleccion <- input$prog_tabla_rows_selected
    nombres <- tabla_actual()$`Nombre completo`[seleccion]
    programa <- input$prog_programa
    n <- tryCatch(marcar_asistencia_censo(RUTA_CENSO, programa, nombres, valor), error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 8)
      NULL
    })
    if (!is.null(n)) {
      programa_preferido(programa)
      cargar(isolate(input$censo_buscar))
      texto <- if (valor == "") "sin dato" else paste0("«", valor, "»")
      showNotification(paste0("Se registró ", texto, " para ", n, " persona(s) en ", nombre_programa(programa), "."),
                       type = "message", duration = 6)
    }
  }
  observeEvent(input$prog_si, marcar("Sí"))
  observeEvent(input$prog_no, marcar("No"))
  observeEvent(input$prog_quitar, marcar(""))

  # ---- Nuevo programa ----
  observeEvent(input$prog_crear, {
    req(autorizado())
    columna <- tryCatch(crear_programa_censo(RUTA_CENSO, input$prog_nuevo_nombre), error = function(e) {
      resultado_crear(list(ok = FALSE, texto = conditionMessage(e)))
      NULL
    })
    if (!is.null(columna)) {
      programa_preferido(columna)
      cargar(isolate(input$censo_buscar))
      updateTextInput(session, "prog_nuevo_nombre", value = "")
      resultado_crear(list(ok = TRUE, texto = paste0("Se creó el programa «", nombre_programa(columna),
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
    res <- resumen_programas(req(censo()))
    if (nrow(res) == 0) return(nota("Todavía no hay programas."))
    tags$ul(class = "lista-ayuda", lapply(seq_len(nrow(res)), function(i) {
      tags$li(tags$b(res$Programa[i]), paste0(" · ", res$Asistieron[i], " asistieron, ", res$`No asistieron`[i],
                                              " no asistieron, ", res$`Sin dato`[i], " sin dato"))
    }))
  })

  # ---- Resumen y tabla por persona ----
  output$prog_grafica <- renderPlotly({
    res <- resumen_programas(req(censo()))
    validate(need(nrow(res) > 0, "Todavía no hay programas."))
    a_plotly(graf_programas(res))
  })
  output$prog_tabla_resumen <- renderDT({
    datatable(resumen_programas(req(censo())), rownames = FALSE, options = list(dom = "t", pageLength = 50, language = idioma_dt))
  })
  output$prog_tabla_personas <- renderDT({
    base <- req(censo())
    cols <- columnas_programa(base)
    t <- data.frame(`Nombre completo` = base$nombre_completo, check.names = FALSE)
    for (c in cols) t[[nombre_programa(c)]] <- ifelse(is.na(normalizar_sino(base[[c]])), "", normalizar_sino(base[[c]]))
    datatable(t, rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE, language = idioma_dt))
  })
}
