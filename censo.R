# =============================================================================
# Pestaña "Censo": consulta y edición de la base del censo en Excel
#
# - Se entra con contraseña; al entrar se abre la pestaña aparte "Base del Censo".
# - Los cambios se escriben directamente en el Excel (solo las celdas que cambian).
# - No se crean copias del Excel: solo se actualiza el archivo de RUTA_CENSO.
#
# Paquetes: readxl, openxlsx y digest
#   install.packages(c("readxl", "openxlsx", "digest"))
# =============================================================================

library(readxl)

# ---- Dónde vive la base del censo ----
# La configuración está en el archivo .Renviron de esta carpeta:
#   CENSO_HOJA          URL de la aplicación web de Apps Script de la hoja de Google (obligatoria en shinyapps.io)
#   CENSO_TOKEN         clave del puente de Apps Script (credenciales/apps_script_censo.gs)
#   CENSO_CLAVE_SHA256  huella de la contraseña (opcional)
# Si CENSO_HOJA está vacía, la interfaz usa el Excel local de CENSO_RUTA.
if (file.exists(".Renviron")) readRenviron(".Renviron")

CENSO_HOJA <- Sys.getenv("CENSO_HOJA")
CENSO_TOKEN <- Sys.getenv("CENSO_TOKEN")
RUTA_CENSO <- if (nzchar(CENSO_HOJA)) CENSO_HOJA else Sys.getenv("CENSO_RUTA", path.expand("~/Desktop/BaseNuevaMerge_v2.1_Censo.xlsx"))

# La contraseña no se guarda tal cual, solo su huella SHA-256.
# Para cambiarla, calcula la nueva huella en la consola de R y ponla en CENSO_CLAVE_SHA256 (.Renviron):
#   digest::digest("nueva contraseña", algo = "sha256", serialize = FALSE)
CLAVE_CENSO_SHA256 <- Sys.getenv("CENSO_CLAVE_SHA256", "36415cd3e02550e0557f4ddce7f934956738bfb2938f71f402d91563a2427e50")
INTENTOS_MAXIMOS <- 5

# Columnas que en el Excel están guardadas como número (las demás van como texto)
COLUMNAS_NUMERICAS <- c("No.", "Asistencia_Ingenium", "EDAD")

ETIQUETAS_CENSO <- c(
  "nombre_completo" = "Nombre completo", "No." = "Número (No.)", "Nombre (s)" = "Nombre(s)",
  "Apellido (s)" = "Apellido(s)", "Parentezco" = "Parentesco", "CURP" = "CURP",
  "DIA" = "Día de nacimiento", "MES" = "Mes de nacimiento", "AÑO" = "Año de nacimiento",
  "F. DE NAC" = "Fecha de nacimiento", "EDAD" = "Edad", "SEXO (MUJER/HOMBRE)" = "Sexo",
  "MPIO" = "Municipio", "EJIDO" = "Ejido", "TELEFONO" = "Teléfono",
  "Asistencia_Ingenium" = "Asistencias a Ingenium", "Escolaridad" = "Escolaridad",
  "Promedio / Aptitudes" = "Promedio / aptitudes", "Lugar de origen" = "Lugar de origen",
  "recibio_celular" = "¿Recibió celular?", "Entrevistado_ProSocial" = "¿Entrevistado por ProSocial?",
  "Asistencia 21 de Abril" = "¿Asistió el 21 de abril?"
)

# Parentesco, municipio, ejido, lugar de origen y escolaridad se escriben libremente
TIPOS_CENSO <- c(
  "SEXO (MUJER/HOMBRE)" = "sexo",
  "recibio_celular" = "sino", "Entrevistado_ProSocial" = "sino", "Asistencia 21 de Abril" = "sino",
  "No." = "numero", "Asistencia_Ingenium" = "numero", "EDAD" = "numero",
  "DIA" = "numero", "MES" = "numero", "AÑO" = "numero"
)
COLUMNAS_SINO <- names(TIPOS_CENSO)[TIPOS_CENSO == "sino"]

SECCIONES_CENSO <- list(
  "Identificación" = c("nombre_completo", "No.", "CURP", "Parentezco"),
  "Nacimiento y sexo" = c("DIA", "MES", "AÑO", "F. DE NAC", "EDAD", "SEXO (MUJER/HOMBRE)"),
  "Ubicación y contacto" = c("MPIO", "EJIDO", "TELEFONO")
)

# Columnas que están en la hoja del censo pero no se muestran en la página:
# el nombre va solo como "Nombre completo" y los demás datos se registran en Programas (base Beneficio)
COLUMNAS_OCULTAS_CENSO <- c("Nombre (s)", "Apellido (s)", "Escolaridad", "Promedio / Aptitudes", "Lugar de origen",
                            "Asistencia_Ingenium", "recibio_celular", "Entrevistado_ProSocial", "Asistencia 21 de Abril")

# ------------------------------------------------------ Lectura y escritura -----

vacio <- function(x) is.na(x) | trimws(x) == ""

# La base es una hoja de Google si la ruta no es un archivo de Excel
es_hoja_google <- function(ruta) !grepl("\\.xlsx?$", ruta, ignore.case = TRUE)

# La hoja de Google se lee y se escribe con la aplicación web de Apps Script
# (credenciales/apps_script_censo.gs), que responde solo si recibe CENSO_TOKEN.
llamar_google <- function(accion, ...) {
  if (!nzchar(CENSO_TOKEN)) stop("Falta CENSO_TOKEN en la configuración.")
  cuerpo <- jsonlite::toJSON(list(token = CENSO_TOKEN, accion = accion, ...), auto_unbox = TRUE, na = "null",
                             null = "null", dataframe = "values", digits = NA)
  resp <- httr::POST(CENSO_HOJA, body = cuerpo, httr::content_type_json(), httr::timeout(90))
  texto <- httr::content(resp, as = "text", encoding = "UTF-8")
  r <- tryCatch(jsonlite::fromJSON(texto, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(r)) stop("La hoja de Google no respondió correctamente (HTTP ", httr::status_code(resp), ").")
  if (!isTRUE(r$ok)) stop("Hoja de Google: ", r$error)
  r
}

# Lee una pestaña (la primera si `pestana` es NULL) como data.frame de texto; NULL si no existe
leer_google <- function(pestana = NULL) {
  r <- llamar_google("leer", pestana = pestana)
  if (!isTRUE(r$existe)) return(NULL)
  v <- r$valores
  if (length(v) == 0) return(data.frame())
  if (!is.matrix(v)) v <- matrix(v, nrow = 1)
  base <- as.data.frame(v[-1, , drop = FALSE], stringsAsFactors = FALSE, optional = TRUE)
  names(base) <- v[1, ]
  base[] <- lapply(base, function(x) { x[!is.na(x) & x == ""] <- NA; x })
  base
}

# Reemplaza toda una pestaña con un data.frame (las columnas numéricas se guardan como número)
reemplazar_google <- function(pestana, datos, solo_si_vacia = FALSE) {
  datos <- as.data.frame(datos, check.names = FALSE)
  numericas <- which(vapply(datos, is.numeric, logical(1)))
  filas <- lapply(seq_len(nrow(datos)), function(i) unname(lapply(datos, `[[`, i)))
  llamar_google("reemplazar", pestana = pestana, encabezados = I(names(datos)), filas = filas,
                numericas = I(unname(numericas)), solo_si_vacia = solo_si_vacia)
}

# Unifica respuestas como "SÍ", "si", "NO" o "n" en "Sí" y "No"
normalizar_sino <- function(x) {
  x <- as.character(x)
  clave <- toupper(trimws(chartr("íÍ", "iI", x)))
  ifelse(is.na(x) | clave == "", NA_character_,
         ifelse(clave %in% c("SI", "S"), "Sí",
                ifelse(clave %in% c("NO", "N"), "No", trimws(x))))
}

leer_censo <- function(ruta = RUTA_CENSO, pestana = NULL) {
  if (es_hoja_google(ruta)) {
    base <- leer_google(pestana)
    if (is.null(base) || ncol(base) == 0) stop("La hoja de Google no tiene la pestaña ", if (is.null(pestana)) "del censo" else paste0("«", pestana, "»"), " o está vacía.")
    return(base)
  }
  if (!file.exists(ruta)) stop("No se encontró el Excel del censo en: ", ruta)
  base <- as.data.frame(read_excel(ruta, sheet = 1, col_types = "text"), check.names = FALSE)
  attr(base, "modificado") <- file.info(ruta)$mtime
  base
}

# Ruta corta para mostrarla en pantalla, por ejemplo ~/Desktop/BaseNuevaMerge_v2.xlsx
ruta_legible <- function(ruta = RUTA_CENSO) {
  if (es_hoja_google(ruta)) return("la hoja de Google del censo")
  sub(paste0("^", path.expand("~")), "~", normalizePath(ruta, mustWork = FALSE))
}

archivo_abierto <- function(ruta = RUTA_CENSO) {
  if (es_hoja_google(ruta)) return(FALSE)
  file.exists(file.path(dirname(ruta), paste0("~$", basename(ruta))))
}

# openxlsx deja referencias a dibujos que no existen dentro del archivo;
# se quitan para que Excel no marque el libro como dañado al abrirlo
limpiar_xlsx <- function(entrada, salida) {
  carpeta <- tempfile()
  dir.create(carpeta)
  on.exit(unlink(carpeta, recursive = TRUE))
  utils::unzip(entrada, exdir = carpeta)
  leer <- function(f) paste(readLines(file.path(carpeta, f), warn = FALSE, encoding = "UTF-8"), collapse = "")
  archivos <- list.files(carpeta, recursive = TRUE, all.files = TRUE)
  for (rel in archivos[grepl("\\.rels$", archivos)]) {
    x <- leer(rel)
    origen <- dirname(dirname(rel))
    for (r in regmatches(x, gregexpr("<Relationship [^>]*/>", x))[[1]]) {
      if (grepl('TargetMode="External"', r, fixed = TRUE)) next
      destino <- sub('.*Target="([^"]+)".*', "\\1", r)
      parte <- if (startsWith(destino, "/")) sub("^/", "", destino) else file.path(origen, destino)
      parte <- gsub("[^/]+/\\.\\./", "", parte)
      if (!file.exists(file.path(carpeta, parte))) x <- sub(r, "", x, fixed = TRUE)
    }
    writeLines(x, file.path(carpeta, rel), useBytes = TRUE)
  }
  tipos <- leer("[Content_Types].xml")
  for (o in regmatches(tipos, gregexpr("<Override [^>]*/>", tipos))[[1]]) {
    parte <- sub('.*PartName="/([^"]+)".*', "\\1", o)
    if (!file.exists(file.path(carpeta, parte))) tipos <- sub(o, "", tipos, fixed = TRUE)
  }
  writeLines(tipos, file.path(carpeta, "[Content_Types].xml"), useBytes = TRUE)
  archivos <- c("[Content_Types].xml", setdiff(list.files(carpeta, recursive = TRUE, all.files = TRUE), "[Content_Types].xml"))
  unlink(salida)
  zip::zip(salida, files = archivos, root = carpeta, mode = "mirror")
  invisible(salida)
}

# ---- Escritura en Google Sheets ----
# Manda a la hoja de Google solo las celdas que cambian (fila de la hoja, columna por nombre)
escribir_hoja_lote <- function(columnas, cambios, pestana = NULL) {
  celdas <- lapply(seq_len(nrow(cambios)), function(k) {
    j <- match(cambios$columna[k], columnas)
    if (is.na(j)) return(NULL)
    v <- trimws(as.character(cambios$valor[k]))
    num <- suppressWarnings(as.numeric(v))
    valor <- if (is.na(v) || v == "") NULL else if (cambios$columna[k] %in% COLUMNAS_NUMERICAS && !is.na(num)) num else v
    list(fila = cambios$fila[k], columna = j, valor = valor)
  })
  celdas <- Filter(Negate(is.null), celdas)
  if (length(celdas)) llamar_google("escribir", pestana = pestana, celdas = celdas)
  invisible(TRUE)
}

# Escribe solo las celdas indicadas; conserva el resto del libro tal cual.
# `cambios` es un data.frame con la fila del Excel, la columna y el valor nuevo.
escribir_celdas_lote <- function(ruta, columnas, cambios, pestana = NULL) {
  if (es_hoja_google(ruta)) return(escribir_hoja_lote(columnas, cambios, pestana))
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Falta el paquete openxlsx. Instálalo con install.packages(\"openxlsx\").")
  }
  libro <- openxlsx::loadWorkbook(ruta)
  for (k in seq_len(nrow(cambios))) {
    fila_excel <- cambios$fila[k]
    col <- cambios$columna[k]
    j <- match(col, columnas)
    if (is.na(j)) next
    valor <- trimws(as.character(cambios$valor[k]))
    if (is.na(valor) || valor == "") {
      openxlsx::deleteData(libro, sheet = 1, cols = j, rows = fila_excel, gridExpand = TRUE)
    } else if (col %in% COLUMNAS_NUMERICAS && !is.na(suppressWarnings(as.numeric(valor)))) {
      openxlsx::writeData(libro, sheet = 1, x = as.numeric(valor), startCol = j, startRow = fila_excel, colNames = FALSE)
    } else {
      openxlsx::writeData(libro, sheet = 1, x = valor, startCol = j, startRow = fila_excel, colNames = FALSE)
    }
  }
  guardar_libro(libro, ruta)
}

guardar_libro <- function(libro, ruta) {
  temporal <- tempfile(fileext = ".xlsx")
  limpio <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(libro, temporal, overwrite = TRUE)
  limpiar_xlsx(temporal, limpio)
  if (!file.copy(limpio, ruta, overwrite = TRUE)) stop("No se pudo escribir en el Excel.")
  unlink(c(temporal, limpio))
  invisible(TRUE)
}

# Agrega una columna vacía (por ejemplo, un programa nuevo) en la posición j
agregar_columna_censo <- function(ruta, columna, j, pestana = NULL) {
  if (es_hoja_google(ruta)) {
    llamar_google("escribir", pestana = pestana, celdas = list(list(fila = 1, columna = j, valor = columna)))
  } else {
    libro <- openxlsx::loadWorkbook(ruta)
    openxlsx::writeData(libro, sheet = 1, x = columna, startCol = j, startRow = 1, colNames = FALSE)
    openxlsx::addStyle(libro, sheet = 1, style = openxlsx::createStyle(textDecoration = "bold"), rows = 1, cols = j)
    guardar_libro(libro, ruta)
  }
  if (!(columna %in% names(leer_censo(ruta, pestana)))) stop("No se pudo agregar la columna a la base.")
  invisible(TRUE)
}

escribir_celdas <- function(ruta, fila_excel, columnas, cambios) {
  valores <- vapply(cambios, function(v) if (is.null(v) || length(v) == 0) NA_character_ else as.character(v), character(1))
  escribir_celdas_lote(ruta, columnas, data.frame(fila = fila_excel, columna = names(cambios), valor = unname(valores),
                                                  stringsAsFactors = FALSE))
}

validar_guardado <- function(ruta) {
  if (es_hoja_google(ruta)) return(invisible(TRUE))
  if (archivo_abierto(ruta)) stop("El Excel está abierto en otro programa. Ciérralo y vuelve a guardar.")
  if (file.access(ruta, 2) != 0) stop("No hay permiso para modificar el Excel.")
}

# Guarda los cambios de una persona. `fila` es la posición en la base leída.
guardar_fila_censo <- function(ruta, fila, nombre_original, cambios) {
  validar_guardado(ruta)
  actual <- leer_censo(ruta)
  if (!identical(actual$nombre_completo[fila], nombre_original)) {
    fila <- match(nombre_original, actual$nombre_completo)
    if (is.na(fila)) stop("La persona ya no está en el Excel. Recarga la base y vuelve a intentarlo.")
  }
  nuevo_nombre <- cambios[["nombre_completo"]]
  if (!is.null(nuevo_nombre)) {
    if (vacio(nuevo_nombre)) stop("El nombre completo no puede quedar vacío.")
    otros <- actual$nombre_completo[-fila]
    if (toupper(trimws(nuevo_nombre)) %in% toupper(trimws(otros))) stop("Ya existe otra persona con ese nombre completo.")
  }
  escribir_celdas(ruta, fila + 1, names(actual), cambios)
  invisible(TRUE)
}

# El nombre completo se guarda en mayúsculas y sin espacios de más, igual que el resto de la base
completar_nombre <- function(valores) {
  if (!is.null(valores[["nombre_completo"]]) && !vacio(valores[["nombre_completo"]])) {
    valores[["nombre_completo"]] <- toupper(gsub("\\s+", " ", trimws(valores[["nombre_completo"]])))
  }
  valores
}

agregar_persona_censo <- function(ruta, valores) {
  validar_guardado(ruta)
  actual <- leer_censo(ruta)
  valores <- completar_nombre(valores)
  nombre <- valores[["nombre_completo"]]
  if (is.null(nombre) || vacio(nombre)) stop("Escribe el nombre completo de la persona.")
  if (toupper(trimws(nombre)) %in% toupper(trimws(actual$nombre_completo))) {
    stop("Ya existe una persona con ese nombre completo. Búscala en \"Buscar y editar\".")
  }
  valores <- valores[!vapply(valores, vacio, logical(1))]
  escribir_celdas(ruta, nrow(actual) + 2, names(actual), valores)
  # Confirmar leyendo de nuevo el Excel que la persona sí quedó guardada
  if (!(nombre %in% leer_censo(ruta)$nombre_completo)) stop("El Excel no se actualizó. Revisa que no esté abierto y vuelve a intentarlo.")
  structure(TRUE, nombre = nombre)
}

# Respuestas de sí/no escritas de otra forma ("SÍ", "no", "n"...) que se pueden unificar
respuestas_sino_por_unificar <- function(base) {
  partes <- lapply(intersect(union(COLUMNAS_SINO, grep("^Programa: ", names(base), value = TRUE)), names(base)), function(col) {
    antes <- base[[col]]
    despues <- normalizar_sino(antes)
    i <- which(!is.na(antes) & !is.na(despues) & antes != despues)
    if (length(i)) data.frame(fila = i + 1, columna = col, valor = despues[i], antes = antes[i], stringsAsFactors = FALSE)
  })
  partes <- Filter(Negate(is.null), partes)
  if (length(partes)) do.call(rbind, partes) else data.frame(fila = integer(), columna = character(), valor = character(), antes = character())
}

unificar_sino_censo <- function(ruta) {
  validar_guardado(ruta)
  actual <- leer_censo(ruta)
  cambios <- respuestas_sino_por_unificar(actual)
  if (nrow(cambios) == 0) return(0L)
  escribir_celdas_lote(ruta, names(actual), cambios)
  nrow(cambios)
}

# ------------------------------------------------------------------ Apoyos -----

# Fecha de nacimiento y edad a partir de día, mes y año (el año puede venir con 2 dígitos)
calcular_nacimiento <- function(dia, mes, anio, hoy = Sys.Date()) {
  d <- suppressWarnings(as.integer(dia))
  m <- suppressWarnings(as.integer(mes))
  a <- suppressWarnings(as.integer(anio))
  if (length(d) != 1 || length(m) != 1 || length(a) != 1 || anyNA(c(d, m, a))) return(NULL)
  if (a < 100) {
    corte <- as.integer(format(hoy, "%y"))
    a <- if (a <= corte) 2000L + a else 1900L + a
  }
  fecha <- as.Date(sprintf("%04d-%02d-%02d", a, m, d), format = "%Y-%m-%d")
  if (is.na(fecha) || fecha > hoy) return(NULL)
  edad <- as.integer(format(hoy, "%Y")) - a - as.integer(format(hoy, "%m%d") < format(fecha, "%m%d"))
  list(fecha = format(fecha, "%d/%m/%Y"), edad = edad)
}

# Columnas de datos de la persona (sin contar las de programas)
columnas_datos <- function(base) setdiff(names(base), c(grep("^Programa: ", names(base), value = TRUE), COLUMNAS_OCULTAS_CENSO))

faltantes_persona <- function(fila) {
  faltan <- names(fila)[vapply(fila, vacio, logical(1))]
  intersect(faltan, columnas_datos(fila))
}

etiqueta_censo <- function(col) {
  e <- ETIQUETAS_CENSO[col]
  ifelse(is.na(e), col, e)
}

tabla_incompletos <- function(base, campo = "") {
  datos <- base[, columnas_datos(base), drop = FALSE]
  faltan <- apply(datos, 1, function(f) sum(vacio(f)))
  lista <- apply(datos, 1, function(f) paste(etiqueta_censo(names(f)[vacio(f)]), collapse = ", "))
  t <- data.frame(`Nombre completo` = base$nombre_completo, `Datos faltantes` = faltan,
                  `Completitud` = paste0(round(100 * (1 - faltan / ncol(datos))), "%"),
                  `Le falta` = lista, check.names = FALSE)
  if (nzchar(campo)) t <- t[vacio(base[[campo]]), ]
  t[order(-t$`Datos faltantes`, t$`Nombre completo`), ]
}

# Control del formulario según el tipo de columna
campo_censo <- function(id, col, valor, opciones) {
  falta <- vacio(valor)
  valor <- if (falta) "" else valor
  etiqueta <- etiqueta_censo(col)
  tipo <- if (col %in% names(TIPOS_CENSO)) TIPOS_CENSO[[col]] else if (startsWith(col, "Programa: ")) "sino" else "texto"
  control <- switch(
    tipo,
    sexo = selectInput(id, etiqueta, selected = valor,
                       choices = c("Sin dato" = "", "Mujer (M)" = "M", "Hombre (H)" = "H",
                                   setdiff(c(valor, opciones), c("", "M", "H")))),
    sino = {
      v <- if (falta) "" else normalizar_sino(valor)
      selectInput(id, etiqueta, selected = v, choices = c("Sin dato" = "", "Sí", "No", setdiff(v, c("", "Sí", "No"))))
    },
    numero = textInput(id, etiqueta, value = valor, placeholder = "Número"),
    textInput(id, etiqueta, value = valor, placeholder = "Escribe aquí")
  )
  tags$div(class = paste("campo", if (falta) "campo-faltante"), control)
}

# Formulario completo organizado por secciones
formulario_censo <- function(prefijo, base, fila = NULL) {
  columnas <- names(base)
  secciones <- SECCIONES_CENSO
  otras <- setdiff(columnas_datos(base), unlist(secciones))
  if (length(otras)) secciones[["Otros datos"]] <- otras
  tags$div(
    class = "ficha-grid",
    lapply(names(secciones), function(sec) {
      cols <- intersect(secciones[[sec]], columnas)
      if (!length(cols)) return(NULL)
      tagList(
        tags$div(class = "ficha-seccion", sec),
        lapply(cols, function(col) {
          j <- match(col, columnas)
          valor <- if (is.null(fila)) NA else base[fila, col]
          campo_censo(paste0(prefijo, j), col, valor, unique(base[[col]][!vacio(base[[col]])]))
        })
      )
    })
  )
}

leer_formulario <- function(input, prefijo, columnas) {
  valores <- lapply(seq_along(columnas), function(j) {
    v <- input[[paste0(prefijo, j)]]
    if (is.null(v)) "" else trimws(as.character(v))
  })
  setNames(valores, columnas)
}

# --------------------------------------------------------------------- UI -----

acceso_ui <- function(prefijo) {
  tagList(
    uiOutput(paste0(prefijo, "_acceso")),
    tags$script(HTML(sprintf("$(document).on('keyup', '#%s_clave', function(e) { if (e.key === 'Enter') { $('#%s_entrar').click(); } });",
                             prefijo, prefijo)))
  )
}

censo_login_ui <- function() acceso_ui("censo")
programas_login_ui <- function() acceso_ui("prog")

censo_contenido_ui <- function() {
  tagList(
    tags$div(
      class = "censo-aviso",
      icon("shield-halved"),
      if (es_hoja_google(RUTA_CENSO)) {
        " Los cambios que guardes aquí se escriben directamente en la hoja de Google del censo y se ven al instante desde cualquier computadora."
      } else {
        tagList(" Los cambios que guardes aquí se escriben directamente en tu Excel ", tags$b(ruta_legible(RUTA_CENSO)),
                ". No se crean copias ni otros archivos. Si tienes ese Excel abierto, ciérralo y vuelve a abrirlo para ver los cambios.")
      }
    ),
    uiOutput("censo_alerta_archivo"),
    cajas(
      caja("Personas en el censo", "censo_total", "users"),
      caja("Datos completos · promedio", "censo_completitud", "circle-check", "secondary"),
      caja("Personas sin CURP", "censo_sin_curp", "id-card", "info"),
      caja("Personas sin teléfono", "censo_sin_tel", "phone", "light")
    ),
    navset_card_tab(
      id = "censo_tabs",
      nav_panel(
        "Buscar y editar", value = "editar",
        layout_sidebar(
          sidebar = sidebar(
            title = "Buscar persona", width = 300, open = "desktop",
            selectizeInput("censo_buscar", "Nombre", choices = NULL,
                           options = list(placeholder = "Escribe un nombre...")),
            uiOutput("censo_resumen_persona"),
            tags$hr(),
            actionButton("censo_recargar", "Recargar Excel", icon = icon("rotate"), class = "btn-outline-primary w-100"),
            actionButton("censo_salir", "Cerrar sesión", icon = icon("right-from-bracket"), class = "btn-outline-secondary w-100 mt-2")
          ),
          uiOutput("censo_ficha"),
          fillable = FALSE
        )
      ),
      nav_panel(
        "Registros incompletos", value = "incompletos",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filtros de esta tabla", width = 280, open = "desktop",
            selectInput("censo_falta_campo", "Personas a las que les falta", choices = c("Cualquier dato" = "")),
            nota("Haz clic en una fila para abrir la ficha de esa persona.")
          ),
          DTOutput("censo_tabla_incompletos"),
          fillable = FALSE
        )
      ),
      nav_panel("Agregar persona", value = "agregar", uiOutput("censo_form_nuevo")),
      nav_panel("Base completa", value = "base", uiOutput("censo_unificar_ui"), DTOutput("censo_tabla_base"))
    )
  )
}

# ----------------------------------------------------------------- Server -----

censo_server <- function(input, output, session) {
  autorizado <- reactiveVal(FALSE)
  intentos <- reactiveVal(0)
  bloqueado_hasta <- reactiveVal(Sys.time() - 1)
  mensaje_error <- reactiveVal("")
  censo <- reactiveVal(NULL)
  version_ficha <- reactiveVal(0)
  version_nuevo <- reactiveVal(0)

  cargar <- function(seleccion = NULL) {
    base <- tryCatch(leer_censo(RUTA_CENSO), error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 8)
      NULL
    })
    censo(base)
    if (!is.null(base)) {
      updateSelectizeInput(session, "censo_buscar", choices = sort(base$nombre_completo),
                           selected = if (!is.null(seleccion) && seleccion %in% base$nombre_completo) seleccion else character(0),
                           server = TRUE)
      updateSelectInput(session, "censo_falta_campo",
                        choices = c("Cualquier dato" = "", setNames(columnas_datos(base), etiqueta_censo(columnas_datos(base)))),
                        selected = isolate(input$censo_falta_campo))
      version_ficha(version_ficha() + 1)
    }
  }

  # ---- Acceso: una misma contraseña abre el Censo y los Programas ----
  tarjeta_acceso <- function(prefijo, titulo, descripcion, pestana) {
    if (!autorizado()) {
      card(
        class = "censo-login",
        tags$div(class = "censo-candado", icon("lock")),
        tags$h3(titulo),
        tags$p(class = "text-muted", descripcion),
        passwordInput(paste0(prefijo, "_clave"), NULL, placeholder = "Contraseña", width = "100%"),
        actionButton(paste0(prefijo, "_entrar"), "Entrar", icon = icon("right-to-bracket"), class = "btn-primary btn-lg w-100"),
        tags$div(class = "censo-error", textOutput(paste0(prefijo, "_error"), inline = TRUE))
      )
    } else {
      card(
        class = "censo-login",
        tags$div(class = "censo-candado", icon("lock-open")),
        tags$h3("Sesión abierta"),
        tags$p(class = "text-muted", paste0("Esta sección está abierta en la pestaña «", pestana, "».")),
        actionButton(paste0(prefijo, "_ir"), paste0("Ir a «", pestana, "»"), icon = icon("arrow-right"), class = "btn-primary btn-lg w-100"),
        actionButton(paste0(prefijo, "_salir2"), "Cerrar sesión", icon = icon("right-from-bracket"), class = "btn-outline-secondary w-100 mt-2")
      )
    }
  }
  output$censo_acceso <- renderUI(tarjeta_acceso(
    "censo", "Censo", "Esta sección contiene datos personales. Escribe la contraseña para abrir la base del censo en una pestaña aparte.",
    "Base del Censo"))
  output$prog_acceso <- renderUI(tarjeta_acceso(
    "prog", "Programas", "Aquí se registra la asistencia de las personas del censo a los programas. Escribe la contraseña para abrir el registro en una pestaña aparte.",
    "Registro de programas"))
  output$censo_error <- renderText(mensaje_error())
  output$prog_error <- renderText(mensaje_error())

  abrir_sesion <- function(destino) {
    autorizado(TRUE)
    nav_insert("pestanas", target = "Censo", position = "after", select = FALSE,
               nav_panel("Base del Censo", icon = icon("address-book"), censo_contenido_ui()))
    nav_insert("pestanas", target = "Programas", position = "after", select = FALSE,
               nav_panel("Registro de programas", icon = icon("clipboard-check"), programas_contenido_ui()))
    cargar()
    nav_select("pestanas", destino)
  }

  intentar_entrar <- function(clave, destino) {
    if (Sys.time() < bloqueado_hasta()) {
      mensaje_error(paste0("Demasiados intentos. Espera ", ceiling(as.numeric(difftime(bloqueado_hasta(), Sys.time(), units = "secs"))), " segundos."))
      return()
    }
    clave <- if (is.null(clave)) "" else trimws(clave)
    if (identical(digest::digest(clave, algo = "sha256", serialize = FALSE), CLAVE_CENSO_SHA256)) {
      intentos(0)
      mensaje_error("")
      abrir_sesion(destino)
    } else {
      Sys.sleep(1)
      intentos(intentos() + 1)
      if (intentos() >= INTENTOS_MAXIMOS) {
        bloqueado_hasta(Sys.time() + 60)
        intentos(0)
        mensaje_error("Contraseña incorrecta. Acceso bloqueado por 1 minuto.")
      } else {
        mensaje_error(paste0("Contraseña incorrecta (intento ", intentos(), " de ", INTENTOS_MAXIMOS, ")."))
      }
    }
  }
  observeEvent(input$censo_entrar, intentar_entrar(input$censo_clave, "Base del Censo"))
  observeEvent(input$prog_entrar, intentar_entrar(input$prog_clave, "Registro de programas"))

  cerrar_sesion <- function(volver) {
    nav_remove("pestanas", target = "Base del Censo")
    nav_remove("pestanas", target = "Registro de programas")
    autorizado(FALSE)
    censo(NULL)
    nav_select("pestanas", volver)
  }
  observeEvent(input$censo_salir, cerrar_sesion("Censo"))
  observeEvent(input$censo_salir2, cerrar_sesion("Censo"))
  observeEvent(input$prog_salir, cerrar_sesion("Programas"))
  observeEvent(input$prog_salir2, cerrar_sesion("Programas"))
  observeEvent(input$censo_ir, nav_select("pestanas", "Base del Censo"))
  observeEvent(input$prog_ir, nav_select("pestanas", "Registro de programas"))

  # ---- Programas (comparte base, carga y acceso) ----
  programas_server(input, output, session, censo, cargar, autorizado)
  observeEvent(input$censo_recargar, {
    req(autorizado())
    cargar(input$censo_buscar)
    showNotification("Base del censo recargada desde el Excel.", type = "message")
  })

  # ---- Resumen ----
  output$censo_alerta_archivo <- renderUI({
    req(autorizado())
    invalidateLater(5000)
    if (archivo_abierto(RUTA_CENSO)) {
      tags$div(class = "alert alert-warning", icon("triangle-exclamation"),
               " El Excel está abierto en otro programa. Ciérralo para poder guardar cambios desde aquí.")
    }
  })
  output$censo_total <- renderText({ req(censo()); comma(nrow(censo())) })
  output$censo_completitud <- renderText({
    req(censo())
    paste0(round(100 * mean(!vacio(as.matrix(censo()[, columnas_datos(censo()), drop = FALSE])))), "%")
  })
  output$censo_sin_curp <- renderText({ req(censo()); comma(sum(vacio(censo()$CURP))) })
  output$censo_sin_tel <- renderText({ req(censo()); comma(sum(vacio(censo()$TELEFONO))) })

  # ---- Buscar y editar ----
  persona <- reactive({
    req(censo(), input$censo_buscar)
    fila <- match(input$censo_buscar, censo()$nombre_completo)
    req(!is.na(fila))
    fila
  })

  output$censo_resumen_persona <- renderUI({
    req(censo())
    if (is.null(input$censo_buscar) || !nzchar(input$censo_buscar)) {
      return(nota("Escribe parte del nombre y elige a la persona para ver su ficha."))
    }
    fila <- persona()
    faltan <- faltantes_persona(censo()[fila, ])
    pct <- round(100 * (1 - length(faltan) / length(columnas_datos(censo()))))
    tagList(
      tags$p(class = "mb-1 fw-bold", paste0("Datos completos: ", pct, "%")),
      tags$div(class = "completitud mb-2", tags$div(style = paste0("width:", pct, "%"))),
      if (length(faltan)) nota(paste0("Le faltan ", length(faltan), " datos: ", paste(etiqueta_censo(faltan), collapse = ", "), "."))
      else nota("Tiene todos sus datos completos.")
    )
  })

  output$censo_ficha <- renderUI({
    req(censo())
    version_ficha()
    if (is.null(input$censo_buscar) || !nzchar(input$censo_buscar)) {
      return(tags$div(class = "text-center text-muted p-5", icon("magnifying-glass", class = "fa-2x"),
                      tags$p(class = "mt-3", "Busca a una persona por su nombre para ver y completar su información.")))
    }
    fila <- persona()
    tagList(
      tags$div(class = "censo-encabezado",
               tags$h4(censo()$nombre_completo[fila]),
               tags$span(class = "text-muted small", paste0("Fila ", fila + 1, " del Excel"))),
      formulario_censo("censo_c_", censo(), fila),
      tags$div(
        class = "ficha-botones",
        actionButton("censo_guardar", "Guardar cambios en el Excel", icon = icon("floppy-disk"), class = "btn-primary"),
        actionButton("censo_calcular", "Calcular fecha de nacimiento y edad", icon = icon("cake-candles"), class = "btn-outline-primary"),
        actionButton("censo_descartar", "Descartar cambios", icon = icon("rotate-left"), class = "btn-outline-secondary")
      )
    )
  })

  llenar_nacimiento <- function(prefijo) {
    cols <- names(censo())
    id <- function(col) paste0(prefijo, match(col, cols))
    r <- calcular_nacimiento(input[[id("DIA")]], input[[id("MES")]], input[[id("AÑO")]])
    if (is.null(r)) {
      showNotification("Revisa el día, mes y año de nacimiento: no forman una fecha válida.", type = "warning")
      return()
    }
    if ("F. DE NAC" %in% cols) updateTextInput(session, id("F. DE NAC"), value = r$fecha)
    if ("EDAD" %in% cols) updateTextInput(session, id("EDAD"), value = as.character(r$edad))
    showNotification(paste0("Fecha ", r$fecha, " y edad ", r$edad, " años. Guarda para escribirlas en el Excel."), type = "message")
  }
  observeEvent(input$censo_calcular, llenar_nacimiento("censo_c_"))
  observeEvent(input$censo_descartar, version_ficha(version_ficha() + 1))

  observeEvent(input$censo_guardar, {
    req(autorizado(), censo())
    base <- censo()
    fila <- persona()
    # Solo se comparan los campos que se ven en la ficha; las columnas ocultas no se tocan
    visibles <- columnas_datos(base)
    nuevos <- leer_formulario(input, "censo_c_", names(base))[visibles]
    actuales <- lapply(base[fila, visibles], function(v) if (vacio(v)) "" else trimws(v))
    cambios <- nuevos[vapply(visibles, function(col) !identical(nuevos[[col]], actuales[[col]]), logical(1))]
    if (!length(cambios)) {
      showNotification("No hay cambios por guardar.", type = "message")
      return()
    }
    if (!is.null(cambios$CURP) && nzchar(cambios$CURP) && nchar(cambios$CURP) != 18) {
      showNotification("Aviso: la CURP normalmente tiene 18 caracteres.", type = "warning")
    }
    nombre_final <- if (!is.null(cambios$nombre_completo)) cambios$nombre_completo else base$nombre_completo[fila]
    resultado <- tryCatch(
      guardar_fila_censo(RUTA_CENSO, fila, base$nombre_completo[fila], cambios),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 10)
        NULL
      }
    )
    if (!is.null(resultado)) {
      cargar(nombre_final)
      showNotification(paste0("Se guardaron ", length(cambios), " dato(s) de ", nombre_final, " en el Excel."),
                       type = "message", duration = 6)
    }
  })

  # ---- Registros incompletos ----
  incompletos <- reactive({
    req(censo())
    tabla_incompletos(censo(), if (is.null(input$censo_falta_campo)) "" else input$censo_falta_campo)
  })
  output$censo_tabla_incompletos <- renderDT({
    datatable(incompletos(), rownames = FALSE, selection = "single",
              options = list(pageLength = 15, scrollX = TRUE,
                             language = list(url = "https://cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")))
  })
  observeEvent(input$censo_tabla_incompletos_rows_selected, {
    nombre <- incompletos()$`Nombre completo`[input$censo_tabla_incompletos_rows_selected]
    updateSelectizeInput(session, "censo_buscar", choices = sort(censo()$nombre_completo), selected = nombre, server = TRUE)
    nav_select("censo_tabs", "editar")
  })

  # ---- Agregar persona ----
  output$censo_form_nuevo <- renderUI({
    req(censo())
    version_nuevo()
    tagList(
      tags$div(class = "censo-aviso", icon("user-plus"),
               " Llena los datos que tengas; el nombre completo es obligatorio. La persona se agrega al final de la base del censo."),
      uiOutput("censo_resultado_alta"),
      formulario_censo("censo_n_", censo()),
      tags$div(
        class = "ficha-botones",
        actionButton("censo_agregar", "Agregar persona al Excel", icon = icon("user-plus"), class = "btn-primary"),
        actionButton("censo_calcular_nuevo", "Calcular fecha de nacimiento y edad", icon = icon("cake-candles"), class = "btn-outline-primary"),
        actionButton("censo_limpiar", "Limpiar formulario", icon = icon("eraser"), class = "btn-outline-secondary")
      )
    )
  })
  observeEvent(input$censo_calcular_nuevo, llenar_nacimiento("censo_n_"))
  observeEvent(input$censo_limpiar, version_nuevo(version_nuevo() + 1))

  resultado_alta <- reactiveVal(NULL)
  output$censo_resultado_alta <- renderUI({
    r <- resultado_alta()
    if (is.null(r)) return(NULL)
    tags$div(class = paste("alert", if (r$ok) "alert-success" else "alert-danger"),
             icon(if (r$ok) "circle-check" else "triangle-exclamation"), " ", r$texto)
  })

  observeEvent(input$censo_agregar, {
    req(autorizado(), censo())
    valores <- leer_formulario(input, "censo_n_", names(censo()))
    resultado <- tryCatch(
      agregar_persona_censo(RUTA_CENSO, valores),
      error = function(e) {
        resultado_alta(list(ok = FALSE, texto = conditionMessage(e)))
        showNotification(conditionMessage(e), type = "error", duration = 10)
        NULL
      }
    )
    if (!is.null(resultado)) {
      nombre <- attr(resultado, "nombre")
      cargar(nombre)
      resultado_alta(list(ok = TRUE, texto = paste0(nombre, " se agregó al Excel. Ahora hay ", nrow(censo()),
                                                   " personas; quedó en la fila ", nrow(censo()) + 1,
                                                   " de la hoja (la fila 1 es el encabezado).")))
      version_nuevo(version_nuevo() + 1)
      showNotification(paste0(nombre, " se agregó al Excel."), type = "message", duration = 8)
    }
  })

  # ---- Unificar respuestas de sí/no ----
  output$censo_unificar_ui <- renderUI({
    req(censo())
    n <- nrow(respuestas_sino_por_unificar(censo()))
    if (n == 0) {
      return(tags$div(class = "censo-aviso", icon("circle-check"),
                      " Todas las respuestas de sí/no están escritas como «Sí» o «No»."))
    }
    tags$div(
      class = "censo-aviso censo-encabezado",
      tags$span(icon("wand-magic-sparkles"),
                paste0(" Hay ", n, " respuestas de sí/no escritas de otra forma (por ejemplo «SÍ», «no» o «n»).")),
      actionButton("censo_unificar", "Unificar a «Sí» / «No» en el Excel", icon = icon("check-double"), class = "btn-primary btn-sm")
    )
  })
  observeEvent(input$censo_unificar, {
    req(autorizado())
    n <- tryCatch(unificar_sino_censo(RUTA_CENSO), error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 10)
      NULL
    })
    if (!is.null(n)) {
      cargar(input$censo_buscar)
      showNotification(paste0("Se unificaron ", n, " respuestas de sí/no en el Excel."), type = "message", duration = 6)
    }
  })

  # ---- Base completa ----
  output$censo_tabla_base <- renderDT({
    req(censo())
    base <- censo()[, columnas_datos(censo()), drop = FALSE]
    names(base) <- etiqueta_censo(names(base))
    datatable(base, rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE,
                             language = list(url = "https://cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")))
  })
}
