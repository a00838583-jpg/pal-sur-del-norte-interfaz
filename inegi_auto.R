# =============================================================================
# Actualización automática de los indicadores de INEGI
#
# - Al arrancar, la app usa los datos guardados más recientes: la pestaña
#   "INEGI_datos" de la hoja de Google (si está configurada) o datos/inegi_datos.rds.
# - Si esos datos tienen más de INEGI_DIAS_ACTUALIZACION días (30 por omisión),
#   se descargan de nuevo en segundo plano; mientras tanto la interfaz sigue
#   funcionando con los datos anteriores.
# - Si la descarga sale incompleta (por ejemplo, la API de INEGI falla), se
#   conservan los datos anteriores.
# - Con Google configurado, los datos nuevos se guardan en la hoja para que no
#   se pierdan cuando shinyapps.io reinicia el servidor.
# =============================================================================

DIAS_ACTUALIZACION_INEGI <- as.numeric(Sys.getenv("INEGI_DIAS_ACTUALIZACION", "30"))
PESTANA_INEGI <- "INEGI_datos"

# Estado compartido por todas las sesiones de la app
inegi <- new.env()
inegi$datos <- NULL
inegi$version <- 0
inegi$estado <- ""
inegi$proceso <- NULL
inegi$temporal <- NULL

usa_google_inegi <- function() nzchar(Sys.getenv("CENSO_HOJA"))

dias_desde_descarga <- function(datos) {
  fecha <- attr(datos, "descargado")
  if (is.null(fecha) || is.na(fecha)) return(Inf)
  as.numeric(difftime(Sys.time(), fecha, units = "days"))
}

# ---- Lectura y guardado en Google Sheets ----

leer_inegi_google <- function() {
  d <- leer_google(PESTANA_INEGI)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  descargado <- as.POSIXct(d$descargado[1], tz = "UTC")
  d$descargado <- NULL
  d$anio <- as.integer(d$anio)
  d$trimestre <- as.integer(d$trimestre)
  d$fecha <- as.Date(d$fecha)
  d$valor <- as.numeric(d$valor)
  attr(d, "descargado") <- descargado
  d
}

guardar_inegi_google <- function(datos) {
  d <- as.data.frame(datos)
  d$fecha <- format(d$fecha, "%Y-%m-%d")
  d$descargado <- format(attr(datos, "descargado"), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  d[] <- lapply(d, as.character)
  reemplazar_google(PESTANA_INEGI, d)
  invisible(TRUE)
}

# ---- Carga inicial ----

cargar_datos_inegi <- function(carpeta = ".") {
  archivo <- file.path(carpeta, "datos", "inegi_datos.rds")
  locales <- if (file.exists(archivo)) tryCatch(readRDS(archivo), error = function(e) NULL)
  nube <- if (usa_google_inegi()) {
    tryCatch(leer_inegi_google(), error = function(e) {
      message("No se pudieron leer los datos de INEGI en Google: ", conditionMessage(e))
      NULL
    })
  }
  candidatos <- Filter(Negate(is.null), list(nube, locales))
  if (length(candidatos) == 0) return(cargar_datos(carpeta))
  fechas <- vapply(candidatos, function(d) {
    f <- attr(d, "descargado")
    if (is.null(f) || is.na(f)) 0 else as.numeric(f)
  }, numeric(1))
  preparar_datos(candidatos[[which.max(fechas)]])
}

# Una descarga nueva solo reemplaza a la anterior si viene completa
descarga_completa <- function(nuevos, anteriores) {
  if (is.null(nuevos) || nrow(nuevos) == 0) return(FALSE)
  if (is.null(anteriores) || nrow(anteriores) == 0) return(TRUE)
  n_distinct(nuevos$id) >= 0.95 * n_distinct(anteriores$id) && nrow(nuevos) >= 0.9 * nrow(anteriores)
}

publicar_datos_inegi <- function(nuevos, carpeta = ".") {
  tryCatch(saveRDS(nuevos, file.path(carpeta, "datos", "inegi_datos.rds")), error = function(e) NULL)
  if (usa_google_inegi()) {
    tryCatch(guardar_inegi_google(nuevos), error = function(e) {
      message("No se pudieron guardar los datos de INEGI en Google: ", conditionMessage(e))
    })
  }
  inegi$datos <- preparar_datos(nuevos)
  inegi$version <- inegi$version + 1
  inegi$estado <- paste0("Datos actualizados automáticamente el ", format(attr(nuevos, "descargado"), "%d/%m/%Y"),
                         ". Se revisan cada ", DIAS_ACTUALIZACION_INEGI, " días.")
  invisible(TRUE)
}

# ---- Descarga en segundo plano ----

iniciar_actualizacion_inegi <- function(carpeta = ".", forzar = FALSE) {
  if (!is.null(inegi$proceso) && inegi$proceso$is_alive()) return(invisible(FALSE))
  if (!forzar && dias_desde_descarga(inegi$datos) < DIAS_ACTUALIZACION_INEGI) return(invisible(FALSE))
  carpeta <- normalizePath(carpeta)
  temporal <- tempfile("inegi_")
  dir.create(file.path(temporal, "datos"), recursive = TRUE)
  file.copy(file.path(carpeta, "catalogo_indicadores.csv"), temporal)
  inegi$temporal <- temporal
  inegi$estado <- "Actualizando los datos de INEGI en segundo plano..."
  inegi$proceso <- callr::r_bg(
    function(script, destino) {
      env <- new.env()
      source(script, local = env, encoding = "UTF-8")
      env$descargar_todo(destino)
      TRUE
    },
    args = list(script = file.path(carpeta, "descargar_datos.R"), destino = temporal),
    supervise = TRUE
  )
  later::later(function() revisar_actualizacion_inegi(carpeta), 20)
  invisible(TRUE)
}

revisar_actualizacion_inegi <- function(carpeta) {
  proceso <- inegi$proceso
  if (is.null(proceso)) return(invisible(NULL))
  if (proceso$is_alive()) {
    later::later(function() revisar_actualizacion_inegi(carpeta), 20)
    return(invisible(NULL))
  }
  exito <- tryCatch({ proceso$get_result(); TRUE }, error = function(e) {
    message("La descarga automática de INEGI falló: ", conditionMessage(e))
    FALSE
  })
  archivo <- file.path(inegi$temporal, "datos", "inegi_datos.rds")
  nuevos <- if (exito && file.exists(archivo)) tryCatch(readRDS(archivo), error = function(e) NULL)
  inegi$proceso <- NULL
  if (!is.null(nuevos) && descarga_completa(preparar_datos(nuevos), inegi$datos)) {
    publicar_datos_inegi(nuevos, carpeta)
  } else {
    inegi$estado <- paste0("La última actualización automática no se completó; se conservan los datos del ",
                           format(attr(inegi$datos, "descargado"), "%d/%m/%Y"), ".")
  }
  unlink(inegi$temporal, recursive = TRUE)
  invisible(NULL)
}
