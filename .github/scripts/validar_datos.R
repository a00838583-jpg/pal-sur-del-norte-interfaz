# =============================================================================
# Revisa que la descarga de INEGI esté completa antes de guardarla y publicarla.
# Si algo falla, el script termina con error y el workflow se detiene.
#
# Uso (desde la carpeta del proyecto):
#   Rscript .github/scripts/validar_datos.R ruta/a/inegi_datos_anterior.rds
# =============================================================================

archivo_anterior <- commandArgs(trailingOnly = TRUE)[1]

falla <- function(...) {
  message("ERROR: ", ...)
  quit(status = 1)
}

suppressMessages(source("funciones.R", encoding = "UTF-8"))

if (!file.exists("datos/inegi_datos.rds")) falla("descargar_datos.R no generó datos/inegi_datos.rds")
nuevos <- readRDS("datos/inegi_datos.rds")
catalogo <- read.csv("catalogo_indicadores.csv", colClasses = "character", fileEncoding = "UTF-8")

indicadores_nuevos <- length(unique(nuevos$id))
indicadores_catalogo <- length(unique(catalogo$id))
message("Descarga: ", nrow(nuevos), " observaciones de ", indicadores_nuevos, " indicadores (catálogo: ",
        indicadores_catalogo, " indicadores)")

if (nrow(nuevos) == 0) falla("la descarga no trajo observaciones")
if (indicadores_nuevos < 0.95 * indicadores_catalogo) {
  falla("faltan indicadores: llegaron ", indicadores_nuevos, " de ", indicadores_catalogo)
}

descargado <- attr(nuevos, "descargado")
if (is.null(descargado) || difftime(Sys.time(), descargado, units = "hours") > 6) {
  falla("los datos no son de esta ejecución")
}

# Comparar con la versión anterior del repositorio
if (!is.na(archivo_anterior) && file.exists(archivo_anterior)) {
  anteriores <- readRDS(archivo_anterior)
  message("Versión anterior: ", nrow(anteriores), " observaciones de ", length(unique(anteriores$id)), " indicadores")
  if (nrow(nuevos) < 0.9 * nrow(anteriores)) {
    falla("la descarga trajo muchas menos observaciones que la versión anterior")
  }
  if (indicadores_nuevos < 0.95 * length(unique(anteriores$id))) {
    falla("la descarga trajo menos indicadores que la versión anterior")
  }
}

# Comprobar que la app puede usar los datos tal como los prepara funciones.R
preparados <- tryCatch(preparar_datos(nuevos), error = function(e) falla("preparar_datos() falló: ", conditionMessage(e)))
if (!all(MUNICIPIOS %in% preparados$lugar)) falla("faltan municipios en los datos preparados")
if (!all(EJES %in% preparados$eje)) falla("faltan ejes temáticos: ", paste(setdiff(EJES, preparados$eje), collapse = ", "))

# Comprobar que los archivos de la app no tengan errores de sintaxis
for (f in c("app.R", "funciones.R", "censo.R", "programas.R", "inegi_auto.R", "descargar_datos.R")) {
  tryCatch(parse(f, encoding = "UTF-8"), error = function(e) falla(f, " tiene un error de sintaxis: ", conditionMessage(e)))
}

message("Validación correcta: los datos se pueden publicar.")
