# =============================================================================
# Descarga de indicadores INEGI (API del Banco de Indicadores)
# Lee catalogo_indicadores.csv (direcciones API de Reto.docx), consulta cada una
# y guarda una tabla larga y ordenada en datos/inegi_datos.rds
#
# Uso: abrir este archivo en RStudio y dar "Source", o desde la consola:
#   source("descargar_datos.R")
# =============================================================================

library(httr)
library(jsonlite)

token <- "7e12efc9-5ea8-749b-7a99-6efa81fd5506"

# Consulta una dirección API y regresa sus observaciones como data frame
consultar_api <- function(url, intentos = 3) {
  peticion <- paste0(url, token, "?type=json")

  for (i in seq_len(intentos)) {
    respuesta <- tryCatch(GET(peticion, timeout(60)), error = function(e) NULL)
    if (!is.null(respuesta) && status_code(respuesta) == 200) break
    Sys.sleep(1)
  }
  if (is.null(respuesta) || status_code(respuesta) != 200) {
    warning("Error en la consulta: ", url)
    return(NULL)
  }

  res_json <- content(respuesta, as = "parsed", type = "application/json")
  if (is.null(res_json$Series)) {
    warning("Sin datos: ", url)
    return(NULL)
  }

  periodo <- c()
  valor <- c()
  for (dato in res_json$Series[[1]]$OBSERVATIONS) {
    periodo <- c(periodo, dato$TIME_PERIOD)
    # Si el periodo no tiene dato (OBS_VALUE es null) guardamos NA
    valor <- c(valor, if (is.null(dato$OBS_VALUE)) NA else dato$OBS_VALUE)
  }

  data.frame(url = url, periodo = periodo, valor = as.numeric(valor))
}

# Descarga todo el catálogo y lo une con la descripción de cada indicador
descargar_todo <- function(carpeta = ".") {
  catalogo <- read.csv(file.path(carpeta, "catalogo_indicadores.csv"),
                       colClasses = "character", fileEncoding = "UTF-8")

  resultados <- vector("list", nrow(catalogo))
  for (i in seq_len(nrow(catalogo))) {
    if (i %% 25 == 0 || i == nrow(catalogo)) message("Consultando ", i, " de ", nrow(catalogo))
    resultados[[i]] <- consultar_api(catalogo$url[i])
  }
  obs <- do.call(rbind, resultados)

  datos <- merge(catalogo, obs, by = "url")
  datos <- subset(datos, !is.na(valor))

  # Periodos anuales ("2020") y trimestrales ("2026/02")
  datos$anio <- as.integer(substr(datos$periodo, 1, 4))
  trimestral <- grepl("/", datos$periodo)
  datos$trimestre <- ifelse(trimestral, as.integer(sub(".*/", "", datos$periodo)), NA)
  datos$fecha <- as.Date(ifelse(trimestral,
                                sprintf("%d-%02d-01", datos$anio, (datos$trimestre - 1) * 3 + 1),
                                sprintf("%d-01-01", datos$anio)))

  datos <- datos[order(datos$eje, datos$id, datos$cve_area, datos$fecha),
                 c("eje", "subtema", "categoria", "sexo", "grupo_edad", "id", "indicador",
                   "unidad", "fuente", "cve_area", "lugar", "periodo", "anio", "trimestre", "fecha", "valor")]
  rownames(datos) <- NULL
  attr(datos, "descargado") <- Sys.time()

  dir.create(file.path(carpeta, "datos"), showWarnings = FALSE)
  saveRDS(datos, file.path(carpeta, "datos", "inegi_datos.rds"))
  write.csv(datos, file.path(carpeta, "datos", "inegi_datos.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  message("Listo: ", nrow(datos), " observaciones de ", length(unique(datos$id)), " indicadores")
  invisible(datos)
}

# Al correr el archivo directamente (Source o Rscript) se hace la descarga
if (sys.nframe() == 0 || identical(environment(), globalenv())) {
  datos <- descargar_todo()
}
