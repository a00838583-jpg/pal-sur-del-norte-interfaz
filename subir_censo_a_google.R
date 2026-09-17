# =============================================================================
# Sube la base del censo (Excel) a la hoja de Google que usará la interfaz.
#
# Se corre UNA sola vez, en RStudio, con esta carpeta como directorio de trabajo:
#   setwd("~/Desktop/interfaz_inegi")
#   source("subir_censo_a_google.R")
#
# Antes: en .Renviron llenar CENSO_HOJA (URL de la aplicación web de Apps Script)
# y CENSO_TOKEN (la clave que está en credenciales/apps_script_censo.gs).
# =============================================================================

library(readxl)

EXCEL_CENSO <- path.expand("~/Desktop/BaseNuevaMerge_v2.xlsx")

readRenviron(".Renviron")
source("censo.R", local = TRUE, encoding = "UTF-8")
if (!nzchar(CENSO_HOJA)) stop("Falta CENSO_HOJA en .Renviron (la URL de la aplicación web de Apps Script).")
if (!file.exists(EXCEL_CENSO)) stop("No se encontró el Excel del censo en: ", EXCEL_CENSO)

# Todo como texto (por ejemplo "08"), salvo las columnas que en el Excel son números
base <- as.data.frame(read_excel(EXCEL_CENSO, sheet = 1, col_types = "text"), check.names = FALSE)
for (col in intersect(COLUMNAS_NUMERICAS, names(base))) {
  num <- suppressWarnings(as.numeric(base[[col]]))
  if (all(is.na(base[[col]]) | !is.na(num))) base[[col]] <- num
}

# No sobrescribe si la hoja ya tiene personas (podría haber cambios hechos desde la interfaz)
reemplazar_google(NULL, base, solo_si_vacia = TRUE)
subidas <- leer_censo(CENSO_HOJA)
message("Listo: la hoja de Google tiene ", nrow(subidas), " personas y ", ncol(subidas), " columnas.")
