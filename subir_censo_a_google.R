# =============================================================================
# Sube la base del censo (Excel) a la hoja de Google que usará la interfaz.
#
# Se corre UNA sola vez, en RStudio, con esta carpeta como directorio de trabajo:
#   setwd("~/Desktop/interfaz_inegi")
#   source("subir_censo_a_google.R")
#
# Antes:
#   1. En .Renviron llenar CENSO_HOJA (URL de la hoja) y CENSO_CREDENCIALES.
#   2. Compartir la hoja de Google como "Editor" con el correo de la cuenta de servicio.
# =============================================================================

library(readxl)
library(googlesheets4)

EXCEL_CENSO <- path.expand("~/Desktop/BaseNuevaMerge_v2.xlsx")
COLUMNAS_NUMERICAS <- c("No.", "Asistencia_Ingenium", "EDAD")

readRenviron(".Renviron")
hoja <- Sys.getenv("CENSO_HOJA")
credenciales <- Sys.getenv("CENSO_CREDENCIALES", "credenciales/cuenta_servicio.json")
if (!nzchar(hoja)) stop("Falta CENSO_HOJA en .Renviron (la URL de la hoja de Google).")
if (!file.exists(credenciales)) stop("No se encontró el archivo de credenciales: ", credenciales)
if (!file.exists(EXCEL_CENSO)) stop("No se encontró el Excel del censo en: ", EXCEL_CENSO)

message("La hoja debe estar compartida como Editor con: ", jsonlite::fromJSON(credenciales)$client_email)
gs4_auth(path = credenciales, scopes = "https://www.googleapis.com/auth/spreadsheets")

# Todo como texto (por ejemplo "08"), salvo las columnas que en el Excel son números
base <- as.data.frame(read_excel(EXCEL_CENSO, sheet = 1, col_types = "text"), check.names = FALSE)
for (col in intersect(COLUMNAS_NUMERICAS, names(base))) {
  num <- suppressWarnings(as.numeric(base[[col]]))
  if (all(is.na(base[[col]]) | !is.na(num))) base[[col]] <- num
}

# No sobrescribir si la hoja ya tiene personas (podría haber cambios hechos desde la interfaz)
if ("Censo" %in% sheet_names(hoja)) {
  existentes <- tryCatch(nrow(read_sheet(hoja, sheet = "Censo", col_types = "c")), error = function(e) 0L)
  if (existentes > 0) {
    stop("La pestaña «Censo» ya tiene ", existentes, " personas. No se sobrescribe para no perder cambios.")
  }
}

sheet_write(base, ss = hoja, sheet = "Censo")
sheet_relocate(hoja, sheet = "Censo", .before = 1)
message("Listo: se subieron ", nrow(base), " personas y ", ncol(base), " columnas a la pestaña «Censo».")
