# =============================================================================
# Sube las bases del Censo y de Beneficio (Excel) a la hoja de Google de la interfaz.
#   - BaseNuevaMerge_v2.1_Censo.xlsx     -> primera pestaña de la hoja (Censo)
#   - BaseNuevaMerge_v2.1_Beneficio.xlsx -> pestaña "Beneficio" (Programas)
#
# Se corre en RStudio, con esta carpeta como directorio de trabajo:
#   setwd("~/Desktop/interfaz_inegi")
#   source("subir_censo_a_google.R")
#
# Antes: en .Renviron llenar CENSO_HOJA (URL de la aplicación web de Apps Script)
# y CENSO_TOKEN (la clave que está en credenciales/apps_script_censo.gs).
#
# Si la hoja ya tiene datos NO se sobrescriben, salvo que antes de correrlo se
# ponga REEMPLAZAR <- TRUE (se pierden los cambios hechos desde la página).
# =============================================================================

library(readxl)

EXCEL_CENSO <- path.expand("~/Desktop/BaseNuevaMerge_v2.1_Censo.xlsx")
EXCEL_BENEFICIO <- path.expand("~/Desktop/BaseNuevaMerge_v2.1_Beneficio.xlsx")
if (!exists("REEMPLAZAR")) REEMPLAZAR <- FALSE

readRenviron(".Renviron")
source("censo.R", local = TRUE, encoding = "UTF-8")
source("programas.R", local = TRUE, encoding = "UTF-8")
if (!nzchar(CENSO_HOJA)) stop("Falta CENSO_HOJA en .Renviron (la URL de la aplicación web de Apps Script).")
for (f in c(EXCEL_CENSO, EXCEL_BENEFICIO)) if (!file.exists(f)) stop("No se encontró el Excel: ", f)

# Todo como texto (por ejemplo "08"), salvo las columnas que en el Excel son números
leer_excel <- function(ruta) {
  base <- as.data.frame(read_excel(ruta, sheet = 1, col_types = "text"), check.names = FALSE)
  for (col in intersect(COLUMNAS_NUMERICAS, names(base))) {
    num <- suppressWarnings(as.numeric(base[[col]]))
    if (all(is.na(base[[col]]) | !is.na(num))) base[[col]] <- num
  }
  base
}

censo <- leer_excel(EXCEL_CENSO)
beneficio <- limpiar_beneficio(leer_excel(EXCEL_BENEFICIO))

reemplazar_google(NULL, censo, solo_si_vacia = !REEMPLAZAR)
reemplazar_google(PESTANA_BENEFICIO, beneficio, solo_si_vacia = !REEMPLAZAR)

message("Listo: Censo con ", nrow(leer_censo(RUTA_CENSO)), " personas y Beneficio con ",
        nrow(leer_beneficio()), " personas en la hoja de Google.")
