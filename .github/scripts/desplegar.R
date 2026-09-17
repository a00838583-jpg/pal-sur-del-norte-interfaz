# =============================================================================
# Publica la interfaz en shinyapps.io desde GitHub Actions.
# Las credenciales llegan como variables de entorno desde GitHub Secrets:
#   SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN y SHINYAPPS_SECRET
# =============================================================================

cuenta <- Sys.getenv("SHINYAPPS_ACCOUNT")
token <- Sys.getenv("SHINYAPPS_TOKEN")
secreto <- Sys.getenv("SHINYAPPS_SECRET")

faltan <- c(SHINYAPPS_ACCOUNT = cuenta, SHINYAPPS_TOKEN = token, SHINYAPPS_SECRET = secreto)
faltan <- names(faltan)[!nzchar(faltan)]
if (length(faltan)) {
  message("ERROR: faltan estos GitHub Secrets: ", paste(faltan, collapse = ", "))
  quit(status = 1)
}

rsconnect::setAccountInfo(name = cuenta, token = token, secret = secreto, server = "shinyapps.io")

# Solo se publican los archivos que usa la app
archivos <- c(
  "app.R", "funciones.R", "censo.R", "programas.R", "inegi_auto.R", "descargar_datos.R",
  "catalogo_indicadores.csv", "datos/inegi_datos.rds",
  "www/estilos.css", "www/logo_pal_sur_del_norte.png"
)
faltantes <- archivos[!file.exists(archivos)]
if (length(faltantes)) {
  message("ERROR: faltan archivos de la app: ", paste(faltantes, collapse = ", "))
  quit(status = 1)
}

# Configuración del censo, creada por el workflow a partir de GitHub Secrets
if (file.exists(".Renviron")) archivos <- c(archivos, ".Renviron")
if (file.exists("credenciales/cuenta_servicio.json")) archivos <- c(archivos, "credenciales/cuenta_servicio.json")

rsconnect::deployApp(
  appDir = ".",
  appFiles = archivos,
  appName = "pal-sur-del-norte",
  appTitle = "Pal Sur del Norte · Indicadores",
  account = cuenta,
  server = "shinyapps.io",
  forceUpdate = TRUE,
  launch.browser = FALSE,
  lint = FALSE
)

message("Publicada en: https://", cuenta, ".shinyapps.io/pal-sur-del-norte/")
