# =============================================================================
# Funciones de apoyo de la interfaz: carga de datos, formatos, indicadores
# clave (KPIs) y gráficas por eje temático.
# Solo usan dplyr, ggplot2 y scales, así que se pueden probar sin Shiny.
# =============================================================================

library(dplyr)
library(ggplot2)
library(scales)

MUNICIPIOS <- c("Aramberri", "Doctor Arroyo", "General Zaragoza", "Mier y Noriega")
LUGARES <- MUNICIPIOS
EJES <- c("Población", "Empleo y ocupación", "Natalidad y mortalidad", "Migración",
          "Salud", "Hogares y vivienda", "Educación")

# Paleta de Pal Sur del Norte (tomada del logo)
MORADO <- "#321A89"
LAVANDA <- "#8986EB"
TURQUESA <- "#00EDC9"
TURQUESA_OSCURO <- "#00B89C"  # para líneas y texto sobre fondo blanco
AMARILLO <- "#F4A700"
GRIS <- "#55546B"

COLORES <- c("Aramberri" = MORADO, "Doctor Arroyo" = TURQUESA_OSCURO, "General Zaragoza" = LAVANDA,
             "Mier y Noriega" = AMARILLO,
             "Hombres" = MORADO, "Mujeres" = TURQUESA_OSCURO, "Total" = GRIS)
PALETA <- c(MORADO, TURQUESA_OSCURO, LAVANDA, AMARILLO, "#D9577E", "#1E7F74", "#5B4CC4", "#B57C00", GRIS)

ORDEN_EDAD <- c(paste(seq(0, 95, 5), "a", seq(4, 99, 5)), "100 y más")
ORDEN_FECUNDIDAD <- paste0(seq(15, 45, 5), "-", seq(19, 49, 5))

CAUSAS <- c("Trabajo", "Familiar", "Educativa", "Inseguridad o violencia", "Otra causa")
INSTITUCIONES <- c("IMSS", "ISSSTE", "PEMEX, SDN o SM", "Seguro Popular", "IMSS-BIENESTAR",
                   "Seguro privado", "Servicios médicos privados", "Otra institución")
DISCAPACIDADES <- c("Ver", "Caminar o moverse", "Escuchar", "Hablar o comunicarse", "Mental")
MOTIVOS_PNEA <- c("Se dedica a los quehaceres del hogar", "Limitación física o mental para trabajar")
EDUCACION <- c("Grado promedio de escolaridad", "Tasa de alfabetización (15 a 24 años)",
               "Asistencia escolar (5 años y más)", "% sin escolaridad", "% sin instrucción",
               "% con escolaridad básica", "% con media superior", "% con instrucción superior",
               "Personas de 6 a 14 años que no saben leer y escribir")
NIVELES_EDUCATIVOS <- c("% sin escolaridad", "% con escolaridad básica", "% con media superior",
                        "% con instrucción superior")

# ---------------------------------------------------------------- Carga -----

cargar_datos <- function(carpeta = ".") {
  archivo <- file.path(carpeta, "datos", "inegi_datos.rds")
  if (file.exists(archivo)) {
    datos <- readRDS(archivo)
  } else {
    message("No existe ", archivo, "; descargando desde la API de INEGI...")
    env <- new.env()
    source(file.path(carpeta, "descargar_datos.R"), local = env, encoding = "UTF-8")
    datos <- env$descargar_todo(carpeta)
  }
  preparar_datos(datos)
}

preparar_datos <- function(datos) {
  descargado <- attr(datos, "descargado")
  # La interfaz trabaja solo con los cuatro municipios
  datos <- datos[datos$lugar %in% MUNICIPIOS, ]
  datos$periodo <- ifelse(is.na(datos$trimestre), as.character(datos$anio),
                          paste0(datos$anio, "-T", datos$trimestre))
  # INEGI publica el indicador 6207049067 como número de personas en 2020,
  # aunque la API lo marca como "Porcentaje"
  personas <- datos$id == "6207049067" & datos$anio >= 2020
  datos$unidad[personas] <- "Personas"
  datos$categoria[personas] <- "Personas de 6 a 14 años que no saben leer y escribir"
  datos <- datos[!(datos$id == "6207049067" & datos$anio < 2020), ]
  attr(datos, "descargado") <- descargado
  datos
}

# -------------------------------------------------------------- Formatos -----

es_porcentaje <- function(unidad) grepl("Porcentaje|Tasa", unidad)

formato_valor <- function(x, unidad) {
  unidad <- rep_len(as.character(unidad), length(x))
  out <- number(x, accuracy = 1, big.mark = ",")
  pct <- es_porcentaje(unidad)
  prom <- grepl("Promedio|escolaridad", unidad)
  out[pct] <- paste0(number(x[pct], accuracy = 0.1, big.mark = ","), "%")
  out[prom] <- number(x[prom], accuracy = 0.01)
  out
}

formato_eje <- function(v, unidad) {
  out <- number(v, big.mark = ",")
  if (es_porcentaje(unidad)) out <- paste0(out, "%")
  out[is.na(v)] <- NA
  out
}

formato_cambio <- function(nuevo, viejo, puntos = FALSE) {
  if (length(nuevo) == 0 || length(viejo) == 0 || is.na(nuevo) || is.na(viejo) || viejo == 0) return("Sin dato")
  d <- if (puntos) nuevo - viejo else 100 * (nuevo / viejo - 1)
  paste0(if (d >= 0) "+" else "", number(d, accuracy = 0.1), if (puntos) " pp" else "%")
}

con_signo <- function(x) paste0(ifelse(x >= 0, "+", ""), comma(x))

# Promedio de los municipios con el año entre paréntesis
promedio_txt <- function(x, unidad, anio) {
  if (length(x) == 0 || all(is.na(x))) return("Sin dato")
  paste0(formato_valor(mean(x, na.rm = TRUE), unidad), " (", anio, ")")
}

rango_txt <- function(a) {
  a <- sort(unique(a))
  if (length(a) > 6) paste0(a[1], " a ", tail(a, 1)) else paste(a, collapse = ", ")
}

orden_lugares <- function(l) {
  l <- unique(as.character(l))
  c(intersect(LUGARES, l), setdiff(l, LUGARES))
}

colores_para <- function(niveles) {
  niveles <- as.character(niveles)
  if (all(niveles %in% names(COLORES))) return(COLORES[niveles])
  setNames(rep_len(PALETA, length(niveles)), niveles)
}

cortes_anio <- function(anios) {
  a <- sort(unique(anios))
  if (length(a) <= 12) a else unique(round(pretty(a, n = 8)))
}

tema_inegi <- function() {
  theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold", color = MORADO))
}

# Tabla con nombres en español y sin columnas vacías
tabla_datos <- function(d) {
  col <- function(v) if (v %in% names(d)) as.character(d[[v]]) else rep(NA_character_, nrow(d))
  t <- data.frame(Municipio = col("lugar"), Indicador = col("indicador"), `Categoría` = col("categoria"),
                  `Grupo de edad` = col("grupo_edad"), Periodo = col("periodo"),
                  Valor = if ("valor" %in% names(d)) round(d$valor, 2) else NA,
                  Unidad = col("unidad"), id = col("id"), check.names = FALSE)
  t[, colSums(!is.na(t)) > 0, drop = FALSE]
}

resumen_ejes <- function(datos) {
  datos |>
    group_by(eje) |>
    summarise(Indicadores = n_distinct(id),
              `Consultas API` = n_distinct(paste(id, cve_area)),
              `Años` = rango_txt(anio),
              .groups = "drop") |>
    arrange(match(eje, EJES)) |>
    rename(`Eje temático` = eje)
}

# ------------------------------------------------------ Gráfica genérica -----
# x: "anio", "lugar", "categoria" o "grupo_edad"
# tipo: "lineas", "barras" (agrupadas), "apiladas" o "proporcion" (100 %)
# color: columna que define las series; si es NULL se arma con lo que varíe

graf_generica <- function(df, x = "anio", tipo = "lineas", color = NULL, facet = NULL,
                          eje_x = NULL, eje_y = NULL, horizontal = FALSE, referencia = NULL) {
  df <- as.data.frame(df)
  if (nrow(df) == 0) return(NULL)

  if (is.null(color)) {
    dims <- setdiff(c("lugar", "categoria", "grupo_edad"), c(x, facet))
    dims <- dims[dims %in% names(df)]
    varia <- dims[vapply(dims, function(v) length(unique(df[[v]])) > 1, logical(1))]
    df$serie <- if (length(varia)) do.call(paste, c(lapply(varia, function(v) df[[v]]), sep = " · ")) else df$lugar
  } else {
    df$serie <- as.character(df[[color]])
  }

  if (x == "anio" && "trimestre" %in% names(df) && any(!is.na(df$trimestre))) x <- "fecha"
  if (x == "anio" && tipo != "lineas") df$anio <- factor(df$anio)
  if (x == "grupo_edad") {
    orden <- c(ORDEN_EDAD, ORDEN_FECUNDIDAD)
    df$grupo_edad <- factor(df$grupo_edad, levels = orden[orden %in% df$grupo_edad])
  }
  if (x == "lugar") {
    niv <- orden_lugares(df$lugar)
    df$lugar <- factor(df$lugar, levels = if (horizontal) rev(niv) else niv)
  }
  if (x == "categoria") {
    niv <- unique(as.character(df$categoria))
    df$categoria <- factor(df$categoria, levels = if (horizontal) rev(niv) else niv)
  }

  niveles <- unique(df$serie)
  if (all(niveles %in% names(COLORES))) niveles <- intersect(names(COLORES), niveles)
  df$serie <- factor(df$serie, levels = niveles)
  cols <- colores_para(niveles)

  if (!"tip" %in% names(df)) {
    df$tip <- paste0("<b>", df$serie, "</b><br>", df$lugar, " · ", df$periodo, "<br>",
                     formato_valor(df$valor, df$unidad))
  }

  if (tipo == "lineas") {
    p <- ggplot(df, aes(x = .data[[x]], y = valor, color = serie, group = serie, text = tip)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2.2) +
      scale_color_manual(values = cols, name = NULL)
  } else {
    posicion <- switch(tipo,
                       barras = position_dodge2(preserve = "single", padding = 0.1),
                       apiladas = "stack",
                       proporcion = "fill")
    p <- ggplot(df, aes(x = .data[[x]], y = valor, fill = serie, text = tip)) +
      geom_col(position = posicion, width = 0.8) +
      scale_fill_manual(values = cols, name = NULL)
  }

  unidad <- if ("unidad" %in% names(df)) df$unidad[1] else ""
  if (tipo == "proporcion") {
    p <- p + scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.02)))
  } else {
    p <- p + scale_y_continuous(labels = function(v) formato_eje(v, unidad),
                                expand = expansion(mult = c(if (tipo == "lineas") 0.05 else 0, 0.08)))
  }
  if (x == "anio" && tipo == "lineas") p <- p + scale_x_continuous(breaks = cortes_anio(df$anio))
  if (!is.null(referencia)) {
    p <- p + geom_hline(yintercept = referencia, linetype = "dashed", color = "#4D4D4D", linewidth = 0.7)
  }
  if (!is.null(facet)) {
    p <- p + facet_wrap(vars(.data[[facet]]), scales = if (tipo == "proporcion") "fixed" else "free_y")
  }
  if (horizontal) p <- p + coord_flip()
  p + labs(x = eje_x, y = eje_y) + tema_inegi()
}

# --------------------------------------------------------- Población -----

datos_pob_total <- function(datos, lugares, sexos, anios) {
  datos |>
    filter(subtema == "Población total", lugar %in% lugares, categoria %in% sexos, anio %in% anios)
}

kpi_poblacion <- function(datos, lugares = MUNICIPIOS) {
  tot <- datos |> filter(subtema == "Población total", lugar %in% lugares, categoria == "Total")
  suma <- function(a) sum(tot$valor[tot$anio == a])
  edades <- datos |> filter(subtema == "Grupos de edad", sexo == "Total", lugar %in% lugares)
  mayores <- sum(edades$valor[edades$grupo_edad %in% ORDEN_EDAD[14:21]])
  list(total = comma(suma(2020)),
       cambio = formato_cambio(suma(2020), suma(2010)),
       mayores = paste0(number(100 * mayores / sum(edades$valor), accuracy = 0.1), "%"))
}

datos_piramide <- function(datos, lugar_sel, porcentaje = TRUE) {
  lugares <- if (lugar_sel == "todos") MUNICIPIOS else lugar_sel
  d <- datos |>
    filter(subtema == "Grupos de edad", sexo %in% c("Hombres", "Mujeres"), lugar %in% lugares) |>
    group_by(grupo_edad, sexo) |>
    summarise(personas = sum(valor), .groups = "drop")
  total <- sum(d$personas)
  d |>
    mutate(grupo_edad = factor(grupo_edad, levels = ORDEN_EDAD),
           valor = if (porcentaje) 100 * personas / total else personas,
           lado = if_else(sexo == "Hombres", -valor, valor),
           tip = paste0("<b>", sexo, " · ", grupo_edad, " años</b><br>", comma(personas), " personas<br>",
                        number(100 * personas / total, accuracy = 0.01), "% del total"))
}

graf_piramide <- function(d, porcentaje = TRUE) {
  limite <- max(abs(d$lado)) * 1.05
  etiqueta <- function(v) if (porcentaje) paste0(number(abs(v), accuracy = 0.1), "%") else comma(abs(v))
  ggplot(d, aes(x = grupo_edad, y = lado, fill = sexo, text = tip)) +
    geom_col(width = 0.9) +
    geom_hline(yintercept = 0, color = "white") +
    coord_flip() +
    scale_y_continuous(limits = c(-limite, limite), labels = etiqueta) +
    scale_fill_manual(values = COLORES[c("Hombres", "Mujeres")], name = NULL) +
    labs(x = "Grupo de edad (años)", y = if (porcentaje) "Porcentaje de la población" else "Personas") +
    tema_inegi()
}

datos_grupos_grandes <- function(datos, lugares) {
  datos |>
    filter(subtema == "Grupos de edad", sexo == "Total", lugar %in% lugares) |>
    mutate(categoria = case_when(grupo_edad %in% ORDEN_EDAD[1:3] ~ "0 a 14 años",
                                 grupo_edad %in% ORDEN_EDAD[4:13] ~ "15 a 64 años",
                                 TRUE ~ "65 años y más")) |>
    group_by(lugar, categoria, unidad, periodo) |>
    summarise(valor = sum(valor), .groups = "drop") |>
    group_by(lugar) |>
    mutate(pct = 100 * valor / sum(valor),
           tip = paste0("<b>", lugar, " · ", categoria, "</b><br>", comma(valor), " personas (",
                        number(pct, accuracy = 0.1), "%)")) |>
    ungroup()
}

# ------------------------------------------------ Empleo y ocupación -----

kpi_empleo <- function(datos) {
  e <- datos |> filter(eje == "Empleo y ocupación")
  if (nrow(e) == 0) return(list(activa = "Sin dato", mujeres = "Sin dato", hogar = "Sin dato"))
  a <- max(e$anio)
  v <- function(cat) e$valor[e$categoria == cat & e$anio == a]
  list(activa = promedio_txt(v("Económicamente activa"), "Porcentaje", a),
       mujeres = promedio_txt(v("Mujeres"), "Porcentaje", a),
       hogar = promedio_txt(v("Se dedica a los quehaceres del hogar"), "Porcentaje", a))
}

# -------------------------------------------- Natalidad y mortalidad -----

# Nacimientos o defunciones, como número o como índice (primer año = 100)
datos_vitales <- function(datos, subtema_sel, lugares, anios, escala = "num", palabra = "casos") {
  d <- datos |>
    filter(subtema == subtema_sel, lugar %in% lugares, anio >= anios[1], anio <= anios[2]) |>
    arrange(lugar, anio)
  if (escala == "indice") {
    d <- d |>
      group_by(lugar) |>
      mutate(tip = paste0("<b>", lugar, "</b><br>", periodo, ": ", comma(valor), " ", palabra, "<br>Índice: ",
                          number(100 * valor / first(valor), accuracy = 0.1), " (", first(anio), " = 100)"),
             valor = 100 * valor / first(valor), unidad = "Índice") |>
      ungroup()
  } else {
    d$tip <- paste0("<b>", d$lugar, "</b><br>", d$periodo, ": ", comma(d$valor), " ", palabra)
  }
  d
}

datos_crecimiento_natural <- function(datos, lugares, anios) {
  datos |>
    filter(subtema %in% c("Nacimientos registrados", "Defunciones registradas"),
           lugar %in% lugares, anio >= anios[1], anio <= anios[2]) |>
    group_by(lugar, anio) |>
    summarise(nacimientos = sum(valor[subtema == "Nacimientos registrados"]),
              defunciones = sum(valor[subtema == "Defunciones registradas"]),
              fuentes = n_distinct(subtema), .groups = "drop") |>
    filter(fuentes == 2) |>
    mutate(valor = nacimientos - defunciones, periodo = as.character(anio), unidad = "Personas",
           categoria = "Crecimiento natural",
           tip = paste0("<b>", lugar, "</b><br>", periodo, "<br>Nacimientos: ", comma(nacimientos, accuracy = 1),
                        "<br>Defunciones: ", comma(defunciones, accuracy = 1), "<br>Crecimiento natural: ", con_signo(valor)))
}

kpi_natalidad <- function(datos) {
  nac <- datos |> filter(subtema == "Nacimientos registrados")
  def <- datos |> filter(subtema == "Defunciones registradas")
  hijos <- datos |> filter(subtema == "Promedio de hijos nacidos vivos")
  sin <- list(nacimientos = "Sin dato", defunciones = "Sin dato", crecimiento = "Sin dato", hijos = "Sin dato")
  if (nrow(nac) == 0 || nrow(def) == 0) return(sin)
  a_n <- max(nac$anio); a_d <- max(def$anio); a_c <- max(intersect(nac$anio, def$anio))
  a_h <- max(hijos$anio)
  list(nacimientos = paste0(comma(sum(nac$valor[nac$anio == a_n])), " (", a_n, ")"),
       defunciones = paste0(comma(sum(def$valor[def$anio == a_d])), " (", a_d, ")"),
       crecimiento = paste0(con_signo(sum(nac$valor[nac$anio == a_c]) - sum(def$valor[def$anio == a_c])), " (", a_c, ")"),
       hijos = paste0(number(mean(hijos$valor[hijos$anio == a_h]), accuracy = 0.01), " (", a_h, ")"))
}

# --------------------------------------------------------- Migración -----

datos_saldo <- function(datos, lugares) {
  datos |>
    filter(subtema == "Flujos migratorios", lugar %in% lugares) |>
    group_by(lugar) |>
    summarise(inmigrantes = sum(valor[categoria == "Inmigrantes"]),
              emigrantes = sum(valor[categoria == "Emigrantes"]), .groups = "drop") |>
    mutate(saldo = inmigrantes - emigrantes,
           resultado = if_else(saldo >= 0, "Gana población", "Pierde población"),
           lugar = factor(lugar, levels = orden_lugares(lugar)))
}

graf_saldo <- function(d) {
  ggplot(d, aes(x = lugar, y = saldo, fill = resultado,
                text = paste0("<b>", lugar, "</b><br>Inmigrantes: ", comma(inmigrantes),
                              "<br>Emigrantes: ", comma(emigrantes), "<br>Saldo: ", comma(saldo)))) +
    geom_col(width = 0.6) +
    geom_hline(yintercept = 0, color = "#4D4D4D") +
    scale_fill_manual(values = c("Gana población" = TURQUESA_OSCURO, "Pierde población" = "#D9577E"), name = NULL) +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = "Inmigrantes menos emigrantes") +
    tema_inegi()
}

graf_calor <- function(d) {
  d$categoria <- factor(d$categoria, levels = intersect(CAUSAS, d$categoria))
  d$lugar <- factor(d$lugar, levels = rev(orden_lugares(d$lugar)))
  ggplot(d, aes(x = categoria, y = lugar, fill = valor,
                text = paste0("<b>", lugar, "</b><br>", categoria, ": ", number(valor, accuracy = 0.1), "%"))) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = paste0(number(valor, accuracy = 0.1), "%")),
              color = ifelse(d$valor > max(d$valor) / 2, "white", "#1F1F1F")) +
    scale_fill_gradient(low = "#ECEBFB", high = MORADO, name = "%") +
    labs(x = NULL, y = NULL) +
    tema_inegi() +
    theme(panel.grid = element_blank())
}

kpi_migracion <- function(datos, lugares = MUNICIPIOS) {
  s <- datos_saldo(datos, lugares)
  causas <- datos |>
    filter(subtema == "Causas de la migración", lugar %in% lugares) |>
    group_by(categoria) |>
    summarise(valor = mean(valor), .groups = "drop") |>
    arrange(desc(valor))
  list(inmigrantes = comma(sum(s$inmigrantes)),
       emigrantes = comma(sum(s$emigrantes)),
       saldo = con_signo(sum(s$saldo)),
       causa = if (nrow(causas)) paste0(causas$categoria[1], " (", number(causas$valor[1], accuracy = 0.1), "%)") else "Sin dato")
}

# ------------------------------------------------------------- Salud -----

graf_cambio_salud <- function(d) {
  w <- d |>
    group_by(lugar) |>
    summarise(v2015 = valor[anio == 2015][1], v2020 = valor[anio == 2020][1], .groups = "drop") |>
    filter(!is.na(v2015), !is.na(v2020))
  if (nrow(w) == 0) return(NULL)
  w$lugar <- factor(w$lugar, levels = rev(orden_lugares(w$lugar)))
  largo <- rbind(data.frame(lugar = w$lugar, anio = "2015", valor = w$v2015),
                 data.frame(lugar = w$lugar, anio = "2020", valor = w$v2020))
  largo$tip <- paste0("<b>", largo$lugar, "</b><br>", largo$anio, ": ", number(largo$valor, accuracy = 0.01), "%")
  w$tip <- paste0("<b>", w$lugar, "</b><br>Cambio: ",
                  mapply(formato_cambio, w$v2020, w$v2015, MoreArgs = list(puntos = TRUE)))
  ggplot() +
    geom_segment(data = w, aes(x = lugar, xend = lugar, y = v2015, yend = v2020, text = tip),
                 color = "#BFBFBF", linewidth = 2) +
    geom_point(data = largo, aes(x = lugar, y = valor, color = anio, text = tip), size = 4) +
    scale_color_manual(values = c("2015" = LAVANDA, "2020" = MORADO), name = NULL) +
    scale_y_continuous(labels = function(v) formato_eje(v, "Porcentaje")) +
    coord_flip() +
    labs(x = NULL, y = "Porcentaje de la población") +
    tema_inegi()
}

# Personas con alguna limitación, como número o como % de la población total 2020
datos_discapacidad <- function(datos, lugares, tipos, medida = "personas") {
  d <- datos |>
    filter(subtema == "Discapacidad", lugar %in% lugares, categoria %in% tipos) |>
    arrange(match(categoria, DISCAPACIDADES), match(lugar, MUNICIPIOS))
  if (medida == "pct") {
    poblacion <- datos |>
      filter(subtema == "Población total", categoria == "Total", anio == 2020) |>
      select(lugar, poblacion = valor)
    d <- d |>
      left_join(poblacion, by = "lugar") |>
      mutate(personas = valor, valor = 100 * personas / poblacion, unidad = "Porcentaje",
             tip = paste0("<b>", lugar, " · ", categoria, "</b><br>", comma(personas), " personas<br>",
                          number(valor, accuracy = 0.01), "% de la población (2020)"))
  } else {
    d$tip <- paste0("<b>", d$lugar, " · ", d$categoria, "</b><br>", comma(d$valor), " personas (2020)")
  }
  d
}

kpi_salud <- function(datos) {
  s <- datos |> filter(subtema == "Afiliación a servicios de salud")
  afiliada <- function(a) mean(s$valor[s$categoria == "Afiliada a algún servicio" & s$anio == a])
  inst <- s |>
    filter(anio == 2020, categoria != "Afiliada a algún servicio") |>
    group_by(categoria) |>
    summarise(valor = mean(valor), .groups = "drop") |>
    arrange(desc(valor))
  disc <- datos |>
    filter(subtema == "Discapacidad") |>
    group_by(categoria) |>
    summarise(valor = sum(valor), .groups = "drop") |>
    arrange(desc(valor))
  cob <- afiliada(2020)
  list(cobertura = if (is.nan(cob)) "Sin dato" else paste0(number(cob, accuracy = 0.1), "%"),
       principal = if (nrow(inst)) paste0(inst$categoria[1], " (", number(inst$valor[1], accuracy = 0.1), "%)") else "Sin dato",
       cambio = formato_cambio(afiliada(2020), afiliada(2015), puntos = TRUE),
       discapacidad = if (nrow(disc)) paste0(disc$categoria[1], " (", comma(disc$valor[1]), " personas)") else "Sin dato")
}

# ------------------------------------------------ Hogares y vivienda -----

datos_jefatura_femenina <- function(datos, lugares, anios) {
  datos |>
    filter(subtema == "Jefatura del hogar", lugar %in% lugares, anio %in% anios) |>
    group_by(lugar, anio, periodo) |>
    summarise(femenina = sum(valor[categoria == "Jefatura femenina"]), total = sum(valor), .groups = "drop") |>
    mutate(valor = 100 * femenina / total, unidad = "Porcentaje", categoria = "% jefatura femenina",
           tip = paste0("<b>", lugar, "</b><br>", periodo, ": ", number(valor, accuracy = 0.1), "%<br>",
                        comma(femenina), " de ", comma(total), " hogares"))
}

kpi_hogares <- function(datos, lugares = MUNICIPIOS) {
  h <- datos |> filter(subtema == "Jefatura del hogar", lugar %in% lugares)
  if (nrow(h) == 0) return(list(total = "Sin dato", femenina = "Sin dato", crecimiento = "Sin dato"))
  ult <- max(h$anio); ini <- min(h$anio)
  fem <- function(a) sum(h$valor[h$anio == a & h$categoria == "Jefatura femenina"])
  tot <- function(a) sum(h$valor[h$anio == a])
  list(total = paste0(comma(tot(ult)), " (", ult, ")"),
       femenina = paste0(number(100 * fem(ult) / tot(ult), accuracy = 0.1), "% (", ult, ")"),
       crecimiento = paste0(formato_cambio(fem(ult), fem(ini)), " (", ini, "–", ult, ")"))
}

# --------------------------------------------------------- Educación -----

kpi_educacion <- function(datos) {
  e <- datos |> filter(eje == "Educación")
  ultimo <- function(cat) {
    d <- e[e$categoria == cat, ]
    if (nrow(d) == 0) return("Sin dato")
    a <- max(d$anio)
    promedio_txt(d$valor[d$anio == a], d$unidad[1], a)
  }
  list(grado = ultimo("Grado promedio de escolaridad"),
       sin_escolaridad = ultimo("% sin escolaridad"),
       superior = ultimo("% con instrucción superior"))
}
