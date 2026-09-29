# ==============================================================================
# 02-Functions.R
# Funciones para la extracción y unión de las bases de Denuncias Ciudadanas
# (Superintendencia de Educación), según esquema ER_DENUNCIAS_PUBL.pdf
# ==============================================================================

# Especificación de tipos por variable ------------------------------------------

# Mapa maestro de las 42 variables del esquema de registro (sección C del PDF),
# en el orden del documento. Las variables no disponibles en un año dado
# (sección D del PDF) se completan con NA al unir.
.col_type_map <- list(
  AGNO                   = "i",  # año de creación
  DEN_ID                 = "c",  # identificador alfanumérico
  DEN_CANAL              = "i",
  DEN_ESTADO             = "i",  # 1: resuelto, 2: activo
  DEN_FEC_CREACION       = "D",  # formato AAAAMMDD
  DEN_MES_CREACION       = "i",
  DEN_TRIMESTRE_CREACION = "i",
  DEN_FEC_TERMINO_DENUNCIA = "D",
  DEN_OFICINA            = "i",
  DEN_DEPARTAMENTO       = "i",
  DEN_AMBITO             = "c",
  DEN_TEMA               = "c",
  DEN_SUBTEMA            = "c",
  DEN_ENVIO_RESP         = "i",
  # Folios como texto en la lectura: 3 registros traen valores corruptos
  # ("1,02E-152", "12130,9", ",170601434") en los archivos originales.
  # Se convierten a entero en extract_denuncias().
  FIS_ID                 = "c",
  FIS_ID_SEGUIMIENTO     = "c",
  PA_ID                  = "c",
  DEN_MRUN               = "c",  # máscara del RUN: puede contener caracteres
  DEN_AFECTADO           = "i",
  DEN_GENERO             = "i",
  DEN_SEXO               = "i",
  DEN_TIPO               = "c",
  AFEC_MRUN              = "c",
  AFEC_GENERO            = "i",
  AFEC_SEXO              = "i",
  AFEC_TIPO              = "c",
  RBD                    = "c",  # identificador: se conserva como texto
  RSIE                   = "c",
  EE_NOMBRE              = "c",
  EE_COD_REGION          = "i",
  EE_COD_PROVINCIA       = "i",
  EE_COD_COMUNA          = "c",  # códigos SUBDERE pueden tener ceros a la izquierda
  EE_NOM_COMUNA          = "c",
  EE_COD_DEPE            = "i",
  EE_COD_DEPE_AGRUP      = "i",
  AFEC_COD_ENSE2         = "i",
  DEN_TIPO_INSTITUCION   = "c",  # en años recientes viene como texto
  SOST_RUT               = "c",
  SOST_MRUN              = "c",
  SOST_NOMBRE            = "c",
  DEN_REGION             = "i",
  DEN_CIBERBULLYING      = "i"
)

# Orden canónico de columnas para la base unida
.denuncia_col_order <- names(.col_type_map)


#' Listar archivos CSV de denuncias
#'
#' Busca recursivamente en `dir` los archivos que siguen la nomenclatura
#' oficial (AAAAMMDD_DENUNCIAS_AAAA_aaaammdd_PUBL.csv) y extrae el año de
#' información desde el nombre del archivo.
list_denuncia_files <- function(dir = DIR_RAW, pattern = PATTERN_DENUNCIAS) {
  files <- list.files(dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
  tibble(
    path = files,
    agno = as.integer(str_extract(basename(files), "(?<=DENUNCIAS_)\\d{4}")),
    # Fecha de elaboración de la base (último bloque AAAAMMDD del nombre)
    fecha_elab = str_extract(basename(files), "\\d{8}(?=_PUBL)")
  ) |>
    arrange(agno)
}


#' Construir especificación de columnas para un archivo dado
#'
#' Intersecta el encabezado real del archivo con el mapa maestro de tipos,
#' ya que la disponibilidad de variables varía por año.
build_col_spec <- function(path) {
  header <- readr::read_lines(path, n_max = 1) |>
    str_remove("^\\ufeff") |>          # BOM en los archivos originales
    str_split_1(";")

  types <- .col_type_map[header]
  if (any(is.na(names(types))) || any(!header %in% names(.col_type_map))) {
    unknown <- setdiff(header, names(.col_type_map))
    stop("Variables no documentadas en ", basename(path), ": ",
         paste(unknown, collapse = ", "))
  }
  do.call(readr::cols_only, setNames(types, header))
}


#' Leer un CSV de denuncias
#'
#' Archivos delimitados por ";", UTF-8 con BOM. Los campos vacíos o con un
#' espacio (" ") se interpretan como missing.
read_denuncia <- function(path) {
  readr::read_delim(
    path,
    delim = ";",
    col_types = build_col_spec(path),
    locale = readr::locale(encoding = "UTF-8", date_format = "%Y%m%d"),
    na = c("", " "),
    trim_ws = TRUE,
    show_col_types = FALSE,
    progress = FALSE
  )
}


#' Extraer y unir todas las bases anuales
#'
#' Lee todos los CSV listados por `list_denuncia_files()` y los apila por
#' filas, ordenando las columnas según el esquema de registro. Las variables
#' no disponibles en un año quedan en NA.
extract_denuncias <- function(dir = DIR_RAW) {
  files <- list_denuncia_files(dir)
  message("Archivos encontrados: ", nrow(files))

  denuncias <- files$path |>
    map(read_denuncia) |>
    bind_rows() |>
    select(all_of(.denuncia_col_order)) |>
    arrange(AGNO, DEN_ID)

  # Conversión de folios a entero: los valores corruptos del archivo
  # original quedan en NA (3 casos en 2016-2017)
  denuncias |>
    mutate(across(
      c(FIS_ID, FIS_ID_SEGUIMIENTO, PA_ID),
      ~ suppressWarnings(readr::parse_integer(.x))
    ))
}


#' Validar conteos por año contra el esquema de registro
#'
#' Compara el número de filas por año con las cifras publicadas en la
#' sección B de ER_DENUNCIAS_PUBL.pdf. El PDF corresponde a la elaboración
#' al 31-03-2026; los archivos elaborados con posterioridad (p.ej. los de
#' 2025 y 2026, elaborados al 30-06-2026) pueden diferir legítimamente, por
#' lo que solo se advierte cuando el conteo difiere en archivos de la
#' misma elaboración del PDF.
validate_denuncias <- function(denuncias,
                               expected = N_DENUNCIAS_ESPERADAS,
                               dir = DIR_RAW,
                               pdf_elab = PDF_ELABORACION) {
  check <- denuncias |>
    count(AGNO, name = "n_extraido") |>
    full_join(expected, by = "AGNO") |>
    left_join(
      list_denuncia_files(dir) |> select(agno, fecha_elab),
      by = c("AGNO" = "agno")
    ) |>
    mutate(
      diff = n_extraido - n_esperado,
      version_pdf = fecha_elab <= pdf_elab,
      ok = diff == 0L
    )

  estrictos <- check |> filter(version_pdf)
  if (all(estrictos$ok)) {
    message("Validación OK: archivos de la elaboración ", pdf_elab,
            " coinciden con el esquema de registro.")
  } else {
    warning("Hay años cuyo conteo no coincide con el esquema de registro; ",
            "revisar tabla retornada.")
  }
  if (any(!check$ok & !check$version_pdf)) {
    message("Nota: los conteos de ",
            paste(check$AGNO[!check$ok & !check$version_pdf], collapse = ", "),
            " difieren porque el archivo es de una elaboración posterior a ",
            "la del PDF (", pdf_elab, ").")
  }
  check
}
