#' @title
#' MotionWatch 8 - Analyse des données de sieste
#'
#' @description
#' Cette fonction extrait les données de sieste de la MotionWatch 8.
#'
#' @author
#' European Sleep Center (Paris, France)
#'
#' Maxime Chauvineau - mchauvineau@europeansleepcenter.fr
#'
#' @returns
#' Tableau de données
#'
#' @param file Chemin du fichier .mtn
#'
#' @examples
#' #extract_nap()
#'
#' @export

extract_nap <- function(file) {

  # Extraire tous les nœuds <ann> qui contiennent un ID pour différencier les nuits
  ann_nodes <- xml2::xml_find_all(
    xml2::read_xml(file),
    "//ann[@type='nap']"
  )

  if (length(ann_nodes) > 0) {

    # Définir les variables d'intérêt
    variables_of_interest <- c(
      "AnalStart", "AnalEnd"
    )

    # Extraction des propriétés pour chaque nuit
    nap_list <- lapply(ann_nodes, function(ann_node) {
      # Extraire l'attribut id et le timestamp `time`
      id <- xml2::xml_attr(ann_node, "id")
      time <- as.POSIXct(
        xml2::xml_attr(
          xml2::xml_find_first(
            ann_node, "./.."), "time"),
        format = "%Y-%m-%d %H:%M:%S"
      )

      # Trouver tous les <property> sous chaque <ann>
      properties <- xml2::xml_find_all(ann_node, ".//property")

      # Extraire les noms et les valeurs
      names <- xml2::xml_text(xml2::xml_find_all(properties, ".//name"))
      values <- xml2::xml_text(xml2::xml_find_all(properties, ".//content"))

      # Filtrer uniquement les variables d'intérêt
      interest_mask <- !is.na(names) & !is.na(values) & names %in% variables_of_interest
      current_nap <- stats::setNames(as.list(values[interest_mask]), names[interest_mask])

      # Ajouter l'id et le timestamp pour chaque nuit
      current_nap$id <- id
      current_nap$time <- time

      return(current_nap)
    })

    # Convertir la liste des nuits en data frame
    df_nap_mtn <- dplyr::bind_rows(lapply(nap_list, function(x) {
      as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)
    }))

    # Convertir certaines colonnes en numérique
    df_nap_mtn <- df_nap_mtn %>%
      dplyr::group_by(id, time) %>%
      dplyr::summarize(dplyr::across(tidyselect::everything(), ~ dplyr::first(stats::na.omit(.)), .names = "{.col}"), .groups = "drop") %>%
      dplyr::mutate(time = as.POSIXct(as.numeric(time), origin = "1970-01-01", tz = "UTC")) %>%
      dplyr::mutate(
        dplyr::across(tidyselect::where(~ is.character(.) && any(grepl(",", .))), ~ as.numeric(gsub(",", ".", .)))
        ) %>%
      dplyr::group_by(id) %>%
      filter(!(any(is.na(AnalStart) | is.na(AnalEnd)))) %>%
      ungroup() %>%
      dplyr::arrange(AnalStart) %>%
      dplyr::ungroup() %>%
      stats::na.omit() %>%
      dplyr::select(-c(id, time)) %>%
      mutate(
        AnalStart = as.POSIXct(AnalStart,
                               origin = extract_start_recording(file), tz = "UTC"),
        AnalEnd = as.POSIXct(AnalEnd, origin =  extract_start_recording(file), tz = "UTC"),
        Date = as.Date(AnalStart),
        NapDuration = as.numeric(difftime(AnalEnd, AnalStart, units = "min"))
      ) %>%
      select(Date, everything()) %>%
      rename(NapStart = AnalStart,
             NapEnd = AnalEnd) %>%
      mutate(
        NapStart = format(NapStart, "%H:%M:%S"),
        NapEnd = format(NapEnd, "%H:%M:%S")
      )

    return(df_nap_mtn)
  }
}
