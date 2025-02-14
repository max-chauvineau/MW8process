#' @title
#' MotionWatch 8 - Données de luminosité
#'
#' @description
#' Cette fonction extrait les données de luminosité de la MotionWatch 8.
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
#' #extract_light()
#'
#' @export

extract_light <- function(file) {
  light_text <- xml2::xml_text(
    xml2::xml_find_first(
      xml2::read_xml(file),
      "//channel[name = 'Light']//following-sibling::data"
    )
  )
  original_light <- as.numeric(unlist(strsplit(light_text, ",")))
  original_length <- length(original_light)

  if (original_length > 1) {
    # Créer la série temporelle initiale basée sur les données d'origine
    start_date <- extract_start_recording(file)
    epoch <- extract_frequency_light(file)
    light <- data.frame(
      time = seq(from = start_date, by = epoch, length.out = length(original_light)),
      light = original_light
    )

    # Trouver les sections de changement (modifications manuelles)
    change_nodes <- xml2::xml_find_all(xml2::read_xml(file), "//change[channel[@edit='true']][channel/name='motion']")

    # Si des modifications existent, traiter chaque changement
    if (length(change_nodes) > 0) {
      for (change_node in change_nodes) {
        # Extraire offset et epoch modifié pour le changement
        offset <- as.numeric(xml2::xml_text(xml2::xml_find_first(change_node, ".//offset")))
        modified_epoch <- as.numeric(xml2::xml_text(xml2::xml_find_first(change_node, ".//epoch")))

        # Extraire les nouvelles données de mouvement pour ce changement
        modified_data_text <- xml2::xml_text(xml2::xml_find_first(change_node, ".//data"))
        modified_data <- as.numeric(unlist(strsplit(modified_data_text, ",")))

        # Calculer l'index de début dans motion basé sur l'offset (en secondes)
        start_index <- offset / epoch + 1
        end_index <- start_index + length(modified_data) - 1

        # Si light est trop courte, l'étendre
        if (end_index > nrow(light)) {
          additional_rows <- data.frame(
            time = seq(
              from = light$time[nrow(light)] + epoch,
              by = epoch,
              length.out = end_index - nrow(light)
            ),
            light = NA
          )
          light <- rbind(light, additional_rows)
        }

        light$light[start_index:end_index] <- modified_data
      }
    }

    light$light[light$light == -1] <- NA
    light <- light %>%
      dplyr::filter(dplyr::row_number() >= min(which(!is.na(light))) & dplyr::row_number() <= max(which(!is.na(light))))
  } else {
    light <- as.data.frame(list(
      time = numeric(0),
      light = numeric(0)
    ))
  }
  return(light)
}
