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
  light <- as.numeric(unlist(strsplit(light_text, ",")))

  original_length <- length(light)
  start_date <- extract_start_recording(file)
  epoch <- extract_frequency_light(file)
  light <- data.frame(
    time = seq(from = start_date, by = epoch, length.out = length(light)),
    count = light
  )
  return(light)
}
