#' @title
#' MotionWatch 8 - Extraction de la version
#'
#' @description
#' Cette fonction extrait la version de la MotionWatch 8.
#'
#' @author
#' European Sleep Center (Paris, France)
#'
#' Maxime Chauvineau - mchauvineau@europeansleepcenter.fr
#'
#' @returns
#' Chaîne de caractères.
#'
#' @param file Chemin du fichier .mtn
#'
#' @examples
#' #extract_device_version()
#'
#' @export

extract_device_version <- function(file) {
  xml2::xml_text(
    xml2::xml_find_first(
      xml2::read_xml(file),
      "//property[name = '=Device']//following-sibling::content"
    )
  )
}
