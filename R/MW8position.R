#' @title
#' MotionWatch 8 - Extraction de la position
#'
#' @description
#' Cette fonction extrait la position (poignet, cheville, gauche, droite...) définit lors de la programmation de la MotionWatch 8.
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
#' #extractMW8position()
#'
#' @export

extract_position <- function(file) {
  xml2::xml_text(
    xml2::xml_find_first(
      xml2::read_xml(file),
      "//property[name = '=Position']//following-sibling::content"
    )
  )
}
