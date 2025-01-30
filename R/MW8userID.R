#' @title
#' MotionWatch 8 - Extraction de l'id
#'
#' @description
#' Cette fonction extrait l'id définit lors de la programmation de la MotionWatch 8.
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
#' #extract_userID()
#'
#' @export

extract_userID <- function(file) {
  xml2::xml_text(
    xml2::xml_find_first(
      xml2::read_xml(file),
      "//property[name = '+UserID']//following-sibling::content"
    )
  )
}
