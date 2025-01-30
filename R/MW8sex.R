#' @title
#' MotionWatch 8 - Extraction du sexe
#'
#' @description
#' Cette fonction extrait le sexe définit lors de la programmation de la MotionWatch 8.
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
#' #extract_sex()
#'
#' @export

extract_sex <- function(file) {
  xml2::xml_text(
    xml2::xml_find_first(
      xml2::read_xml(file),
      "//property[name = '+Sex']//following-sibling::content"
    )
  )
}
