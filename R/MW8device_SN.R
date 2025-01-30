#' @title
#' MotionWatch 8 - Extraction du SN
#'
#' @description
#' Cette fonction extrait le numéro de série (SN) de la MotionWatch 8.
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
#' #extract_device_SN()
#'
#' @export

extract_device_SN <- function(file) {
  xml2::xml_text(
    xml2::xml_find_first(
      xml2::read_xml(file),
      "//property[name = '=SerialNo']//following-sibling::content"
    )
  )
}
