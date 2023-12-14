#' Function to Clean and Standardize Player Names
#'
#' This function takes a string representing a player's name and performs cleaning operations to standardize it.
#' It removes any non-alphabetic characters and converts the name to lowercase. The function also addresses 
#' any double spaces that might occur as a result of character removal, ensuring a clean, standardized output.
#'
#' @param name A string representing a player's name.
#'
#' @return A cleaned and standardized version of the player's name as a string. The returned name
#'         is in lowercase, with non-alphabetic characters removed, and without double spaces.
#'
#' @examples
#' clean_player_name("Tangela  Smith")        # Returns "tangela smith"
#' clean_player_name("Marie  Ferdinand-Harris") # Returns "marie ferdinandharris"
#' clean_player_name("A'ja Wilson")   # Returns "aja wilson"
#' 
#' @export
#' 
clean_player_name <- function(name) {
  # Remove non-alphabetic characters and convert to lower case
  clean_name <- tolower(gsub("[^a-zA-Z ]", "", name)) 
  # Remove double spaces if any
  clean_name <- gsub("  ", " ", clean_name) 
  return(clean_name)
}