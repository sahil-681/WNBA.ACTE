#' Function to Remove Middle Names from a Full Name
#'
#' This function takes a full name string as input and returns the name with the middle name(s) removed.
#' It retains only the first and last names. The function handles names with multiple parts but does not
#' alter single-part names.
#'
#' @param full_name A string representing a person's full name.
#'
#' @return A string of the person's name with the middle name(s) removed, consisting of just the first
#'         and last names. If the input name has only one part, it returns the name as is.
#'
#' @examples
#' remove_middle_name("DeMya Chakheia Walker") # Returns "DeMya Walker"
#' remove_middle_name("Ruthie Bolton")         # Returns "Ruthie Bolton"
#' remove_middle_name("Diana")                 # Returns "Diana"
#' 
#' @export
#' 
remove_middle_name <- function(full_name) {
  name_parts <- unlist(strsplit(full_name, " "))
  if (length(name_parts) > 1) {
    return(paste(name_parts[1], name_parts[length(name_parts)], sep = " "))
  } else {
    return(full_name)
  }
}