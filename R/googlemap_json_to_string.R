#' Convert Googlemap JSON style sheet to ggmap API format
#'
#' Takes a JSON style guide for Google Maps and returns a Google Map API string for styling
#'
#'
#' @param style_list JSON format object, easiest to build one of these using snazzymaps.com or similar service
#'
#' @return Base R heatmap plot with row labels on left instead of right side
#'
#' @examples
#' mapstyle <- rjson::fromJSON(file = "./R/mapstyle3.json") %>%
#'             googlemap_json_to_string(.)
#' ggmap::get_googlemap(style=mapstyle)
#'
#' @export

googlemap_json_to_string <-
  function (style_list)
  {
    style_string <- ""
    for (i in 1:length(style_list)) {
      if ("featureType" %in% names(style_list[[i]])) {
        style_string <- paste0(style_string, "feature:",
                               style_list[[i]]$featureType, "|")
      }
      if ("elementType" %in% names(style_list[[i]])) {
        style_string <- paste0(style_string, "element:",
                               style_list[[i]]$elementType, "|")
      }
      elements <- style_list[[i]]$stylers
      a <- lapply(elements, function(x) paste0(names(x), ":",
                                               x)) %>% unlist() %>% paste0(collapse = "|")
      style_string <- paste0(style_string, a)
      if (i < length(style_list)) {
        style_string <- paste0(style_string, "&style=")
      }
    }
    style_string <- gsub("#", "0x", style_string)
    style_string <- gsub("[|]", "%7C", style_string)
    return(style_string)
  }
