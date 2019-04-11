updateDescendants <- function(tag, update) {
  # First check tag
  if (class(tag) != "shiny.tag") {
    return(tag)
  }
  
  tag$children = lapply(tag$children, update)
  return(update(tag))
}

tagHasClass <- function(tag, class) {
  tagClass = tag %>% tagGetAttribute("class")
  if (is.null(tagClass)) {
    return(FALSE)
  }
  tagClasses = strsplit(tagClass, " ", fixed=TRUE)
  return(class %in% tagClasses)
}

sliderValue <- function(slider, live=NA, delayed=NA) {
  if (is.na(live) && is.na(delayed)) {
    live = TRUE
    delayed = FALSE
  } else if (is.na(live)) {
    live = !delayed
  } else if (is.na(delayed)) {
    delayed = !live
  }
  
  if (live && delayed) {
    class = "slider-value-both"
  } else if (live) {
    class = "slider-value-live"
  } else if (delayed) {
    class = "slider-value-delayed"
  } else {
    warning("Slider with live=FALSE and delayed=FALSE makes no sense")
    class = NA
  }
  
  return(slider %>% updateDescendants(function(tag) {
    if (class(tag) == "shiny.tag" && tag$name == "input" && tagHasClass(tag, "js-range-slider")) {
      return(tag %>% tagAppendAttributes(class=class))
    } else {
      return(tag)
    }
  }))
}

