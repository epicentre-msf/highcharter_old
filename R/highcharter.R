#' Create a Highcharts chart widget
#'
#' This function creates a Highchart chart using \pkg{htmlwidgets}. The
#' widget can be rendered on HTML pages generated from R Markdown, Shiny, or
#' other applications.
#'
#' @param hc_opts A `list` object containing options defined as 
#'    \url{http://api.highcharts.com/highcharts}.
#' @param theme A \code{hc_theme} class object-
#' @param type A character value to set if use Highchart, Highstock or
#'   Highmap. Options are \code{"chart"}, \code{"stock"} and \code{"map"}.
#' @param width A numeric input in pixels.
#' @param height  A numeric input in pixels.
#' @param elementId	Use an explicit element ID for the widget.
#'      
#' @importFrom htmlwidgets createWidget sizingPolicy
#'
#' @export
highchart <- function(hc_opts = list(),
                      theme = getOption("highcharter.theme"),
                      type = "chart",
                      width = NULL,
                      height = NULL,
                      elementId = NULL) {
  
  assertthat::assert_that(type %in% c("chart", "stock", "map"))
  
  opts <- .join_hc_opts()

  if (identical(hc_opts, list()))
    hc_opts <- opts$chart
  
  unfonts <- unique(c(.hc_get_fonts(hc_opts), .hc_get_fonts(theme))) 
  
  opts$chart <- NULL
  
  # forward options using x
  x <- list(
    hc_opts = hc_opts,
    theme = theme,
    conf_opts = opts,
    type = type,
    fonts = unfonts,
    debug = getOption("highcharter.debug")
  )
  
  attr(x, "TOJSON_ARGS") <- list(pretty = getOption("highcharter.debug"))
  
  # create widget
  htmlwidgets::createWidget(
    name = "highchart",
    x,
    width = width,
    height = height,
    package = "highcharter",
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      knitr.figure = FALSE,
      browser.fill = TRUE,
      padding = 0
      )
  )
}

#' Reports whether x is a highchart object
#' 
#' @param x An object to test
#' @export
is.highchart <- function(x) {
  inherits(x, "highchart") || inherits(x, "highchart2") || inherits(x, "highchartzero")
}

#' Widget output function for use in Shiny
#'
#' @param outputId The name of the input.
#' @param width A numeric input in pixels.
#' @param height  A numeric input in pixels. 
#'
#' @importFrom htmlwidgets shinyWidgetOutput 
#' @export
highchartOutput <- function(outputId, width = "100%", height = "400px"){
  shinyWidgetOutput(outputId, "highchart", width, height,
                    package = "highcharter")
}

#' Widget render function for use in Shiny
#'
#' @param expr A highchart expression. 
#' @param env A enviorment.
#' @param quoted  A boolean value.
#' 
#' @importFrom htmlwidgets shinyRenderWidget
#' @export
renderHighchart <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
    } # force quoted
  shinyRenderWidget(expr, highchartOutput, env, quoted = TRUE)
}


#' Create a Highcharts chart widget
#' 
#' This widgets don't support options yet.
#'
#' This function creates a Highchart chart using \pkg{htmlwidgets}. The
#' widget can be rendered on HTML pages generated from R Markdown, Shiny, or
#' other applications.
#'
#' @param hc_opts A `list` object containing options defined as 
#'    \url{http://api.highcharts.com/highcharts}.
#' @param theme A \code{hc_theme} class object
#' @param width A numeric input in pixels.
#' @param height  A numeric input in pixels.
#' @param elementId	Use an explicit element ID for the widget.
#' @param debug A boolean value if you want to print in the browser console the 
#'    parameters given to `highchart`.
#'
#' @export
highchart2 <- function(hc_opts = list(),
                       theme = NULL,
                       width = NULL,
                       height = NULL,
                       elementId = NULL,
                       debug = FALSE) {
  
  unfonts <- unique(c(.hc_get_fonts(hc_opts), .hc_get_fonts(theme))) 
  
  # forward options using x
  x <- list(
    hc_opts = hc_opts,
    theme = theme,
    fonts = unfonts,
    debug = debug
    
  )
  
  # create widget
  htmlwidgets::createWidget(
    name = "highchart2",
    x,
    width = width,
    height = height,
    package = "highcharter",
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      knitr.figure = FALSE,
      knitr.defaultWidth = "100%",
      browser.fill = TRUE
      )
  )
}

#' @rdname highchartOutput
#' @export
highchartOutput2 <- function(outputId, width = "100%", height = "400px"){
  shinyWidgetOutput(outputId, "highchart2", width, height,
                    package = "highcharter")
}

#' @rdname renderHighchart
#' @export
renderHighchart2 <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  shinyRenderWidget(expr, highchartOutput2, env, quoted = TRUE)
}

#' @rdname highchart2 
#' @export
highchartzero <- function(hc_opts = list(),
                          theme = NULL,
                          width = NULL,
                          height = NULL,
                          elementId = NULL) {
  
  # unfonts <- unique(c(.hc_get_fonts(hc_opts), .hc_get_fonts(theme))) 
  # forward options using x
  x <- list(
    hc_opts = hc_opts
  )
  
  # create widget
  htmlwidgets::createWidget(
    name = "highchartzero",
    x,
    width = width,
    height = height,
    package = "highcharter",
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      knitr.figure = FALSE,
      knitr.defaultWidth = "100%",
      browser.fill = TRUE
    )
  )
}

#' @export
#' 
highchartProxy <- function(shinyId, data = NULL, session = shiny::getDefaultReactiveDomain()) {
  
  if (is.null(session)) {
    stop("highchartProxy must be called from the server function of a Shiny app")
  }
  
  if (!is.null(session$ns) && nzchar(session$ns(NULL)) && substring(shinyId, 1, nchar(session$ns(""))) != session$ns("")) {
    shinyId <- session$ns(shinyId)
  }
  
  structure(
    list(
      session = session,
      id = shinyId,
      x = structure(
        list(data = data)
      )
    ),
    class = "highchart_Proxy"
  )
}

checkProxy <- function(proxy) {
  if (!"highchart_Proxy" %in% class(proxy)) 
    stop("This function must be used with a highchart proxy object")
}

#' Set a new title or subtitle for the chart.
#'
#' @param proxy
#' @param title_opts  New title options. 
#' @param subtitle_opts New subtitle options.
#' @param redraw Whether to redraw the chart or wait for a later call.
#'
#' @export
#' 
hc_set_title <- function(proxy, title_opts = NULL, subtitle_opts = NULL, redraw = FALSE) {
  checkProxy(proxy)
  proxy$session$sendCustomMessage(
    type = 'set-title', 
    message = list(
      id = proxy$id, 
      titleOpts = title_opts,
      subtitleOptions = subtitle_opts,
      redraw = redraw)
  )
  proxy
}

#' @export
#' 
hc_add_plotline <- function(proxy, options, axis = 'x') {
  checkProxy(proxy)
  proxy$session$sendCustomMessage(
    type = 'add-plotline', 
    message = list(
      id = proxy$id,
      axis = axis,
      options = options)
  )
  proxy
}

#' @export
#' 
hc_add_plotband <- function(proxy, options) {
  checkProxy(proxy)
  proxy$session$sendCustomMessage(
    type = 'add-plotband', 
    message = list(
      id = proxy$id,
      options = options)
  )
  proxy
}

#' @export
#' 
hc_remove_plotline <- function(proxy, id, axis = 'x') {
  checkProxy(proxy)
  proxy$session$sendCustomMessage(
    type = 'remove-plotline', 
    message = list(
      id = proxy$id,
      axis = axis,
      band = id)
  )
  proxy
}

#' @export
#' 
hc_remove_plotband <- function(proxy, id) {
  checkProxy(proxy)
  proxy$session$sendCustomMessage(
    type = 'remove-plotband', 
    message = list(
      id = proxy$id,
      band = id)
  )
  proxy
}

#' @export
#' 
hc_set_visible <- function(proxy, serie = 0, visible = TRUE, redraw = TRUE) {
  checkProxy(proxy)
  
  proxy$session$sendCustomMessage(
    type = 'set-visible', 
    message = list(
      id = proxy$id, 
      serie = serie,
      visible = visible,
      redraw = redraw)
  )

  return(proxy)
}

#' @export
#' 
hc_set_extremes <- function(proxy, axes = 0, newmin = NA, newmax = NA, redraw = TRUE, animation = TRUE) {
  checkProxy(proxy)
  
  proxy$session$sendCustomMessage(
    type = 'set-extremes', 
    message = list(
      id = proxy$id, 
      axes = axes,
      newMin = newmin,
      newMax = newmax,
      redraw = redraw,
      animation = animation)
  )

  return(proxy)
}

#' @export
#' 
hc_set_data <- function(proxy, serie = 0, type, data, mapping = hcaes(), redraw = FALSE, animation = NULL, updatePoints = TRUE) {
  checkProxy(proxy)
  
  data <- mutate_mapping(data, mapping)
  
  series <- data_to_series(data, mapping, type = type)

  for(i in 1:length(series))
    proxy$session$sendCustomMessage(
      type = 'set-data', 
      message = list(
        id = proxy$id, 
        serie = i-1,
        data = series[[i]]$data,
        redraw = redraw,
        animation = animation,
        updatePoints = updatePoints)
    )

  return(proxy)
}

#' @export
#' 
hc_set_data_map <- function(proxy, data, redraw = FALSE, animation = NULL, updatePoints = TRUE) {
  checkProxy(proxy)
  
  data <- list_parse(data)
  
  proxy$session$sendCustomMessage(
    type = 'set-data', 
    message = list(
      id = proxy$id, 
      serie = 0,
      data = data,
      redraw = redraw,
      animation = animation,
      updatePoints = updatePoints)
  )

  return(proxy)
}

#' @export
#' 
hc_redraw <- function(proxy) {
  checkProxy(proxy)
    
  proxy$session$sendCustomMessage(
    type = 'redraw', 
    message = list(id = proxy$id)
  )

  return(proxy)
}