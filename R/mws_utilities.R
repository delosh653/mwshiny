#' Renders server output.
#'
#' @param serverValues traditional Shiny inputs, as well as user-created values (reactive values), to be used in rendering output
#' @param session traditional Shiny server session value
#' @param output traditional shiny output
#' @param serv_out_list a named list of functions that render output. Each function is of the form function(calc, session), where calc is a named list containing the traditional Shiny input and reactive values that have calculated values derived from input, and session is the traditional Shiny server session value. It returns the results of a Shiny render function. The name of each function corresponds to its output label.
#' @return traditional Shiny output argument
#' @keywords internal
#' @noRd
serverFunct <- function(serverValues, session, output, serv_out_list){
  # go through each output rendering and render
  if (length(serv_out_list) > 0){
    for (v in 1:length(serv_out_list)){
      # check for errors related to numbers of inputs
      tryCatch({
        # renderoutput and assign to output
        output[[names(serv_out_list)[v]]] <- serv_out_list[[v]](serverValues, session)
      }, error = function(e){
        if (length(grep("unused argument ", as.character(e)[1], fixed = T))>0 |
            length(grep("is missing, with no default", as.character(e)[1], fixed = T))>0){
          e$message <- paste("Argument", v, "of serv_out's functions does not have 2 arguments")
        }
        stop(e)
      })
    }
  }
  return(output)
}

#' Renders user interface for all mwshiny windows.
#'
#' @param win_titles vector of uniquely named strings, corresponding to window titles. Must be same length as ui_win, and titles must be same index as corresponding ui page in ui_win. No windows can be named 'WindowSelector', and titles cannot have spaces.
#' @param ui_list list of shiny ui pages. Must be same length as win_titles, and ui page must be same index as corresponding title in win_titles.
#' @return ui: user interfaces for all windows
#' @keywords internal
#' @noRd
mwsUI <- function(win_titles, ui_list) {
  # force evaluation of the titles and the list, so that they can be used
  force(win_titles)
  force(ui_list)

  # return function to create UI pages
  function(req) {
    # get the window information
    qs <- parseQueryString(req$QUERY_STRING)

    qs <- req$QUERY_STRING
    # take the selected window
    mw_win <- substr(qs, 2, nchar(qs))

    mw_win <- if (!is.null(mw_win) && nchar(mw_win) > 0) {
      # return a user selected window
      mw_win
    } else {
      # otherwise, show the "windowselector" window
      NULL
    }

    if (is.null(mw_win)) {
      # show the window selector window
      mwsSelectorPage(win_titles)
    } else if (mw_win %in% names(ui_list)) {
      mwsPage(ui_list[[mw_win]])
    } else {
      NULL
    }
  }
}

#' Creates the "Window Selector" page.
#'
#' @param win_titles vector of uniquely named strings, corresponding to window titles. Must be same length as ui_win, and titles must be same index as corresponding ui page in ui_win. No windows can be named 'WindowSelector', and titles cannot have spaces.
#' @return user interface for window selector page
#' @keywords internal
#' @noRd
mwsSelectorPage <- function(win_titles) {
  #  get each of the windows to select
  win_select <- lapply(seq_along(win_titles), function(i) {
    # get each of the titles
    win_title <- win_titles[[i]]
    # create a link for each window
    tags$h2(
      tags$a(href = paste0("?",win_title),
             win_title
      )
    )
  })

  # create and return the window selector page
  shiny::bootstrapPage(
    shiny::div(class = "Window",
               shiny::div(
                 style = htmltools::css(
                   position = "absolute", top = "50%", left = "50%",
                   margin_right = "-50%", transform = "translate(-50%, -50%)"),
                 win_select
               )
    )
  )
}

#' Creates a selected multiwindow shiny page.
#'
#' @param ui a selection from ui_list, the list of shiny ui pages.
#' @return user interface for the selected multi-window page
#' @keywords internal
#' @noRd
mwsPage <- function(ui) {
  shiny::bootstrapPage(ui)
}
