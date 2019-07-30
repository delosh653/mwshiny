#' Renders server output.
#'
#' @param serverValues traditional Shiny inputs, as well as user-created values (reactive values), to be used in rendering output
#' @param session traditional Shiny server session value
#' @param output traditional shiny output
#' @param serv_out_list a named list of functions that render output. Each function is of the form function(calc, session), where calc is a named list containing the traditional Shiny input and reactive values that have calculated values derived from input, and session is the traditional Shiny server session value. It returns the results of a Shiny render function. The name of each function corresponds to its output label.
#' @return traditional Shiny output argument
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
#' @param depend deprecated; previously was a way to declare HTML dependencies, but now they are inferred from elements of \code{ui_list}.
#' @return ui: user interfaces for all windows
mwsUI <- function(win_titles, ui_list, depend = NULL) {
  force(win_titles)
  force(ui_list)

  if (!is.null(depend)) {
    warning(call. = FALSE, "The 'mwsUI' function's 'depend' parameter is no longer used")
  }

  function(req) {
    qs <- parseQueryString(req$QUERY_STRING)

    mw_win <- qs$mw_win
    mw_win <- if (!is.null(mw_win) && length(mw_win) == 1 && grepl("^\\d+$", mw_win, perl = TRUE)) {
      as.integer(mw_win)
    } else {
      NULL
    }

    if (is.null(mw_win)) {
      mswSelectorPage(win_titles)
    } else if (mw_win %in% seq_along(ui_list)) {
      mswPage(ui_list[[mw_win]])
    } else {
      NULL
    }
  }
}

mswSelectorPage <- function(win_titles) {
  win_select <- lapply(seq_along(win_titles), function(i) {
    win_title <- win_titles[[i]]
    tags$h2(
      tags$a(href = paste0("?mw_win=", i),
        win_title
      )
    )
  })

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

mswPage <- function(ui) {
  shiny::bootstrapPage(ui)
}
