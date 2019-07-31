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
#' @param ui_list named list of shiny UI pages. The name of each entry in the UI page list corresponds to its window title. No windows can be named 'WindowSelector', titles must be uniquely named, and titles cannot have spaces.
#' @return ui: user interfaces for all windows
mwsUI <- function(ui_list) {

  # window titles
  win_titles <- names(ui_list)
  win_select <- ""
  for (w in win_titles){
    # check if titles have spaces
    if (grepl(" ", w)){
      stop(paste("Window titles cannot have spaces. Please remove space in window title:", w))
    }

    win_select <- paste0(win_select,
                         '<h2><a href="?',w,'">',w,'</a></h2>')
  }

  other_win <- list()
  if (length(ui_list) > 0){
    for (u in 1:length(ui_list)){
      other_win[[length(other_win)+1]] <- tags$div(ui_list[[u]], class = paste0(win_titles[u], " Window"))
    }
  }

  # check if there is a html tag -- stop
  if (grepl("</html>", other_win, fixed= T)[1]){
    stop("The <html> tag is reserved for the main page. Please remove any occurences of the <html> tag in your UIs.")
  }

  # check if there is a body tag -- suggest change to div with a warning
  if (grepl("</body>", other_win, fixed= T)[1]){
    warning("The <body> tag is reserved for the main page. We suggest you change your <body> tags to <div> tags in your UIs, as errors may occur.")
  }

  ui <- shiny::shinyUI(shiny::bootstrapPage(
    shiny::HTML('<script type="text/javascript">
                $(function() {
                $("div.Window").hide();
                var tokens = window.location.href.split("?");
                if (tokens.length > 1) {
                var shown_window = tokens[1];
                $("div."+shown_window).show();
                } else {
                $("div.WindowSelector").show();
                }
                });
                </script>'),
    shiny::div(class="WindowSelector Window",
               shiny::HTML(win_select),
               style='position: absolute;
               top: 50%; left: 50%;
               margin-right: -50%;
               transform: translate(-50%, -50%)'
    ),
    other_win
    ))

  return(ui)
}
