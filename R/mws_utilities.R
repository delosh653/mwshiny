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
#' @param depend named list of package dependencies, with the names corresponding to the packages for which .js and .css files will be imported. If value is NA, all .js and .css files will be imported. Otherwise, value must be a vector of strings corresponding to the .js and .css files to be imported, with location relative to the package folder.
#' @return ui: user interfaces for all windows
mwsUI <- function(win_titles, ui_list, depend) {
  win_select <- ""
  for (w in win_titles){
    # check if titles have spaces
    if (grepl(" ", w)){
      stop(paste("Window titles cannot have spaces. Please remove space in window title:", w))
    }

    win_select <- paste0(win_select,
                         '<h2><a href="?',w,'">',w,'</a></h2>')
  }

  other_win <- ""
  if (length(ui_list) > 0){
    for (u in 1:length(ui_list)){
      other_win <- paste0(other_win,
                          '<div class="',win_titles[u],' Window">',ui_list[[u]],'</div>')
    }
  }

  # check if there is a html tag -- stop
  if (grepl("</html>", other_win, fixed= T)[1]){
    stop("The <html> tag is reserved for the main page. Please remove any occurences of the <html> tag in your UIs.")
  }

  # check if there is a body tag -- change to div, with a warning
  if (grepl("</body>", other_win, fixed= T)[1]){
    warning("The <body> tag is reserved for the main page. All <body> tags will be changed to <div>.")
    # change all body tags to div
    other_win <- gsub("body","div",other_win)
  }

  # get dependencies
  head_add <- list()
  if (length(depend) > 0){
    for (d in names(depend)){
      if (is.na(depend[[d]])[1]){
        # if there's nothing there, we infer that you want all js and css files imported for dependency
        js_files <- list.files(path=system.file(package = d, mustWork = T),pattern = "\\.js$", recursive = T)
        css_files <- list.files(path=system.file(package = d, mustWork = T),pattern = "\\.css$", recursive = T)
        head_add[[length(head_add)+1]] <- htmltools::htmlDependency(name = d, version = packageVersion(d),
                                                                    package = d,
                                                                    src = "",
                                                                    script = js_files,
                                                                    stylesheet = css_files
        )
      } else {
        # we add the specified files one by one to the head
        for (s in depend[[d]]){
          if (endsWith(s, ".js")){
            head_add[[length(head_add)+1]] <- tags$head(shiny::includeScript(system.file(s, package = d, mustWork = T)))
          } else if (endsWith(s, ".css")){
            head_add[[length(head_add)+1]] <- tags$head(shiny::includeCSS(system.file(s, package = d, mustWork = T)))
          }
        }

      }
    }
  }

  ui <- shiny::shinyUI(shiny::bootstrapPage(
    head_add,
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
    shiny::HTML(other_win)
    ))

  return(ui)
}
