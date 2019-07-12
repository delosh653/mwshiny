#' Runs Shiny app in multiple specified windows.
#'
#' @param win_titles vector of uniquely named strings, corresponding to window titles. Must be same length as ui_win, and titles must be same index as corresponding ui page in ui_win. No windows can be named 'WindowSelector', and titles cannot have spaces.
#' @param ui_win list of Shiny ui pages. Must be same length as win_titles, and ui page must be same index as corresponding title in win_titles.
#' @param serv_calc a named list of functions that calculate variables derived from user input, to be used in rendering output. Each function is of the form function(calc, session), where calc is a named list containing the traditional Shiny input and user-created reactive values, and session is the traditional Shiny server session value. All calculated variables that are needed to render output should be added, named, to the calc list. When using reactive functions such as observeEvent(), each should be contained in a separate function, and variables dependent on these reactions should be added to calc. Note that these functions follow all Shiny conventions (reactive values must be accessed in a reactive context, etc.).
#' @param serv_out a named list of functions that render output. Each function is of the form function(calc, session), where calc is a named list containing the traditional Shiny input and reactive values that have calculated values derived from input, and session is the traditional Shiny server session value. It returns the results of a Shiny render function. The name of each function corresponds to its output label. Note that these functions follow all Shiny conventions (reactive values must be accessed in a reactive context, etc.).
#' @param depend named list of package dependencies, with the names corresponding to the packages for which .js and .css files will be imported. If value is NA, all .js and .css files will be imported. Otherwise, value must be a vector of strings corresponding to the .js and .css files to be imported, with location relative to the package folder.
#' @return Shiny app object (i.e., it runs the app)
#' @export
#' @import shiny
#' @importFrom htmltools htmlDependency
#' @importFrom utils packageVersion
#' @examples
#' if(interactive()){
#' # Run a simple 2-window app, initially bringing up the window selector window:
#' ui_win <- list()
#' ui_win[[1]] <- fluidPage(numericInput(inputId = "click", label = "a", value = 1))
#' ui_win[[2]] <- fluidPage(plotOutput("clickplot"))
#' serv_out <- list()
#' serv_out[["clickplot"]] <- function(calc, session){
#'   renderPlot({
#'       plot(1:calc$click,1:calc$click)
#'   })
#' }
#' mwsApp(c("clickinput","clickoutput"),
#'     ui_win,
#'     list(),
#'     serv_out)
#' }
mwsApp <- function(win_titles=c(), ui_win=list(), serv_calc=list(), serv_out=list(), depend = list()){
  # safeguards for improper arguments
  if ("WindowSelector" %in% win_titles){
    stop("Argument win_titles contains the reserved window name 'WindowSelector'")
  }

  if (length(unique(win_titles)) != length(win_titles)){
    stop("Argument win_titles contains duplicate window titles.")
  }

  if (typeof(ui_win)!="list"){
    stop("Argument ui_win is not a list")
  }

  if (typeof(serv_calc)!="list"){
    stop("Argument serv_calc is not a list")
  }

  if (typeof(serv_out)!="list"){
    stop("Argument serv_out is not a list")
  }

  if (typeof(depend)!="list"){
    stop("Argument depend is not a list")
  }

  if (length(win_titles)!=length(ui_win)){
    stop("Arguments win_titles and ui_win have different lengths: ",
         length(win_titles), " and ", length(ui_win))
  }

  if (is.null(names(serv_out)) & length(serv_out) > 0){
    stop("Argument serv_out is unnamed")
  }

  if (is.null(names(depend)) & length(depend) > 0){
    stop("Argument depend is unnamed")
  }

  # no checks for depends contents -- it will throw errors later

  # compute ui
  ui <- mwsUI(win_titles, ui_win, depend)

  # preallocate serverValues
  serverValues <- shiny::reactiveValues()


  # create server, getting output
  mws_server <- shiny::shinyServer(function(input,output,session){
    observe({
      for (inputId in names(input)) {
        serverValues[[inputId]] <- input[[inputId]]
      }
    })

    # run each of the server calculation functions
    if (length(serv_calc) > 0){
      for (s in 1:length(serv_calc)){

        # check for errors related to numbers of inputs
        tryCatch({
          serv_calc[[s]](serverValues, session)
        }, error = function(e){
          if (length(grep("unused argument ", as.character(e)[1], fixed = T))>0 |
              length(grep("is missing, with no default", as.character(e)[1], fixed = T))>0){
            e$message <- paste("Argument", s, "of serv_calc's functions does not have 2 arguments")
          }
          stop(e)
        })
      }
    }

    # then allocate each of these to output
    serverFunct(serverValues, session, output, serv_out)

  })

  # run the app!
  shiny::shinyApp(ui, mws_server)
}
