#' Runs Shiny app in multiple specified windows.
#'
#' @param ui_win named list of shiny UI pages. The name of each entry in the UI page list corresponds to its window title. No windows can be named 'WindowSelector', titles must be uniquely named, and titles cannot have spaces.
#' @param serv_calc a named list of functions that calculate variables derived from user input, to be used in rendering output. Each function is of the form function(calc, session), where calc is a named list containing the traditional Shiny input and user-created reactive values, and session is the traditional Shiny server session value. All calculated variables that are needed to render output should be added, named, to the calc list. When using reactive functions such as observeEvent(), each should be contained in a separate function, and variables dependent on these reactions should be added to calc. Note that these functions follow all Shiny conventions (reactive values must be accessed in a reactive context, etc.).
#' @param serv_out a named list of functions that render output. Each function is of the form function(calc, session), where calc is a named list containing the traditional Shiny input and reactive values that have calculated values derived from input, and session is the traditional Shiny server session value. It returns the results of a Shiny render function. The name of each function corresponds to its output label. Note that these functions follow all Shiny conventions (reactive values must be accessed in a reactive context, etc.).
#' @return Shiny app object (i.e., it runs the app)
#' @export
#' @import shiny
#' @examples
#' if(interactive()){
#' # Run a simple 2-window app, initially bringing up the window selector window:
#' ui_win <- list()
#' ui_win[["clickinput"]] <- fluidPage(numericInput(inputId = "click", label = "a", value = 1))
#' ui_win[["clickoutput"]] <- fluidPage(plotOutput("clickplot"))
#' serv_out <- list()
#' serv_out[["clickplot"]] <- function(calc, session){
#'   renderPlot({
#'       plot(1:calc$click,1:calc$click)
#'   })
#' }
#' mwsApp(ui_win, list(), serv_out)
#' }
mwsApp <- function(ui_win=list(), serv_calc=list(), serv_out=list()){
  # safeguards for improper arguments
  win_titles <- names(ui_win)
  if ("WindowSelector" %in% win_titles){
    stop("Argument win_titles contains the reserved window name 'WindowSelector'")
  }

  if (length(unique(win_titles)) != length(win_titles)){
    stop("Argument win_titles contains duplicate window titles.")
  }

  if (any(grepl(" ", win_titles, fixed = T))){
    stop(paste("Window titles cannot have spaces. Please remove all spaces in window title."))
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

  if (is.null(names(serv_out)) & length(serv_out) > 0){
    stop("Argument serv_out is unnamed")
  }

  if (is.null(names(ui_win)) & length(ui_win) > 0){
    stop("Argument depend is unnamed")
  }

  # compute ui
  ui <- mwsUI(names(ui_win),ui_win)

  # preallocate serverValues
  serverValues <- shiny::reactiveValues()


  # create server, getting output
  mws_server <- (function(input,output,session){ #shiny::shinyServer
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
