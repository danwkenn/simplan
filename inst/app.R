library(shiny)

node = "alpha"
distribution_details <- yaml::read_yaml('distribution_details.yml')

ui <- fluidPage(
  withMathJax(),
  textInput(paste0(node,"_distribution"),
  "What base probability distribution does this node have?"),
  uiOutput('pdf_formula')
)

server <- function(input, output, session) {

  input_name <- reactive(paste0(node,"_distribution"))

  parameters <- reactive({
    distribution_details[[input[[input_name()]]]][["parameters"]]
  })

  output$param_input <- renderUI({
    print(input[[input_name()]])
    textInput(
      paste0(node,"_param1"),
      paste0(names(parameters())[[1]]))
  })

  observeEvent(input[[input_name()]], {

    # Identify any old input fields:
    print("Removing old input fields")
    inputnames <- names(input)
    to_remove <- inputnames[grepl(paste0(node,"_paraminput_",".*"), inputnames)]
    # Remove:
    for(i in seq.int(1,length(to_remove), length.out = length(to_remove))){
    removeUI(
      selector = paste0("div:has(> #",to_remove[i],")"),
      multiple = TRUE
    )
    }

    # If valid distribution, then create fields for inputing the value.
    if(input[[input_name()]] %in% names(distribution_details)){
      for(i in rev(names(parameters()))){
    insertUI(
      selector = paste0("#",input_name()),
      where = "afterEnd",
      ui = textInput(paste0(node,"_paraminput_", i),
                     paste0(i,": ",
                            distribution_details[[input[[input_name()]]]][["parameters"]][[i]]$description
                     ))
    )}
    }
  })

  observeEvent(input[[input_name()]], {

    text <- ""
    if(!is.null(print(distribution_details[[input[[input_name()]]]][["latex_pdf"]]))){
      text <- paste0(
        "Probability Density Function: ",
        distribution_details[[input[[input_name()]]]][["latex_pdf"]][[1]])
    }

  output$pdf_formula <- renderUI({
    withMathJax(helpText(text))})
  }
  )
}

shinyApp(ui, server)
