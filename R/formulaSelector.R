# We'll wrap our Shiny Gadget in an addin.
generateFormula <- function() {
  library(shiny)
  library(miniUI)

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniPage(
    gadgetTitleBar("Formula Generator"),
    miniContentPanel(
      textInput("formulaName", "Formula Name", value = "myFormula"),
      uiOutput("dfSelect"),
      uiOutput("targetSelect"),
      uiOutput("featureSelect"),
      actionButton("insertCode", label = "Insert Code")
    )
  )

  server <- function(input, output, session) {



    # Pick the training dataset
    output$dfSelect <- renderUI({
      dfList <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
      selectInput("trainingDataset", "Choose a Training Dataset", dfList)
    })



    # Get the target feature
    df <- reactive({
      if(!is.null(input$trainingDataset)){
        x <- get(as.character(input$trainingDataset))
      }
    })


    output$targetSelect <- renderUI({
      df <- df()
      featureList <- colnames(df)
      selectInput("target", "Choose a Target", featureList)
    })


    # Finally, pick features
    output$featureSelect <- renderUI({
      # Get column names
      df <- df()
      allFeatures <- colnames(df)

      # Filter out the target variable
      target <- input$target
      featureList <- allFeatures[!(allFeatures %in% target)]

      selectizeInput('featureSelectize', "Select Features", featureList, multiple = TRUE)
    })


    # Generate formula
    observeEvent(input$insertCode, {
      # Get components of formula
      target <- input$target
      features <- input$featureSelectize

      # Generate the formula
      dynamicFormula <- paste0("\n", input$formulaName,
                               " <- as.formula(",
                               target,
                               " ~ ",
                               paste(features, collapse = " + "),
                               ")")

      rstudioapi::insertText(dynamicFormula)

    })



    # Listen for 'done' events. When we're finished, we'll
    # insert the current time, and then stop the gadget.
    observeEvent(input$done, {
      stopApp()
    })

  }

  # We'll use a pane viwer, and set the minimum height at
  # 300px to ensure we get enough screen space to display the clock.
  viewer <- dialogViewer("Formula Generator")
  runGadget(ui, server, viewer = viewer)

}

# Try the app!
#generateFormula()

# Now all that's left is sharing this addin -- put this function
# in an R package, provide the registration metadata at
# 'inst/rstudio/addins.dcf', and you're ready to go!
