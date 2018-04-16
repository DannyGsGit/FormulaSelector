# We'll wrap our Shiny Gadget in an addin.
generateList <- function() {
  library(shiny)
  library(miniUI)

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniPage(
    gadgetTitleBar("List Generator"),
    miniContentPanel(
      textInput("listName", "List Name", value = "myList"),
      uiOutput("dfSelect"),
      uiOutput("featureSelect"),
      actionButton("insertCode", label = "Insert Code")
    )
  )

  server <- function(input, output, session) {



    # Pick the training dataset
    output$dfSelect <- renderUI({
      dfList <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
      selectInput("trainingDataset", "Choose a Data Frame", dfList)
    })



    # Get the target feature
    df <- reactive({
      if(!is.null(input$trainingDataset)){
        x <- get(as.character(input$trainingDataset))
      }
    })




    # Finally, pick features
    output$featureSelect <- renderUI({
      # Get column names
      df <- df()
      featureList <- colnames(df)


      selectizeInput('featureSelectize', "Select Features for List", featureList, multiple = TRUE)
    })


    # Generate formula
    observeEvent(input$insertCode, {
      # Get components of formula
      features <- input$featureSelectize

      # Generate the formula
      dynamicFormula <- paste0("\n", input$listName,
                               " <- c('",
                               paste(features, collapse = "', '"),
                               "')")

      # Insert the text into RStudio
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
  viewer <- dialogViewer("List Generator")
  runGadget(ui, server, viewer = viewer)


}

# Try the app!
#generateFormula()

# Now all that's left is sharing this addin -- put this function
# in an R package, provide the registration metadata at
# 'inst/rstudio/addins.dcf', and you're ready to go!
