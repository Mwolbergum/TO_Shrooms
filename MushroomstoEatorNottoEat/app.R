#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# app.R

# Assuming 'your_model' is the object representing your trained model
# Replace 'your_model' with the actual variable name of your trained model

# ... (other necessary library and model loading code)
library(caret)
library(neuralnet)
library(class)
library(kernlab)
library(C50)
library(janitor)
library(tidyr)
library(dplyr)
library(randomForest)

# Load libraries and model
library(shiny)
if (!require(here)) install.packages("here")
library(here)

# Load the decision tree model
tree_model <- readRDS(here("tree_model.RDS"))
log_model <- readRDS("log_model.rds")

# Define UI
ui <- fluidPage(
  
    
  
  titlePanel(HTML("<h1 style='color: white;'>Mushroom Predictor</h1>")),

  style = "background-color: #426B29;",
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #EEE78E;",
      # Input for each variable in your model
      selectInput("cap_shape", "Cap Shape:", choices = c("Bell" = "b", "Conical" = "c", "Convex" = "x", "Flat" = "f", "Knobbed" = "k", "Sunken" = "s")),
      selectInput("cap_surface", "Cap Surface:", choices = c("Fibrous" = "f", "Grooves" = "g", "Scaly" = "y", "Smooth" = "s")),
      selectInput("cap_color", "Cap Color:", choices = c("Brown" = "n", "Buff" = "b", "Cinnamon" = "c", "Gray" = "g", "Green" = "r", "Pink" = "p", "Purple" = "u", "Red" = "e", "White" = "w", "Yellow" = "y")),
      selectInput("bruises", "Bruises:", choices = c("Yes" = "t", "No" = "f")),
      selectInput("odor", "Odor:", choices = c("Almond" = "a", "Anise" = "l", "Creosote" = "c", "Fishy" = "y", "Foul" = "f", "Musty" = "m", "None" = "n", "Pungent" = "p", "Spicy" = "s")),
      selectInput("gill_attachment", "Gill Attachment:", choices = c("Attached" = "a", "Descending" = "d", "Free" = "f", "Notched" = "n")),
      selectInput("gill_spacing", "Gill Spacing:", choices = c("Close" = "c", "Crowded" = "w", "Distant" = "d")),
      selectInput("gill_size", "Gill Size:", choices = c("Broad" = "b", "Narrow" = "n")),
      selectInput("gill_color", "Gill Color:", choices = c("Black" = "k", "Brown" = "n", "Buff" = "b", "Chocolate" = "h", "Gray" = "g", "Green" = "r", "Orange" = "o", "Pink" = "p", "Purple" = "u", "Red" = "e", "White" = "w", "Yellow" = "y")),
      selectInput("stalk_shape", "Stalk Shape:", choices = c("Enlarging" = "e", "Tapering" = "t")),
      selectInput("stalk_root", "Stalk Root:", choices = c("Bulbous" = "b", "Club" = "c", "Cup" = "u", "Equal" = "e", "Rhizomorphs" = "z", "Rooted" = "r", "Missing" = "?")),
      selectInput("stalk_surface_above_ring", "Stalk Surface Above Ring:", choices = c("Fibrous" = "f", "Scaly" = "y", "Silky" = "k", "Smooth" = "s")),
      selectInput("stalk_surface_below_ring", "Stalk Surface Below Ring:", choices = c("Fibrous" = "f", "Scaly" = "y", "Silky" = "k", "Smooth" = "s")),
      selectInput("stalk_color_above_ring", "Stalk Color Above Ring:", choices = c("Brown" = "n", "Buff" = "b", "Cinnamon" = "c", "Gray" = "g", "Orange" = "o", "Pink" = "p", "Red" = "e", "White" = "w", "Yellow" = "y")),
      selectInput("stalk_color_below_ring", "Stalk Color Below Ring:", choices = c("Brown" = "n", "Buff" = "b", "Cinnamon" = "c", "Gray" = "g", "Orange" = "o", "Pink" = "p", "Red" = "e", "White" = "w", "Yellow" = "y")),
      selectInput("veil_type", "Veil Type:", choices = c("Partial" = "p", "Universal" = "u")),
      selectInput("veil_color", "Veil Color:", choices = c("Brown" = "n", "Orange" = "o", "White" = "w", "Yellow" = "y")),
      selectInput("ring_number", "Ring Number:", choices = c("None" = "n", "One" = "o", "Two" = "t")),
      selectInput("ring_type", "Ring Type:", choices = c("Evanescent" = "e", "Flaring" = "f", "Large" = "l", "None" = "n", "Pendant" = "p", "Sheathing" = "s", "Zone" = "z")),
      selectInput("spore_print_color", "Spore Print Color:", choices = c("Black" = "k", "Brown" = "n", "Buff" = "b", "Chocolate" = "h", "Green" = "r", "Orange" = "o", "Purple" = "u", "White" = "w", "Yellow" = "y")),
      selectInput("population", "Population:", choices = c("Abundant" = "a", "Clustered" = "c", "Numerous" = "n", "Scattered" = "s", "Several" = "v", "Solitary" = "y")),
      selectInput("habitat", "Habitat:", choices = c("Grasses" = "g", "Leaves" = "l", "Meadows" = "m", "Paths" = "p", "Urban" = "u", "Waste" = "w", "Woods" = "d")),
      # ... Add more selectInput statements for other variables
      actionButton("submit_btn", "Submit")
    ),
    mainPanel(
      textOutput("prediction_result")
    )
  )
)

# Define Server
server <- function(input, output) {
  observeEvent(input$submit_btn, {
    # Collect user inputs
    user_inputs <- data.frame(
      cap.shape = input$cap_shape,
      cap.surface = input$cap_surface,
      cap.color = input$cap_color,
      bruises = input$bruises,
      odor = input$odor,
      gill.attachment = input$gill_attachment,
      gill.spacing = input$gill_spacing,
      gill.size = input$gill_size,
      gill.color = input$gill_color,
      stalk.shape = input$stalk_shape,
      stalk.root = input$stalk_root,
      stalk.surface.above.ring = input$stalk_surface_above_ring,
      stalk.surface.below.ring = input$stalk_surface_below_ring,
      stalk.color.above.ring = input$stalk_color_above_ring,
      stalk.color.below.ring = input$stalk_color_below_ring,
      veil.type = input$veil_type,
      veil.color = input$veil_color,
      ring.number = input$ring_number,
      ring.type = input$ring_type,
      spore.print.color = input$spore_print_color,
      population = input$population,
      habitat = input$habitat
      
      # ... Add more columns for other variables
    )
    
    # Make predictions using the loaded model
    prediction <- predict(log_model, newdata = user_inputs, type = "response")
    
    # Display the prediction result
    output$prediction_result <- renderText({
      paste("Prediction: ", prediction)
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)
