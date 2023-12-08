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
library(shinyjs)
if (!require(here)) install.packages("here")
library(here)

shrooms <- read.csv(here("mushrooms.csv"), stringsAsFactors = TRUE)

#in our dataset all veil types are "p" so they were removed
shrooms$veil.type <- NULL
# class is our output variable so it was made numeric, 1 means poisonous, 0  means edible
shrooms$class <- ifelse(shrooms$class == "p", 1, 0)
colnames(shrooms)[1] ="is_poisonous"


# Load the decision tree model
tree_model <- readRDS(here("tree_model.RDS"))
# load in the logistic model
log_model <- readRDS(here("log_model.rds"))

# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Alfa+Slab+One&display=swap"
    ),
    tags$style(
      HTML("
        body {
          background-image: url('mushies2.jpg');  /* Adjust the path and file name accordingly */
          background-size: cover;
          background-repeat: no-repeat;
          background-attachment: fixed;
          background-position: center;
          margin: 0; /* Remove default body margin */
          height: 100vh; /* Set the height to 100% of the viewport height */
          font-family: 'Alfa Slab One', cursive;
        }
      ")
    )
  ),
  
  titlePanel(HTML("<h1 style='color: white;'>Mushroom Marketplace ShroomCheck </h1>")),
  
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
      selectInput("ring_type", "Ring Type:", choices = c("Cobwebby" = "c", "Evanescent" = "e", "Flaring" = "f", "Large" = "l", "None" = "n", "Pendant" = "p", "Sheathing" = "s", "Zone" = "z")),
      selectInput("spore_print_color", "Spore Print Color:", choices = c("Black" = "k", "Brown" = "n", "Buff" = "b", "Chocolate" = "h", "Green" = "r", "Orange" = "o", "Purple" = "u", "White" = "w", "Yellow" = "y")),
      selectInput("population", "Population:", choices = c("Abundant" = "a", "Clustered" = "c", "Numerous" = "n", "Scattered" = "s", "Several" = "v", "Solitary" = "y")),
      selectInput("habitat", "Habitat:", choices = c("Grasses" = "g", "Leaves" = "l", "Meadows" = "m", "Paths" = "p", "Urban" = "u", "Waste" = "w", "Woods" = "d")),
      # ... Add more selectInput statements for other variables
      actionButton("submit_btn", "Submit")
    ),
     mainPanel(
      style = "background-color: #87ae73; background-size: cover; padding: 30px; border-radius: 10px; font-family: 'Alfa Slab One', cursive;",
      h4("Result:", style = "margin-bottom: 30px;"),  # Title with some margin
      tags$span(
        textOutput("prediction_result"),
        style = "font-size: 20px;"  # Adjust the font size as needed
        
      ),
      tags$img(src = "manymushies.gif", style = "width: 100%;"),
      tags$div(
        style = "border: 2px solid #426B29; padding: 15px; margin-top: 20px;",
        tags$h4(
          tags$a("Mushroom Glossary", href = "https://www.realmushrooms.com/mushroom-anatomy-parts/", style = "color: white; text-decoration: none; background-color: black")
        )
      ),)
     )
    )
 


# Define Server
server <- function(input, output) {
  observeEvent(input$submit_btn, {
    # Collect user inputs
    user_inputs <- data.frame(
      # changed the naming scheme of the inputs to work
      # for each input it will see if it exists in the levels of the model, if it does then it outputs it, otherwise it replaces with the most common class
      cap.shape = input$cap_shape,
      cap.surface = input$cap_surface,
      cap.color = input$cap_color,
      bruises = input$bruises,
      odor = input$odor,
      gill.attachment = ifelse(input$gill_attachment %in% levels(shrooms$gill.attachment), 
                               input$gill_attachment, 
                               names(which.max(table(shrooms$gill.attachment)))),
      gill.spacing = ifelse(input$gill_spacing %in% levels(shrooms$gill.spacing), 
                            input$gill_spacing, 
                            names(which.max(table(shrooms$gill.spacing)))),
      gill.size = input$gill_size,
      gill.color = input$gill_color,
      stalk.shape = input$stalk_shape,
      stalk.root = ifelse(input$stalk_root %in% levels(shrooms$stalk.root), 
                          input$stalk_root, 
                          names(which.max(table(shrooms$stalk.root)))),
      stalk.surface.above.ring = input$stalk_surface_above_ring,
      stalk.surface.below.ring = input$stalk_surface_below_ring,
      stalk.color.above.ring = input$stalk_color_above_ring,
      stalk.color.below.ring = input$stalk_color_below_ring,
      veil.type = input$veil_type,
      veil.color = input$veil_color,
      ring.number = ifelse(input$ring_number %in% levels(shrooms$ring.number), 
                           input$ring_number, 
                           names(which.max(table(shrooms$ring.number)))), #no need
      ring.type = ifelse(input$ring_type %in% levels(shrooms$ring.type), 
                         input$ring_type, 
                         names(which.max(table(shrooms$ring.type)))),
      spore.print.color = input$spore_print_color,
      population = input$population,
      habitat = input$habitat
      
      # ... Add more columns for other variables
    )
    
    # Make predictions using the loaded model
    prediction <- predict(log_model, newdata = user_inputs, type = "response")
    
    # Display the prediction result
    output$prediction_result <- renderText({
      if (round(prediction, 9) == 1) {
        "POISONOUS DO NOT CONSUME"
      } else {
        "Munch Away ;)"
        }
      })
  })
}

# Run the Shiny app
shinyApp(ui, server)
