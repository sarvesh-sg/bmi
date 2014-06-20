library(shiny)
shinyUI(pageWithSidebar(
    headerPanel("Body Mass Index(BMI)"),
    sidebarPanel(
      numericInput('height','Enter your Height in centimeters',170,min = 1, step = 1),
      sliderInput("weight", "Select your Weight in Kilo grams:", 
                  min = 1, max = 200, value = 70, step= 0.1),
      numericInput('age','Enter your Age in years',21,step = 1),
      selectInput("gender", "Gender",choices = c("Male", "Female")),
      radioButtons("country", label = h3("Choose your Country"), 
                   choices = list("Hong Kong" = 1, "Japan" = 2,"Singapore" = 3,"Others" = 4),
                   selected = 4),
      submitButton("Submit")
    ),
    mainPanel(
      h1("Results"),
      h3("Your BMI is:"),
      verbatimTextOutput("bmi"),
      h3("Age you Entered:"),
      verbatimTextOutput("age"),
      h3("You are Classified as:"),
      verbatimTextOutput("status"),
      h5("Did you know? Age matters while classifying, children < age 20"),
      h3("The Method: "),
      tableOutput("msg")
      )
  ))