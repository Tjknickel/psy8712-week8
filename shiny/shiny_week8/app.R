#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Calls necessary packages. This was done to run the app with the following code lines.
library(dplyr)
library(ggplot2)
library(shiny)

# Adds a title panel for the web application, a sidebar panel with 3 dropdown inputs, and a main panel that displays a scatterplot based on user selected values of the 3 select inputs. This was done in order to update the scatterplot based on user selected inputs relating to participant gender, whether or not to display the purple shaded error band, and whether to filter based on if participants completed the assessment before July 1, 2017.
ui <- fluidPage(
  titlePanel("Correlation between mean scores on Questions 1 to 6 and Questions 8 to 10"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Select Gender:", choices = c("All", "Male", "Female"), selected = "All"),
      selectInput("error_band", "Error Band:", choices = c("Display Error Band" = TRUE, "Suppress Error Band" = FALSE), selected = TRUE),
      selectInput("date", "Participants that completed the assessment before July 1, 2017:", choices = c("Include", "Exclude"), selected = "Include")
    ),
    mainPanel(plotOutput('meanxy_plot')
    )
  )
)

# Define server logic required to draw a scatterplot updated based on user inputs. The data that goes into the scatterplot (using ggplot) is changed/filtered based on user inputs from the previous lines of code within the ui. Specifically, the data is filtered for gender (Male or Female) if the default "All" is not selected and filtered for the end time of the assessment if the default "Exclude" is not selected. The "se" argument within geom_smooth is also updated based on whether the user selects to include/exclude the error band. This is done to also update the scatterplot based on user inputs. 
server <- function(input, output) {
    week8_rds <- readRDS("week8.rds")
    output$meanxy_plot <- renderPlot({
      week8_rds %>%
        filter(input$gender == "All" | gender == input$gender) %>%
        filter(input$date == "Include" | timeEnd >= as.Date("2017-07-01")) %>%
      ggplot(aes(x = mean_x, y = mean_y)) +
        geom_point() +
        geom_smooth(method = "lm", color = "purple", fill = "purple", se = as.logical(input$error_band)) +
        labs(x = "Mean Scores of Q1-Q6", y = "Mean Scores of Q8-Q10")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
