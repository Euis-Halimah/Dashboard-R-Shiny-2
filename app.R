# Install and load required packages
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("DT")) install.packages("DT")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

# Create a sample dataset
dataset <- data.frame(
  Day = 1:10,
  Left_Sidebar = c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7),
  Center_Page = c(3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9),
  Right_Sidebar = c(3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)

#create data for ANOVA
Y <- c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7,
       3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9,
       3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
X <- gl(3,10,30)#n perlakuan, k perulangan, 20 unit percobaan
levels(X) <- c("Left_Sidebar", "Center_Page", "Right_Sidebar")
CTR<- data.frame(Y, X)
CTR

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Ad Placement on CTR"),
  dashboardSidebar(
    fileInput("file", "Upload Data (CSV format)"),
    h4("Or enter data manually:"),
    numericInput("day", "Day", value = 0),
    numericInput("left_sidebar", "Left Sidebar CTR", value = 0),
    numericInput("center_page", "Center Page CTR", value = 0),
    numericInput("right_sidebar", "Right Sidebar CTR", value = 0),
    actionButton("addDataButton", "Add")
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Dataset",
        status = "info",
        solidHeader = TRUE,
        DTOutput("table")
      ),
      box(
        title = "ANOVA Analysis",
        status = "success",
        solidHeader = TRUE,
        verbatimTextOutput("anova_result")
      ),
      box(
      title = "A Summary output of ANOVA Analysis",
      status = "primary",
      solidHeader = TRUE,
      helpText("Diketahui X adalah data CTR dan Y adalah posisi peletakan iklan, berdasarkan output ANOVA didapatkan p-value 2.37e-11 < 0.05
           yang artinya model anova signifikan maka dengan menggunakan tingkat kepercayaan 95% data yang ada menolak H0 sehingga dapat 
           disimpulkan bahwa CTR dipengaruhi oleh posisi penempatan iklan")
      ),
      box(
        title = "Placement Visualization",
        status = "warning",
        solidHeader = TRUE,
        plotOutput("placement_plot")
      )
    )
  )
)

# Define server
server <- function(input, output) {
  # Add Data
  observeEvent(input$addDataButton, {
    new_data <- data.frame(
      Day = input$day,
      Left_Sidebar = input$left_sidebar,
      Center_Page = input$center_page,
      Right_Sidebar = input$right_sidebar
    )
    data <- bind_rows(dataset, new_data)
  })
  # Render dataset table
  output$table <- renderDT({
    datatable(dataset, rownames = FALSE)
  })
  
  # Perform ANOVA analysis and render results
  output$anova_result <- renderPrint({
    anova_result <- aov(Y ~ X, data = CTR)
    summary(anova_result)
  })
  
  # Create placement visualization
  output$placement_plot <- renderPlot({
    ggplot(dataset, aes(x = Day)) +
      geom_line(aes(y = Left_Sidebar, color = "Left Sidebar")) +
      geom_line(aes(y = Center_Page, color = "Center Page")) +
      geom_line(aes(y = Right_Sidebar, color = "Right Sidebar")) +
      labs(title = "Ad Placement over Days", y = "Placement Value") +
      scale_color_manual(values = c("Left Sidebar" = "blue", "Center Page" = "green", "Right Sidebar" = "red"))
  })
}

# Run the application
shinyApp(ui, server)
