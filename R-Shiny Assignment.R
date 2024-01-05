library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

ui <- fluidPage(
  titlePanel("Claims Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Claims Data Here"),
      numericInput("tail", "Select Tail Factor", value = 1.1, min = 0.1, step = 0.1)
    ),
    mainPanel(
      h3("User Input - Claims Data"),
      br(),
      tableOutput("contents"),
      br(),
      h3("Claims Development Triangle"),
      br(),
      tableOutput("triangle"),
      br(),
      h3("Projected Claims"),
      br(),
      plotOutput("plot")
    )
  )
)

# ------------------------------------------------------------

server <- function(input, output, session) {
  Claims_Data <- reactive({
    req(input$file)
    File <- input$file
    read_excel(File$datapath, range = "Assignment!B3:D9")
    
  })

# ------------------------------------------------------------
  
  Claims_Triangle <- reactive({
    data <- Claims_Data()
    Loss_Year_Number <- n_distinct(data$`Loss Year`)
    Dev_Year_Number <- n_distinct(data$`Development Year`)
    
    Loss_Years <- sort(unique(data$`Loss Year`))
    Dev_Years <- sort(unique(data$`Development Year`))
    
    Triangle <- matrix(0, nrow = Loss_Year_Number, ncol = Dev_Year_Number, byrow = TRUE)
    
    rownames(Triangle) <- Loss_Years
    colnames(Triangle) <- Dev_Years
    
    for (row in 1:nrow(data)) {
      LYear <- as.character(data[row, 1])
      DYear <- as.character(data[row, 2])
      Triangle[LYear, DYear] <- as.double(data[row, 3])
      
    }
    
    Cumulative_Triangle <- t(apply(Triangle, 1, cumsum))
    n <- Dev_Year_Number
    
    Dev_Factors <- sapply(1:(n-1), function(i) {
      sum(Cumulative_Triangle[c(1:(n-i)), i+1]) / sum(Cumulative_Triangle[c(1:(n-i)), i])
      
    })
    
    Dev_Factors <- c(Dev_Factors, input$tail)
    
    Full_Triangle <- cbind(Cumulative_Triangle, "4" = rep(0, Loss_Year_Number))
    
    for (k in 1:n) {
      Full_Triangle[(n-k+1):n, k+1] <- Full_Triangle[(n-k+1):n, k] * Dev_Factors[k]
      
    }
    
    round(Full_Triangle)
    
    colnames(Full_Triangle) <- paste("Development Year", colnames(Full_Triangle), sep = " ")
    rownames(Full_Triangle) <- paste("Loss Year", rownames(Full_Triangle), sep = " ")
    
    Full_Triangle
    
  })
  
# ------------------------------------------------------------  
  
  Claims_Plot <- reactive({
    Full_Triangle <- Claims_Triangle()
    data <- Claims_Data()
    
    Loss_Year_Number <- n_distinct(data$`Loss Year`)
    Dev_Year_Number <- n_distinct(data$`Development Year`)
    
    Loss_Years <- sort(unique(data$`Loss Year`))
    Dev_Years <- sort(unique(data$`Development Year`))
    
    Development = rep(c(1:(Dev_Year_Number+1)), times = Loss_Year_Number)
    Loss = as.character(rep(Loss_Years, each = Dev_Year_Number+1))
    Amount = round(c(t(Full_Triangle)))
    
    data_plot <- data.frame(Development, Loss, Amount)
    
    ggplot(data_plot, aes(x = Development, y = Amount, group = Loss, color = Loss)) +
      geom_point(size = 2) +
      geom_line(linetype = "dashed", size = 0.5) +  # Corrected parameter name to size
      geom_text(aes(label = scales::comma(Amount)), hjust = 0.5, vjust = -1, color = "darkblue", size = 3.5) +
      xlab("Development Year") +
      ylab("Claims Amount ($)") +
      labs(title = "Cumulative Paid Claims Graph") +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12))
    
  })

# ------------------------------------------------------------  
  
  output$contents <- renderTable({
    Claims_Data()
  }, digits = 0, align = "ccc")
  
  output$triangle <- renderTable({
    Claims_Triangle()
  }, rownames = TRUE, digits = 0, width = 600)
  
  output$plot <- renderPlot({
    Claims_Plot()
  })
}

# ------------------------------------------------------------

shinyApp(ui, server)
