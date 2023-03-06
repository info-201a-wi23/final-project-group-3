library("dplyr")
library("ggplot2")
library("scales")
library("plotly")
library("bslib")
library(shiny)

# Get the original data from Tidy Tuesday
tuesdata <- tidytuesdayR::tt_load("2021-01-26")

# Extract the plastics data
plastics <- tuesdata$plastics

top_20_companies <- plastics %>%
  group_by(parent_company) %>%
  summarize(company_total_plastics = sum(grand_total, na.rm = TRUE)) %>%
  arrange(desc(company_total_plastics)) %>% # Order companies by their total plastic emissions in descending order
  filter(!parent_company %in% c("null", "NULL", "Grand Total", "Unbranded")) %>% # Remove rows where parent_company is "null" or "NULL"
  filter(row_number() <= 20)

chart <- top_20_companies %>% filter(!parent_company %in% c("null", "NULL", "Grand Total", "Unbranded")) %>% group_by(parent_company) %>% summarize(total=sum(company_total_plastics, na.rm=TRUE))
View(chart)

server <- function(input, output) {
  
  output$plot <- renderPlotly({
    
    #top_20_companies <- plastics %>%
      #group_by(parent_company) %>%
      #summarize(company_total_plastics = sum(grand_total, na.rm = TRUE)) %>%
      #arrange(desc(company_total_plastics)) %>% 
      #filter(!parent_company %in% c("null", "NULL", "Grand Total", "Unbranded")) %>% 
      #filter(row_number() <= 20)
    
    chart <- top_20_companies %>% filter(parent_company %in% input$user_selection) %>% group_by(parent_company) %>% summarize(total = sum(company_total_plastics))
    
    plot <- ggplot(data = chart) +
      geom_col(mapping = aes(x = reorder(parent_company, +total), y = total, fill = parent_company)) +
      coord_flip() +
      labs(title = "Top 20 Plastic-Emitting Companies", x = "Parent Company", y = "Total Plastic Emitted (million metric tons)") +
      theme(legend.position = "none") +
      scale_y_continuous(labels = label_number_si())
    
    return(ggplotly(plot))
  })
  
  plotlyOutput(outputId="plot")
}