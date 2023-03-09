library(shiny)
library("dplyr")
library("ggplot2")
library("scales")
library("plotly")
library("bslib")
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

chart <- top_20_companies %>%
  filter(!parent_company %in% c("null", "NULL", "Grand Total", "Unbranded")) %>%
  group_by(parent_company) %>%
  summarize(total = sum(company_total_plastics, na.rm = TRUE))

# Group all countries and sum the # of events and volunteers
events_and_volunteers_per_country <- plastics %>%
  group_by(country) %>%
  summarize(num_events = sum(num_events, na.rm = TRUE), num_volunteers = sum(volunteers, na.rm = TRUE)) %>%
  filter(!country %in% c("EMPTY"))

# # Create a scatter plot of events and volunteers to show specific countries as outliers
# ggplot(events_and_volunteers_per_country, aes(x = num_events, y = num_volunteers)) +
#   geom_point(aes(color = country)) +
#   geom_text(aes(label = country), hjust = -0.2, vjust = 0.5) +
#   labs(title = "Number of Events vs Number of Volunteers by Country", x = "Number of Events", y = "Number of Volunteers") +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   scale_x_continuous(breaks = seq(0, max(events_and_volunteers_per_country$num_events), by = 10000)) +
#   scale_y_continuous(breaks = seq(0, max(events_and_volunteers_per_country$num_volunteers), by = 300000))

by_type <- plastics %>%
  group_by(year) %>%
  summarize(
    empty = sum(empty, na.rm = TRUE),
    hdpe = sum(hdpe, na.rm = TRUE),
    ldpe = sum(ldpe, na.rm = TRUE),
    o = sum(o, na.rm = TRUE),
    pet = sum(pet, na.rm = TRUE),
    pp = sum(pp, na.rm = TRUE),
    ps = sum(ps, na.rm = TRUE),
    pvc = sum(pvc, na.rm = TRUE)
  )
data_2019 <- by_type %>%
  filter(year == 2019)
data_2020 <- by_type %>%
  filter(year == 2020)
plot_3_df_1 <- data.frame(
  type = c("empty", "hdpe", "ldpe", "o", "pet", "pp", "ps", "pvc"),
  count = c(
    data_2019$empty, data_2019$hdpe, data_2019$ldpe,
    data_2019$o, data_2019$pet, data_2019$pp, data_2019$ps, data_2019$pvc
  )
)
plot_3_df_2 <- data.frame(
  type = c("empty", "hdpe", "ldpe", "o", "pet", "pp", "ps", "pvc"),
  count = c(
    data_2020$empty, data_2020$hdpe, data_2020$ldpe,
    data_2020$o, data_2020$pet, data_2020$pp, data_2020$ps, data_2020$pvc
  )
)

server <- function(input, output) {
  output$plot <- renderPlotly({
    # top_20_companies <- plastics %>%
    # group_by(parent_company) %>%
    # summarize(company_total_plastics = sum(grand_total, na.rm = TRUE)) %>%
    # arrange(desc(company_total_plastics)) %>%
    # filter(!parent_company %in% c("null", "NULL", "Grand Total", "Unbranded")) %>%
    # filter(row_number() <= 20)

    chart <- top_20_companies %>%
      filter(parent_company %in% input$user_selection) %>%
      group_by(parent_company) %>%
      summarize(total = sum(company_total_plastics))

    plot <- ggplot(data = chart) +
      geom_col(mapping = aes(x = reorder(parent_company, +total), y = total, fill = parent_company)) +
      coord_flip() +
      labs(title = "Top 20 Plastic-Emitting Companies", x = "Parent Company", y = "Total Plastic Emitted (million metric tons)") +
      theme(legend.position = "none") +
      scale_y_continuous(labels = label_number_si())

    return(ggplotly(plot))
  })

  plotlyOutput(outputId = "plot")

  output$plot2 <- renderPlotly({
    
    chart2 <- events_and_volunteers_per_country %>% 
      filter(country %in% input$panel2_selection)

    
    plot2 <- ggplot(data = chart2, aes(x = num_events, y = num_volunteers)) +
      geom_point(aes(color = country)) +
      labs(title = "Number of Events vs Number of Volunteers by Country",
           x = "Number of Events", 
           y = "Number of Volunteers") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_x_continuous(breaks = seq(0, max(events_and_volunteers_per_country$num_events), by = 10000)) +
      scale_y_continuous(breaks = seq(0, max(events_and_volunteers_per_country$num_volunteers), by = 300000))

        
    return(ggplotly(plot2))
  })
  
  plotOutput(outputId = "plot2")
  
  output$plot_3 <- renderPlotly({
    if (input$year_selection_3 == "2019") {
      filtered_3 <- plot_3_df_1 %>% filter(type %in% input$type_selection)
    } else {
      filtered_3 <- plot_3_df_2 %>% filter(type %in% input$type_selection)
    }
    p3 <- ggplot(data = filtered_3, aes(x = type, y = count, fill = type)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set3") +
      labs(
        title = paste(
          "Total Count of Different Types of Plastics in",
          input$year_selection_3
        ),
        x = "Type of Plastics",
        y = "Total Count of Each Type of Plastic"
      ) +
      scale_y_continuous(labels = label_number_si())
    return(ggplotly(p3, tooltip = c("x", "y")))
  })
  plotlyOutput(outputId = "plot_3")
}