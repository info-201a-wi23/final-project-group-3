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
View(chart)

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