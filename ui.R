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

# Filter data for plot 1
top_20_companies <- plastics %>%
  group_by(parent_company) %>%
  summarize(company_total_plastics = sum(grand_total, na.rm = TRUE)) %>%
  arrange(desc(company_total_plastics)) %>% # Order companies by their total plastic emissions in descending order
  filter(!parent_company %in% c("null", "NULL", "Grand Total", "Unbranded")) %>% # Remove rows where parent_company is "null" or "NULL"
  filter(row_number() <= 20)

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
# Group all countries and sum the # of events and volunteers
events_and_volunteers_per_country <- plastics %>%
  group_by(country) %>%
  summarize(num_events = sum(num_events, na.rm = TRUE), num_volunteers = sum(volunteers, na.rm = TRUE)) %>%
  filter(!country %in% c("EMPTY"))

# Create a scatter plot of events and volunteers to show specific countries as outliers
ggplot(events_and_volunteers_per_country, aes(x = num_events, y = num_volunteers)) +
  geom_point(aes(color = country)) +
  geom_text(aes(label = country), hjust = -0.2, vjust = 0.5) +
  labs(title = "Number of Events vs Number of Volunteers by Country", x = "Number of Events", y = "Number of Volunteers") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, max(events_and_volunteers_per_country$num_events), by = 10000)) +
  scale_y_continuous(breaks = seq(0, max(events_and_volunteers_per_country$num_volunteers), by = 300000))

# Application title
intro_panel <- tabPanel(
  "Introduction",
  fluidPage()
)
# An introductory page that provides an overview of the project. What major questions are you seeking to answer? What data are you using to answer those questions? Please provide a URL link to the original source(s) of the data. Where did the data come from, and what are possible ethical questions or limitations to consider with this dataset? You should also include some additional "flare" on this landing page, such as an image.

plot_panel <- tabPanel(
  "Emissions by Company",
  sidebarPanel(
    selectInput(
      inputId = "user_selection",
      label = "Select Company",
      choices = top_20_companies$parent_company,
      selected = "The Coca-Cola Company",
      multiple = TRUE
    )
  ),
  mainPanel(plotlyOutput("plot"))
)

plot_panel_2 <- tabPanel(
  "Number of Events vs Number of Volunteers",
  sidebarPanel(
    selectInput(
      inputId = "panel2_selection",
      label = "Select Country",
      choices = events_and_volunteers_per_country$country,
      selected = "United States of America",
      multiple = TRUE)),

                         mainPanel(plotOutput(outputId = "plot2"))
)

plot_panel_3 <- tabPanel(
  "Emissions by Plastic Type",
  sidebarPanel(
    selectInput(
      inputId = "year_selection_3",
      label = "Select Year",
      choices = c("2019", "2020"),
      selected = "2019",
      multiple = FALSE
    ),
    selectInput(
      inputId = "type_selection",
      label = "Select Plastic Type",
      choices = plot_3_df_1$type,
      selected = "pet",
      multiple = TRUE
    )
  ),
  mainPanel(plotlyOutput("plot_3"))
)

conclusion_panel <- tabPanel(
  "Conclusion"
)
# A conclusion page of summary takeaways that hones in on at least 3 major takeaways from the project, which should be related to a specific aspect of your analysis. This page should include at least 250 words of text, but feel free to incorporate tables, graphics, or other elements necessary to convey your conclusions.

ui <- navbarPage(
  "Climate Change Project",
  intro_panel,
  plot_panel,
  plot_panel_2,
  plot_panel_3,
  conclusion_panel
)
