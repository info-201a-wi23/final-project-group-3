library("markdown")
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

# Filter data for plot 1
top_20_companies <- plastics %>%
  group_by(parent_company) %>%
  summarize(company_total_plastics = sum(grand_total, na.rm = TRUE)) %>%
  arrange(desc(company_total_plastics)) %>% # Order companies by their total plastic emissions in descending order
  filter(!parent_company %in% c("null", "NULL", "Grand Total", "Unbranded")) %>% # Remove rows where parent_company is "null" or "NULL"
  filter(row_number() <= 20)


# Application title
intro_panel <- tabPanel(
  "Introduction",
  fluidPage(

  )
)
# An introductory page that provides an overview of the project. What major questions are you seeking to answer? What data are you using to answer those questions? Please provide a URL link to the original source(s) of the data. Where did the data come from, and what are possible ethical questions or limitations to consider with this dataset? You should also include some additional "flare" on this landing page, such as an image.

plot_panel <- tabPanel("Emissions by Company", 
                       sidebarPanel(
                         selectInput(inputId = "user_selection",
                                     label = "Select Company",
                                     choices = top_20_companies$parent_company,
                                     selected = "The Coca-Cola Company",
                                     multiple = TRUE)),
                         
                       mainPanel(plotlyOutput("plot"))
)

plot_panel_2 <- tabPanel("Second Plot")

plot_panel_3 <- tabPanel("Third Plot")

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
