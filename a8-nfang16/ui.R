library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)


shinyUI(navbarPage(
  "A8",
  tabPanel(
    "Scatter",
    titlePanel("Midwest Poverty By Percentage"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "x_var",
          label = h3("Select X Variable"),
          choices = list(
            "Percentage Below Poverty" = "percbelowpoverty",
            "Percentage Children Below Poverty" = "percchildbelowpovert",
            "Percentage Adult Poverty" = "percadultpoverty",
            "Percentage Elderly Poverty" = "percelderlypoverty",
            "Percentage College" = "percollege"
          ),
          selected = "Poverty Population"
        ),
        selectInput(
          inputId = "y_var",
          label = h3("Select Y Variable"),
          choices = list(
            "Poverty Population" = "poppovertyknown",
            "Percentage Below Poverty" = "percbelowpoverty",
            "Percentage Children Below Poverty" = "percchildbelowpovert",
            "Percentage Adult Poverty" = "percadultpoverty",
            "Percentage Elderly Poverty" = "percelderlypoverty",
            "Percentage College" = "percollege"
          ),
          selected = "Percentage Adult Poverty"
        ),
        sliderInput(
          inputId = "poppovertyknown",
          label = "Filter Population Values",
          min = min(midwest$poppovertyknown),
          max = max(midwest$poppovertyknown),
          value = 100000
        )
      ),
      mainPanel(
        plotlyOutput("scatter", height = 350)
      )
    ) #closes sidebarlayout
  ), #closes tab panel  
  
  tabPanel(
    "Plot",
    titlePanel("Ethnic Makeup of the Midwest"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          inputId = "state",
          label = h3("By State"),
          choices = list(
            "Illinois" = "IL",
            "Indiana" = "IN",
            "Michigan" = "MI",
            "Ohio" = "OH",
            "Wisconsin" = "WI"),
          selected = "MI"
          ),
        selectInput(
          inputId = "xvar", 
          label = h3("Race 1"),
          choices = list(
            "Number Asian" = "popasian",
            "Number White" = "popwhite",
            "Number Black" = "popblack",
            "Number American Indian" = "popamerindian",
            "Number Other" = "popother"
          ),
          selected = "Number Asian"
        ),
        selectInput(
          inputId = "yvar", 
          label = h3("Race 2"),
          choices = list(
            "Number White" = "popwhite",
            "Number Black" = "popblack",
            "Number American Indian" = "popamerindian",
            "Number Asian" = "popasian",
            "Number Other" = "popother"
          ),
          selected = "Number White"
        )
      ),
      mainPanel(
        plotlyOutput("plot")
      )
    ) #sidebarlayout
  ) #tabpanel
))
