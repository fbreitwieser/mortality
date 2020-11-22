library(shiny)
library(ggplot2)
library(cowplot)


stmf <- read.csv("stmf.csv", skip = 2, stringsAsFactors = F)

stmf <- tidyr::gather(stmf, AgeGroup, Deaths, D0_14:RTotal) 
stmf$Type <- substr(stmf$AgeGroup, 0, 1)
stmf$AgeGroup <- sub("^[DR]", "", stmf$AgeGroup)

all_age_groups <- unique(stmf$AgeGroup)
all_countries <- unique(stmf$CountryCode)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Hello Shiny!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      shiny::selectInput(inputId = "country", 
                         label = "Country",
                         choices = all_countries,
			 selected = c("AUT", "CHE", "CZE"),
                         multiple = TRUE),

      shiny::selectInput(inputId = "age_group", 
                         label = "Age Group",
                         choices = all_age_groups,
			 selected = all_age_groups,
                         multiple = TRUE),

      shiny::radioButtons(inputId = "sex", 
                         label = "Sex",
                         choices = c("Male"="m", Female="f", Both="b"),
			 selected = "b"),

      shiny::radioButtons(inputId = "type", 
                         label = "Type",
                         choices = c(Deaths="D", "Mortality rate (Deaths/1k/week)"="R"),
			 selected = "D"),
      shiny::checkboxInput(inputId = "normalize",
			   label = "Normalize counts"),
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot")

    )
  )
)

server <- function(input, output) {


  output$distPlot <- renderPlot({

    country <- input$country
    dat <- subset(stmf, CountryCode %in% country & AgeGroup %in% input$age_group & Type == input$type & Sex == input$sex)
    country_plus <- setNames((seq_along(country) - 1)/length(country), sort(country))
    dat$Week <- dat$Week + country_plus[dat$Country]
 
    if (input$normalize) {
      dat <- plyr::ddply(dat, c("CountryCode", "AgeGroup", "Type", "Sex", "Week"), function(x) {
        x$Deaths <- x$Deaths - mean(x$Deaths[x$Year %in% c(2016,2017,2018,2019)])
        x
      })
    }
    ggplot(subset(dat, Year == 2020),aes(x=Week, y=Deaths, color = CountryCode)) +
	geom_vline(xintercept=as.numeric(strftime(Sys.time(), format = "%V")), alpha=0.5) +
	geom_step(data=subset(dat, Year == 2016), alpha=0.25) +
	geom_step(data=subset(dat, Year == 2017), alpha=0.25) +
	geom_step(data=subset(dat, Year == 2018), alpha=0.25) +
	geom_step(data=subset(dat, Year == 2019), alpha=0.25) +
	geom_step(size=1.5, alpha = 0.75) +
        facet_wrap(~AgeGroup, scales = "free_y") +
    theme_minimal_grid()

    })

}

shinyApp(ui, server)
