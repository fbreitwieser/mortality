library(shiny)
library(ggplot2)
library(cowplot)
library(magrittr)
library(DT)


stmf <- read.csv("stmf.csv", skip = 2, stringsAsFactors = F)

stmf <- tidyr::gather(stmf, AgeGroup, Deaths, D0_14:RTotal) 
stmf$Type <- substr(stmf$AgeGroup, 0, 1)
stmf$AgeGroup <- sub("^[DR]", "", stmf$AgeGroup)

all_age_groups <- unique(stmf$AgeGroup)
all_countries <- unique(stmf$CountryCode)

ui <- fluidPage(

  titlePanel("Weekly mortality data in Europe"),

  sidebarLayout(

    sidebarPanel(

      shiny::selectInput(inputId = "country", 
                         label = "Country",
                         choices = all_countries,
			 selected = c("AUT", "CHE", "CZE"),
                         multiple = TRUE),

      shiny::selectInput(inputId = "age_group", 
                         label = "Age Group",
                         choices = all_age_groups,
			 selected = c("15_64", "65_74", "75_84", "85p"),
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
      "Week of latest data in 2020 per country:",
      tableOutput(outputId = "dat_info")
    ),

    mainPanel(

      plotOutput(outputId = "distPlot"),
      DTOutput(outputId = "dat")

    )
  )
)


server <- function(input, output) {

  stmfi <- reactive({
    dat <- subset(stmf, CountryCode %in% input$country & Sex == input$sex)
    plyr::ddply(dat, c("CountryCode", "AgeGroup", "Type", "Sex", "Week"), function(x) {
        x$NormalDeaths <- mean(x$Deaths[x$Year %in% c(2016,2017,2018,2019)])
        x$AdditionalDeaths <- x$Deaths - x$NormalDeaths
        x
})
  })

  output$dat_info <- renderTable({
    dat <- subset(stmfi(), Year == 2020)
    data.frame(WeekNr=tapply(dat$Week, dat$CountryCode, max))
  }, rownames=TRUE)

  output$dat <- renderDT({
    dat <- subset(stmfi(), Year == 2020)

    #tapply(dat$Deaths, dat$
    plyr::daply(dat, c("AgeGroup", "CountryCode"), function(x) {
	# Exclude the data from the last week
	x <- x[x$Week < max(x$Week), ]
	sprintf("%.f (%.2f%%)", sum(x$AdditionalDeaths),100*sum(x$AdditionalDeaths)/sum(x$NormalDeaths))
    })

  })


  output$distPlot <- renderPlot({

    dat <- subset(stmfi(), AgeGroup %in% input$age_group  & Type == input$type & Sex == input$sex)

    country <- input$country

    # Shift week slightly for better readability
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
