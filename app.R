library(shiny)
library(ggplot2)
library(cowplot)
library(magrittr)
library(DT)

# Remap HMD names
country_map <- 
	c(AUS = "Australia",
	  AUS2 = "Australia",
	  AUT = "Austria",
	  BLR = "Belarus",
	  BEL = "Belgium",
	  BGR = "Bulgaria",
	  CAN = "Canada",
	  CHL = "Chile",
	  HRV = "Croatia",
	  CZE = "Czechia",
	  DNK = "Denmark",
	  EST = "Estonia",
	  FIN = "Finland",
	  FRATNP = "France",
	  FRACNP = "France Civ",
	  DEUTNP = "Germany",
	  DEUTE = "East Germany",
	  DEUTW = "West Germany",
	  GRC = "Greece",
	  HKG = "Hong Kong",
	  HUN = "Hungary",
	  ISL = "Iceland",
	  Ireland = "IRE",
	  ISR = "Israel",
	  ITA = "Italy",
	  JPN = "Japan",
	  LVA = "Latvia",
	  LTU = "Lithuania",
	  LUX = "Luxembourg",
	  NLD = "Netherlands",
	  NZL_NP = "New Zealand",
	  NZL_MA = "New Zealand (Maori)",
	  NZL_NM = "New Zealand (Non-Maori)",
	  NOR = "Norway",
	  POL = "Poland",
	  PRT = "Portugal",
	  KOR = "Republic of Korea",
	  RUS = "Russia",
	  SVK = "Slovakia",
	  SVN = "Slovenia",
	  ESP = "Spain",
	  SWE = "Sweden",
	  CHE = "Switzerland",
	  TWN = "Taiwan",
	  GBR_NP = "U.K.",
	  GBRTENW = "England & Wales",
	  GBRCENW = "England & Wales Civ",
	  GBR_SCO = "Scotland",
	  GBR_NIR = "Northern Ireland",
	  USA = "USA",
	  UKR = "Ukraine")

age_map <- c(
	     "0_14" = "Age 0 - 14",
	     "15_64" = "Age 15 - 64",
	     "65_74" = "Age 65 - 74",
	     "75_84" = "Age 75 - 84",
	     "85p" = "Age 85+",
	     "Total" = "Total")

read_stmf_data <- function(file) {
  stmf <- read.csv(file, skip = 2, stringsAsFactors = F)
  stmf <- tidyr::gather(stmf, AgeGroup, Deaths, D0_14:RTotal) 
  stmf$Type <- substr(stmf$AgeGroup, 0, 1)
  stmf$AgeGroup <- age_map[sub("^[DR]", "", stmf$AgeGroup)]
  stmf$Country <- country_map[stmf$CountryCode]
  return(stmf)
}



stmf <- read_stmf_data("stmf.csv")

all_age_groups <- unique(stmf$AgeGroup)
all_countries <- unique(stmf$Country)


ui <- fluidPage(

  titlePanel("Excess mortality in 2020/2021"),

  sidebarLayout(

    sidebarPanel(
		 "Data from ",a("mortality.org", href="https://www.mortality.org/"),

      shiny::selectInput(inputId = "country", 
                         label = "Country",
                         choices = all_countries,
			 selected = c("Germany", "Italy", "USA"),
                         multiple = TRUE),

      shiny::selectInput(inputId = "age_group", 
                         label = "Age Group",
                         choices = all_age_groups,
			 selected = c("Age 15 - 64", "Age 65 - 74", "Age 75 - 84", "Age 85+"),
                         multiple = TRUE),

      shiny::radioButtons(inputId = "sex", 
                         label = "Sex",
                         choices = c("Male"="m", Female="f", Both="b"),
			 selected = "b"),

      shiny::radioButtons(inputId = "type", 
                         label = "Type",
                         choices = c(Deaths="D", "Mortality rate (Deaths/1k/week)"="R"),
			 selected = "R"),
      shiny::checkboxInput(inputId = "normalize",
			   label = "Normalize graphs based on 2015-2019 data"),
      shiny::checkboxInput(inputId = "start_at_zero",
			   label = "Start y-axis at zero",
			   value = TRUE),
      shiny::radioButtons(inputId = "year",
			  choices = c("2020", "2021"),
			  selected = "2021",
			  label = "Year")
    ),

    mainPanel(
      tabsetPanel(type="tabs",
         tabPanel("Mortality plot",
                  plotOutput(outputId = "distPlot")),
	 tabPanel("Excess mortality",
                  DTOutput(outputId = "dat"),
	          "Numbers are showing the difference in the number of deaths compared to the average number of deaths in 2015 - 2019"),
         tabPanel("Data", 
      		shiny::radioButtons(inputId = "year2", label = "Year",
					 choices=2015:2021), 
		  DTOutput(outputId = "dat2")
	 )
	 )
    )
  )
)


server <- function(input, output) {

  stmfi <- reactive({
    dat <- subset(stmf, Country%in% input$country & Sex == input$sex)
    plyr::ddply(dat, c("Country", "AgeGroup", "Type", "Sex", "Week"), function(x) {
        x$NormalDeaths <- mean(x$Deaths[x$Year %in% c(2016,2017,2018,2019)])
        x$AdditionalDeaths <- x$Deaths - x$NormalDeaths
        x
})
  })

  output$dat_info <- renderTable({
    dat <- subset(stmfi(), Year == input$year)
    data.frame(WeekNr=tapply(dat$Week, dat$Country, max))
  }, rownames=TRUE)

  output$dat2 <- renderDT({
    dat <- subset(stmf, Year == input$year2)
    male <- subset(dat, Sex == "m")
    female <- subset(dat, Sex == "f")

    plyr::ddply(data.frame(Country = male$Country,
			   AgeGroup = male$AgeGroup,
			   MaleDeaths = male$Deaths,
			   FemaleDeaths = female$Deaths),
		c("Country"),
	function(x) {
		tapply(x$MaleDeaths/x$FemaleDeaths,
		       x$AgeGroup,
		       function(y) { round(mean(y[is.finite(y)],na.rm=T), 2) }
		       )
    })

  })

  output$dat <- renderDT({
    dat <- subset(stmfi(), Year == input$year)

    #tapply(dat$Deaths, dat$
    dat1 <- plyr::daply(dat, c("AgeGroup", "Country"), function(x) {
	# Exclude the data from the last week
	x <- x[x$Week < max(x$Week), ]
	sprintf("%.f (%.2f%%)", sum(x$AdditionalDeaths),100*sum(x$AdditionalDeaths)/sum(x$NormalDeaths))
    })

    rbind(Data=sprintf("Week 1-%s",tapply(dat$Week, dat$Country, max)),
          dat1)

  })


  output$distPlot <- renderPlot({

    dat <- subset(stmfi(), AgeGroup %in% input$age_group  & Type == input$type & Sex == input$sex)

    country <- input$country

    # Shift week slightly for better readability
    country_plus <- setNames((seq_along(country) - 1)/length(country), sort(country))
    dat$Week <- dat$Week + country_plus[dat$Country]
 
    if (input$normalize) {
      dat <- plyr::ddply(dat, c("Country", "AgeGroup", "Type", "Sex", "Week"), function(x) {
        x$Deaths <- x$Deaths - mean(x$Deaths[x$Year %in% c(2016,2017,2018,2019)])
        x
      })
    }
    g <- ggplot(subset(dat, Year == input$year),aes(x=Week, y=Deaths, color = Country))
   
    if (input$start_at_zero) {
        g <- g + expand_limits(y=0) + geom_hline(yintercept=0)
    }
    g <- g +
	geom_vline(xintercept=as.numeric(strftime(Sys.time(), format = "%V")), alpha=0.5) +
	geom_step(data=subset(dat, Year == 2016), alpha=0.25) +
	geom_step(data=subset(dat, Year == 2017), alpha=0.25) +
	geom_step(data=subset(dat, Year == 2018), alpha=0.25) +
	geom_step(data=subset(dat, Year == 2019), alpha=0.25) +
	geom_step(size=1.5, alpha = 0.75) +
        facet_wrap(~AgeGroup, scales = "free_y") +
    theme_minimal_grid()

    g

    })

}

shinyApp(ui, server)
