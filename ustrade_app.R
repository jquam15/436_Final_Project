library(shiny)
library(tidyverse)
library(maps)
library(DT)
library(bslib)
library(thematic)
library(plotly)

#read in the trade data
ustrade = read.csv("https://github.com/jquam15/436_Final_Project/raw/main/ustrade.csv") %>%
  rename("Deficit"="Deficit.Surplus")

#load in the world map with latitude and longitude coordinates from the "maps" package---this allows us to plot the outline of countries
world_map = map_data("world")

#there are several countries where the name of the country doesn't match up across data sources:

# world countries --> trade countries
# Antigua -> Antigua and Barbuda
# Cocos Islands -> Cocos (Keeling) Islands
# Czech Republic -> Czechia
# French Southern and Antarctic Lands -> French South Antarctic Territory
# Grenadines -> Grenada
# Ivory Coast --> Cote d'Ivoire
# Myanmar --> Burma
# Republic of Congo --> Republic of the Congo
# Swaziland --> Eswatini
# Taiwan --> Chinese Taipei
# UK --> United Kingdom 

#change these country names in the "world_map" dataset so that they match up with the "ustrade" dataset
world_map$region[world_map$region == "Antigua"] = "Antigua and Barbuda"
world_map$region[world_map$region == "Cocos Islands"] = "Cocos (Keeling) Islands"
world_map$region[world_map$region == "Czech Republic"] = "Czechia"
world_map$region[world_map$region == "French Southern and Antarctic Lands"] = "French South Antarctic Territory"
world_map$region[world_map$region == "Grenadines"] = "Grenada"
world_map$region[world_map$region == "Ivory Coast"] = "Cote d'Ivoire"
world_map$region[world_map$region == "Myanmar"] = "Burma"
world_map$region[world_map$region == "Republic of Congo"] = "Republic of the Congo"
world_map$region[world_map$region == "Swaziland"] = "Eswatini"
world_map$region[world_map$region == "Taiwan"] = "Chinese Taipei"
world_map$region[world_map$region == "UK"] = "United Kingdom"

#create table to use to plot data, sum deficit by year
deficit = ustrade %>%
  group_by(Year) %>%
  summarise(Deficit = (sum(Deficit)))

#this is a function that takes a year (selected by the user) and returns a map of total trade for the US for each country in 
#the world that they traded with
us_world_trade = function(year) {
  #filter the ustrade dataset to only take the year the user selected
  ustrade = ustrade %>%
    filter(Year == year) %>%
    #create a new column called TotalTrade that is the sum of imports and exports
    mutate(TotalTrade = log10(Imports + Exports))
  
  #merge this world map with our trade data so that we can fill countries by how much total trade they have
  plotting_df = world_map %>%
    #merge the two datasets by their appropriate column name---take all rows in the world map dataset (left join) as we need all of them to make the world map (even if the US didn't trade with that country we still need that country to create the map)
    merge(ustrade, by.x = "region", by.y = "Country", all.x = T) %>%
    #arrange the data accordingly
    arrange(group, order)
  
  #make the plot with the countries filled in by how much they traded
  p = ggplot(plotting_df, aes(x = long, y = lat, group = group, fill = TotalTrade)) +  #take a logarithmic scale so we can differentiate between countries
    geom_polygon(aes(text = paste0("Country: ", region, "\n", "Imports: ", format(Imports, scientific=T, digits=3), "\n", "Exports: ", format(Exports, sceintific=T, digits=3)))) + #plot the polygons with tooltip
    #fill colors from light red to dark red
    scale_fill_gradient(low="#ffcccb", high="#8b0000", na.value = "#000000") +
    #remove grid lines and axis labels (lat and long) as they aren't important to the plot
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      title = element_text(size=18),
      legend.text = element_text(size=12),
      legend.title = element_text(size=12)
    ) +
    #change the title to the legend
    labs(title=paste0("Heatmap of US Total Trade by Country for ", year), fill = "Total Trade")
  #want the plot to have tooltips so use plotly package to create the plot
  ggplotly(p)
}

#this is a function that plots the US total trade deficit by year over time
plot_ts_deficit = function() {
  ggplot(deficit, aes(x=Year, y=Deficit), xlim = c(1995,2020)) +
    geom_line(color = "red") +
    scale_color_brewer(palette = "Set1") +
    labs(
      x = "Year",
      y = "Trade Deficit",
      title = "Trade Deficit Over Time"
    ) +
    theme(
      legend.position = "top right",
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      title = element_text(size=18),
      axis.text = element_text(size=12),
      legend.text = element_text(size=12),
      legend.title = element_text(size=12)
    ) + 
    #create own y-axis labels because scale_y_log10 was not working properly
    scale_y_continuous(breaks = c(-10e11, -9e11,-8e11,-7e11,-6e11,-5e11,-4e11,-3e11,-2e11,-1e11), 
                       labels = c("-1 trillion","-900 billion","-800 billion","-700 billion","-600 billion","-500 billion",
                                  "-400 billion","-300 billion","-200 billion", "-100 billion")) + #remove linespace
    scale_x_continuous(breaks = 1995:2020) 
}

#this is a function to plot trade for imports/exports between the US and the user selected country over the user selected year range
#it also illustrates the deficit between the us and each country by year
two.line.plot<-function(start_date, end_date, country){
  ustrade%>%
    filter(Country==country & Year<=end_date & Year>=start_date)%>%
    #mutate(Deficit=-Deficit)%>%
    pivot_longer(cols=c("Imports","Exports"), names_to="Type", values_to = "Value")%>%
    ggplot(aes(x=Year))+
    geom_line(aes(y=Value/1000000000, group=Type, col=Type))+
    labs(title=paste0("US Imports and Exports with ", country), x="Year", y="USD (in billions)")+
    scale_color_manual(values=c("darkgreen", "darkblue"))+
    geom_col(aes(y=Deficit/1000000000, fill="Deficit"), position = "identity", alpha=.1)+
    scale_fill_manual(values="red")+
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          legend.box="vertical",
          title = element_text(size=18),
          axis.text = element_text(size=12),
          legend.text = element_text(size=12)) +
    scale_x_continuous(breaks=end_date:start_date)
}

#get a list of continents
years = unique(ustrade$Year)
#get descriptive text
descriptive_text_worldmap = "This visual allows you to select a year and then displays a heatmap of the world with the total trade (Imports + Exports) that the US did with each country. The data is scaled by taking the common logarithm of total trade, so that there is more of a distinction between countries. The purpose of this plot is to give a general idea of who the US is trading the most with for a certain year. Darker shades of red mean more trade while lighter shades of red mean less trade. Countries that are shaded with black either did not trade with the US, are a US territory, or are a territory of another country and therefore their trade is accounted for already. In earlier years, some countries are also shaded black because they did not exist for that year (Ex: South Sudan). Additionally if you'd like more information you can hover over a country and see the country's name and common logarithm of total trade. You also can Here are some helpful benchmarks for understanding the values for the common logarithm of total trade in dollars: 6 = million, 7 = ten million, 8 = hundred million, 9 = billion, and so on. You can zoom in on the plot by clicking and holding to create a box brush."


#UI

years = unique(ustrade$Year)
descriptive_text_worldmap = "This visual allows you to select a year and then displays a heatmap of the world with the total trade (Imports + Exports) that the US did with each country. The data is scaled by taking the common logarithm of total trade, so that there is more of a distinction between countries. The purpose of this plot is to give a general idea of who the US is trading the most with for a certain year. Darker shades of red mean more trade while lighter shades of red mean less trade. Countries that are shaded with black either did not trade with the US, are a US territory, or are a territory of another country and therefore their trade is accounted for already. In earlier years, some countries are also shaded black because they did not exist for that year (Ex: South Sudan). Additionally if you'd like more information you can hover over a country and see the country's name and common logarithm of total trade. You also can see the amount of imports and exports. Here are some helpful benchmarks for understanding the values for the common logarithm of total trade in dollars: 6 = million, 7 = ten million, 8 = hundred million, 9 = billion, and so on. You can zoom in on the plot by clicking and holding to create a box brush."
descriptive_text_ts = "If you are more interested with trade between the US and a specific country over time, you can use this visual to select a country and year range. This visual shows both imports and exports between the US and the selected country, as well as how the trade deficit/surplus has changed over time. Countries that have deficit values that are positive indicate that the US has a trade surplus of the specified value when trading with that country. Negative values indicate a trade deficit."

ui = fluidPage(
  theme = bs_theme(
    bootswatch = "darkly",
    base_font = font_google("PT Serif")
  ),
  #carry the theme (font style) throughout the table and plots
  thematic_shiny(font="auto"),
  #set a title for the visualization
  titlePanel("Analyzing US Trade from 1995 to 2020"),
  #set up the main panel and side panel for the heatmap
  sidebarLayout(
    #the side panel will hold the box to select a year and the descriptive text
    sidebarPanel(
      #grabs the user input from a list of years (default is 2020)
      selectInput("year", "Select a Year: ", years, selected=2020, multiple=F),
      #this element is the descriptive text for the plot
      textOutput("descriptive_text_worldmap")
    ),
    #set up the main panel
    mainPanel(
      #plot output for the world heat map and make the plot taller so it isn't super stretched
      plotlyOutput("worldmap", height=600)
    )
    
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select a Country: ", unique(ustrade$Country), selected=c("Mexico"), multiple=F),
      sliderInput("year_ts", "Select a Year Range:", 1995, 2020, c(1995, 2020), sep = ""),
      textOutput("descriptive_text_ts")
    ),
    mainPanel(
      plotOutput("country_ts", height=600)
    ),
    position = "right"
  ),
  
  div(
    fluidRow(
      style="margin-top: 3em",
      h1("Total US Trade Deficit Over Time", align="center"),
      p("*Click and hold to brush plot and update the data table*", align="center")
    )
  ),
  
  #set up the UI for the total trade deficit over time
  div(
    fluidRow(
      style = "margin-top: 3em;",
      column(8, align="center", offset=2, plotOutput("ts_deficit", height=600, brush = brushOpts("plot_brush", direction="x")))
    ),
  ),
  
  #output a data table that is sorted in descending order by Total Trade
  dataTableOutput("table")
  
)


#Server 
server = function(input, output) {
  #render the world map
  output$worldmap = renderPlotly({
    #call the helper function to generate the plot with the user input specifying the year
    ggplotly(us_world_trade(input$year)) 
  }) 
  #render the plot with the US total deficit over time 
  output$ts_deficit = renderPlot({
    plot_ts_deficit()
  })
  
  #render country ts plot
  output$country_ts <- renderPlot({
    two.line.plot(input$year_ts[1], input$year_ts[2], input$country)
  })
  
  
  #handle brushing
  df_selection = reactiveVal(rep(TRUE, nrow(ustrade)))
  observeEvent(
    input$plot_brush, {
      new_values = brushedPoints(ustrade, input$plot_brush, allRows=TRUE)$selected_
      df_selection(new_values)
    }
  )
  
  #output the data table with only the brushed rows
  output$table = renderDataTable({
    ustrade %>%
      mutate(selected = df_selection()) %>%
      filter(selected) %>%
      arrange(Deficit) %>%
      select(-c(Continent, selected))
  })
  
  
  #this outputs the descriptive text for the side panel for the world map
  output$descriptive_text_worldmap = renderText({
    descriptive_text_worldmap
  })
  
  output$descriptive_text_ts = renderText({
    descriptive_text_ts
  })
  
}


#Run App
shinyApp(ui, server)

