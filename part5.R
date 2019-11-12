# Tan Kah Wang (29442826)
# load libraries
library(leaflet)
library(shiny)
library(ggplot2)

coral_data<-read.csv("data.csv")
names(coral_data)<-c("year","type","site","bleaching","longtitude","latitude")
# change the column "bleaching" to numeric form instead of % form
coral_data$bleaching<-as.numeric(sub("%","",coral_data$bleaching))
coral_data_type<-split(coral_data,coral_data$type)

ui <- fluidPage(
  # Application title
  headerPanel("Coral Bleaching in the Great Barrier Reef from 2010 to 2017"),
  leafletOutput("coral_map_plot"), # create map canvas on the page
  sidebarLayout(
    sidebarPanel(
      selectInput("coral_type", "Coral Type:", 
                  c("Blue Corals", 
                    "Hard Corals",
                    "Sea Fans",
                    "Sea Pens",
                    "Soft Corals")
      ),
      
      radioButtons("smoother_type", "Smoother Type:",
                   list("Linear",
                        "Polynomial",
                        "LOESS"
      ))),
    
    # Show the caption and plot of the requested coral type
    mainPanel(
      h3(textOutput("caption")),
      h3(textOutput("caption2")),
      plotOutput("myPlot")
    )
  )
)

server<-function(input, output,session) {
  
  # Return the formula text for printing as a caption
  output$caption <- reactiveText(function() {
    paste("Coral Type: ", input$coral_type)
  })
  output$caption2 <- reactiveText(function() {
    paste("Smoother Type: ", input$smoother_type)
  })
  
  output$coral_map_plot<-renderLeaflet({
    leaflet(data=coral_data) %>% addTiles() %>%
      addMarkers(
        ~longtitude,
        ~latitude,
        label = ~as.character (site),
        labelOptions = labelOptions(noHide = T,direction="left")
      )
  })
  
  # Generate a plot of the requested coral_type's bleaching against year 
  
  output$myPlot <- reactivePlot(function() {
    # check for the input variable coral_type
    if (input$coral_type == "Blue Corals") {
      new_coral_data <- coral_data_type$"blue corals"
    }
    else if (input$coral_type == "Hard Corals") {
      new_coral_data <- coral_data_type$"hard corals"
    }
    else if (input$coral_type == "Sea Fans") {
      new_coral_data <- coral_data_type$"sea fans"
    }
    else if (input$coral_type == "Sea Pens") {
      new_coral_data <- coral_data_type$"sea pens"
    }
    else {
      new_coral_data <- coral_data_type$"soft corals"
    }
    
    p <- ggplot(new_coral_data, aes(year, bleaching)) + 
      geom_point() + 
      facet_wrap(~latitude+site,ncol=8,labeller=label_context) +
      theme(strip.text.x = element_text(size = 15)) +
      ylab("% of bleaching") +
      xlab("Year")
      theme_bw()
    
    # check for the input variable smoother_type
    if (input$smoother_type == "Linear") {
      p <- p + geom_smooth(method="lm",color="blue",size=0.2,formula=y~x,se=FALSE)
    }
    else if (input$smoother_type == "Polynomial") {
      p <- p + geom_smooth(method="lm",color="blue",size=0.2,formula=y~poly(x,2),se=FALSE)
    }
    else {
      p <- p + geom_smooth(method="loess",color="blue",size=0.2,se=FALSE)
    }
    
    print(p)
  })
  
}

shinyApp(ui, server)