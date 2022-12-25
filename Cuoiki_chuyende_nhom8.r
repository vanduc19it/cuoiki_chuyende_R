library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(ggthemes)
library(tidyverse)  
library(mlbench)
library(caret)
library(xgboost)
library(randomForest)
library(cowplot)
library(corrplot) 
library(ggmap)
library(forcats)
library(reshape2)
library(viridis)
library(gridExtra)
library(maptools)
library(raster)
library(rgdal)

ui <- dashboardPage(
  dashboardHeader(title = "War Russia-Ukaraine"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "sumary", icon = icon("list-alt")),
      menuItem("Structure", tabName = "structure", icon = icon("th")),
      menuItem("Data", tabName = "data", icon = icon("th")),
      menuItem("Table", tabName = "table", icon = icon("table")),
      menuItem("Plot", tabName = "plot", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    # Body content
      tabItems(
        
        tabItem(
          tabName = "data",
            fluidPage(  
            tabBox(
              id = "tabset5",
              height = "1000px",
              width = 12,
              tabPanel(
                'Data',
                box(width=2,collapsible =TRUE,uiOutput("xa")),
                box(width=10,collapsible =TRUE,DT::dataTableOutput("data")),
                
              ),
              
            )
          ),
          
        ),
  
        tabItem(tabName = "plot",
                fluidPage(  
                  
                  tabBox(
                    id = "tabset1",
                    height = "1000px",
                    width = 12,
                    tabPanel(
                      "Hit Cities",
                      box(collapsible =TRUE,plotOutput("plot8")),
                      box(collapsible =TRUE,plotOutput("plot9")),
                    ),
                    
                    tabPanel(
                      "Loss of Equipment",
                      box(collapsible =TRUE,plotOutput("plot6")),
                      box(collapsible =TRUE,plotOutput("plot7")),
                      box(collapsible =TRUE,plotOutput("plot10")),
                      box(collapsible =TRUE,plotOutput("plot12")),
  
                    ),
                    
                    tabPanel("Death & prisoners",
                             box(collapsible =TRUE,plotOutput("plot11")),
                             box(collapsible =TRUE,plotOutput("plot5")),
                    ),
              
                  )
                ),
                
        ),

        tabItem(tabName = "sumary",
                h2("summary"),
                fluidPage(  
                  tabBox(
                    id = "tabset10",
                    height = "1000px",
                    width = 12,
                    tabPanel(
                      'Summary',
                      box(width=2, collapsible =TRUE,uiOutput("select")),
                      box(width=10,collapsible =TRUE,verbatimTextOutput("sum11")),

                    ),
                  )
                ),
   
        ),

        tabItem(tabName = "structure",
                h2("structure"),
                fluidPage(  
                  tabBox(
                    id = "tabset10",
                    height = "1000px",
                    width = 12,
                    tabPanel(
                      'Structure',
                      box(width=10, collapsible =TRUE,verbatimTextOutput("cl")),
                    
                    ),
                    
                  )
                ),
    
        ),
        tabItem(
          tabName = "table",
          fluidPage(  
            tabBox(
              id = "tabset9",
              height = "1000px",
              width = 12,
              tabPanel(
                'Table',
                
                box(width=10,collapsible =TRUE,DT::dataTableOutput("location")),
                
              ),
              
            )
          ),
          
        ),
        
        tabItem(tabName = "aboutme",
              
        )
               
        )
      )
)
  

dt1 <- read.csv("C:/Users/acer/OneDrive/Documents/R-ChuyenDe/cuoiki/equipment_Ukraine_Russia_War.csv")
dt2 <- read.csv("C:/Users/acer/OneDrive/Documents/R-ChuyenDe/cuoiki/russia_losses_personnel.csv")  
  

server <- function(input, output) {

  output$plot5 <- renderPlot(
    plot4
  )
  
  output$plot11 <- renderPlot(
    plot3
  )
  
 # plot air equipment loss
  output$plot6 <- renderPlot({
    #Air 
    melt_air <- dt %>% dplyr::select(c("air_vehicles", "drone", "date"))
    melt_air <- melt(melt_air, id = "date")
    options(repr.plot.width = 18, repr.plot.height = 8)
    
    ggplot(melt_air, aes(x = date, y = value, color = variable)) +  
      geom_line(size=1.8)+
      scale_x_date(date_breaks='1 month', date_label = "%b")+
      geom_text(data = subset(melt_air, date==max(dt$date)), aes(label=value),hjust=2,vjust=0, size=8)+
      labs(title="Air-based Equipment Loss", color=NULL)+
      theme(title= element_text(face="bold", hjust=4, size=30), 
            axis.text.x = element_text(size=18),
            axis.text.y = element_text(size=18),
            legend.position = "bottom",
            legend.text = element_text(size=18),
            panel.grid=element_line(size=0.4, color="gray", linetype=2))+
      scale_color_manual(values = c("orange2", "orange4"),
                         labels= c("Aircraft & Helicopter", "Drones"))
    
  }
    
  )
  
  # plot water equipment loss
  output$plot7 <- renderPlot({
 
    # Water
    melt_water <- dt %>% dplyr::select(c("naval.ship", "cruise.missiles", "date"))
    melt_water <- melt(melt_water, id="date")
    options(repr.plot.width=18, repr.plot.height=10)
    
    ggplot(melt_water, aes(x=date,y=value, color=variable))+
      geom_line(size=1.8)+
      scale_y_continuous( limits = c(0, max(melt_water$value)), breaks= seq(0,max(melt_water$value),by=50))+
      scale_x_date(date_breaks='1 month', date_label = "%b")+
      geom_text(data = subset(melt_water, date==max(dt$date)),
                aes(label=value,hjust=2,vjust=0),
                size=8)+
      labs(title="Water-based Equipment Loss", color=NULL)+
      theme(title= element_text(face="bold", hjust=4, size=30), 
            axis.text.x = element_text(size=18),
            axis.text.y = element_text(size=18),
            legend.position = "top",
            legend.text = element_text(size=18),
            panel.grid=element_line(size=0.4, color="gray", linetype=2))+
      scale_color_manual(values = c("orange3", "red"),
                         labels= c("Naval Ship", "Cruise Missiles"))
    
  }
  )

  # plot field-based equipment loss
  output$plot10 =  renderPlot({
    # Field
    library(viridis)
    melt_field<- dt %>% dplyr::select(c("tank", "APC", "field.artillery","MRL","anti.aircraft.warfare","vehicles.and.fuel.tanks","date"))
    melt_field <- melt(melt_field, id="date")
    
    options(repr.plot.width=18, repr.plot.height = 12)
    ggplot(melt_field, aes(x=date,y=value, color=variable))+
      geom_line(size=1.8)+
      geom_text(data = subset(melt_field, date==max(dt$date)),
                aes(label=value,hjust=1,vjust=-0.5), 
                size=8)+
      labs(title="Field-based Equipment Loss",color=NULL)+
      scale_y_continuous( limits = c(0, max(melt_field$value)), breaks= seq(0,max(melt_field$value),by=500))+
      scale_x_date(date_breaks= "1 month", date_label = "%b")+
      theme(title= element_text(face="bold", hjust=-0.5, size=30),
            axis.text.x = element_text(size=18),
            axis.text.y = element_text(size=18),
            legend.position = "bottom",
            legend.text = element_text(size=18),
            panel.grid=element_line(size=0.4, color="gray", linetype=2))+
      scale_color_brewer(type= "qual", palette = "Dark2")
  })
  
  # plot special equipment loss
  output$plot12 =  renderPlot({
    ggplot(dt, aes(x= date,y= special.equipment)) +
      geom_line(size=1.8, color="red", alpha=0.5)+
      labs(title="Special Equipment Loss",x="",y="",color=NULL)+
      geom_text(aes(x=max(dt$date), y=max(dt$special.equipment), label = max(dt$special.equipment)), size=8, vjust=-1)+
      theme(title= element_text(face="bold", hjust=-0.5, size=30),
            axis.text.x = element_text(size=18),
            axis.text.y = element_text(size=18),
            legend.position = "bottom",
            legend.text = element_text(size=18),
            panel.grid=element_line(size=0.4, color="gray", linetype=2))+
      scale_color_brewer(type= "qual", palette = "Dark2")+
      scale_x_date(date_breaks= "1 month", date_label = "%b")
   
  })
  
  
    head(dt1)
    head(dt2)
    
    
    any(is.na(dt1))
    any(is.na(dt2))
    

    #Set NA to 0 in mobile SRBM system
    dt$mobile.SRBM.system[is.na(dt$mobile.SRBM.system)] <- 0 
    
    #Replace NA with maxmimum in POW
    dt$POW[is.na(dt$POW)] <- 496
    
    # Replace any other NA present with 0
    dt[is.na(dt)] <- 0
    
    #location
    city <- unlist(strsplit(dt$greatest.losses.direction, ","))
    city<- gsub(" ", "", city)
    city<- unlist(strsplit(city, "and"))
    location <- as.data.frame(table(city))
    location$city <- as.character(location$city)
    
    register_google(key = "AIzaSyBq9XoYLGnsq8jCToodjoHT6o-CvpirLQY", write = TRUE)
    
    latlong <- geocode(location$city)
    location <- cbind(location, latlong) %>% dplyr::rename(long = lon)
    location
    
    #hit cities
    Ukraine<-getData("GADM", country="UA", level=0)
    Ukraine1<-getData("GADM", country="UA", level=1)
    
    options(repr.plot.width=20, repr.plot.height = 20)
    
    
    plot200 <- ggplot()+
      geom_polygon(data=Ukraine1, aes(long, lat, group=group), color="white",fill="dodgerblue1", alpha=0.8)+
      geom_point(data=location, aes(long,lat), color="red", size=6)+
      geom_text(data=location, aes(label=city, x= long, y=lat), size=7, color="black", fontface="bold", check_overlap=TRUE)+
      labs(title="Ukraine : Cities Hit by Russia", x="", y="")+
      theme(axis.text.x = element_text(size=18),
            axis.text.y = element_text(size=18),
            title= element_text(size=30, face="bold"))
    plot100 <- ggplot(location, aes(Freq, reorder(city, Freq), alpha=0.1))+
      geom_col(size=1, col="red")+
      labs(title="Number of Times Attacked", x="", y="")+
      theme(axis.text.x = element_text(size=18),
            axis.text.y = element_text(size=18),
            title= element_text(size=30, face="bold"),
            legend.position = "none")+
      scale_x_continuous(breaks = seq(0, max(dt$special.equipment), by=10))
    
    grid.arrange(plot2, plot1, nrow = 2)
    

    #death toll
    options(repr.plot.width=20, repr.plot.height = 12)
    plot3 <- dt %>% 
      group_by(month = lubridate::floor_date(date, "month")) %>%
      summarise(total=max(personnel)) %>%
      ggplot(., aes(x=month, y=total, alpha=0.1))+
      geom_col(aes(color=total), size=1.5)+
      geom_line(color="red", size = 1.5, linetype=2)+
      labs(title="Death Toll", x="", y="")+
      geom_text(aes(y=total,label=total), vjust=-0.8, size=8)+
      scale_x_date(date_breaks = '1 month', date_labels = "%b")+
      theme(panel.grid.major = element_line(size=0.2, color="gray3", linetype=2),
            axis.text.x = element_text(size=18),
            axis.text.y = element_text(size=18),
            title = element_text(size=30, face="bold"),
            legend.position="none")+
      scale_color_steps(low = "orange", high = "red3")
    
    #prisoners
    plot4 <- dt %>% 
      group_by(month = lubridate::floor_date(date, "month")) %>%
      summarise(total=max(POW)) %>%
      
      ggplot(., aes(x=month, y=total))+
      geom_col(alpha=0.4, col="red", size=1.5)+
      geom_line(color="red", size=2, linetype=2) + 
      geom_text(aes(y=total,label=total),vjust=-0.8, size=8)+
      labs(title="Russian War Prisoners", x="", y="")+
      scale_x_date(date_breaks = '1 month', date_labels = "%b")+
      scale_y_continuous(limits = c(0, 800), breaks= seq(0,800,by=100))+
      theme(panel.grid.major = element_line(size=0.2, color="gray3", linetype=2),
            axis.text.x = element_text(size=18),
            axis.text.y = element_text(size=18),
            title = element_text(size=30, face="bold"),
            legend.position="none")+
      scale_color_steps(low = "orange", high = "red3")
    
    grid.arrange(plot3, plot4, ncol=2)
    
    output$plot8 <- renderPlot(
      plot200
    )
    output$plot9 <- renderPlot(
      plot100
    )
    
    #làm sạch dữ liệu
    clean_data <- reactive({
      na.omit(dt1)
    })
    
    #dữ liệu chứa các cột là số
    data_num <- reactive({
      data_num <- select_if (clean_data(), is.numeric) 
    })
    #hàm trả về checkbox tab summary
    output$select <- renderUI({
      checkboxGroupInput("show_vars", "Columns in data to show:", names(clean_data()), selected = names(clean_data()))
    })
    #hàm trả về checkbox tab data
    output$xa <- renderUI({
      checkboxGroupInput("show_vars1", "Columns in data to show:", names(clean_data()), selected = names(clean_data()))
    })
    #hàm tóm tắt dữ liệu
    output$sum11 <- renderPrint({
      summary(clean_data()[, input$show_vars, drop = FALSE])
    })
    
    #hàm hiển thị dữ liệu dạng bảng
    output$data <- DT::renderDataTable({
      clean_data()[, input$show_vars1, drop = FALSE]
    })
    #hàm hiển thị bảng location
    output$location <- DT::renderDataTable({
      location
    })
    #hàm hiển thị cấu trúc dữ liệu
    output$cl <- renderPrint({
      str(clean_data())
    })
  
}

shinyApp(ui, server)


