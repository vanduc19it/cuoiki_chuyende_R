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
                    tabPanel("Scatter plot",
                             box(collapsible =TRUE,plotOutput("plot13")),
                             box(collapsible =TRUE,plotOutput("plot14")),

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
dt3 <- read.csv("C:/Users/acer/OneDrive/Documents/R-ChuyenDe/cuoiki/add_data.csv") 

server <- function(input, output) {
  
  
  
  # 1.1 Trích xuất các biến liên quan
  
  #date: ngày xảy ra war giữa UKraina với Russia
  #day: thứ tự ngày từ ngày 1...đến ngày cuối
  #aircraft: số lượng phi cơ sử dụng
  #helicopter: số lượng máy bay trực thăng sử dụng
  #tank: so lương xe tang được sử dụng, là phương tiện chiến đấu bọc thép được dùng làm vũ khí tấn công chính trong chiến đấu trên bộ,
  #APC:Armored Personnel Carrier: số lượng xe bọc thép sử dụng vận chuyển nhân viên và thiết bị trong các khu vực chiến đấu.
  #field artillery: số lượng pháo binh dã chiến sử dụng để hỗ trợ quân đội trên chiến trường
  #MRL:Multiple Rocket Launcher: số lượng hệ thống bệ phóng tên lửa sử dụng ht pháo phản lực để bắn tên lữa
  #military auto: số lượng xe quân sự sử dụng cho hoạt động và vận tải quân sự trên bộ, bao gồm cả phương tiện chiến đấu
  #fuel tank: số lượng thùng chứa nhiên liệu cho chất lỏng dễ cháy
  #drone: số lượng máy bay ko người lái được sử dụng
  #naval ship: số lượng tàu hải quân sử dụng có khả năng chống chịu thiệt hại và được trang bị hệ thống vũ khí
  #anti-aircraft warfare:số lượng hệ thống phòng không sử dụng, là bất kỳ cách chiến đấu chống lại máy bay quân sự trong chiến đấu từ mặt đất,
  #special equipment: số lượng thiết bị đặc biệt đc sử dụng vd hệ thống radar, vũ khí, vật tư quân sự và thiết bị có thể dễ dàng sử dụng cho mục đích quân sự
  #mobile SRBM system:số lượng tên lữa đạn đạo tầm ngắn sử dụng
  #greatest losses direction: nơi tổn thất lớn nhất
  #vehicles and fuel tanks: xe và thùng nhiên liệu sử dụng
  #cruise missiles:số lượng tên lữa hành trình sử dụng
  # Personnel: số lượng quân lính Nga,
  # Prisoner of War: số lượng người lính Nga bị giam giữ ngay sau một cuộc xung đột vũ trang.
  
  # Các từ viết tắt
  # APC - Tàu chở quân bọc thép,
  # Máy bay không người lái: UAV - Máy bay không người lái, RPA - Phương tiện được điều khiển từ xa,
  # MRL - Nhiều tên lửa phóng,
  # POW- Tù nhân chiến tranh,
  # SRBM - Tên lửa đạn đạo tầm ngắn.
  
  
  
  view(dt1)
  view(dt2)
  
  # show all colname in dataset
  colnames(dt1)
  
  nrow(dt1)
  ncol(dt1)
  #tom tat tap du lieu
  summary(dt1)
  
  any(is.na(dt1))
  any(is.na(dt2))
  
  
  
  
  # 1.2 Loại bỏ thay thế các giá trị missing value
  
  #Set NA to 0 in mobile SRBM system
  dt1$mobile.SRBM.system[is.na(dt1$mobile.SRBM.system)] <- 0 
  
  #Replace NA with maxmimum in POW
  dt2$POW[is.na(dt2$POW)] <- 496
  
  # Replace any other NA present with 0
  dt1[is.na(dt1)] <- 0
  dt2[is.na(dt2)] <- 0
  
  view(dt1)
  view(dt2)
  
  # 2.Thống kê các số liệu liên quan
  
  
  mergeDataset <- merge(dt3, dt1, by = "date", all.x=TRUE,all.y=TRUE)
  mergeDataset <- mergeDataset[complete.cases(mergeDataset), ]
  View(mergeDataset)

  #  find top 5 area most area by died quantity
  top5died <- head(mergeDataset[order(mergeDataset$died, decreasing = TRUE),], 5)
  top5died[c(2, 18)]
  
  # find top 5 area most area by injured quantity
  top5injured <- head(mergeDataset[order(mergeDataset$injured, decreasing = TRUE),], 5)
  top5injured[c(3, 18)]

  #(head(): show ra n rows dau tien)
  #the most died days
  day1 <- head(mergeDataset[order(mergeDataset$died, decreasing = TRUE),], 1)
  day1[c(1, 2)]
  #at least died days
  day2 <- head(mergeDataset[order(mergeDataset$died, decreasing = FALSE),], 1)
  day2[c(1, 2)]
  #the most injured days
  day3 <- head(mergeDataset[order(mergeDataset$injured, decreasing = TRUE),], 1)
  day3[c(1, 3)]
  #at least injured days
  day4 <- head(mergeDataset[order(mergeDataset$injured, decreasing = FALSE),], 1)
  day4[c(1, 3)]

  #total number of died
  sum(mergeDataset$died)

  #total number of injured
  sum(mergeDataset$injured)
  
  #total number of personel
  sum(dt2$personnel)
  
  #total number of POW
  sum(dt2$POW)
  # 
  # #select any col 
  # mergeDataset%>%select(helicopter)
  # 
  #total aircraft used
  sum(mergeDataset$aircraft)

  # total number of injured of each day and add to dataset
  mergeDataset <- mutate(mergeDataset, total = died + injured)

  # area died more than 10000 people
  mergeDataset[mergeDataset$died >= 10001, c(1,2, 18)]

  # area injured more than 100000 people
  mergeDataset[mergeDataset$injured >= 100001, c(1,3, 18)]

  
  # 2.1 Tạo bảng số liệu thống kê
  
  # thống kê số lượng sử dụng các thiết bị trên tống số tất cả tb
  a1 <-sum(dt1$helicopter) / sum(dt1$tank+dt1$helicopter+dt1$drone + dt1$naval.ship  ) * 100
  
  a2<-sum(dt1$tank) / sum(dt1$tank+dt1$helicopter+dt1$drone + dt1$naval.ship ) * 100
  
  a3<-sum(dt1$drone) / sum(dt1$tank+dt1$helicopter+dt1$drone + dt1$naval.ship ) * 100
  
  a4<-sum(dt1$naval.ship) / sum(dt1$tank+dt1$helicopter+dt1$drone + dt1$naval.ship ) * 100
  
  a5 <-sum(dt1$helicopter)
  a6<-sum(dt1$tank)
  a7<-sum(dt1$drone)
  a8<-sum(dt1$naval.ship)
  
  
  data= matrix(c(a1, a2, a3, a4), ncol=4, byrow=TRUE)
  colnames(data) <- c('tank','helicopter','drone','ship')
  
  quanity<- c(a5, a6,a7,a8)
  data1 <- rbind(data, quanity)
  data1
  rownames(data) <- c('percent')
  
  final=as.table(data)
  final
  final <- as.data.frame(table(final))
  
  
  # 2.2 Vẽ các biểu đồ minh họa
  #bieudo
  hist(mergeDataset$died)
  hist(mergeDataset$injured)
  hist(dt2$personnel)
  hist(dt2$POW)
  
  #location
  city <- unlist(strsplit(dt$greatest.losses.direction, ","))
  
  city<- gsub(" ", "", city)
  
  city<- unlist(strsplit(city, "and"))
  
  location <- as.data.frame(table(city))
  location
  location$city <- as.character(location$city)
  
  register_google(key = "AIzaSyBq9XoYLGnsq8jCToodjoHT6o-CvpirLQY", write = TRUE)
  
  latlong <- geocode(location$city)
  location <- cbind(location, latlong) %>% dplyr::rename(long = lon)
  location
  

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
    
    
    ##Scatter plot
    # vẽ biểu đồ phân tán thể hiện mối liên quan giữa 2 cột

  
    output$plot13 <- renderPlot({

      dt1 %>%
        dplyr::select(c('tank','fuel.tank'))%>%
        ggplot(aes(x=tank,y=fuel.tank))+
        geom_point()+
        geom_smooth(method = "lm",  se=T)+
        labs(title = 'TANK AND FUEL.TANK ARE POSITIVELY CORRELATED')

    }
     
    )
    output$plot14 <- renderPlot({
      mergeDataset1 <- merge(dt2, dt1, by = "date", all.x=TRUE,all.y=TRUE)
      mergeDataset1 <- mergeDataset1[complete.cases(mergeDataset1), ]
      view(mergeDataset1)
      mergeDataset1 %>%
        dplyr::select(c('personnel','aircraft'))%>%
        ggplot(aes(x=personnel,y=aircraft))+
        geom_point()+
        geom_smooth(method = "lm",  se=T)+
        labs(title = 'personnel AND aircraft are positively correlated')
      
    }
    
    )
  
    
    fuel.tank  <- dt1$fuel.tank
    tank <- dt1$tank
    
    library(ggpubr)
    ggscatter(data=dt1, x="fuel.tank", y="tank")+
      geom_smooth(method="lm", se=T)+ #đường hồi quy tuyến tính, se: sai số
      xlab("helicopter")
      ylab("tank")
    
    #phân tichs hoi quy tuyen tinh
    lm <- lm(fuel.tank~tank, data=dt1)
   
    
    summary(lm) #hiển thị kq ptich
    anova(lm) #phantich phuong sai
    
    coef(lm) #tinh héso hoi quy
    fitted.values(lm) #xác dinh gtri y du doan tuong ung gtri x
    residuals(lm) # tinh sai so
    
    library(broom)
    augment(lm)
    
    new_data<- data.frame(helicopter)
    new_data
    predict(lm, new_data) #dung predict de du doan dau ra của mo hinh hoi quy tuyen tinh lm

    
    
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
    #total days attack
    #hàm hiển thị bảng location
    output$location <- DT::renderDataTable({
      location
    })
    #hàm hiển thị bảng location
    output$abcd <- DT::renderDataTable({
      final
    })
    #hàm hiển thị cấu trúc dữ liệu
    output$cl <- renderPrint({
      str(clean_data())
    })
  
}

shinyApp(ui, server)


