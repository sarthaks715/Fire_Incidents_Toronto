#install.packages("shiny")
install.packages("shinydashboard")
#install.packages("ggplot2")
install.packages("tidyverse")
install.packages("tidyr")
#install.packages("dplyr")
install.packages("plotly")
install.packages("viridis")
install.packages("quantmod")
install.packages("gganimate")
install.packages("sunburstR")
install.packages("gifski")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)
library(viridis)
library(quantmod)
library(gganimate)
library(sunburstR)
library(gifski)

#reading the data
data <- read.csv("Fire Incidents Data.csv")
#creating a anew column of dollar loss slabs according to estimated dollar loss column
data$new_col <- ifelse(data$Estimated_Dollar_Loss>=0 & data$Estimated_Dollar_Loss<100, "$0 to $100",
                ifelse(data$Estimated_Dollar_Loss>=100 & data$Estimated_Dollar_Loss<=500, "$100 to $500",
                ifelse(data$Estimated_Dollar_Loss>500 & data$Estimated_Dollar_Loss<=2500, "$500 to $2500",
                ifelse(data$Estimated_Dollar_Loss>2500 & data$Estimated_Dollar_Loss<10000, "$2500 to $10000",
                ifelse(data$Estimated_Dollar_Loss>10000 & data$Estimated_Dollar_Loss<=50000, "$10000 to $50000",
                ifelse(data$Estimated_Dollar_Loss>=50000 , "more than $50000",
                NA))))))

#cleaning the data (seprating the columns)
data <- separate(data,Fire_Alarm_System_Presence,into=c("Fire_Alarm_System_Presence_code","Fire_Alarm_System_Presence"),sep="-")
new <- data %>% select(Fire_Alarm_System_Presence,new_col,X_id)

#making a new datagraph for animated graph
animated <- data.frame(data %>% group_by(new_col,Fire_Alarm_System_Presence) %>% summarise(freq=length(X_id)))

# Fire alarm present dataframefor animation graph
ss<- animated[animated$Fire_Alarm_System_Presence=="  Fire alarm system present",]

#deleted the NA's from teh dataframe
ss <- na.omit(ss)

#new column of transition revel used in the animated graph
ss$new2_col <- ifelse(ss$new_col=="$0 to $100" ,1,
               ifelse(ss$new_col=="$100 to $500" ,2,
               ifelse(ss$new_col=="$500 to $2500" ,3,
               ifelse(ss$new_col=="$2500 to $10000" ,4,
               ifelse(ss$new_col=="$10000 to $50000" ,5,
               ifelse(ss$new_col=="more than $50000" ,6,NA))))))

#chaning the column type
ss$new2_col <- as.double(ss$new2_col)


ss$new_col <- factor(ss$new_col, levels = ss$new_col[order(ss$new2_col)])
  
ss$Fire_Alarm_System_Presence <- as.character(ss$Fire_Alarm_System_Presence)  


#animated graph for presence of fire alarm 
z <- ss %>%
  ggplot( aes(x=new_col, y=freq, group=Fire_Alarm_System_Presence, color=Fire_Alarm_System_Presence)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Fire Alarm Presence Effect") +
  ylab("Count of fire incidents") +
  xlab("Dollar Loss Slabs") +
  transition_reveal(new2_col) + 
  theme(panel.background = element_rect(fill = "lightblue",
  colour = "black",size = 0.5,linetype = "solid"),
  axis.text.x = element_text(size = 8, angle = 45,hjust=1),
  axis.text.y = element_text(size = 8), legend.position = "none")
animate(z, duration = 5, fps = 20, width = 200, height = 200, renderer = gifski_renderer())
anim_save("outfile.gif") 
  yes <- list(src = "outfile.gif",
         contentType = 'image/gif',
         width = 700,
         height = 500
  )  
  

#Fire alarm not present 
  
c<-animated[!animated$Fire_Alarm_System_Presence == "  Fire alarm system present",]
cnew <- data.frame(c %>% group_by(new_col) %>% summarise(freq = sum(freq)))
cnew$Fire_Alarm_System_Presence <- "No fire alarm present"
cnew <- na.omit(cnew)

#new column of transition revel used in the animated graph
cnew$new2_col <- ifelse(cnew$new_col=="$0 to $100" ,1,
                 ifelse(cnew$new_col=="$100 to $500" ,2,
                 ifelse(cnew$new_col=="$500 to $2500" ,3,
                 ifelse(cnew$new_col=="$2500 to $10000" ,4,
                 ifelse(cnew$new_col=="$10000 to $50000" ,5,
                 ifelse(cnew$new_col=="more than $50000" ,6,NA))))))

cnew$new_col <- factor(cnew$new_col, levels = cnew$new_col[order(cnew$new2_col)])
  
cnew$new2_col <- as.double(cnew$new2_col)

cnew <- cnew[order(cnew$new2_col),]

cnew$Fire_Alarm_System_Presence <- as.character(cnew$Fire_Alarm_System_Presence)  

#animated graph for no presence of fire alarm 
j <- cnew %>%
    ggplot( aes(x=new_col, y=freq, group=Fire_Alarm_System_Presence, color=Fire_Alarm_System_Presence)) +
    geom_line() +
    geom_point() +
    scale_color_viridis(discrete = TRUE) +
    ggtitle("Fire Alarm Absence") +

    ylab("Count of fire incidents") +
    xlab("Dollar Loss Slabs") +
    transition_reveal(new2_col) + 
    theme(panel.background = element_rect(fill = "lightblue",
    colour = "black",size = 0.5,linetype = "solid"),
    axis.text.x = element_text(size = 8, angle = 45,hjust=1),
          axis.text.y = element_text(size = 8),legend.position = "none")
animate(j, duration = 5, fps = 20, width = 200, height = 200, renderer = gifski_renderer())

 anim_save("outfile2.gif") 
  no <- list(src = "outfile2.gif",
              contentType = 'image/gif',
              width = 700,
              height = 500
  )  
  
#ui server creating dashboard
ui <- dashboardPage(skin="red",
  dashboardHeader(title = "Fire incidents"),
  dashboardSidebar( sidebarMenu(
  menuItem("SunBurst", tabName = "dashboard", icon = icon("sun")),
  menuItem("Heatmap", tabName = "streamgraph", icon = icon("stream")),
  menuItem("animation", tabName = "animation", icon = icon("chart-line"))
    
  )),
dashboardBody(
tabItems(tabItem(tabName = "dashboard",
fluidRow(
#plot of sunburst visualisation     
sunburstOutput("plot1")),
fluidRow(
  h1("Sunburst to show the effect of Fire Alarm System and its working on Evacuation of people (Hover inside out to 
     find out the relationship in %)",align="center"))),
#tab for heatmap graph    
tabItem(tabName = "streamgraph",
      fluidRow(
      plotlyOutput("plot2"))
      ,
      fluidRow(
        h1("Showing the count of the Fire Incidents according to Daywise and hour wise",align="center"))),
#tab for animated visualisation
tabItem(tabName = "animation", sidebarLayout(sidebarPanel(div(style="display: inline-block;vertical-align:top;width: 200px;",
            #setting the style of the selectinput
            selectInput("variable", "Fire Alaram System:", 
            c("Fire Alaram Present" = "y", 
            "Fire Alaram Absent" = "n"
                )
                ))),
    fluidRow( 
    imageOutput("plot3")))
      ))))
      
#server
server <- function(input,output) {
#sunburst plot
output$plot1 <- renderSunburst({ 
#data read    
data <- read.csv("Fire Incidents Data.csv")
#data cleaning    
data <- data[!(is.na(data$Fire_Alarm_System_Presence) | data$Fire_Alarm_System_Presence==""), ]
data <- data[!(is.na(data$Fire_Alarm_System_Operation) | data$Fire_Alarm_System_Operation==""), ]
data <- data[!(is.na(data$Fire_Alarm_System_Impact_on_Evacuation) | data$Fire_Alarm_System_Impact_on_Evacuation==""), ]
data <- data[!(data$Fire_Alarm_System_Presence=="Unknown"),]
data <- data[!(data$Fire_Alarm_System_Operation=="Unknown"),]
data <- data[!(data$Fire_Alarm_System_Impact_on_Evacuation=="Unknown"),]
#seprating the column using the seprator
data <- separate(data,Fire_Alarm_System_Presence,into=c("Fire_Alarm_System_Presence_Code","Fire_Alarm_System_Presence"),sep="-")
data <- separate(data,Fire_Alarm_System_Operation,into=c("Fire_Alarm_System_Operation_Code","Fire_Alarm_System_Operation"),sep="-")
data <- separate(data,Fire_Alarm_System_Impact_on_Evacuation,into=c("Fire_Alarm_System_Impact_on_Evacuation_Code","Fire_Alarm_System_Impact_on_Evacuation"),sep="-")

#making new dataframe for sunburst graph    
new2 <- data %>% select(Fire_Alarm_System_Presence,Fire_Alarm_System_Operation,Fire_Alarm_System_Impact_on_Evacuation
                           ,X_id)

sungraph <- data.frame(new2 %>% group_by(Fire_Alarm_System_Impact_on_Evacuation,Fire_Alarm_System_Operation,Fire_Alarm_System_Presence) %>% summarise(freq=length(X_id)))
    
sungraph <- sungraph %>% filter(Fire_Alarm_System_Presence != "") %>% mutate(path = paste(Fire_Alarm_System_Presence,Fire_Alarm_System_Operation, Fire_Alarm_System_Impact_on_Evacuation,sep="-")) %>%
            dplyr::select(path, freq)
    
p <- sunburst(sungraph, legend=FALSE)
    
  
   
  })
#heatmap plot  
output$plot2 <- renderPlotly({  
  data <- read.csv("Fire Incidents Data.csv")
data <- separate(data,TFS_Alarm_Time,into=c("Date","tfs_time_arrival"),sep="T")
#making a new column of weekdays using date column
data$weekday <- weekdays(as.Date(data$Date))
#making a new column of hour using time column
data$hour<-format(strptime(data$tfs_time_arrival,"%H:%M:%S"),'%H')
graph <- data.frame(data %>% group_by(weekday,hour) %>% summarise(freq=length(X_id)))



graph <- graph %>%
  mutate(text = paste0("Hour: ", hour, "\n", "Day: ", weekday, "\n", "Value: ",round(freq,2)))

#heatmap
fp <- ggplot(graph, aes(hour, weekday, fill= freq, text=text)) + 
  geom_tile() 

ggplotly(fp, tooltip="text")
  }
  )

#plot of animation graph  
output$plot3 <- renderImage({  
  if(input$variable == "y") {
    yes
      
    }
  else if(input$variable == "n") {
    no
    }
    
},deleteFile = FALSE)
  
}

#run app  
shinyApp(ui, server)

