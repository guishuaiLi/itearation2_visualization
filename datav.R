#   df <- read.csv(file="desktop/deGenes.csv", header=T)
#   df
#  library(ggplot2)
#   install.packages("fansi")
# magrittr
# df <- read.csv(file="desktop/5122/data1_area_2.csv", header=T)

install.packages(c('DBI','RMySQL'))         
library(DBI)

library(pool)
con <- DBI::dbConnect(RMySQL::MySQL(), dbname = 'db-ta18', host = "100.27.49.207", port = 3306, user = "admin", password = "19980525Crncyh.")
library(RMySQL)

# use root to connect the database
channel <- dbConnect(MySQL(),
                     user="admin", #user name
                     password="19980525Crncyh.", #password
                     dbname="TA18-db", #database name
                     host="db-ta18.cprsbnope6tt.us-east-1.rds.amazonaws.com") #host
dbGetQuery(channel, "show databases;")
dbListTables(channel)  # list data
dbGetInfo(channel) # check the information from database
result = dbSendQuery(channel, "select * from falls_by_location") # 查询 interactions_test 表，
data.frame = fetch(result)  # get data
###数据可视化
### 画图1 摔倒人数与地点跟年龄的关系
data_1 <- data.frame[data.frame$Gender=="Persons",]

ggplot(data_1, aes(x = Location, y = Num_falls, fill = Age)) +
  # 条形图函数：position设置条形图类型为簇状
  geom_bar(position = "dodge", stat = "identity")

ggplot(data.frame, aes(x = Location, y = Num_falls))

install.packages(c('shiny','shinyWidgets'))

library(shiny)
library(shinyWidgets)

ui<- fluidPage(
  sidebarLayout(
    actionBttn(
      inputId = "bttn",
      label = "APPLY!",
      style = "pill", 
      color = "danger"
    ),
    textOutput("click_times")
    ),
  sidebarLayout(
    actionBttn(
      inputId = "bttn",
      label = "APPLY!",
      style = "pill", 
      color = "danger"
    ),
    textOutput("click_times")
  )
)


server <- function(input, output){
  
  bttn_click <- 0
  bttn_text <- NULL
  output$click_times <- renderText({
    if(input$bttn > bttn_click){
      bttn_click  <<-  bttn_click + 1
      bttn_text <<- paste("Click times =", input$bttn)
    }
    bttn_text
  })
}

shinyApp(ui, server)


result1 = dbSendQuery(channel, "select * from falls_by_location2") 
data.frame = fetch(result1)  # 获取数据
data.frame$Gender
data.frame$Location
data.frame$Total
ndb<-data.frame[, c("Gender", "Location", "Total")]
install.packages("plotly")
install.packages("dplyr")
library(dplyr)
library(plotly)
ndb<-distinct(ndb)
ndb<-ndb[c(19:27),]
ndb$Location<-factor(ndb$Location,
                 levels=c("Driveway","Outdoor areas","Garage","Bathroom","Kitchen","Bedroom","Laundry","Indoor living areas","Other and unspecified in the home"),
                 labels=c("Driveway","Outdoor","Garage","Bathroom","Kitchen","Bedroom","Laundry","Indoor","Others")
)
o_status ="success"

#选取颜色
install.packages(RColorBrewer)
library(RColorBrewer)
axis=list(tickfont = list(
  family = "Times New Roman",# 设置字体
  size = 0.5,                  # 字体大小
  color = "black" ), 
  tick0 = 0,                                   # 当tickmode = "linear"时，设置第一个刻度线的位置
          dtick = 1)                                # 设置刻度线的间隔，与tick0配合使用)
fig <- plot_ly(ndb, x=~Location, y=~Total, names='Total', type='bar',color =brewer.pal(9,'BuGn'),alpha = 0.7, showlegend = FALSE) %>% layout(xaxis = axis)
fig

result2= dbSendQuery(channel, "select * from falls_byInjury") 
data2 = fetch(result2)  # 获取数据
data2<-data2[c(127:189),]

###图2
result3= dbSendQuery(channel, "select * from falls_byInjury2") 
data3 = fetch(result3)  # 获取数据
data3<-data3[c(113:168),]
A1<-data3[which(data3$Age == "65-69"),]
A2<-data3[which(data3$Age == "70-74"),]
A3<-data3[which(data3$Age == "75-79"),]
A4<-data3[which(data3$Age == "80-84"),]
A5<-data3[which(data3$Age == "85-89"),]
A6<-data3[which(data3$Age == "90-94"),]
A7<-data3[which(data3$Age == "95+"),]
A1$Injury
data3 <- group_by(data3,Injury)
sum <- summarise(data3, Total =sum(Num_falls))
a<-right_join(data3, sum, by = "Injury")
a<-a[, c("Gender", "Injury", "Total")]
a<-distinct(a)
a$Injury<-factor(a$Injury,
                     levels=c("Fracture","Dislocation","Soft-Tissue Injury","Open Wound","Intracranial Injury",
                              "Internal organ or vessel of trunk","Superficial injury","Other Unspecific injuries"),
                     labels=c("Fracture","Dislocation","Soft-Tissue","Open Wound","Intracranial",
                              "Internal","Superficial","Others")
)
axis=list(tickfont = list(
  family = "Times New Roman",# 设置字体
  size = 0.01,                  # 字体大小
  color = "black" ), 
  tick0 = 0,                                   # 当tickmode = "linear"时，设置第一个刻度线的位置
  dtick = 1)                                # 设置刻度线的间隔，与tick0配合使用)

fig1 <- plot_ly(a, x=~Injury, y=~Total, names='Total', type='bar',color =brewer.pal(8,'BuGn'),alpha = 0.7,width =2, showlegend = FALSE) %>% layout(xaxis = axis)
fig1

p <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) %>%
  add_trace(
    r = A1$Num_falls,
    theta = A1$Injury,
    name = A1$Age
  ) %>%
  add_trace(
    r = A2$Num_falls,
    theta = A2$Injury,
    name = A2$Age
  ) %>%
  add_trace(
    r = A3$Num_falls,
    theta = A3$Injury,
    name = A3$Age
  ) %>%
  add_trace(
    r = A4$Num_falls,
    theta = A4$Injury,
    name = A4$Age
  ) %>%
  add_trace(
    r = A5$Num_falls,
    theta = A5$Injury,
    name = A5$Age
  ) %>%
  add_trace(
    r = A6$Num_falls,
    theta = A6$Injury,
    name = A6$Age
  ) %>%
  add_trace(
    r = A7$Num_falls,
    theta = A7$Injury,
    name = A7$Age
  ) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = TRUE
      )
    )
  )
p


#图3
result4= dbSendQuery(channel, "select * from falls_byBodyPart3") 
data4 = fetch(result4)  # 获取数据
data3<-data3[c(169:252),]
A1<-data4[which(data4$Age == "65-69"),]
A2<-data4[which(data4$Age == "70-74"),]
A3<-data4[which(data4$Age == "75-79"),]
A4<-data4[which(data4$Age == "80-84"),]
A5<-data4[which(data4$Age == "85-89"),]
A6<-data4[which(data4$Age == "90-94"),]
A7<-data4[which(data4$Age == "95+"),]

p <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) %>%
  add_trace(
    r = A1$Num_falls,
    theta = A1$Body_Part,
    name = A1$Age
  ) %>%
  add_trace(
    r = A2$Num_falls,
    theta = A2$Body_Part,
    name = A2$Age
  ) %>%
  add_trace(
    r = A3$Num_falls,
    theta = A3$Body_Part,
    name = A3$Age
  ) %>%
  add_trace(
    r = A4$Num_falls,
    theta = A4$Body_Part,
    name = A4$Age
  ) %>%
  add_trace(
    r = A5$Num_falls,
    theta = A5$Body_Part,
    name = A5$Age
  ) %>%
  add_trace(
    r = A6$Num_falls,
    theta = A6$Body_Part,
    name = A6$Age
  ) %>%
  add_trace(
    r = A7$Num_falls,
    theta = A7$Body_Part,
    name = A7$Age
  ) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = TRUE
      )
    )
  )
p

data4 <- group_by(data4,Body_Part)
sum <- summarise(data4, Total =sum(Num_falls))
bb<-right_join(data4, sum, by = "Body_Part")
bb<-bb[, c("Gender", "Body_Part", "Total")]
bb<-distinct(bb)
bb<- bb[c(25:36),]
bb$Body_Part<-factor(bb$Body_Part,
                     levels=c("Head","Neck","Thorax","Abdomen/lower back/pelvis","Upper arm","Forearm","Wrist and hand",
                              "Hip and thigh","Knee and lower limb","Ankle and foot","Other, multiple and incompletely specified body regions"),
                     labels=c("Head","Neck","Thorax","back*","Upper arm","Forearm","Wrist",
                              "Thigh","Knee","Ankle","Others")
)
bb<- bb[c(1:11),]
axis=list(tickfont = list(
  family = "Times New Roman",# 设置字体
  size = 0.5,                  # 字体大小
  color = "black" ), 
  tick0 = 0,                                   # 当tickmode = "linear"时，设置第一个刻度线的位置
  dtick = 1)                                # 设置刻度线的间隔，与tick0配合使用)
fig2 <- plot_ly(bb, x=~Body_Part, y=~Total, names='Total', type='bar',color = I("green4"),alpha = 0.7,width =2) %>% layout(xaxis = axis)
fig2
###数据可视化 
###1.雷达图
install.packages("fmsb")
library(fmsb)
radarchart(data_1)

