library(rvest)
library(XML)
library(magrittr)
library(bitops)
library(httr)
library(RCurl)
library(ggplot2)
library(scales) 
library(RColorBrewer)


mypath <- "/Users/joshchang1112/Desktop/cs-x-programming/Final project/"
all_data <- matrix(ncol=10,nrow=0)
# 結合多張圖的函示(擷取自網路)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) { 
  library(grid) 
  
  # Make a list from the ... arguments and plotlist 
  plots <- c(list(...), plotlist) 
  
  numPlots = length(plots) 
  
  # If layout is NULL, then use 'cols' to determine layout 
  if (is.null(layout)) { 
    # Make the panel 
    # ncol: Number of columns of plots 
    # nrow: Number of rows needed, calculated from # of cols 
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), 
                     ncol = cols, nrow = ceiling(numPlots/cols)) 
  } 
  
  if (numPlots==1) { 
    print(plots[[1]]) 
    
  } else { 
    # Set up the page 
    grid.newpage() 
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout)))) 
    
    # Make each plot, in the correct location 
    for (i in 1:numPlots) { 
      # Get the i,j matrix positions of the regions that contain this subplot 
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE)) 
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, 
                                      layout.pos.col = matchidx$col)) 
    } 
  } 
}  
# 爬蟲1 抓取個年度前100名資料 

for (tyear in 2013:2017){
  stockURL <- paste("http://www.boxofficemojo.com/yearly/chart/?page=1&view=releasedate&view2=domestic&yr=",tyear,"&p=.htm",sep="")
  
  sdat1 <- html(stockURL)  %>% 
    html_nodes("table") %>%
    extract2(4) %>%
    html_nodes("td") %>%
    html_text()
  temp <- matrix(sdat1[15:914],nrow=100,byrow = TRUE)
  
  all_temp <- cbind(rep(tyear,100),temp)
  
  all_data <- rbind(all_data,all_temp)
}

# 資料框整理1 為欄位命名
BoxofficeDF <- data.frame(all_data)
colnames(BoxofficeDF) <- c("Year","Yearly_Ranking","Title","Studio","Total_Gross","Number_of_Theaters","Opening_Gross","Number_of_Opening_Theaters","Open_Date","End_Date")




data <- list()
title <- list()
date <- list()
url.hyper <- list()

# 爬蟲2 爬標題跟連結網址
for(year in c(2013:2017)){
  url <- paste("http://www.boxofficemojo.com/yearly/chart/?yr=",year,"&p=.htm",sep="")
  html <- htmlParse(GET(url),encoding = "UTF-8")
  title.list <- xpathSApply(html, "//a[@href]", xmlValue)
  url.list <- xpathSApply(html, "//a[@href]", xmlAttrs)
  title <- rbind(title, as.matrix(title.list))
  url.hyper <- rbind(url.hyper, as.matrix(url.list))
}
title
title <- unlist(title)
url.hyper
hyperlink <- unlist(url.hyper)
title
hyperlink
#hyperlink


title.movie <- c()
# 51 432 814

# 資料整理1 取出5年的標題
for(i in 51:348){
  if((i - 51) %% 3 == 0){
    title.movie <- c(title.movie, title[i])
  }
}
for(i in 432:729){
  if((i - 432) %% 3 == 0){
    title.movie <- c(title.movie, title[i])
  }
}
for(i in 814:1111){
  if((i - 814) %% 3 == 0){
    title.movie <- c(title.movie, title[i])
  }
}
for(i in 1196:1493){
  if((i - 1196) %% 3 == 0){
    title.movie <- c(title.movie, title[i])
  }
}
for(i in 1578:1875){
  if((i - 1578) %% 3 == 0){
    title.movie <- c(title.movie, title[i])
  }
}


title.movie

# 資料整理2 找出超連結
hyperlink <- as.character(hyperlink)
hyperlink_url <- "http://www.boxofficemojo.com" 
HYPER <- c()
count = 1
i = 55
while(count <= 500){
  HYPER <- c(HYPER, paste(hyperlink_url,hyperlink[i],sep = ""))
  if(i == 352){
    i = 437
  }
  else if(i == 737){
    i = 823
  }
  else if(i == 1123){
    i = 1209
  }
  else if(i == 1509){
    i = 1595
  }
  i <- i + 3
  count <- count + 1
}
HYPER <- HYPER[1:500]

HYPER

# 爬蟲3 爬超連結裡的各種資料
genre <- list()
for(i in c(1:500)){
  html <- htmlParse(GET(HYPER[i]),encoding = "UTF-8")
  genre.list <- xpathSApply(html, "//td[@valign='top']", xmlValue)
  genre <- rbind(genre, as.matrix(genre.list))
}
genre <- unlist(genre)

#genre(資料太大 會造成電腦過熱當機)

# 資料框整理2 為3種欄位做清洗 分別為種類 分級 時間長度
count = 1
i = 6
genre.new <- c()
runtime.new <- c()
rating.new <- c()

genre.new <- c(genre.new,genre[grep("^Genre:",genre)])

genre.new

if(grep("^Runtime:",genre)){
  runtime.new <- c(runtime.new,genre[grep("^Runtime:",genre)])
}
runtime.new

if(grep("^MPAA",genre)){
  rating.new <- c(rating.new,genre[grep("^MPAA",genre)])
}
rating.new

genre.new <- gsub("Genre: ", "", genre.new)
runtime.new <- gsub("Runtime: ", "", runtime.new)
rating.new <- gsub("MPAA Rating: ", "", rating.new)
genre.new
runtime.new
rating.new



write.table(BoxofficeDF,file=paste0(mypath, "BoxofficeDF.csv"),row.names=FALSE,sep=",")
write.table(genre.new,file=paste0(mypath, "Genre.csv"),row.names=FALSE,sep=",")
write.table(runtime.new,file=paste0(mypath, "RunningTime.csv"),row.names=FALSE,sep=",")
write.table(rating.new,file=paste0(mypath, "Rating.csv"),row.names=FALSE,sep=",")


# 資料框整理3 上映時間 結束時間:計算總上映天數
data <-read.table(paste0(mypath, "BoxofficeDF2.txt"), header=T,sep="\t", fileEncoding = "utf-16",quote = "")
data$Open_Date <- BoxofficeDF$Open_Date
data$End_Date <- BoxofficeDF$End_Date

Open_Date <- strsplit(as.character(data$Open_Date), split = '/', fixed = T)
Open_Date <- as.data.frame(Open_Date)
Open_Date <- t(Open_Date)
Open_Date[,1]
Open_Month <- Open_Date[,1]
data$Open_Month <- as.numeric(Open_Month)

End_Date <- strsplit(as.character(data$End_Date), split = '/', fixed = T)
End_Date <- as.data.frame(End_Date)
End_Date <- t(End_Date)
End_Date[,1]
End_Month <- End_Date[,1]
data$End_Month <- as.numeric(End_Month)

exactly_Date <- paste(as.character(data$Year),as.character(data$Open_Date),sep = "/")

# 確切上映時間
exactly_Date


exactly_End_Date <- c()

# 確切結束上映時間
for(i in c(1:500)){
  if(is.na(data$End_Month[i]) == TRUE){
    next
  }
  if(data$End_Month[i] < data$Open_Month[i]){
    exactly_End_Date[i] <- paste(as.character(data$Year[i] + 1),as.character(data$End_Date[i]),sep = "/")
  }
  else{
    exactly_End_Date[i] <- paste(as.character(data$Year[i]),as.character(data$End_Date[i]),sep = "/")
  }
  
}


exactly_End_Date

#確切上映天數
data$Playing_day <- as.Date(exactly_End_Date) - as.Date(exactly_Date)

# 不巧有負值 幫他加365
data[415,"Playing_day"] <- data[415,"Playing_day"] + 365


# 資料框整理4 將特殊符號移除
whichColumn=list(5,6,7,8)

for(i in whichColumn){
  data[ ,i] <- as.character(data[ ,i])
  data[ ,i] <- gsub("\\$","", data[ ,i])
  data[ ,i] <- gsub(",","", data[ ,i])
  data[ ,i] <- gsub(" ","", data[ ,i])
  data[ ,i] <- gsub('\\"',"", data[ ,i])
  data[ ,i] <- as.numeric(data[ ,i])
}

for(i in 1:500){
  if(data$Studio[i]=="WB (NL)"){
    data$Studio[i]<-"WB"
  }
}

# 資料框整理5 將時間統一為 分鐘
hours <- c()
mins <- c()


time <- strsplit(as.character(data$Running.time), split = " hrs. ", fixed = T)
#time

for(i in c(1:500)){
  mins <- c(mins, as.numeric(strsplit(time[[i]][2], split = " min.", fixed = T)))
}
#mins

for(i in c(1:500)){
  hours <- c(hours, as.numeric(time[[i]][1]))
}
#hours

total.time <- hours*60 + mins
data$Running.time <- total.time
data$Running.time
str(data)

#######新變量##########
data[data$Running.time < 90, "Running.length"] <- "short"
data[data$Running.time >= 90 & data$Running.time < 120, "Running.length"] <- "medium"
data[data$Running.time >= 120 & data$Running.time < 150, "Running.length"] <- "medium long"
data[data$Running.time >= 150, "Running.length"] <- "long"
data$Running.length <- factor(data$Running.length, levels = c("long", "medium long", "medium", "short"))

data$Gross_level <- as.integer(data$Total_Gross/100000000)

#排名強度
for(i in c(1:500)){
  if(data$Yearly_Ranking[i] <= 40){
    data$Ranking[i] <- as.integer((data$Yearly_Ranking[i] - 1)/10) + 1}
  else{
    data$Ranking[i] <- 5
  }
   
}
data$Open_Month = as.character(data$Open_Month)
str(data)
data[(data[,"Open_Month"] == 1), "Open_Month"] <- "Jan"
data[(data[,"Open_Month"] == 2), "Open_Month"] <- "Feb"
data[(data[,"Open_Month"] == 3), "Open_Month"] <- "Mar"
data[(data[,"Open_Month"] == 4), "Open_Month"] <- "Apr"
data[(data[,"Open_Month"] == 5), "Open_Month"] <- "May"
data[(data[,"Open_Month"] == 6), "Open_Month"] <- "Jun"
data[(data[,"Open_Month"] == 7), "Open_Month"] <- "Jul"
data[(data[,"Open_Month"] == 8), "Open_Month"] <- "Aug"
data[(data[,"Open_Month"] == 9), "Open_Month"] <- "Sep"
data[(data[,"Open_Month"] == 10), "Open_Month"] <- "Oct"
data[(data[,"Open_Month"] == 11), "Open_Month"] <- "Nov"
data[(data[,"Open_Month"] == 12), "Open_Month"] <- "Dec"


Genre.maintype <- c("Action","Adventure","Animation","Comedy","Drama","Crime","Family","Fantasy","Foreign","Horror","Music","Romance","Sci-Fi","Thriller","War")
Genre.dataframe <- data.frame(Genre.maintype, 0,0,0,0,0,0,0)
names(Genre.dataframe) <- c("Type","Number","Avg_Gross","Avg_Days","Na","Summer_Vacation","Christmas","Halloween")
data$Genre <- as.character(data$Genre)
for(j in c(1:15)){
  tmp <- grepl(Genre.maintype[j],data$Genre)
  for(i in c(1:500)){
    if(tmp[i] == TRUE){
      Genre.dataframe$Number[j] = Genre.dataframe$Number[j] + 1
      Genre.dataframe$Avg_Gross[j] = Genre.dataframe$Avg_Gross[j] + data$Total_Gross[i]
      if(is.na(data$Playing_day[i]) == TRUE){
        Genre.dataframe$Na[j] = Genre.dataframe$Na[j] + 1
      }
      else{
        Genre.dataframe$Avg_Days[j] = Genre.dataframe$Avg_Days[j] + data$Playing_day[i]
      }
      
      if(data$Open_Month[i] == "Nov" || data$Open_Month[i] == "Dec"){
        Genre.dataframe$Christmas[j] = Genre.dataframe$Christmas[j] + 1
        data$Holiday[i] = "Christmas"
      }
      else if(data$Open_Month[i] == "Jun" || data$Open_Month[i] == "Jul" || data$Open_Month[i] == "Aug"){
        Genre.dataframe$Summer_Vacation[j] = Genre.dataframe$Summer_Vacation[j] + 1
        data$Holiday[i] = "Summer"
      }
      else if(data$Open_Month[i] == "Oct"){
        Genre.dataframe$Halloween[j] = Genre.dataframe$Halloween[j] + 1
        data$Holiday[i] = "Halloween"
      }
      else{
        data$Holiday[i] = "None"
      }
    }
  }
}

Genre.dataframe$Avg_Gross <- as.integer(Genre.dataframe$Avg_Gross / Genre.dataframe$Number)
Genre.dataframe$Avg_Days <- as.integer(Genre.dataframe$Avg_Days / (Genre.dataframe$Number - Genre.dataframe$Na))

#EDA Start

# imdb 排名
title <- list()
for(year in c(2013:2017)){
  url <- paste("https://www.imdb.com/search/title?year=",year,",",year,"&sort=boxoffice_gross_us,esc&page=1&ref_=adv_nxt",sep="")
  print(url)
  html <- htmlParse(GET(url),encoding = "UTF-8")
  title.list <- xpathSApply(html, "//span[@class='rating-rating ']/span[@class = 'value']", xmlValue)
  title2.list <- xpathSApply(html, "//p[@class='rating-rating ']/span[@class = 'value']", xmlValue)
  title <- rbind(title, as.matrix(title.list))
  
  url <- paste("https://www.imdb.com/search/title?year=",year,",",year,"&sort=boxoffice_gross_us,esc&page=2&ref_=adv_nxt",sep="")
  print(url)
  html <- htmlParse(GET(url),encoding = "UTF-8")
  title.list <- xpathSApply(html, "//span[@class='rating-rating ']/span[@class = 'value']", xmlValue)
  title <- rbind(title, as.matrix(title.list))
}
title <- unlist(title)
title <- as.numeric(title)
title[464] <- mean(title)
data$imdb_Rate <- as.numeric(title)
data


##畫圖：平########


# 1. 基本資料

# 分級 長度 票房 上映天數

p1 <- ggplot(data) + 
  geom_histogram(aes(x = Running.time),fill = "red",color = "black",
                 na.rm = TRUE) +
  xlab("Running Time") + ylab("Count") +  ggtitle("Running time Distribution") +
  theme_grey(base_size = 13)

p2 <- ggplot(data) + 
  geom_histogram(aes(x = Playing_day),fill = "orange",color = "black",
                 na.rm = TRUE) +
  xlab("Playing Day") + ylab("Count") +  ggtitle("Playing Day Distribution") +
  theme_grey(base_size = 13)

p3 <- ggplot(data) + 
  geom_histogram(aes(x = Total_Gross),fill = "pink",color = "black",
                 na.rm = TRUE) +
  xlab("Number of Theaters") + ylab("Count") +  ggtitle("Theaters Number Distribution") +
  theme_grey(base_size = 13)



p4 <- ggplot(data) +
  geom_bar(aes(x = factor(1), fill = Rating))+
     xlab("") + ylab("") + ggtitle("Rating Distribution") +
  coord_polar("y", start=0) +
  theme_grey(base_size = 13) +
  scale_fill_manual(values=c("#00BA38",'#619CFF',"orange","#F8766D"))
  
#將四張圖合併輸出（可用可不用）
multiplot(p1, p2, p3, p4,cols=2)

# 種類數量分佈表
Genre.dataframe$Type <- factor(Genre.dataframe$Type)


### 可知票房通常較高的種類為 Adventure Animation *Fantasy
### Fantasy 因為 Star Wars 的關係被拉高很多


 
#########畫圖:陳###########
data$Year <- as.factor(data$Year)
data$Yearly_Ranking <- as.factor(data$Yearly_Ranking)
data$Rating <- as.factor(data$Rating)
data$Running.length <- as.factor(data$Running.length)
data$Ranking <- as.factor(data$Ranking)
data$Gross_level <- as.factor(data$Gross_level)






########draw by UCLA##########

#1.Rating相關
Rating_p1 <- ggplot(data) +
  geom_bar(aes(x = factor(1), fill = Rating))+
  xlab("") + ylab("") + ggtitle("Rating Distribution") +
  coord_polar("y", start=0) +
  theme_grey(base_size = 13) +
  scale_fill_manual(values=c("#00BA38",'#619CFF',"orange","#F8766D"))
Rating_p1

Rating_p2 <- ggplot(data,aes(x=Rating,y=Total_Gross/1000000,fill = Rating))+geom_boxplot()+xlab("Rating")+ylab("Total Gross (Million)")
Rating_p3 <- ggplot(data,aes(x=Rating,y=Running.time,fill = Rating))+geom_boxplot()+xlab("Rating")+ylab("Running Time")

multiplot(Rating_p2, Rating_p3,cols=2)

#2.Genre相關
ggplot(data,aes(x=Genre))+geom_bar()+xlab("Genre")+ylab("Count")+coord_flip()
ggplot(data,aes(x=Genre,y=Total_Gross/1000000))+geom_boxplot()+xlab("Genre")+ylab("Total Gross (Million)")+coord_flip()
ggplot(data,aes(x=Genre,y=Total_Gross/1000000))+geom_histogram(stat="identity",aes(fill=Rating))+scale_x_discrete()+xlab("Genre")+ylab("Total Gross (Million)")+coord_flip()


ggplot(data = Genre.dataframe) + 
  geom_col(aes(x = Type, y = Number,fill = Type)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))

Genre.dataframe$Type <- factor(Genre.dataframe$Type)

ggplot(data = Genre.dataframe) + 
  geom_col(aes(x = Type, y = Avg_Gross,fill = Type)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))

ggplot(data = Genre.dataframe) + 
  geom_col(aes(x = Type, y = Avg_Days,fill = Type)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))



#3.Studio相關
ggplot(data,aes(x=Studio))+geom_bar()+xlab("Studio")+ylab("Count")+coord_flip()
ggplot(data,aes(x=Studio,y=Total_Gross/1000000))+geom_boxplot()+xlab("Studio")+ylab("Total Gross (Million)")+coord_flip()
ggplot(data,aes(x=Studio,y=Total_Gross/1000000))+geom_histogram(stat="identity",aes(fill=Rating))+xlab("Studio")+ylab("Total Gross (Million)")+coord_flip()
ggplot(data, aes(x = Studio, fill = Ranking )) +
  geom_bar() +
  labs(x = 'Studio', y = 'Count') +
  theme_grey(base_size = 16) +
  coord_flip()

# 4 Running Length
Running_p1 <- ggplot(data) + 
  geom_histogram(aes(x = Running.time),fill = "#F8766D",color = "black",
                 na.rm = TRUE) +
  xlab("Running Time") + ylab("Count") +  ggtitle("Running time Distribution") +
  theme_grey(base_size = 15)

Running_p2 <- ggplot(data) +
  geom_bar(aes(x = factor(1), fill = Running.length))+
  xlab("") + ylab("") + ggtitle("Running_Length Distribution") +
  coord_polar("y", start=0) +
  theme_grey(base_size = 15) +
  scale_fill_manual(values=c("orange",'#619CFF',"#F8766D","#00BA38")) + 
  theme(legend.position="bottom")

multiplot(Running_p1, Running_p2,cols=2)

ggplot(data, aes(x = Running.length, fill = Ranking )) +
  geom_bar(stat = 'count', position = 'fill') +
  labs(x = 'Ranking', y = 'Ratio') +
  theme_grey(base_size = 16) + 
  scale_fill_manual(values=c("#FF61CC","#00A9FF",'#7CAE00',"orange","#F8766D"))


data$Gross_level <- factor(data$Gross_level, levels = c("9","6","5","4","3","2","1","0"))

ggplot(data, aes(x = Running.length, fill = Gross_level )) +
  geom_bar(stat = 'count', position = 'fill') +
  labs(x = 'Ranking', y = 'Ratio') +
  theme_grey(base_size = 16)  + 
  scale_fill_manual(values=c("#CD9600",'#7CAE00',"#00BE67","#00A9FF","#C77CFF","#00BFC4","#FF61CC","#F8766D"))

##5. 月份圖

data$Open_Month <- factor(data$Open_Month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
ggplot(data = data) + 
  geom_bar(aes(x = Open_Month,fill = Open_Month)) + guides(fill=FALSE)

# 聖誕特映 暑假強檔(總票房 平均票房 總上映家數) 表 圖 呈現



filter(data) %>%
  group_by(Holiday) %>%
  summarise(Month_Percentage = length(Holiday) / 500 )


ggplot(data) +
  geom_bar(aes(x = factor(1), fill = Holiday))+
  xlab("") + ylab("") +
  coord_polar("y", start=0) +
  theme_grey(base_size = 13)

filter(Genre.dataframe) %>%
  group_by(Type) %>%
  summarise(Number = Number,Summer_Percentage = Summer_Vacation / Number )


filter(Genre.dataframe) %>%
  group_by(Type) %>%
  summarise(Number = Number, Summer_Percentage = Christmas / Number )

filter(Genre.dataframe) %>%
  group_by(Type) %>%
  summarise(Number = Number, Summer_Percentage = Halloween / Number )





# 暑假的電影會比較適合小朋友看嗎？(動畫比例較高 分級呈現) 結果並沒有

ggplot(data, aes(x = Open_Month, fill = Rating )) +
  geom_bar(stat = 'count', position = 'fill') +
  labs(x = 'Running.length', y = 'Ratio') +
  theme_grey(base_size = 16)

ggplot(data, aes(x = Open_Month, fill =Genre)) +
  geom_bar(stat = 'count', position = 'fill') +
  labs(x = 'Running.length', y = 'Ratio') +
  theme_grey(base_size = 16)

#6.Opening表現相關
ggplot(data,aes(x=Opening_Gross/1000000,y=Total_Gross/1000000))+geom_point()+xlab("Opening Week Gross (Million)")+ylab("Total Gross (Million)")
ggplot(data,aes(x=Number_of_Opening_Theaters,y=Number_of_Theaters))+geom_point()+xlab("Number of Opening Theaters")+ylab("Number of Theaters")

str(data)

# 7.imdb相關
require(scales)
my_trans <- function(from=0) 
{
  trans <- function(x) x-from
  inv <- function(x) x+from
  trans_new("myscale", trans, inv, 
            domain = c(from, 7))
}

ggplot(data) + 
  geom_bar(aes(x = Open_Month, y = imdb_Rate, fill = Open_Month),position = 'dodge', stat = "summary", fun.y = "mean") + 
   scale_y_continuous(trans = my_trans( from = 6)) 
    



ggplot(data) + 
  geom_point(aes(x = imdb_Rate, y = Total_Gross, color = Running.length),size = 2)



save(data, file = paste0(mypath, "data0612.rda"))
load(file = paste0(mypath, "data0612.rda"))

# Random Forest
data2 = subset(data,Year != 2017)
data2 = data2[,c(4,5,7,8,11,12,13,14,17,21)]

data$Genre <- factor(data$Genre)
data2$Genre <- factor(data2$Genre,levels = levels(data$Genre))
str(data2)
library(randomForest)
rf_model <- randomForest(Total_Gross ~ Studio  + Opening_Gross + Running.time + Rating + Open_Month  + Genre + imdb_Rate, data = data2) 


data4 = subset(data,Year == 2017)
data4 = data4[,c(4,5,7,8,11,12,13,14,17,21)]

data4$Genre <- factor(data4$Genre, levels = levels(data$Genre) )

str(data4)
prediction1 <- predict(rf_model,data4)

data4$Total_Gross <- prediction1

# xgboost
library(xgboost)
data2 = subset(data,Year != 2017)
data2 = data2[,c(4,5,7,8,11,12,13,14,17,21)]

data3 = subset(data,Year == 2017)
data3 = data3[,c(4,5,7,8,11,12,13,14,17,21)]

str(data2)
data2$Studio <- as.numeric(data2$Studio)
data2$Genre <- as.numeric(factor(data2$Genre))
data2$Rating <- as.numeric(data2$Rating)
data2$Open_Month <- as.numeric(data2$Open_Month)
data2$Running.length <- as.numeric(data2$Running.length)

data3$Studio <- as.numeric(data3$Studio)
data3$Genre <- as.numeric(factor(data3$Genre))
data3$Rating <- as.numeric(data3$Rating)
data3$Open_Month <- as.numeric(data3$Open_Month)
data3$Running.length <- as.numeric(data3$Running.length)

str(data3)

dtrain <- xgb.DMatrix(data = as.matrix(data2) , label = data2$Total_Gross)
dtest <- xgb.DMatrix(data = as.matrix(data3) , label = data3$Total_Gross)


xgb.params = list(
  #col的抽樣比例，越高表示每棵樹使用的col越多，會增加每棵小樹的複雜度
  colsample_bytree = 0.5,                    
  # row的抽樣比例，越高表示每棵樹使用的col越多，會增加每棵小樹的複雜度
  subsample = 0.5,                      
  booster = "gbtree",
  # 樹的最大深度，越高表示模型可以長得越深，模型複雜度越高
  max_depth = 2,           
  # boosting會增加被分錯的資料權重，而此參數是讓權重不會增加的那麼快，因此越大會讓模型愈保守
  eta = 0.03,
  # 或用'mae'也可以
  eval_metric = "rmse",                      
  objective = "reg:linear",
  # 越大，模型會越保守，相對的模型複雜度比較低
  gamma = 0)               

cv.model = xgb.cv(
  params = xgb.params, 
  data = dtrain,
  nfold = 5,     # 5-fold cv
  nrounds=200,   # 測試1-100，各個樹總數下的模型
  # 如果當nrounds < 30 時，就已經有overfitting情況發生，那表示不用繼續tune下去了，可以提早停止                
  early_stopping_rounds = 30, 
  print_every_n = 20 # 每20個單位才顯示一次結果，
) 

tmp = cv.model$evaluation_log

plot(x=1:nrow(tmp), y= tmp$train_rmse_mean, col='red', xlab="nround", ylab="rmse", main="Avg.Performance in CV") 
points(x=1:nrow(tmp), y= tmp$test_rmse_mean, col='blue') 
legend("topright", pch=1, col = c("red", "blue"), 
       legend = c("Train", "Validation") )

best.nrounds = cv.model$best_iteration 
best.nrounds

xgb.model = xgb.train(paras = xgb.params, 
                      data = dtrain,
                      nrounds = best.nrounds) 

xgb_y = predict(xgb.model, dtest)
mean((xgb_y - data3$Total_Gross)^2) # MSE
mean((data4$Total_Gross - data3$Total_Gross)^2) # MSE
data3$Total_Gross <- xgb_y
data3$Previous_Ranking <- as.numeric(data$Yearly_Ranking[401:500])
data3$Ranking <- 101 - rank(data3$Total_Gross)

data4$Previous_Ranking <- as.numeric(data$Yearly_Ranking[401:500])
data4$Ranking <- 101 - rank(data4$Total_Gross)

library(ggplot2)

ggplot(data = data4) + 
  geom_point(aes(x = Previous_Ranking, y = Ranking), color = "firebrick1", size = 1) 

sum((data4$Ranking - data4$Previous_Ranking)^2) 


