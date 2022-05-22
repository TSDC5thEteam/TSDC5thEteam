################################################################################
library(shiny)
library(shinythemes)
library(quantmod)
library(ggplot2)
library(cowplot)
library(shinydashboard)
library(rclipboard)
library(shinycustomloader)
library(shinyWidgets)
library(dplyr)
library(rvest)
library(magrittr)
library(ggpubr)
library(pdftools)
library(tidyverse)
library(stringr)
library(div)
library(DT)
library(xml2)
library(quantmod)
library(PerformanceAnalytics)
################################################################################

###外部 function

##處理好的資料sdata(包括計算用矩陣) 
stock_data<-function(snum,fromdate,todate){
  ###################################################################
  ##原矩陣
  #from為起始時間 #Sys.Date()
  sdata <- get(getSymbols(snum,src="yahoo",from=fromdate,to=as.Date(todate)+1))#套件會少一天
  
  #將時間序列轉為變數
  sdata_date<-index(sdata)
  sdata<-as.data.frame(sdata)
  sdata<-cbind(sdata_date,sdata)
  #算有幾列
  sd_nrow<-nrow(sdata)
  ##################################################################
  ##計算用矩陣
  #往前算90天
  s_c_data <- get(getSymbols(snum,src="yahoo",from=as.Date(fromdate)-90,to=todate)) 
  
  #將時間序列轉為變數
  sdata_c_date<-index(s_c_data)
  s_c_data<-as.data.frame(s_c_data)
  s_c_data<-cbind(sdata_c_date,s_c_data)
  s_c_nrow<-nrow(s_c_data)
  #################################################################
  #回傳股票代碼、起始日期、
  return(list(snum,fromdate,todate,
              sdata,sd_nrow,
              s_c_data,s_c_nrow)
  )
}

##股票的介面圖 
stock_interface<-function(stock_data_fun,date_interval_set){
  options(scipen = 999)
  
  #將資訊輸入
  sdata_f<-stock_data_fun
  snum<-sdata_f[[1]]
  fromdate<-sdata_f[[2]]
  todate<-sdata_f[[3]]
  data<-sdata_f[[4]]
  sdata_date<-sdata_f[[4]][,"sdata_date"]
  
  #k棒_分組
  
  Group<-rep(0,length(data[,paste0(snum,".Open")]))
  equal_pt<-rep(0,length(data[,paste0(snum,".Open")]))
  
  for(i in 1:length(data[,paste0(snum,".Open")])){
    ifelse(data[,paste0(snum,".Open")][i]>=data[,paste0(snum,".Close")][i]
           ,group<-"陰線",group<-"陽線")  
    ifelse(data[,paste0(snum,".Open")][i]==data[,paste0(snum,".Close")][i]
           ,equal_pt[i]<-data[,paste0(snum,".Open")][i],equal_pt[i]<-NA)
    Group[i]<-group
  }
  data<-cbind(data,Group,equal_pt)
  
  #k棒粗細調整
  fromdate<-as.Date(fromdate)
  todate<-as.Date(todate)
  ifelse(fromdate-todate<=14,candle_width<-0.25,candle_width<-0.5)
  
  #k棒
  #ggplot +號放後面
  candle_p<-ggplot(data=data,aes(x=sdata_date))+
    geom_linerange(aes(ymin=data[,paste0(snum,".Low")]
                       ,ymax=data[,paste0(snum,".High")]
    )
    ,size=0.2)+
    geom_rect(aes(xmin=sdata_date-candle_width,xmax=sdata_date+candle_width
                  ,ymin=pmin(data[,paste0(snum,".Open")],data[,paste0(snum,".Close")])
                  ,ymax=pmax(data[,paste0(snum,".Open")],data[,paste0(snum,".Close")])
                  ,fill=data[,"Group"])
    )+
    geom_point(aes(y = equal_pt), shape = "-", size = 4, color = "black",na.rm=T)+
    
    scale_fill_manual(values = c("陰線"="#00bb00","陽線"="#ce0000"))+
    labs(x="",y="價錢"
         ,title=paste0("K棒圖  ",snum," ","[",sdata_date[1],"/",sdata_date[length(sdata_date)],"]"),fill=NULL)+
    
    scale_x_date(date_breaks = date_interval_set, date_labels=c("%m/%d"))+
    theme(axis.text.y = element_text(margin = margin(r = -21)),legend.position = "none")
  #guides(fill=F)
  #theme_bw() + theme(panel.grid=element_blank())+
  #theme(axis.text.x = element_text(angle=30))
  #all_p<-plot_grid(candle_p,volume_p
  # ,nrow=2,axis = c( "tblr"), align = "v")
  
  #匯出圖
  print(candle_p)
}

##股票的介面圖 
Bb_p<-function(stock_data_fun,date_interval_set){
  
  #將資訊輸入
  sdata_f<-stock_data_fun
  snum<-sdata_f[[1]]
  data<-sdata_f[[4]]
  sdata_date<-sdata_f[[4]][,"sdata_date"]
  sd_nrow<-sdata_f[[5]]
  s_c_data<-sdata_f[[6]]
  s_c_nrow<-sdata_f[[7]]
  Close= s_c_data[,paste0(snum,".Close")]
  MA_20<-c(rep(0,length(Close)))
  for(i in 20:length(Close)){MA_20[i]=mean(Close[c(i-19):i])}
  MA_UP<-c(rep(0,length(Close)))
  for(i in 20:length(Close)){MA_UP[i]=mean(Close[c(i-19):i])+2*sd(Close[c(i-19):i])}
  MA_DOWN<-c(rep(0,length(Close)))
  for(i in 20:length(Close)){MA_DOWN[i]=mean(Close[c(i-19):i])-2*sd(Close[c(i-19):i])}
  
  MA_20<-MA_20[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  MA_UP<-MA_UP[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  MA_DOWN<-MA_DOWN[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  afterc_sdata<-s_c_data[c(s_c_nrow-sd_nrow+1):s_c_nrow,]
  
  Group<-rep(0,length(data[,paste0(snum,".Open")]))
  equal_pt<-rep(0,length(data[,paste0(snum,".Open")]))
  
  for(i in 1:length(data[,paste0(snum,".Open")])){
    ifelse(data[,paste0(snum,".Open")][i]>=data[,paste0(snum,".Close")][i]
           ,group<-"fall",group<-"rise")  
    ifelse(data[,paste0(snum,".Open")][i]==data[,paste0(snum,".Close")][i]
           ,equal_pt[i]<-data[,paste0(snum,".Open")][i],equal_pt[i]<-NA)
    Group[i]<-group
  }
  data<-cbind(data,Group,equal_pt)
  
  #k棒
  #ggplot +號放後面
  bb_p<-ggplot(data=data,aes(x=sdata_date))+
    geom_linerange(aes(ymin=data[,paste0(snum,".Low")]
                       ,ymax=data[,paste0(snum,".High")]
    )
    ,size=0.2)+
    geom_rect(aes(xmin=sdata_date-0.5,xmax=sdata_date+0.5
                  ,ymin=pmin(data[,paste0(snum,".Open")],data[,paste0(snum,".Close")])
                  ,ymax=pmax(data[,paste0(snum,".Open")],data[,paste0(snum,".Close")])
                  ,fill=data[,"Group"])
    )+
    geom_point(aes(y = equal_pt), shape = "-", size = 4, color = "black",na.rm=T)+
    
    scale_fill_manual(values = c("fall"="#00bb00","rise"="#ce0000"))+
    labs(x="",y="價錢"
         ,title=paste0("布林通道  ",snum," ","[",sdata_date[1],"/",sdata_date[length(sdata_date)],"]"),fill=NULL)+
    scale_x_date(date_breaks = date_interval_set , date_labels=c("%m/%d"))+
    theme(axis.text.y = element_text(margin = margin(r = -21)),legend.position = "none")+        
    geom_line(aes(y = MA_20, color = "MA_20"))+
    geom_line(aes(y = MA_DOWN,color = "MA_DOWN"))+
    geom_line(aes(y = MA_UP,color = "MA_UP"))+
    scale_color_manual(values=c("MA_20"="blue","MA_UP"="blue","MA_DOWN"="blue"))
  
  
  print(bb_p)
}

##股票的介面圖 
Ma_p<-function(stock_data_fun,date_interval_set){
  options(scipen = 999)
  
  #將資訊輸入
  sdata_f<-stock_data_fun
  snum<-sdata_f[[1]]
  data<-sdata_f[[4]]
  sdata_date<-sdata_f[[4]][,"sdata_date"]
  sd_nrow<-sdata_f[[5]]
  
  s_c_data<-sdata_f[[6]]
  s_c_nrow<-sdata_f[[7]]
  
  #k棒_分組
  
  Group<-rep(0,length(data[,paste0(snum,".Open")]))
  equal_pt<-rep(0,length(data[,paste0(snum,".Open")]))
  
  for(i in 1:length(data[,paste0(snum,".Open")])){
    ifelse(data[,paste0(snum,".Open")][i]>=data[,paste0(snum,".Close")][i]
           ,group<-"陰線",group<-"陽線")  
    ifelse(data[,paste0(snum,".Open")][i]==data[,paste0(snum,".Close")][i]
           ,equal_pt[i]<-data[,paste0(snum,".Open")][i],equal_pt[i]<-NA)
    Group[i]<-group
  }
  data<-cbind(data,Group,equal_pt)
  
  #MA計算
  ma5<-rep(NA,s_c_nrow)
  ma20<-rep(NA,s_c_nrow)
  ma60<-rep(NA,s_c_nrow)
  
  for(i in 5:s_c_nrow){
    #輸入收盤價
    today_close<-s_c_data[i,4]
    #計算5日平均
    ma5[i]<-mean(s_c_data[c(i-4):i,4])
  }
  
  for(i in 20:s_c_nrow){
    #輸入收盤價
    today_close<-s_c_data[i,4]
    #計算20日平均
    ma20[i]<-mean(s_c_data[c(i-19):i,4])
  }
  
  for(i in 60:s_c_nrow){
    #輸入收盤價
    today_close<-s_c_data[i,4]
    #計算5日平均
    ma60[i]<-mean(s_c_data[c(i-59):i,4])
  }
  
  #將計算完的矩陣扣到只剩想觀察的日期
  ma5<-ma5[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  ma20<-ma20[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  ma60<-ma60[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  afterc_sdata<-s_c_data[c(s_c_nrow-sd_nrow+1):s_c_nrow,]
  
  #k棒+MA
  ma_p<-ggplot(data=data,aes(x=sdata_date))+
    #k棒
    geom_linerange(aes(ymin=data[,paste0(snum,".Low")]
                       ,ymax=data[,paste0(snum,".High")]
    )
    ,size=0.2)+
    geom_rect(aes(xmin=sdata_date-0.5,xmax=sdata_date+0.5
                  ,ymin=pmin(data[,paste0(snum,".Open")],data[,paste0(snum,".Close")])
                  ,ymax=pmax(data[,paste0(snum,".Open")],data[,paste0(snum,".Close")])
                  ,fill=data[,"Group"])
    )+
    geom_point(aes(y = equal_pt), shape = "-", size = 4, color = "black",na.rm=T)+
    
    scale_fill_manual(values = c("陰線"="#00bb00","陽線"="#ce0000"))+
    
    #MA
    geom_line(aes(y=ma5,col="五日均線"),na.rm=T)+
    geom_line(aes(y=ma20,col="二十日均線"),na.rm=T)+
    geom_line(aes(y=ma60,col="六十日均線"),na.rm=T)+
    
    #調整
    labs(x="",y="價錢",color=""
         ,title=paste0("移動平均線  ",snum," ","[",sdata_date[1],"/",sdata_date[length(sdata_date)],"]"),fill=NULL)+
    
    scale_x_date(date_breaks = date_interval_set, date_labels=c("%m/%d"))+
    theme(axis.text.y = element_text(margin = margin(r = -21)),legend.position = "none")+
    scale_color_manual(values = c("五日均線"="blue","二十日均線"="orange","六十日均線"="purple"))+
    theme(legend.position = c(0.97,0.96))+
    guides(fill=F)
  #匯出圖
  print(ma_p)
}

##價量圖
Volume_p<-function(data,date_interval_set){
  #將資訊輸入
  sdata_f<-data
  snum<-sdata_f[[1]]
  
  sdata<-sdata_f[[4]]
  sd_nrow<-sdata_f[[5]]
  
  s_c_data<-sdata_f[[6]]
  s_c_nrow<-sdata_f[[7]]
  
  #價量_分組
  Group_vol<-rep(0,length(s_c_data[,paste0(snum,".Close")]))
  
  for(i in 2:length(s_c_data[,paste0(snum,".Close")])){
    
    if(s_c_data[i,paste0(snum,".Close")]>s_c_data[i-1,paste0(snum,".Close")]){
      group<-"陽線"
    }else if(s_c_data[i,paste0(snum,".Close")]<s_c_data[i-1,paste0(snum,".Close")]){
      group<-"陰線"
    }else{
      group<-"相同"
    }
    
    Group_vol[i]<-group
  }
  s_c_data<-cbind(s_c_data,Group_vol)
  
  #將計算完的矩陣扣到只剩想觀察的日期
  afterc_sdata<-s_c_data[c(s_c_nrow-sd_nrow+1):s_c_nrow,]
  
  #價量圖
  volume_p<- ggplot(afterc_sdata , aes(x = sdata_c_date)) +
    geom_bar(aes(y = afterc_sdata[,paste0(snum,".Volume")]
                 ,fill=afterc_sdata[,"Group_vol"]),stat = "identity")+
    scale_x_date(date_breaks = date_interval_set, date_labels=c("%m/%d"))+
    scale_fill_manual(values = c("white","相同"="yellow","陽線"="#ce0000","陰線"="#00bb00"))+
    theme(axis.text.y = element_text(margin = margin(r = -56)),legend.position = "none")+
    labs(title = "價量圖",x="",y="成交量",fill=NULL)
  
  #theme(element_text(margin=unit(.4,"cm")))
  #scale_y_continuous(breaks = c(4,4.25,4.5,5,6,8))
  #scale_x_date(breaks = NULL)+
  #scale_x_date(date_breaks = "1 month", date_labels=c("%m/%d"))+
  #scale_y_continuous(breaks = NULL)+
  #theme(axis.text.x = element_text(angle=30))
  #theme_bw() + theme(panel.grid=element_blank())
  #theme(axis.text.y = element_text(angle = 30,hjust = 1,vjust = 1))}
  print(volume_p)
}

##Bias_p fun
Bias_p<-function(data,whether_riskline1,risk_line1,risk_line2,date_interval_set){
  
  #將資訊輸入
  sdata_f<-data
  snum<-sdata_f[[1]]
  
  sdata<-sdata_f[[4]]
  sd_nrow<-sdata_f[[5]]
  
  s_c_data<-sdata_f[[6]]
  s_c_nrow<-sdata_f[[7]]
  
  #計算
  five_Bias<-rep(NA,s_c_nrow)
  twenty_Bias<-rep(NA,s_c_nrow)
  
  for(i in 5:s_c_nrow){
    #輸入收盤價
    today_close<-s_c_data[i,4]
    #計算5日平均
    fivedayavg_close<-mean(s_c_data[c(i-4):i,4])
    #計算5日乖離率
    five_Bias[i]<-(today_close-fivedayavg_close)/fivedayavg_close*100
  }
  
  for(i in 20:s_c_nrow){
    #輸入收盤價
    today_close<-s_c_data[i,4]
    #計算20日平均
    twentydayavg_close<-mean(s_c_data[c(i-19):i,4])
    #計算20日乖離率
    twenty_Bias[i]<-(today_close-twentydayavg_close)/twentydayavg_close*100
  }
  #將計算完的矩陣扣到只剩想觀察的日期
  five_Bias<-five_Bias[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  twenty_Bias<-twenty_Bias[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  afterc_sdata<-s_c_data[c(s_c_nrow-sd_nrow+1):s_c_nrow,]
  
  #plot
  bias_p<-ggplot(data=afterc_sdata,aes(x =sdata_c_date))+
    geom_line(aes(y=five_Bias,col="五日線"),na.rm=T)+
    geom_line(aes(y=twenty_Bias,col="二十日線"),na.rm=T)+
    labs(title = "BIAS 乖離率",x="",y="Bias(%)",color="")+
    geom_line(aes(y=rep(0,sd_nrow)))+
    scale_x_date(date_breaks = date_interval_set, date_labels=c("%m/%d"))+
    theme(axis.text.y = element_text(margin = margin(r = -20)))+
    scale_color_manual(values = c("五日線"="blue","二十日線"="orange"))+
    theme(legend.position = c(0.97,0.965))
  #col=要在aes裡面，並承上起下，顏色才會繼承
  #灰色部份邊線#top right bottom left#margin(t=0,r=10,b=0,l=10)
  #theme(axis.text.y.right =(element_text()))
  #theme(element_text(margin=unit(0.1,"cm")))
  #theme(axis.text.x = element_text(angle=30))
  
  #有警示線的圖
  bias_risk_p<-bias_p+
    geom_line(aes(y=rep(risk_line1,sd_nrow)),linetype = "dashed",col="#CC0000")+#界線
    geom_line(aes(y=rep(-risk_line2,sd_nrow)),linetype = "dashed",col="#339933")
  
  #條件判斷是否要加入警示線
  ifelse(whether_riskline1==T,print(bias_risk_p),print(bias_p))
  
}

#Kd_p fun
Kdj_p<-function(data,whether_riskline2,risk_line1,risk_line2,date_interval_set){
  
  #將資訊輸入
  sdata_f<-data
  snum<-sdata_f[[1]]
  
  sdata<-sdata_f[[4]]
  sd_nrow<-sdata_f[[5]]
  
  s_c_data<-sdata_f[[6]]
  s_c_nrow<-sdata_f[[7]]
  
  
  #########
  
  high=s_c_data[,3]
  low=s_c_data[,4]
  close=s_c_data[,5]
  
  ##############
  
  high2 <- rep(NA,s_c_nrow,10)
  low2 <- rep(NA,s_c_nrow,10)
  
  #<KDJ>
  #計算 KDJ 值
  # 找最近九天內最大、最小值
  
  for(i in 9:s_c_nrow) {
    high2[i] <- max(high[c(i-8):i])
    low2[i] <- min(low[c(i-8):i])
  }
  
  highM=high2
  lowM=low2
  
  #highM=as.data.frame(highM)
  #lowM=as.data.frame(lowM)
  #計算RSV
  rsv <- rep(NA,s_c_nrow)
  
  for(i in 9:s_c_nrow) {
    rsvlow=lowM[i]
    rsvhigh=highM[i]
    rsvclose=close[i]
    rsv[i]=((rsvclose-rsvlow)/(rsvhigh-rsvlow))*100
  }
  RSV=rsv
  
  #計算KDJ
  #建構矩陣
  KDJ = matrix(NA, s_c_nrow, 3)  # 構建存放數據的矩陣
  KDJ = as.data.frame(KDJ)           # 轉換為data.frame
  colnames(KDJ) = c("K", "D", "J")   # 1-3列的名稱為K,D,J
  KDJ[1:8, ] = 50
  
  for(i in c(9:s_c_nrow)) {
    KDJ[i, 1]=2/3 *KDJ[c(i-1), 1]+1/3*RSV[i]  #K
    KDJ[i, 2]=2/3 *KDJ[c(i-1), 2]+1/3*KDJ[i,1]  #D
    KDJ[i, 3]=3*KDJ[i, 2]-2*KDJ[i, 1]  #J 
  }
  
  #將計算完的矩陣扣到只剩想觀察的日期
  K<-KDJ[c(s_c_nrow-sd_nrow+1):s_c_nrow,1]
  D<-KDJ[c(s_c_nrow-sd_nrow+1):s_c_nrow,2]
  J<-KDJ[c(s_c_nrow-sd_nrow+1):s_c_nrow,3]
  afterc_sdata<-s_c_data[c(s_c_nrow-sd_nrow+1):s_c_nrow,]
  stock_length<-length(c(s_c_nrow-sd_nrow+1):s_c_nrow)
  
  #畫圖
  kdj_p<-ggplot(data=afterc_sdata,aes(x =sdata_c_date))+
    geom_line(aes(y=K,col="K線"),na.rm=T)+
    geom_line(aes(y=D,col="D線"),na.rm=T)+
    geom_line(aes(y=J,col="J線"),na.rm=T)+
    labs( title = "KDJ 隨機指標",y = 'KDJ',x='日期',color="")+
    scale_x_date(date_breaks = date_interval_set, date_labels=c("%m/%d"))+
    theme(axis.text.y = element_text(margin = margin(r = -18)))+
    scale_color_manual(values = c("K線"="orange","D線"="blue","J線"="#D28EFF"))+
    theme(legend.position = c(0.98,0.95))
  
  kdj_risk_p<- kdj_p +
    geom_line(aes(y=rep(risk_line1,sd_nrow)),linetype = "dashed",col="#CC0000")+#界線
    geom_line(aes(y=rep(risk_line2,sd_nrow)),linetype = "dashed",col="#339933")
  
  #條件判斷是否要加入警示線
  ifelse(whether_riskline2==T,print(kdj_risk_p),print(kdj_p))
  
  
}

#Rsi_p fun
Rsi_p <- function (data,whether_riskline3,risk_line1,risk_line2,date_interval_set){
  #將資訊輸入
  sdata_f<-data
  snum<-sdata_f[[1]]
  
  sdata<-sdata_f[[4]]
  sd_nrow<-sdata_f[[5]]
  
  s_c_data<-sdata_f[[6]]
  s_c_nrow<-sdata_f[[7]]
  
  price=s_c_data[,5]
  ###計算RSI###
  #分別以U、D及rsi建立長度為length(price)的行
  rsi=array(0,s_c_nrow)
  N <- length(price)
  U <- rep(0,N)
  D <- rep(0,N)
  
  #令Lprice為隔日收盤價
  Lprice <- Lag(price,1)
  
  #計算5日RSI#
  five_rsi <- rep(NA,N)
  for (i in 5:N){
    #若當日收盤價大於等於前一日收盤價則動量記為U否則記入D
    if (price[i]>=Lprice[i]){
      U[i] <- price[i]- Lprice[i]
    } else{
      D[i] <- Lprice[i]- price[i]
    }
    #從第i-5+1日開始至第i日計算U的平均及D的平均
    if (i>5){
      AvgUp <- mean(U[(i-5+1):i])
      AvgDn <- mean(D[(i-5+1):i])
      
      #RSI=SMA(U,n)/(SMA(U,n)+SMA(D,n))*100%
      five_rsi[i] <- AvgUp/(AvgUp+AvgDn)*100 
    }
  }
  
  #計算20日RSI#
  twenty_rsi <- rep(NA,N)
  for (i in 20:N){
    #若當日收盤價大於等於前一日收盤價則動量記為U否則記入D
    if (price[i]>=Lprice[i]){
      U[i] <- price[i]- Lprice[i]
    } else{
      D[i] <- Lprice[i]- price[i]
    }
    #從第i-20+1日開始至第i日計算U的平均及D的平均
    if (i>20){
      AvgUp <- mean(U[(i-20+1):i])
      AvgDn <- mean(D[(i-20+1):i])
      
      #RSI=SMA(U,n)/(SMA(U,n)+SMA(D,n))*100%
      twenty_rsi[i] <- AvgUp/(AvgUp+AvgDn)*100 
    }
  }
  
  #計算60日RSI#
  sixty_rsi <- rep(NA,N)
  for (i in 60:N){
    #若當日收盤價大於等於前一日收盤價則動量記為U否則記入D
    if (price[i]>=Lprice[i]){
      U[i] <- price[i]- Lprice[i]
    } else{
      D[i] <- Lprice[i]- price[i]
    }
    #從第i-60+1日開始至第i日計算U的平均及D的平均
    if (i>60){
      AvgUp <- mean(U[(i-60+1):i])
      AvgDn <- mean(D[(i-60+1):i])
      
      #RSI=SMA(U,n)/(SMA(U,n)+SMA(D,n))*100%
      sixty_rsi[i] <- AvgUp/(AvgUp+AvgDn)*100 
    }
  }
  
  #將計算完的矩陣扣到只剩想觀察的日期
  five_rsi<-five_rsi[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  twenty_rsi<-twenty_rsi[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  sixty_rsi<-sixty_rsi[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  afterc_sdata<-s_c_data[c(s_c_nrow-sd_nrow+1):s_c_nrow,]
  
  
  #畫圖
  rsi_p=ggplot(data=afterc_sdata,aes(x =sdata_c_date))+
    geom_line(aes(y=five_rsi,col="五日RSI"),na.rm=T)+
    geom_line(aes(y=twenty_rsi,col="二十日RSI"),na.rm=T)+
    geom_line(aes(y=sixty_rsi,col="六十日RSI"),na.rm=T)+
    #geom_hline(yintercept= 50, linetype = 'dashed', color = "red")+
    labs(title = "RSI 相對強弱指數",x="",y="RSI(%)",color="")+
    scale_x_date(date_breaks = date_interval_set, date_labels=c("%m/%d"))+
    theme(axis.text.y = element_text(margin = margin(r = -20)))+
    scale_color_manual(values = c("五日RSI"="blue","二十日RSI"="orange","六十日RSI"="purple"))+
    theme(legend.position = c(0.97,0.95))
  
  #有警示線的圖
  rsi_risk_p<-rsi_p+
    geom_line(aes(y=rep(risk_line1,sd_nrow)),linetype = "dashed",col="#CC0000")+#界線
    geom_line(aes(y=rep(risk_line2,sd_nrow)),linetype = "dashed",col="#339933")
  
  #條件判斷是否要加入警示線
  ifelse(whether_riskline3==T,print(rsi_risk_p),print(rsi_p))
}

#Obv_p fun
Obv_p<-function(data,whether_riskline4,risk_line,date_interval_set){
  
  #將資訊輸入
  sdata_f<-data
  snum<-sdata_f[[1]]
  
  sdata<-sdata_f[[4]]
  sd_nrow<-sdata_f[[5]]
  
  s_c_data<-sdata_f[[6]]
  s_c_nrow<-sdata_f[[7]]
  
  
  #計算OBV
  change = diff(s_c_data[,5])
  signs=rep(0,length(s_c_data[,5]))
  signs[1] =0
  for (i in 2:length(s_c_data[,5])) {
    signs[i]=sign(change[i-1])
  }
  v=s_c_data[,6] *signs
  OBV= rep(0,length(s_c_data[,5]))
  OBV[1]=s_c_data[1,6]
  for(i in 2:length(s_c_data[,5])){
    OBV[i]=OBV[i-1]+v[i]
  }
  
  #將計算完的矩陣扣到只剩想觀察的日期
  OBV<-OBV[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  afterc_sdata<-s_c_data[c(s_c_nrow-sd_nrow+1):s_c_nrow,]
  
  #畫圖
  high=max(OBV)
  low=min(OBV)
  obv_p=ggplot(data=afterc_sdata,aes(x =sdata_c_date))+
    geom_line(aes(y=OBV,col="OBV線"))+
    scale_y_continuous(breaks =seq(signif(low,5),signif(high,5),1000000000))+
    scale_x_date(date_breaks = date_interval_set, date_labels =c("%m/%d"))+#,expand = c(0,0)
    scale_color_manual(values = c("OBV線"="blue"))+
    theme(axis.text.y = element_text(margin = margin(r = -65)))+
    theme(legend.position = c(0.975,0.985))+
    labs(title = "OBV 能量潮指標",x="",y="OBV",color="")
  
  #有警示線的圖
  obv_risk_p<-obv_p+
    geom_line(aes(y=rep(risk_line,sd_nrow)),linetype = "dashed",col="#CC0000")#界線
  
  #條件判斷是否要加入警示線
  ifelse(whether_riskline4==T,print(obv_risk_p),print(obv_p))
  
}

#Arbr_p fun
Arbr_p<-function(data,whether_riskline5,risk_line,date_interval_set){
  #將資訊輸入
  sdata_f<-data
  snum<-sdata_f[[1]]
  
  sdata<-sdata_f[[4]]
  sd_nrow<-sdata_f[[5]]
  
  s_c_data<-sdata_f[[6]]
  s_c_nrow<-sdata_f[[7]]
  
  open=s_c_data[,2]
  high=s_c_data[,3]
  low=s_c_data[,4]
  close=s_c_data[,5]
  #新增昨日收盤價
  s_c_data<-mutate(s_c_data,close_yesterday=Lag(close,1))
  #建立矩陣計算
  s_c_data<-mutate(s_c_data,"upA", "downA","upB", "downB")
  s_c_data[9] <- (high-open) 
  s_c_data[10] <- (open-low)
  s_c_data[11] <- (high-s_c_data$close_yesterday) 
  s_c_data[12] <- (s_c_data$close_yesterday-low)
  #index的資料_迴圈
  ARindex=rep(NA,s_c_nrow)
  BRindex=rep(NA,s_c_nrow)
  
  for (i in 26:s_c_nrow) {
    #輸入分子
    upA=sum(s_c_data[c(i-25):i,9])
    #輸入分母
    downA=sum(s_c_data[c(i-25):i,10])
    #輸入分子
    upB=sum(s_c_data[c(i-25):i,11])
    #輸入分母
    downB=abs(sum(s_c_data[c(i-25):i,12]))
    #帶入公式
    ARindex[i]=(upA/downA)
    #帶入公式
    BRindex[i]=(upB/downB)
  }
  
  #將計算完的矩陣扣到只剩想觀察的日期
  ARindex<-ARindex[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  BRindex<-BRindex[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  afterc_sdata<-s_c_data[c(s_c_nrow-sd_nrow+1):s_c_nrow,]
  
  #plot
  arbr_p<-ggplot(data=afterc_sdata,aes(x =sdata_c_date))+
    geom_line(aes(y=ARindex,col="AR指標"),na.rm=T)+
    geom_line(aes(y=BRindex,col="BR指標"),na.rm=T)+
    labs(title = "AR&BR 人氣指標&收盤指標",x="",y="ARBR(%)",color="")+
    scale_x_date(date_breaks = date_interval_set, date_labels=c("%m/%d"))+
    theme(axis.text.y = element_text(margin = margin(r = -20)))+
    scale_color_manual(values = c("AR指標"="blue","BR指標"="orange"))+
    theme(legend.position = c(0.975,0.97))
  
  #有警示線的圖
  arbr_risk_p<-arbr_p+
    geom_line(aes(y=rep(risk_line,sd_nrow)),linetype = "dashed",col="#CC0000")#界線
  
  #條件判斷是否要加入警示線
  ifelse(whether_riskline5==T,print(arbr_risk_p),print(arbr_p))
  
  
  
}

###財報

#損益表
Income.statement=function(data){
  #將資訊輸入
  sdata_f<-data
  snum<-sdata_f[[1]]
  snum=substring(snum, 1, 4)
  
  A='https://statementdog.com/analysis/'
  B='/income-statement'
  url=paste(A,snum,B)
  htmlcontent=read_html(url)
  titlepath='//td | //th | //*[contains(concat( " ", @class, " " ), concat( " ", "data-explain-title", " " ))]'
  titles=htmlcontent %>% html_nodes(xpath=titlepath) %>% html_text()
  mytable=data.frame(title=titles)
  mytable=mytable[-c(1),]
  dim(mytable)<-matrix(c(5,10))
  mytable=t(mytable)
  rowtitle=mytable[1:10,c(1)]
  mytable=mytable[,2:5]
  mytable=data.frame(mytable,row.names=rowtitle)
  snum<-sdata_f[[1]]
  main.title=paste0(snum,"損益表")
  footnote="單位:台幣(元)"
  mytable=ggtexttable(mytable,cols = NULL,theme = ttheme(base_style = "light",base_size = 15,base_colour = "black",padding = unit(c(10,10), "mm")))
  mytable%>%
    tab_add_title(text = main.title, face = "bold", size=40.0,padding = unit(1.5, "line"))%>%
    tab_add_footnote(text = footnote,face = "bold", size=25.0,padding = unit(1.5, "line"))
}

#現金流量表
Cash.flow.statement=function(data){
  #將資訊輸入
  sdata_f<-data
  snum<-sdata_f[[1]]
  snum=substring(snum, 1, 4)
  
  A= "https://statementdog.com/analysis/ "
  B='/cash-flow-statement'
  url=paste(A,snum,B)
  htmlcontent=read_html(url)
  titlepath='//td | //th | //*[contains(concat( " ", @class, " " ), concat( " ", "data-explain-title", " " ))]'
  titles=htmlcontent %>% html_nodes(xpath=titlepath) %>% html_text()
  mytable=data.frame(title=titles)
  mytable=mytable[-c(1),]
  dim(mytable)<-matrix(c(5,9))
  mytable=t(mytable)
  rowtitle=mytable[1:9,c(1)]
  mytable=mytable[,2:5]
  mytable=data.frame(mytable,row.names=rowtitle)
  snum<-sdata_f[[1]]
  main.title=paste0(snum,"現金流量表")
  footnote="單位:台幣(元)"
  mytable=ggtexttable(mytable,cols = NULL,theme = ttheme(base_style = "light",base_size = 15,base_colour = "black",padding = unit(c(10,10), "mm")))
  mytable%>%
    tab_add_title(text = main.title, face = "bold", size=40.0,padding = unit(1.5, "line"))%>%
    tab_add_footnote(text = footnote,face = "bold", size=25.0,padding = unit(1.5, "line"))
}

#資產負債表
Balance.sheet=function(data){
  #將資訊輸入
  sdata_f<-data
  snum<-sdata_f[[1]]
  snum=substring(snum, 1, 4)
  
  A= "https://statementdog.com/analysis/"
  B='/liabilities-and-equity'
  url=paste(A,snum,B)
  htmlcontent=read_html(url)
  titlepath='//td | //th | //*[contains(concat( " ", @class, " " ), concat( " ", "data-explain-title", " " ))]'
  titles=htmlcontent %>% html_nodes(xpath=titlepath) %>% html_text()
  mytable=data.frame(title=titles)
  mytable=mytable[-c(1),]
  dim(mytable)<-matrix(c(5,10))
  mytable=t(mytable)
  rowtitle=mytable[1:10,c(1)]
  mytable=mytable[,2:5]
  mytable=data.frame(mytable,row.names=rowtitle)
  snum<-sdata_f[[1]]
  main.title=paste0(snum,"資產負債表")
  footnote="單位:台幣(元)"
  mytable=ggtexttable(mytable,cols = NULL,theme = ttheme(base_style = "light",base_size = 15,base_colour = "black",padding = unit(c(10,10), "mm")))
  mytable%>%
    tab_add_title(text = main.title, face = "bold", size=40.0,padding = unit(1.5, "line"))%>%
    tab_add_footnote(text = footnote,face = "bold", size=25.0,padding = unit(1.5, "line"))}

###股票代碼
# 上櫃公司
xurl_OTC = "https://tw.stock.yahoo.com/h/kimosel?tse=2"
html_page_OTC = read_html(url(xurl_OTC), encoding = "CP950")
# 爬所有類股名稱
xpath_OTC = "//td[@class='c3']/a"
target_OTC = xml_find_all(html_page_OTC, xpath_OTC)
urls_OTC = xml_attr(target_OTC, "href")
cates_OTC = xml_text(target_OTC)
cates_OTC = cates_OTC[c(-1,-33,-34)] # 扣除
# 爬各類股的網址
urls_OTC = paste0("https://tw.stock.yahoo.com/h/kimosel?tse=2", urls_OTC)
urls_OTC = urls_OTC[c(-1,-33,-34)] # 扣除
# 爬各類股底下的個股代號跟名稱
tmp_OTC = NULL
stocks_name_OTC = NULL
stocks_code_OTC = NULL
xpath_OTC = "//a[@class = 'none']"
for(i in 1:length(urls_OTC))
{
  tmp_OTC = read_html(url(urls_OTC[i]), encoding = "CP950")
  stocks_code_OTC[[i]] = sapply(strsplit(gsub("\n", "", xml_text(xml_find_all(tmp_OTC, xpath_OTC))), " "), function(x) unlist(x)[1])
  stocks_name_OTC[[i]] = sapply(strsplit(gsub("\n", "", xml_text(xml_find_all(tmp_OTC, xpath_OTC))), " "), function(x) unlist(x)[2])
}
# 組成data.frame
data_OTC = NULL
for(i in 1:length(cates_OTC))
{
  tmp_OTC = sapply(stocks_code_OTC[i], function(x) unlist(x))
  tmp1_OTC = sapply(stocks_name_OTC[i], function(x) unlist(x))
  tmp2_OTC = rep(cates_OTC[i], length(tmp_OTC))
  tmp3_OTC = data.frame(category = tmp2_OTC, code = tmp_OTC, stock = tmp1_OTC)
  data_OTC = rbind(data_OTC, tmp3_OTC)
}
data_OTC=data_OTC[c(-815:-1235),]


# 上市公司
xurl_TSE = "https://tw.stock.yahoo.com/h/kimosel"
html_page_TSE = read_html(url(xurl_TSE), encoding = "CP950")
# 爬所有類股名稱
xpath_TSE = "//td[@class='c3']/a"
target_TSE = xml_find_all(html_page_TSE, xpath_TSE)
urls_TSE = xml_attr(target_TSE, "href")
cates_TSE = xml_text(target_TSE)
cates_TSE = cates_TSE[c(-13,-33,-34)] # 扣除
# 爬各類股的網址
urls_TSE = paste0("https://tw.stock.yahoo.com", urls_TSE)
urls_TSE = urls_TSE[c(-13,-33,-34)] # 扣除
# 爬各類股底下的個股代號跟名稱
tmp_TSE = NULL
stocks_name_TSE = NULL
stocks_code_TSE = NULL
xpath_TSE = "//a[@class = 'none']"
for(i in 1:length(urls_TSE))
{
  tmp_TSE = read_html(url(urls_TSE[i]), encoding = "CP950")
  stocks_code_TSE[[i]] = sapply(strsplit(gsub("\n", "", xml_text(xml_find_all(tmp_TSE, xpath_TSE))), " "), function(x) unlist(x)[1])
  stocks_name_TSE[[i]] = sapply(strsplit(gsub("\n", "", xml_text(xml_find_all(tmp_TSE, xpath_TSE))), " "), function(x) unlist(x)[2])
}
# 組成data.frame
data_TSE = NULL
for(i in 1:length(cates_TSE))
{
  tmp_TSE = sapply(stocks_code_TSE[i], function(x) unlist(x))
  tmp1_TSE = sapply(stocks_name_TSE[i], function(x) unlist(x))
  tmp2_TSE = rep(cates_TSE[i], length(tmp_TSE))
  tmp3_TSE = data.frame(category = tmp2_TSE, code = tmp_TSE, stock = tmp1_TSE)
  data_TSE = rbind(data_TSE, tmp3_TSE)
}
data_TSE=data_TSE[-c(838,840,842,844,846,848,850,852,854,869,873,880,882,888,895,897,899,901,908,
                     928,931,933,935,941,946,950,953,956,958,960,962,964,967,970,975,978,980,985,
                     988,990,994,997,999,1001,1003,1006,1009,1011,1014,1017,1019,1022,1024,1027,
                     1030:1078,1086:1103,1187:1339),]

#半導體上市公司
url_1='https://tw.stock.yahoo.com/h/kimosel.php?tse=1&cat=%E5%8D%8A%E5%B0%8E%E9%AB%94&form=menu&form_id=stock_id&form_name=stock_name&domain=0'
htmlcontent_1=read_html(url_1)
titlepath_1='//*[contains(concat( " ", @class, " " ), concat( " ", "none", " " ))]'
code=htmlcontent_1 %>% html_nodes(xpath=titlepath_1) %>% html_text()
data_TSE_SC=data.frame(title=code)

#上市
data_TSE=paste0(data_TSE$code,".TW"," ",data_TSE$stock," ","上市")
#上市公司--半導體
data_TSE_SC=str_split(substring(data_TSE_SC[,1],2), pattern = " ", n = 2,simplify = TRUE)
data_TSE_SC=paste0(data_TSE_SC[,1],".TW"," ",data_TSE_SC[,2]," ","上市")
#上櫃
data_OTC=paste0(data_OTC$code,".TWO"," ",data_OTC$stock," ","上櫃")




###台股美股大盤
#台股大盤
stock_data_TWII<-function(snum,fromdate,todate){
  ###################################################################
  ##原矩陣
  #from為起始時間 #Sys.Date()
  sdata <- getSymbols(paste0('^',"TWII"),auto.assign=FALSE,src="yahoo",from=fromdate,to=as.Date(todate)+1)
  
  #將時間序列轉為變數
  sdata_date<-index(sdata)
  sdata<-as.data.frame(sdata)
  sdata<-cbind(sdata_date,sdata)
  #算有幾列
  sd_nrow<-nrow(sdata)
  #################################################################
  ##計算用矩陣
  #往前算90天
  s_c_data <- getSymbols(paste0('^',"TWII"),auto.assign=FALSE,src="yahoo",from=as.Date(fromdate)-90,to=todate)
  
  #將時間序列轉為變數
  sdata_c_date<-index(s_c_data)
  s_c_data<-as.data.frame(s_c_data)
  s_c_data<-cbind(sdata_c_date,s_c_data)
  s_c_nrow<-nrow(s_c_data)
  ################################################################
  #回傳股票代碼、起始日期、
  return(list(snum='TWII',fromdate,todate,
              sdata,sd_nrow,
              s_c_data,s_c_nrow)
  )
}

##股票的介面圖 
#台股大盤+布林
TWII_BB_p<-function(stock_data_TWII_fun){
  
  #將資訊輸入
  sdata_f<-stock_data_TWII_fun
  snum<-sdata_f[[1]]
  fromdate<-sdata_f[[2]]
  todate<-sdata_f[[3]]
  data<-sdata_f[[4]]
  sdata_date<-sdata_f[[4]][,"sdata_date"]
  sd_nrow<-sdata_f[[5]]
  s_c_data<-sdata_f[[6]]
  s_c_nrow<-sdata_f[[7]]
  Close= s_c_data[,paste0(snum,".Close")]
  MA_20<-c(rep(0,length(Close)))
  for(i in 20:length(Close)){MA_20[i]=mean(Close[c(i-19):i])}
  MA_UP<-c(rep(0,length(Close)))
  for(i in 20:length(Close)){MA_UP[i]=mean(Close[c(i-19):i])+2*sd(Close[c(i-19):i])}
  MA_DOWN<-c(rep(0,length(Close)))
  for(i in 20:length(Close)){MA_DOWN[i]=mean(Close[c(i-19):i])-2*sd(Close[c(i-19):i])}
  #EGMD<-as.data.frame(EGMD)
  #date <- as.Date(row.names(EGMD))
  
  MA_20<-MA_20[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  MA_UP<-MA_UP[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  MA_DOWN<-MA_DOWN[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  afterc_sdata<-s_c_data[c(s_c_nrow-sd_nrow+1):s_c_nrow,]
  
  Group<-rep(0,length(data[,paste0(snum,".Open")]))
  equal_pt<-rep(0,length(data[,paste0(snum,".Open")]))
  
  for(i in 1:length(data[,paste0(snum,".Open")])){
    ifelse(data[,paste0(snum,".Open")][i]>=data[,paste0(snum,".Close")][i]
           ,group<-"rise",group<-"fall")  
    ifelse(data[,paste0(snum,".Open")][i]==data[,paste0(snum,".Close")][i]
           ,equal_pt[i]<-data[,paste0(snum,".Open")][i],equal_pt[i]<-NA)
    Group[i]<-group
  }
  data<-cbind(data,Group,equal_pt)
  
  #k棒粗細調整
  fromdate<-as.Date(fromdate)
  todate<-as.Date(todate)
  ifelse(fromdate-todate<=14,histogram_width<-0.25,histogram_width<-0.5)
  
  #k棒
  #ggplot +號放後面
  candle_p<-ggplot(data=data,aes(x=sdata_date))+
    geom_linerange(aes(ymin=data[,paste0(snum,".Low")]
                       ,ymax=data[,paste0(snum,".High")]
    )
    ,size=0.2)+
    geom_rect(aes(xmin=sdata_date-histogram_width,xmax=sdata_date+histogram_width
                  ,ymin=pmin(data[,paste0(snum,".Open")],data[,paste0(snum,".Close")])
                  ,ymax=pmax(data[,paste0(snum,".Open")],data[,paste0(snum,".Close")])
                  ,fill=data[,"Group"])
    )+
    geom_point(aes(y = equal_pt), shape = "-", size = 4, color = "black",na.rm=T)+
    
    scale_fill_manual(values = c("fall"="#00bb00","rise"="#ce0000"))+
    labs(x="",y="價錢"
         ,title=paste0("台股大盤",snum," ","[",sdata_date[1],"/",sdata_date[length(sdata_date)],"]"),fill=NULL)+
    scale_x_date(position="top",date_breaks = "1 month", date_labels=c("%m/%d"))+
    theme(axis.text.y = element_text(margin = margin(r = -21)),legend.position = "none")+        
    geom_line(aes(y = MA_20),
              color = "blue", size = 0.5)+
    geom_line(aes(y = MA_DOWN),
              color = "blue",  size = 0.5)+
    geom_line(aes(y = MA_UP),
              color = "blue",  size = 0.5)
  print(candle_p)
}

#台股大盤+移動平均線
TWII_MA_p<-function(stock_data_TWII_fun){
  
  #將資訊輸入
  sdata_f<-stock_data_TWII_fun
  snum<-sdata_f[[1]]
  fromdate<-sdata_f[[2]]
  todate<-sdata_f[[3]]
  data<-sdata_f[[4]]
  sdata_date<-sdata_f[[4]][,"sdata_date"]
  sd_nrow<-sdata_f[[5]]
  s_c_data<-sdata_f[[6]]
  s_c_nrow<-sdata_f[[7]]
  Close= s_c_data[,paste0(snum,".Close")]
  MA_5<-c(rep(0,length(Close)))
  for(i in 5:length(Close)){MA_5[i]=mean(Close[c(i-4):i])}
  MA_20<-c(rep(0,length(Close)))
  for(i in 20:length(Close)){MA_20[i]=mean(Close[c(i-19):i])}
  MA_60<-c(rep(0,length(Close)))
  for(i in 60:length(Close)){MA_60[i]=mean(Close[c(i-59):i])}
  #EGMD<-as.data.frame(EGMD)
  #date <- as.Date(row.names(EGMD))
  
  MA_5<-MA_5[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  MA_20<-MA_20[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  MA_60<-MA_60[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  afterc_sdata<-s_c_data[c(s_c_nrow-sd_nrow+1):s_c_nrow,]
  
  Group<-rep(0,length(data[,paste0(snum,".Open")]))
  equal_pt<-rep(0,length(data[,paste0(snum,".Open")]))
  
  for(i in 1:length(data[,paste0(snum,".Open")])){
    ifelse(data[,paste0(snum,".Open")][i]>=data[,paste0(snum,".Close")][i]
           ,group<-"rise",group<-"fall")  
    ifelse(data[,paste0(snum,".Open")][i]==data[,paste0(snum,".Close")][i]
           ,equal_pt[i]<-data[,paste0(snum,".Open")][i],equal_pt[i]<-NA)
    Group[i]<-group
  }
  data<-cbind(data,Group,equal_pt)
  
  #k棒粗細調整
  fromdate<-as.Date(fromdate)
  todate<-as.Date(todate)
  ifelse(fromdate-todate<=14,histogram_width<-0.25,histogram_width<-0.5)
  
  #k棒
  #ggplot +號放後面
  candle_p<-ggplot(data=data,aes(x=sdata_date))+
    geom_linerange(aes(ymin=data[,paste0(snum,".Low")]
                       ,ymax=data[,paste0(snum,".High")]
    )
    ,size=0.2)+
    geom_rect(aes(xmin=sdata_date-histogram_width,xmax=sdata_date+histogram_width
                  ,ymin=pmin(data[,paste0(snum,".Open")],data[,paste0(snum,".Close")])
                  ,ymax=pmax(data[,paste0(snum,".Open")],data[,paste0(snum,".Close")])
                  ,fill=data[,"Group"])
    )+
    geom_point(aes(y = equal_pt), shape = "-", size = 4, color = "black",na.rm=T)+
    
    scale_fill_manual(values = c("fall"="#00bb00","rise"="#ce0000"))+
    labs(x="",y="價錢"
         ,title=paste0("台股大盤",snum," ","[",sdata_date[1],"/",sdata_date[length(sdata_date)],"]"),fill=NULL)+
    scale_x_date(position="top",date_breaks = "1 month", date_labels=c("%m/%d"))+
    theme(axis.text.y = element_text(margin = margin(r = -21)),legend.position = "none")+        
    geom_line(aes(y = MA_5),
              color = "blue", size = 0.5)+
    geom_line(aes(y = MA_20),
              color = "orange",  size = 0.5)+
    geom_line(aes(y = MA_60),
              color = "purple",  size = 0.5)
  print(candle_p)
}


#美股大盤
stock_data_SOX<-function(snum,fromdate,todate){
  ###################################################################
  ##原矩陣
  #from為起始時間 #Sys.Date()
  sdata <- getSymbols(paste0('^',"SOX"),auto.assign=FALSE,src="yahoo",from=fromdate,to=as.Date(todate)+1)
  
  #將時間序列轉為變數
  sdata_date<-index(sdata)
  sdata<-as.data.frame(sdata)
  sdata<-cbind(sdata_date,sdata)
  #算有幾列
  sd_nrow<-nrow(sdata)
  #################################################################
  ##計算用矩陣
  #往前算90天
  s_c_data <- getSymbols(paste0('^',"SOX"),auto.assign=FALSE,src="yahoo",from=as.Date(fromdate)-90,to=todate)
  
  #將時間序列轉為變數
  sdata_c_date<-index(s_c_data)
  s_c_data<-as.data.frame(s_c_data)
  s_c_data<-cbind(sdata_c_date,s_c_data)
  s_c_nrow<-nrow(s_c_data)
  ################################################################
  #回傳股票代碼、起始日期、
  return(list(snum='SOX',fromdate,todate,
              sdata,sd_nrow,
              s_c_data,s_c_nrow)
  )
}

##股票的介面圖 
#美股大盤+布林
SOX_BB_p<-function(stock_data_SOX_fun){
  
  #將資訊輸入
  sdata_f<-stock_data_SOX_fun
  snum<-sdata_f[[1]]
  fromdate<-sdata_f[[2]]
  todate<-sdata_f[[3]]
  data<-sdata_f[[4]]
  sdata_date<-sdata_f[[4]][,"sdata_date"]
  sd_nrow<-sdata_f[[5]]
  s_c_data<-sdata_f[[6]]
  s_c_nrow<-sdata_f[[7]]
  Close= s_c_data[,paste0(snum,".Close")]
  MA_20<-c(rep(0,length(Close)))
  for(i in 20:length(Close)){MA_20[i]=mean(Close[c(i-19):i])}
  MA_UP<-c(rep(0,length(Close)))
  for(i in 20:length(Close)){MA_UP[i]=mean(Close[c(i-19):i])+2*sd(Close[c(i-19):i])}
  MA_DOWN<-c(rep(0,length(Close)))
  for(i in 20:length(Close)){MA_DOWN[i]=mean(Close[c(i-19):i])-2*sd(Close[c(i-19):i])}
  #EGMD<-as.data.frame(EGMD)
  #date <- as.Date(row.names(EGMD))
  
  MA_20<-MA_20[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  MA_UP<-MA_UP[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  MA_DOWN<-MA_DOWN[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  afterc_sdata<-s_c_data[c(s_c_nrow-sd_nrow+1):s_c_nrow,]
  
  Group<-rep(0,length(data[,paste0(snum,".Open")]))
  equal_pt<-rep(0,length(data[,paste0(snum,".Open")]))
  
  for(i in 1:length(data[,paste0(snum,".Open")])){
    ifelse(data[,paste0(snum,".Open")][i]>=data[,paste0(snum,".Close")][i]
           ,group<-"rise",group<-"fall")  
    ifelse(data[,paste0(snum,".Open")][i]==data[,paste0(snum,".Close")][i]
           ,equal_pt[i]<-data[,paste0(snum,".Open")][i],equal_pt[i]<-NA)
    Group[i]<-group
  }
  data<-cbind(data,Group,equal_pt)
  
  #k棒粗細調整
  fromdate<-as.Date(fromdate)
  todate<-as.Date(todate)
  ifelse(fromdate-todate<=14,histogram_width<-0.25,histogram_width<-0.5)
  
  #k棒
  #ggplot +號放後面
  candle_p<-ggplot(data=data,aes(x=sdata_date))+
    geom_linerange(aes(ymin=data[,paste0(snum,".Low")]
                       ,ymax=data[,paste0(snum,".High")]
    )
    ,size=0.2)+
    geom_rect(aes(xmin=sdata_date-histogram_width,xmax=sdata_date+histogram_width
                  ,ymin=pmin(data[,paste0(snum,".Open")],data[,paste0(snum,".Close")])
                  ,ymax=pmax(data[,paste0(snum,".Open")],data[,paste0(snum,".Close")])
                  ,fill=data[,"Group"])
    )+
    geom_point(aes(y = equal_pt), shape = "-", size = 4, color = "black",na.rm=T)+
    
    scale_fill_manual(values = c("fall"="#00bb00","rise"="#ce0000"))+
    labs(x="",y="價錢"
         ,title=paste0("美股大盤",snum," ","[",sdata_date[1],"/",sdata_date[length(sdata_date)],"]"),fill=NULL)+
    scale_x_date(position="top",date_breaks = "1 month", date_labels=c("%m/%d"))+
    theme(axis.text.y = element_text(margin = margin(r = -21)),legend.position = "none")+        
    geom_line(aes(y = MA_20),
              color = "blue", size = 0.5)+
    geom_line(aes(y = MA_DOWN),
              color = "blue",  size = 0.5)+
    geom_line(aes(y = MA_UP),
              color = "blue",  size = 0.5)
  print(candle_p)
}

#美股大盤+移動平均線
SOX_MA_p<-function(stock_data_SOX_fun){
  
  #將資訊輸入
  sdata_f<-stock_data_SOX_fun
  snum<-sdata_f[[1]]
  fromdate<-sdata_f[[2]]
  todate<-sdata_f[[3]]
  data<-sdata_f[[4]]
  sdata_date<-sdata_f[[4]][,"sdata_date"]
  sd_nrow<-sdata_f[[5]]
  s_c_data<-sdata_f[[6]]
  s_c_nrow<-sdata_f[[7]]
  Close= s_c_data[,paste0(snum,".Close")]
  MA_5<-c(rep(0,length(Close)))
  for(i in 5:length(Close)){MA_5[i]=mean(Close[c(i-4):i])}
  MA_20<-c(rep(0,length(Close)))
  for(i in 20:length(Close)){MA_20[i]=mean(Close[c(i-19):i])}
  MA_60<-c(rep(0,length(Close)))
  for(i in 60:length(Close)){MA_60[i]=mean(Close[c(i-59):i])}
  #EGMD<-as.data.frame(EGMD)
  #date <- as.Date(row.names(EGMD))
  
  MA_5<-MA_5[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  MA_20<-MA_20[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  MA_60<-MA_60[c(s_c_nrow-sd_nrow+1):s_c_nrow]
  afterc_sdata<-s_c_data[c(s_c_nrow-sd_nrow+1):s_c_nrow,]
  
  Group<-rep(0,length(data[,paste0(snum,".Open")]))
  equal_pt<-rep(0,length(data[,paste0(snum,".Open")]))
  
  for(i in 1:length(data[,paste0(snum,".Open")])){
    ifelse(data[,paste0(snum,".Open")][i]>=data[,paste0(snum,".Close")][i]
           ,group<-"rise",group<-"fall")  
    ifelse(data[,paste0(snum,".Open")][i]==data[,paste0(snum,".Close")][i]
           ,equal_pt[i]<-data[,paste0(snum,".Open")][i],equal_pt[i]<-NA)
    Group[i]<-group
  }
  data<-cbind(data,Group,equal_pt)
  
  #k棒粗細調整
  fromdate<-as.Date(fromdate)
  todate<-as.Date(todate)
  ifelse(fromdate-todate<=14,histogram_width<-0.25,histogram_width<-0.5)
  
  #k棒
  #ggplot +號放後面
  candle_p<-ggplot(data=data,aes(x=sdata_date))+
    geom_linerange(aes(ymin=data[,paste0(snum,".Low")]
                       ,ymax=data[,paste0(snum,".High")]
    )
    ,size=0.2)+
    geom_rect(aes(xmin=sdata_date-histogram_width,xmax=sdata_date+histogram_width
                  ,ymin=pmin(data[,paste0(snum,".Open")],data[,paste0(snum,".Close")])
                  ,ymax=pmax(data[,paste0(snum,".Open")],data[,paste0(snum,".Close")])
                  ,fill=data[,"Group"])
    )+
    geom_point(aes(y = equal_pt), shape = "-", size = 4, color = "black",na.rm=T)+
    
    scale_fill_manual(values = c("fall"="#00bb00","rise"="#ce0000"))+
    labs(x="",y="價錢"
         ,title=paste0("美股大盤",snum," ","[",sdata_date[1],"/",sdata_date[length(sdata_date)],"]"),fill=NULL)+
    scale_x_date(position="top",date_breaks = "1 month", date_labels=c("%m/%d"))+
    theme(axis.text.y = element_text(margin = margin(r = -21)),legend.position = "none")+        
    geom_line(aes(y = MA_5),
              color = "blue", size = 0.5)+
    geom_line(aes(y = MA_20),
              color = "orange",  size = 0.5)+
    geom_line(aes(y = MA_60),
              color = "purple",  size = 0.5)
  print(candle_p)
}

#籌碼面
chip.surface=function(data){
  #將資訊輸入
  sdata_f<-data
  snum<-sdata_f[[1]]
  snum=substring(snum, 1, 4)
  url=paste0("https://histock.tw/stock/chips.aspx?no=",snum)
  htmlcontent=read_html(url)
  titlepath='//*[contains(concat( " ", @class, " " ), concat( " ", "w2", " " ))] | //*[contains(concat( " ", @class, " " ), concat( " ", "w1", " " ))]'
  titles=htmlcontent %>% html_nodes(xpath=titlepath) %>% html_text()
  mytable=data.frame(title=titles)
  mytable=as.matrix(mytable)
  dim(mytable)<-matrix(c(6,32))
  mytable=t(mytable)
  rowtitle=mytable[,c(1)]
  mytable=mytable[,2:5]
  mytable=cbind(rowtitle,mytable)
  mytable=mytable[-17,]
  mytable=data.frame(mytable)
  snum<-sdata_f[[1]]
  main.title=paste0(snum,"籌碼面")
  footnote="單位:台幣(元)"
  mytable=ggtexttable(mytable,rows  =  NULL,cols = NULL,theme = ttheme(base_style = "light",base_size = 15,base_colour = "black"))
  mytable%>%
    tab_add_title(text = main.title, face = "bold", size=40.0,padding = unit(1.5, "line"))%>%
    tab_add_footnote(text = footnote,face = "bold", size=25.0,padding = unit(1.5, "line"))}

#盒形圖

box_p<-function(data){
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) { 
    library(grid) 
    plots <- c(list(...), plotlist)
    numPlots = length(plots)
    if (is.null(layout)) { 
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), 
                       ncol = cols, nrow = ceiling(numPlots/cols)) 
    } 
    
    if (numPlots==1) { 
      print(plots[[1]]) 
      
    } else { 
      
      grid.newpage() 
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout)))) 
      
      for (i in 1:numPlots) { 
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE)) 
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, 
                                        layout.pos.col = matchidx$col)) 
      } 
    } 
  }  
  
  #將資訊輸入
  sdata_f<-data
  snum<-sdata_f[[1]]
  
  sdata<-sdata_f[[4]]
  sd_nrow<-sdata_f[[5]]
  
  s_c_data<-sdata_f[[6]]
  s_c_nrow<-sdata_f[[7]]
  ##########
  afterc_sdata<-s_c_data[c(s_c_nrow-sd_nrow+1):s_c_nrow,]
  
  open=afterc_sdata[,2]
  high=afterc_sdata[,3]
  low=afterc_sdata[,4]
  close=afterc_sdata[,5]
  volume=afterc_sdata[,6]
  adjusted=afterc_sdata[,7]
  ####
  open<-as.data.frame(open)
  high<-as.data.frame(high)
  low<-as.data.frame(low)
  close<-as.data.frame(close)
  volume<-as.data.frame(volume)
  adjusted<-as.data.frame(adjusted)
  ####
  O=ggplot(open, aes(y=open))+scale_x_discrete()+geom_boxplot()+ xlab("開盤價")+ylab("")+
    labs(caption = "開盤時(股票市場開始營業時)的股票價格")+theme(plot.caption=element_text(hjust=0.5,size=11))
  
  H=ggplot(high, aes(y=high))+scale_x_discrete()+geom_boxplot()+ xlab("最高價")+ylab("")+
    labs(caption = "一天中最高的成交價")+theme(plot.caption=element_text(hjust=0.5,size=11))
  L=ggplot(low, aes(y=low))+scale_x_discrete()+geom_boxplot()+ xlab("最低價")+ylab("")+
    labs(caption = "一天中最低的成交價")+theme(plot.caption=element_text(hjust=0.5,size=11))
  C=ggplot(close, aes(y=close))+scale_x_discrete()+geom_boxplot()+ xlab("收盤價")+ylab("")+
    labs(caption = "收盤時(股票市場關門打烊時)的股票價格")+theme(plot.caption=element_text(hjust=0.5,size=11))
  V=ggplot(volume, aes(y=volume))+scale_x_discrete()+geom_boxplot()+ xlab("成交量")+ylab("")+
    labs(caption = "某一特定資產在一段時間內交易的數量")+theme(plot.caption=element_text(hjust=0.5,size=11))
  A=ggplot(adjusted, aes(y=adjusted))+scale_x_discrete()+geom_boxplot()+ xlab("調整價")+ylab("")+
    labs(caption = "調整後的收盤價")+theme(plot.caption=element_text(hjust=0.5,size=11))
  
  multiplot(O,H,L,C,V,A, cols=3)  
  
}

#回測
backtesting=function(snum,startDT,endDT,in_strategy,out_strategy){
  #
  test<-getSymbols(snum,auto.assign=FALSE,from=startDT,to=endDT)
  close <- Cl(test)
  ma5 <- SMA(close, 5)
  ma10 <- SMA(close, 10)
  ma20 <- SMA(close, 20)
  
  #進出場策略選擇
  #做多訊號
  #布林值轉數值。等於1有符合，等於0沒有
  long_insite<-0
  long_outsite<-0
  
  switch(in_strategy,
         "ma5>ma10&ma10>ma20"={long_insite<-as.numeric(ma5>ma10&ma10>ma20)}#進場訊號
  )
  switch(out_strategy,
         "ma5<ma10"={long_outsite<-as.numeric(ma5<ma10)}#出場訊號
  )
  
  #
  roc <- ROC(type="discrete",close)#用離散方式計算每日收益
  ret <- roc * long_insite  #當天有無報酬率
  test=cbind(test,ma5,ma10,ma20,long_insite,long_outsite,ret)
  
  #
  colnames(test)=c("開盤價","最高價","最低價","收盤價","成交量","調整價","五日移動平均線","十日移動平均線","二十日移動平均線","進場訊號","出場訊號","ROI")
  backtest_p <- charts.PerformanceSummary(ret) #1.累計報酬率 2.單日報酬率 3.下跌圖
  return(list(test,backtest_p))
}

#上架後使中文不顯示亂碼
options(shiny.usecairo = FALSE)#強制使用R的png()
#解決ShinyApps上没有中文字體的問題
font_home <- function(path = '') file.path('~', '.fonts', path)
if (Sys.info()[['sysname']] == 'Linux' &&
    system('locate wqy-zenhei.ttc') != 0 &&
    !file.exists(font_home('wqy-zenhei.ttc'))) {
  if (!file.exists('wqy-zenhei.ttc'))
    curl::curl_download(
      'https://github.com/rstudio/shiny-examples/releases/download/v0.10.1/wqy-zenhei.ttc',
      'wqy-zenhei.ttc'
    )
  dir.create(font_home())
  file.copy('wqy-zenhei.ttc', font_home())
  system2('fc-cache', paste('-f', font_home()))
}
rm(font_home)


if (.Platform$OS.type == "windows") {
  if (!grepl("Chinese", Sys.getlocale())) {
    warning(
      "You probably want Chinese locale on Windows for this app",
      "to render correctly. See ",
      "https://github.com/rstudio/shiny/issues/1053#issuecomment-167011937"
    )
  }
}


################################################################################

ui <- fluidPage(
  absolutePanel(
    top = 110, left = "auto", right = 40,height = "auto",draggable = TRUE,
    dropdownButton(
      label = "圖片設定",
      ##可加ui
      radioButtons("diff_dates_interval",label = "",
                   choices = c("年"="years","月"="months","日"="days"),
                   selected = "months"),
      tabsetPanel(
        id = "chose_dates_interval",
        type = "hidden",
        tabPanel("years",#要與輸入的名字相同
                 sliderInput("slide_year","以幾年為間隔",min=1,max=10,value=1)
        ),
        tabPanel("months",
                 sliderInput("slide_month","以幾月為間隔",min=1,max=12,value=1)
        ),
        tabPanel("days",
                 sliderInput("slide_day","以幾日為間隔",min=1,max=366,value=7)
        )
      ),
      right = T,icon = icon("sliders-h"),
      status = "primary",circle = FALSE
    )
  ),
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
      }
    "))
  ),
  
  #主題
  theme=shinytheme("superhero"),
  #themeSelector(),#自行選擇主題
  titlePanel("韭菜·史塔克"),
  #多頁
  navbarPage(
    # 頁面標題
    "股票查詢",
    # 第一頁
    tabPanel(
      "技術面",icon = icon("chart-line"), 
      sidebarLayout(
        sidebarPanel(
          
          #helpText("請輸入台股編號4或6碼"),
          #拉頁找代碼
          #selectInput(
          #"canpabbnm",
          #label = "",
          #choices =list("台股編號"=list[,1]),
          #selected = "2603.TW"
          #),
          h3("選擇股票"),
          pickerInput(
            inputId = "canpabbnm",
            label = "請輸入台股編號4或6碼",
            choices = list(
              上市=c(data_TSE_SC,data_TSE),
              上櫃=c(data_OTC)),
            selected="2603.TW 長榮 上市",
            multiple = FALSE,
            options = pickerOptions(`live-search` = TRUE),
            choicesOpt = list(
              style = rep(("color: black; background: white;"),5000))
          ),
          
          dateRangeInput("date", label = h3("選擇時間"),separator="到",
                         start  = "2021-12-15",
                         end    = "2022-04-14",
                         min    = "2000-08-15",#可改
                         max    = Sys.Date()
          ),
          
          helpText("請以正常時間順序輸入"),
          helpText("例:2021-01-01 到 2021-12-31"),
          
          #k棒、bb、ma
          #k棒、bb、ma
          selectInput("base","基本訊息:",
                      c("K棒圖" = "candle_p",
                        "布林通道" = "bb_p",
                        "移動平均線" = "ma_p"),
                      selected = "K棒圖"
          ),
          
          #是否要成交量
          checkboxInput("volplot", label = "是否要顯示成交量",value =T),
          
          #指標輸入位置
          h3("選擇指標"),
          selectInput("index1","指標訊息:",
                      c("BIAS乖離率"="BIAS","KDJ隨機指標"= "KDJ","RSI相對強弱指數"="RSI"
                        ,"OBV能量潮指標" = "OBV","AR人氣指標&BR收盤指標" = "AR&BR"),
                      selected = "BIAS"),
          #radioButtons( 改成上面的selectinput 7個指標的點選按鈕所佔的空間太多
          #  "index1",label = "",
          #  choices = c("BIAS乖離率"="BIAS","KDJ隨機指標"= "KDJ","RSI相對強弱指數"="RSI"
          #              ,"OBV能量潮指標" = "OBV","AR開價指標&BR收盤價強弱指" = "AR&BR"),
          #  selected = "BIAS"
          #)
          
          #
          tabsetPanel(#根據不同條件變更ui
            id = "params",
            type = "hidden",
            tabPanel("BIAS",#要與輸入的名字相同
                     checkboxInput("whether_riskline1", label = "是否要顯示風險界線",value =T),
                     conditionalPanel(condition = "input.whether_riskline1 == true",
                                      numericInput("up_bias_risk",label="上界線",min=0,value=11),
                                      numericInput("dn_bias_risk",label="下界線",min=0,value=11)
                     ),
                     checkboxInput("whether_image1", label = "是否要顯示教學圖",value = F)
            ),
            tabPanel("KDJ",
                     checkboxInput("whether_riskline2", label = "是否要顯示風險界線",value =T),
                     conditionalPanel(condition = "input.whether_riskline2 == true",
                                      numericInput("up_kdj_risk", "上界線",min=0,value = 80),
                                      numericInput("dn_kdj_risk", "下界線",min=0,value = 20)
                     ),                     
                     checkboxInput("whether_image2", label = "是否要顯示教學圖",value =F)
            ),
            tabPanel("RSI", 
                     checkboxInput("whether_riskline3", label = "是否要顯示風險界線",value =T),
                     conditionalPanel(condition = "input.whether_riskline3 == true",
                                      numericInput("up_rsi_risk", "超買區界值",min=0,value = 70),
                                      numericInput("dn_rsi_risk", "超賣區界值",min=0,value = 30)
                     ),
                     checkboxInput("whether_image3", label = "是否要顯示教學圖",value =F)
            ),
            tabPanel("OBV",
                     checkboxInput("whether_riskline4", label = "是否要顯示風險界線",value =T),
                     conditionalPanel(condition = "input.whether_riskline4 == true",
                                      numericInput("obv_risk", "界線", value = 1, min = 0),
                     ),
                     checkboxInput("whether_image4", label = "是否要顯示教學圖",value =F)
            ),
            tabPanel("AR&BR",
                     checkboxInput("whether_riskline5", label = "是否要顯示風險界線",value =T),
                     conditionalPanel(condition = "input.whether_riskline5 == true",
                                      numericInput("arbr_risk", label="界線", value = 1, min = -3),
                     ),                     
                     checkboxInput("whether_image5", label = "是否要顯示教學圖",value =F)
            )),
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("股票趨勢圖",
                     box(
                       title = "", status = "primary", solidHeader = TRUE,width =NULL,height = NULL,
                       withLoader(plotOutput("smplot"), type="html", loader="pacman")
                     ),
                     #價量輸出位置
                     box(
                       title = "", status = "primary", solidHeader = TRUE,width =NULL,height = NULL,
                       withLoader(uiOutput("vol_position"), type="html", loader="pacman")
                     ),
                     #指標輸出位置
                     box(
                       title = "", status = "primary", solidHeader = TRUE,width =NULL,height = NULL,
                       withLoader(plotOutput("indexplot"), type="html", loader="pacman")
                     ),
                     br(),
                     #教學圖
                     conditionalPanel(condition = "input.whether_image1 == true",
                                      wellPanel(img(src="bias.png", height=430,width=760),style = "background: white")),
                     conditionalPanel(condition = "input.whether_image2 == true",
                                      wellPanel(img(src="kdj.png", height=430,width=760),style = "background: white")),
                     conditionalPanel(condition = "input.whether_image3 == true",
                                      wellPanel(img(src="rsi.png", height=430,width=760),style = "background: white")),
                     conditionalPanel(condition = "input.whether_image4 == true",
                                      wellPanel(img(src="obv.png", height=430,width=760),style = "background: white")),
                     conditionalPanel(condition = "input.whether_image5 == true",
                                      wellPanel(img(src="ar.png", height=430,width=760),style = "background: white"),
                                      br(),
                                      wellPanel(img(src="br.png", height=430,width=760),style = "background: white")),
                     
                     #plotOutput(
                     #outputId,
                     #width = "100%",
                     #height = "400px",
                     #click = NULL,
                     #dblclick = NULL,
                     #hover = NULL,
                     #brush = NULL,
                     #inline = FALSE)
            ),
            tabPanel("台/美股票大盤",
                     h4("台股大盤產業中以半導體占比最多，進而延伸至美股中屬於半導體產業指數，就是費城半導體指數(SOX)，因此兩個的走勢相近。詳細資訊可參考", style = "color:white;"),
                     p(tags$li(tags$b(tags$a(href="https://www.herishare.com/taiex-trend-relationship/", "此網址。", class="externallink")),
                     #選擇大盤指標
                     selectInput("index2","選擇台股大盤指標:",
                                 c("移動平均線"= "TWII_MA","布林通道"="TWII_BB"),
                                 selected = "移動平均線"
                     ),
                     selectInput("index3","選擇美股大盤指標:",
                                 c("移動平均線"= "SOX_MA","布林通道"="SOX_BB"),
                                 selected = "移動平均線"
                     ),
                     box(
                       title = "", status = "primary", solidHeader = TRUE,width =NULL,height = NULL,
                       withLoader(plotOutput("TWIIplot"), type="html", loader="pacman")
                     ),
                     box(
                       title = "", status = "primary", solidHeader = TRUE,width =NULL,height = NULL,
                       withLoader(plotOutput("SOXplot"), type="html", loader="pacman")
                     )
            ),
            tabPanel("個股股價行情表",
                     style = "background-color: #ffffff;",
                     br(),
                     box(
                       title = "", status = "primary", solidHeader = TRUE,width =NULL,height = NULL,
                       withLoader( DT::dataTableOutput("smtable"), type="html", loader="pacman")
                     )
            ),
            
            tabPanel("彙整表",
                     br(),
                     box(
                       title = "", status = "primary", solidHeader = TRUE,width =NULL,height = NULL,
                       withLoader( verbatimTextOutput("smsumy"), type="html", loader="pacman")
                     ),
                     box(
                       title = "", status = "primary", solidHeader = TRUE,width =NULL,height = NULL,
                       withLoader( plotOutput("box_p"), type="html", loader="pacman")
                     )
            )
          )
        )
      )
    ),
    # 第二頁
    tabPanel(
      "基本面(簡易財務報表)",icon = icon("balance-scale"),value = "about",
      mainPanel(
        tabsetPanel(
          br(),
          box(
            title = "", status = "primary", solidHeader = TRUE,width =NULL,height = NULL,
            withLoader(  plotOutput("Income_statement",height = "600px",width="620px"), type="html", loader="pacman")
          ),
          br(),
          box(
            title = "", status = "primary", solidHeader = TRUE,width =NULL,height = NULL,
            withLoader(  plotOutput("Cash_flow_statement",height = "600px",width="620px"), type="html", loader="pacman")
          ),
          br(),
          box(
            title = "", status = "primary", solidHeader = TRUE,width =NULL,height = NULL,
            withLoader(  plotOutput("Balance_sheet",height = "600px",width="620px"), type="html", loader="pacman")
          ),
          p(tags$li(tags$b(tags$a(href="https://statementdog.com/", "資料來源:財務報表", class="externallink")),
                    ""))
        )
      )
    ),
    # 第三頁
    tabPanel(
      "籌碼面",icon = icon("chart-area"),
      mainPanel(
        br(),
        box(
          title = "", status = "primary", solidHeader = TRUE,width =NULL,height = NULL,
          withLoader(plotOutput("chip_surface",height = "820px",width="400px"), type="html", loader="pacman")
        ),
        p(tags$li(tags$b(tags$a(href="https://histock.tw/%E5%8F%B0%E8%82%A1%E5%A4%A7%E7%9B%A4", "資料來源:籌碼面", class="externallink")),
                  ""))
      )
    ),
    #第四頁
    tabPanel(
      "回測分析",icon = icon("trello"),
      mainPanel(
        h4("回測的用意是擬定一組策略，然後帶回到以前的日子中，看此策略的報酬是否不錯。
           所以可以擬訂很多組策略來自行觀察哪組最佳。而我們這邊的策略是，當五日線大於十日線、
           十日線大於二十日線時作買進，當五日線一跌破二十日線時作賣出。
           可以先用我們設定的回測去研究此方法對哪支股票的報酬率最好。", style = "color:white;"),
        h5("上圖:累計收益、中圖:日收益、下圖:下跌圖(將下跌成分獨立繪出，有助於我們分析虧損狀況和研究彌補措施)", style = "color:white;"),
        br(),
        box( 
          title = "", status = "primary", solidHeader = TRUE,width =NULL,height = NULL,
          withLoader(plotOutput("cumulative_return",height = "820px",width="500px"), type="html", loader="pacman")
        ),
        br(),
        downloadButton("backtesting", "下載回測分析excel"),
        br(),
        br()
      )
    ),
    #第五頁
    navbarMenu(
      "常見問題", icon = icon("info-circle"),
      tabPanel("指標含意", value = "about",
               mainPanel(width=15,
                         mainPanel(width=15,
                                   h3("關於指標", style = "color:white;"),
                                   p(tags$li(tags$b(tags$a(href="https://zh.wikipedia.org/wiki/%E5%B8%83%E6%9E%97%E5%B8%A6", "布林通道 (Bollinger Bands,BBands)", class="externallink")),
                                             ":又稱布林帶、布林格帶狀、保力加通道，是由約翰．布林格(John Bollinger)所提出的概念，可以從中看出買賣訊號、進出場時機。")), 
                                   
                                   p(tags$li(tags$b(tags$a(href="https://zh.wikipedia.org/wiki/%E4%B9%96%E9%9B%A2%E7%8E%87", "乖離率 (Bias Ratio)", class="externallink")),
                                             ":代表當日股票收盤價或盤中市價與移動平均線的差距，以分析股價偏離某時期平均價(平均成本)的程度。")),
                                   
                                   p(tags$li(tags$b(tags$a(href="https://wiki.mbalib.com/zh-tw/BRAR%E6%8C%87%E6%A0%87", "開價指標 (Popularity Index,AR)", class="externallink")),
                                             ":以當天開盤價為基礎，分別比較當天最高價、最低價，透過一定期間內開盤價在股價中的強弱比率，反映市場買賣人氣。")),
                                   
                                   p(tags$li(tags$b(tags$a(href="https://wiki.mbalib.com/zh-tw/BRAR%E6%8C%87%E6%A0%87", "收盤價強弱指標 (Willingness Index,BR)", class="externallink")),
                                             ":是反映當前情況下多空雙方力量爭鬥的結果。不同的是它是以前一日的收盤價為基礎，與當日的最高價、最低價相比較，依固定公式計算出來的強弱指標。")),
                                   
                                   p(tags$li(tags$b(tags$a(href="https://zh.wikipedia.org/wiki/%E7%A7%BB%E5%8B%95%E5%B9%B3%E5%9D%87", "移動平均線 (Moving Average,MA)", class="externallink")),
                                             ":又稱滾動平均值、滑動平均，在統計學中是一種通過創建整個數據集中不同子集的一系列平均數來分析數據點的計算方法。")),
                                   
                                   p(tags$li(tags$b(tags$a(href="https://zh.wikipedia.org/wiki/KD%E6%8C%87%E6%A8%99", "隨機指標 (Stochasticsoscillator , KDJ)", class="externallink")),
                                             ":是技術分析中的一種動量分析方法，採用超買和超賣的概念，由喬治·萊恩(George C. Lane)在1950年代推廣使用。指標藉由比較收盤價格和價格的波動範圍，預測價格趨勢何時逆轉。")),
                                   
                                   p(tags$li(tags$b(tags$a(href="https://www.cmoney.tw/learn/course/cmoney/topic/139", "逆勢指標 (Relative Strength Index,RSI)", class="externallink")),
                                             ":是一個藉由比較價格升降多寡，計算出價格強度的指標。RSI 指數的應用，是根據「漲久必跌，跌久必漲」的原則，去按段高低價並做買賣。")),
                                   p(tags$li(tags$b(tags$a(href="https://www.moneydj.com/kmdj/wiki/wikiviewer.aspx?keyid=1caf93e4-a640-422a-8db1-d20574d9f163", "能量潮指標(On Balance Volume,OBV)", class="externallink")),
                                             ":也有人稱之為人氣指標，是一種依據行情的漲跌，來累計或刪去市場的成交量值，而以此累算值作為市場行情動能變化趨勢的指標。"))
                         ))
      ),
      tabPanel("指標技術應用", value = "about",
               mainPanel(width=15,
                         h3("指標技術應用", style = "color:white;"),
                         p("布林通道(Bollinger Bands , BBands):",style ="color:Orange")),
               tags$li("碰到通道下線 : 股價止跌或上漲。"),
               tags$li("碰到通道上線 : 股價下跌。"),
               tags$li("股價由下往上穿越下線：股價可能短期會反轉。"),
               tags$li("股價由下往上穿越中線：股價可能會加速向上，是買進訊號。"),
               tags$li("股價在中線與上線之間：代表目前為多頭行情。"),
               tags$li("股價由上往下跌破上線：暗示上漲趨勢結束，是賣出的訊號。"),
               tags$li("股價由上往下跌破中線：股價可能會下跌，是賣出訊號。"),
               tags$li("股價在中線與下線之間：代表目前為空頭行情。"),
               br(),
               p("乖離率(Bias Ratio):",style ="color:Orange"),
               tags$li("正乖離：收盤價 > 移動平均價 →股價高於均價，股價下跌修正的可能性變高。"),
               tags$li("負乖離：收盤價 < 移動平均價→股價低於均價，股價上漲修正的可能性變高。"),
               br(),
               p("開價指標(Popularity Index , AR):",style ="color:Orange"),
               tags$li("AR=1 : 強弱買賣平衡狀態。"),
               tags$li("AR>1 : 走高能量>走低能量→股價強勢期。"),
               tags$li("AR<1 : 走高能量>走低能量→股價弱勢期。"),
               tags$li("AR過高 : 股價可能進入高檔，賣出訊號。"),
               tags$li("AR過低 : 股價可能跌入谷底，買入訊號。"),
               br(),
               p("收盤價強弱指標 (Willingness Index , BR):",style ="color:Orange"),
               tags$li("BR=1 : 買賣意願平衡。"),
               tags$li("BR>1 : 股價行情不會在長期持續上漲 (已步入高價圈)。"),
               tags$li("BR<1 : 股價行情不會在長期持續下跌 (已步入低價圈)。"),
               br(),
               p("移動平均線 (Moving Average ,MA):",style ="color:Orange"),
               tags$li("黃金交叉：短均線向上突破長均線(當移動平均線20MA向上突破60MA)，表示短期內可能會上漲、有波段漲幅，適合多單進場或是空單出場。"),
               tags$li("死亡交叉：短均線向下突破長均線(當移動平均線20MA跌破60MA)，表示短期內可能會下跌、有波段跌幅，適合空單進場或是多單出場。"),
               br(),
               p("隨機指標(Stochasticsoscillator , KDJ):",style ="color:Orange"),
               tags$li("高檔鈍化：連三天KDJ值>80，股價表現強勢，買進訊號。"),
               tags$li("低檔鈍化：連三天KDJ值<20，股價表現弱勢，賣出訊號。"),
               tags$li("黃金交叉：K值由下而上穿越D值，行情看好，買進訊號。"),
               tags$li("死亡交叉：K值由上而下穿越D值，行情看差，賣出訊號。"),
               tags$li("J值大於100時，股價會形成頭部而出現回落，賣出訊號。"),
               tags$li("J值小於0時，股價會形成底部而產生反彈，買進訊號。"),
               br(),
               p("逆勢指標(Relative Strength Index , RSI):",style ="color:Orange"),
               tags$li("RSI升至70時，代表該證券已被超買，股票未來可能會下跌。"),
               tags$li("RSI升至30時，代表該證券已被超賣，股票未來可能會上漲。"),
               tags$li("黃金交叉: 當短期RSI線向上穿過長期的RSI線時，價格上漲，買入信號。"),
               tags$li("死亡交叉: 當短期RSI線向下跌破長期的RSI線時，價格下跌，賣出信號。"),
               br(),
               p("能量潮指標(On Balance Volume , OBV):",style ="color:Orange"),
               tags$li("OBV下降，行情上升時，為賣出信號，表示買盤無力。"),
               tags$li("OBV上升，行情下降，為買進信號，表示逢低接手轉強。"),
               tags$li("OBV緩慢上升，為買進信號，表示買盤轉強。"),
               tags$li("OBV急速上升，為賣出信號，表示買盤全力介入，多頭即將力竭。"),
               tags$li("OBV值從正的累積數轉為負的累積數時，為下跌趨勢，持倉應做空。反之，OBV 從負的累積數轉為正數，則應該跟進做多。"),
               tags$li("若在OBV線的累計值高點，價格無法突破，此為上漲壓力帶，行情經常會在此高點遇阻而反轉，需特別注意。"),
               tags$li("運用OBV線時，需配合K線觀察，尤其是價格趨勢在盤檔能否一舉突破壓力帶，OBV線的變動方向是重要指標。"),
               br(),
               br()
      ),
      #教學圖
      tabPanel("指標教學圖", value = "about",
               mainPanel(
                 h3("指標教學圖", style = "color:white;"),
                 h5("MA 移動平均線", style = "color:white;"),
                 div(tags$img(src="ma.png",height="430px",width="760px",alt="圖片維修中")),
                 h5("Bollinger Band 布林通道", style = "color:white;"),
                 div(tags$img(src="bb.png",height="430px",width="760px",alt="圖片維修中")),
                 h5("BISA 乖離率", style = "color:white;"),
                 div(tags$img(src="bias.png",height="430px",width="760px",alt="圖片維修中")),
                 h5("KDJ 隨機指標", style = "color:white;"),
                 div(tags$img(src="kdj.png",height="430px",width="760px",align ="圖片維修中")),
                 h5("RSI 相對強弱指數", style = "color:white;"),
                 div(tags$img(src="rsi.png",height="430px",width="760px",align = "圖片維修中")),
                 h5("OBV 能量潮指標", style = "color:white;"),
                 div(tags$img(src="obv.png",height="430px",width="760px",align = "圖片維修中")),
                 h5("AR開價指標", style = "color:white;"),
                 div(tags$img(src="ar.png",height="430px",width="760px",align = "圖片維修中")),
                 h5("BR收盤價強弱指標", style = "color:white;"),
                 div(tags$img(src="br.png",height="430px",width="760px",align = "圖片維修中"))
               )
      )),
    #分享
    navbarMenu(
      "分享與反饋", icon = icon("share-alt"),
      tabPanel(
        messageItem(
          textInput("複製連結", "", "http://127.0.0.1:7786/"),
          uiOutput("clip"),
          icon = icon("link")
        )),
      tabPanel(
        messageItem(
          from = "聯絡信箱",
          message =  "",
          icon = icon("envelope"),
          href = "mailto: tkustatdc@gmail.com"
        )),
      tabPanel(
        messageItem(
          from = 'Twitter',
          message = "",
          icon = icon("twitter"),
          href = "https://twitter.com/intent/tweet?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&text=New%20Zealand%20Trade%20Intelligence%20Dashboard"
        )),
      tabPanel(
        messageItem(
          from = 'Facebook',
          message = "",
          icon = icon("facebook"),
          href = "https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Ftradeintelligence.mbie.govt.nz"
        )),
      tabPanel(
        messageItem(
          from = 'Tumblr',
          message = "",
          icon = icon("tumblr"),
          href = "http://www.tumblr.com/share?v=3&u=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&t=New%20Zealand%20Trade%20Intelligence%20Dashboard"
        ))
    )
  )
)

server = function(input,output,session) {
  
  ##隨時更新資料的函數
  react_fun <- reactiveValues()
  
  ##處理股票代碼字串反應式 “2603.TW 長榮 上市”
  react_stocknum_str<- reactive({
    stocknum_str<-function(snum_str){
      snum_str <- strsplit(snum_str,split=" ")
      return(snum_str[[1]][1])
    }
    stocknum_str(input$canpabbnm)
  })
  
  ##獲取股票資訊的反應式
  react_sdata<-reactive({
    stock_data(react_stocknum_str(),input$date[1],input$date[2])
  })
  react_sdata_TWII<-reactive({
    stock_data_TWII(react_stocknum_str(),input$date[1],input$date[2])
  })
  react_sdata_SOX<-reactive({
    stock_data_SOX(react_stocknum_str(),input$date[1],input$date[2])
  })
  
  ########validate###########
  react_valid_dateorder_abbr<- reactive({
    validate(need(input$date[2]>input$date[1] , "請檢查時間順序是否輸入錯誤")
    )
  })
  ##########################
  
  ##一般資訊
  output$smplot = renderPlot({
    #validate dateorder、abbr
    react_valid_dateorder_abbr()
    
    #選擇基本資訊反應式
    react_fun$baseinfo<-input$base
    #反應式匯入的字串執行相應程式
    switch(react_fun$baseinfo,
           "candle_p"=stock_interface(react_sdata(),react_dates_interval_str()),
           "ma_p"=Ma_p(react_sdata(),react_dates_interval_str()),
           "bb_p"=Bb_p(react_sdata(),react_dates_interval_str())
    )
  })
  
  ##價量圖
  output$vol_position <- renderUI({
    #ui需選擇其中一種方式去呈現(不能用False會出現error)
    if(input$volplot==T){
      output$volumeplot = renderPlot({
        #validate dateorder、abbr
        react_valid_dateorder_abbr()
        Volume_p(react_sdata(),react_dates_interval_str())
      })
      box(
        title = "", status = "primary", solidHeader = TRUE,width =NULL,height = NULL,
        withLoader(plotOutput("volumeplot"), type="html", loader="pacman")
      )
    }else{
      #用列印text的方式去取代沒有plot列印出
      output$vol_noplot<-renderText({invisible("")})#塞一個不指派就列印不出來的無字字串
      textOutput("vol_noplot")
    }
  })   
  
  ##指標圖UI變動
  observeEvent(input$index1,{
    updateTabsetPanel(inputId = "params", selected = input$index1)
  }) 
  
  ##圖設定變動
  observeEvent(input$diff_dates_interval,{#warning
    updateTabsetPanel(inputId = "chose_dates_interval", selected = input$diff_dates_interval)
  })
  
  #預設 以彈出視窗作為ui第一次actionbutton還是無法觸發
  default_val<-reactiveValues()
  default_val$dates_interval="1 months"
  ##根據選的年月日變動要貼的不同字串
  react_dates_interval_str<- reactive({
    switch(input$diff_dates_interval,#warning
           "years" = {default_val$dates_interval<-paste0(input$slide_year," ",input$diff_dates_interval)},
           "months" = {default_val$dates_interval<-paste0(input$slide_month," ",input$diff_dates_interval)},
           "days" = {default_val$dates_interval<-paste0(input$slide_day," ",input$diff_dates_interval)}
    )
  })
  
  ##根據選擇作不同指標圖反應式
  react_index_plot <- reactive({
    switch(input$index1,
           "BIAS" = Bias_p(react_sdata(),input$whether_riskline1,
                           input$up_bias_risk,input$dn_bias_risk,
                           react_dates_interval_str()
           ),
           "KDJ" = Kdj_p(react_sdata(),input$whether_riskline2,
                         input$up_kdj_risk,input$dn_kdj_risk,
                         react_dates_interval_str()),
           
           "RSI" = Rsi_p(react_sdata(),input$whether_riskline3,
                         input$up_rsi_risk,input$dn_rsi_risk,
                         react_dates_interval_str()
           ),
           "OBV" = Obv_p(react_sdata(),input$whether_riskline4,
                         input$obv_risk,react_dates_interval_str()),
           
           "AR&BR" = Arbr_p(react_sdata(),input$whether_riskline5,
                            input$arbr_risk,react_dates_interval_str()
           )
    )
  })
  
  ##匯出指標圖
  output$indexplot = renderPlot({
    #validate dateorder、abbr
    react_valid_dateorder_abbr()
    react_index_plot()
  })
  
  ##股票資訊
  output$smtable = DT::renderDataTable({
    #validate dateorder_Babbr
    react_valid_dateorder_abbr()
    #
    react_fun$firstdate<-input$date[1]
    react_fun$todate<-input$date[2]
    #
    react_fun$temcanpdf = as.data.frame(getSymbols( react_stocknum_str(),src="yahoo",
                                                    from=react_fun$firstdate,to=react_fun$todate,
                                                    auto.assign = F)
    )
    DT::datatable(react_fun$temcanpdf , options = list(pageLength = 25,order = list(0, 'desc')),
                  colnames = c('日期','開盤價', '最高價','最低價', '收盤價', '成交量','調整價'))
  })
  
  ##股票summary table
  output$smsumy = renderPrint({
    #validate dateorder_Babbr
    react_valid_dateorder_abbr()
    #
    react_fun$firstdate<-input$date[1]
    react_fun$todate<-input$date[2]
    #
    react_fun$temcanpdf = as.data.frame(getSymbols( react_stocknum_str(),src="yahoo",
                                                    from=react_fun$firstdate,to=react_fun$todate,
                                                    auto.assign = F)
    )
    summary(react_fun$temcanpdf)
  })
  
  ##新增複製網址按鈕
  output$clip <- renderUI({
    output$clip <- renderUI({
      #validate dateorder_Babbr
      react_valid_dateorder_abbr()
      #
      rclipButton(
        inputId = "clipbtn",
        label = "複製連結",
        clipText = input$copytext, 
        icon = icon("clipboard")
      )
    })
  })
  if (interactive()){
    observeEvent(input$clipbtn, clipr::write_clip(input$copytext))
  }
  
  ##財報
  output$Income_statement =renderPlot({
    #validate dateorder_Babbr
    react_valid_dateorder_abbr()
    #
    Income.statement(react_sdata())
  })
  
  output$Cash_flow_statement = renderPlot({
    #validate dateorder_Babbr
    react_valid_dateorder_abbr()
    #
    Cash.flow.statement(react_sdata())
  })
  output$Balance_sheet = renderPlot({
    #validate dateorder_Babbr
    react_valid_dateorder_abbr()
    #
    Balance.sheet(react_sdata())
  })
  
  ##台股大盤
  ##根據選擇作不同指標圖反應式
  react_TWII_plot <- reactive({
    switch(input$index2,
           "TWII_MA" = TWII_MA_p(react_sdata_TWII()),
           "TWII_BB" = TWII_BB_p(react_sdata_TWII())
    )
  })
  
  ##匯出指標圖
  output$TWIIplot = renderPlot({
    #validate dateorder、abbr
    react_valid_dateorder_abbr()
    react_TWII_plot()
  })
  
  ##美股大盤
  ##根據選擇作不同指標圖反應式
  react_SOX_plot <- reactive({
    switch(input$index3,
           "SOX_MA" = SOX_MA_p(react_sdata_SOX()),
           "SOX_BB" = SOX_BB_p(react_sdata_SOX())
    )
  })
  
  ##匯出指標圖
  output$SOXplot = renderPlot({
    #validate dateorder、abbr
    react_valid_dateorder_abbr()
    react_SOX_plot()
  })
  
  ##籌碼面
  output$chip_surface = renderPlot({
    #隨時更新資料
    react_fun$firstdate<-input$date[1]
    react_fun$todate<-input$date[2]
    #validate dateorder_Babbr
    react_valid_dateorder_abbr()
    #
    react_fun$index_name<-input$index1
    #
    sdata<-stock_data(react_stocknum_str(),react_fun$firstdate,react_fun$todate)
    chip.surface(sdata)
  })
  
  ##盒形圖
  output$box_p= renderPlot({
    #隨時更新資料
    react_fun$firstdate<-input$date[1]
    react_fun$todate<-input$date[2]
    #validate dateorder_Babbr
    react_valid_dateorder_abbr()
    #
    sdata<-stock_data(react_stocknum_str(),react_fun$firstdate,react_fun$todate)
    box_p(sdata)
  })
  
  ##回測
  output$cumulative_return = renderPlot({
    #隨時更新資料
    react_fun$firstdate<-input$date[1]
    react_fun$todate<-input$date[2]
    #validate dateorder_Babbr
    react_valid_dateorder_abbr()
    #
    react_fun$index_name<-input$index1
    #
    backtesting(react_stocknum_str(),react_fun$firstdate,react_fun$todate,
                in_strategy="ma5>ma10&ma10>ma20",out_strategy="ma5<ma10")
  })
  output$cr_table = renderPrint({
    #validate dateorder_Babbr
    react_valid_dateorder_abbr()
    #
    react_fun$firstdate<-input$date[1]
    react_fun$todate<-input$date[2]
    #
    backtesting(react_stocknum_str(),react_fun$firstdate,react_fun$todate,
                in_strategy="ma5>ma10&ma10>ma20",out_strategy="ma5<ma10")[[1]]
  })
  
  #下載回測excel
  output$backtesting <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(),".csv", sep="")
    },
    content = function(file) {
      write.csv(backtesting(react_stocknum_str(),react_fun$firstdate,react_fun$todate,
                            in_strategy="ma5>ma10&ma10>ma20",out_strategy="ma5<ma10")[[1]], file,
                row.names=TRUE
                
      )
      
    }
  )
  
  
  
}




###執行
shinyApp(ui=ui,server=server)
