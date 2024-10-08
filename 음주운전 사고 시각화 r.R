setwd("C:/Users/임정은/Desktop/24년 1학기/금융통계")
library(tidyverse)
df<-read.csv("df_f1.csv") #파일 불러오기
View(df)
str(df)

x11()
df$요일 <- factor(df$요일, levels = c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일", "일요일")) #요일 데이터 순서 설정
df %>% 
  ggplot(aes(x = 요일))+
  geom_bar(col="black",fill="skyblue",alpha=0.5)+
  labs(x="요일", y = "사고수", title = "요일별 음주운전 사고수") #요일별 음주운전 사고수 시각화

df$시간대 <- factor(df$시간대, levels = c("새벽", "낮", "퇴근", "저녁", "밤", "아침", "출근")) #시간 데이터 순서 설정
df %>% 
  ggplot(aes(x = 시간대))+
  geom_bar(col="black",fill="skyblue",alpha=0.5)+
  labs(x="시간대", y = "사고수", title = "시간대별 음주운전 사고수") #시간대별 음주운전 사고수 시각화

holiday_count <- sum(df$휴일 == 1) #휴일 사고건수
weekday_count <- sum(df$휴일 == 0) #평일 사고건수
holiday<-holiday_count/351 #휴일 일일 음주운전 건수
weekday<-weekday_count/744 #평일 사고수를 평일갯수로 나눔
data <- data.frame(
  category = c("휴일", "평일"),
  ratio = c(holiday_ratio, weekday_ratio),
  group = c(rep("휴일", length(holiday_ratio)), rep("평일", length(weekday_ratio))) 
) #평일과 휴일 사고수 비를 데이터 프레임으로
total <- sum(data$ratio) #휴일비와 평일비 합
data$ratio <- data$ratio / total #합으로 각 비를 나눔

p<-ggplot(data, aes(x = category, y = ratio, fill = category)) +
  geom_bar(stat = "identity") +
  labs(title = "휴일과 평일 사고 비율",
       x = "구분",
       y = "비율") +
  scale_fill_manual(values = c("orange", "skyblue")) +
  theme_minimal() #휴일과 평일의 사고비율 시각화
p + geom_text(aes(label = round(ratio * 100, 2), y = ratio), vjust = -0.5) #비율 텍스트로

df$time<- substring(df$사고일시, 12,13) #시간 데이터 추출
df %>% 
  ggplot(aes(x = time))+
  geom_bar(col="black",fill="skyblue",alpha=0.5)+
  labs(x="시간", y = "사고수", title = "시간별 음주운전 사고수") #시간별 사고수 시각화

df$month<- substring(df$사고일시, 6,7) #월데이터 추출
month_names <- c("1월", "2월", "3월", "4월", "5월", "6월", "7월", "8월", "9월", "10월", "11월", "12월") #데이터 이름 재설정
for (i in 1:length(df$month)) {
  df$month[i] <-month_names[as.numeric(df$month[i])]
} #이름 설정
df$month <- factor(df$month, levels=month_names) #월 데이터 순서 설정
df %>% 
  ggplot(aes(x = month))+
  geom_bar(col="black",fill="skyblue",alpha=0.5)+
  labs(x="월", y = "사고수", title = "월별 음주운전 사고수") #월별 사고수 시각화

df2<-read.csv("df_f7.csv")
str(df2)
df2 %>% 
  ggplot(aes(x = 사고시))+
  geom_bar(col="black",fill="skyblue",alpha=0.5)+
  labs(x="시간", y = "사고수", title = "시간별 음주운전 사고수")
df_selected <- df2 %>%
  select(사고.건수, 주류.인허가.음식점, 교통안전지수, 도로환경) #사고 건수와 다른 독립변수 추출

library(GGally)
df_selected %>% 
  ggpairs(showStrips=T) #상관관계 시각화

df<-read.csv(("df_f8.csv"))
str(df)
df$중분류<-as.factor(df$중분류)
x11()
df %>% 
  ggplot(aes(x = 중분류))+
  geom_bar(col="black",fill="skyblue",alpha=0.5)+
  labs(x="구", y = "사고수", title = "구별 음주운전 사고수")+
  coord_flip() #구별 음주운전 사고수 시각화
