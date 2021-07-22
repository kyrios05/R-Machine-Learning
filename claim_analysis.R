if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}
if(!require(ggplot2)){install.packages("ggplot2"); require(ggplot2)}
if(!require(readxl)){install.packages("readxl"); require(readxl)}
if(!require(lubridate)){install.packages("lubridate"); require(lubridate)}
if(!require(data.table)){install.packages("data.table"); require(data.table)}
if(!require(stringr)){install.packages("stringr"); require(stringr)}
if(!require(plotly)){install.packages("plotly"); require(plotly)}

source('claim_analysis_function.R')

#클레임 데이터를 불러오고..
claim_df = read_xlsx(file.choose()) 
str((claim_df))

#캘린더 데이터를 불러오자..
calendar_df = fread(file.choose())

#이중에서는 Key가 될 날짜와 요일, 휴무여부, 몇번째 주인지만 가져온다. 
calendar_df <- calendar_df %>% select(Date, weekday, Week_WED)

#두데이터를 합치기 전에 Key값의 데이터헝을 맞춰주자..
calendar_df$Date <- ymd(calendar_df$Date, tz='Asia/Seoul')
claim_df$month <- as.POSIXct(format(as.POSIXct(claim_df$month), tz = 'UTC'), tz = 'Asia/Seoul')

#두 데이터를 Join하지..
claim_df <- dplyr::left_join(claim_df, calendar_df, by = c('month' = 'Date'))


#============= 나중에 삭제 : 시작========================
#데이터 수가 너무 작아서 랜덤하게 데이터를 생성해서 장난쳐보자..
#나중에 실제 데이터로 할 때는 이거 지워야 함..
new_claim_df <- claim_df %>% select(-c(weekday, Week_WED, month, base_dt))
new_claim_df <- new_claim_df[sample(nrow(new_claim_df),100000,replace=T), ]

new_claim_df$month <- sample(seq(as.Date('2020/01/01'),as.Date('2021/08/01'), by="day"), 100000, replace = TRUE) %>% sort()
new_claim_df$base_dt <- new_claim_df$month %>% as.character() %>% str_replace_all('-', '')
new_claim_df <- dplyr::left_join(new_claim_df, calendar_df, by = c('month' = 'Date'))
rm(claim_df) 
claim_df <- new_claim_df
rm(new_claim_df)
#============= 나중에 삭제 : 끝========================

#base_dt와 Week_WED는 ordered factor로 전환해야할 듯.. 
claim_df$base_dt <- as.ordered(claim_df$base_dt)
claim_df$Week_WED <- as.ordered(claim_df$Week_WED)


#주단위로 전체 트랜드를 구해보자..
summary_weekly <- plot_all_issues_weekly_trend(data = claim_df, from = "20 05/1주", to = "21 03/1주")
ggplotly(summary_weekly$graph)
summary_weekly$total_issue_avg
summary_weekly$total_issue_sd

#일단위로 전체 ㅌ랜드를 구해보자...
summary_daily <- plot_all_issues_daily_trend(data = claim_df, from = "2021-01-01", to = "2021-03-01")
ggplotly(summary_daily$graph)
summary_daily$total_issue_avg
summary_daily$total_issue_sd


#대분류 기준으로 이슈 트랜드를 구해보자..
summary_daily_lcls <- plot_all_issues_daily_trend_by_lcls(data = claim_df, lcls = "TV_VOD" , from = "2021-01-01", to = "2021-03-01")
ggplotly(summary_daily_lcls$graph)
summary_daily_lcls$total_issue_avg
summary_daily_lcls$total_issue_sd

summary_weekly_lcls <- plot_all_issues_weekly_trend_by_lcls(data = claim_df, lcls = "TV_VOD" , from = "20 05/1주", to = "21 03/1주")
ggplotly(summary_weekly_lcls$graph)
summary_weekly_lcls$total_issue_avg
summary_weekly_lcls$total_issue_sd


#대분류를 하나 잡고 하위 중분류의 트랜드를 구해보자..
summary_weekly_mcls <- plot_all_issues_daily_trend_by_mcls(data = claim_df, lcls = "TV_VOD" , from = "2021-01-01", to = "2021-03-01")
ggplotly(summary_weekly_mcls$graph)
summary_weekly_mcls$issue_summary

summary_daily_mcls <- plot_all_issues_weekly_trend_by_mcls(data = claim_df, lcls = "TV_VOD" , from = "20 05/1주", to = "21 03/1주")
ggplotly(summary_daily_mcls$graph)
summary_daily_mcls$issue_summary
