if(!require(ggplot2)){install.packages("ggplot2"); require(ggplot2)}
if(!require(readxl)){install.packages("readxl"); require(readxl)}
if(!require(lubridate)){install.packages("lubridate"); require(lubridate)}
if(!require(data.table)){install.packages("data.table"); require(data.table)}
if(!require(stringr)){install.packages("stringr"); require(stringr)}

# from 과 to는 "20 05/1주" 같은 형태로 전달해야 함
# return 값은 트랜드를 정리한 리스트..
plot_all_issues_weekly_trend <- function(data, from, to){
  
  #주별로 전체 이슈 개수 구해보고..
  issue_cnt_week <- data %>% group_by(Week_WED) %>% summarise(count = n())
  #평균 이슈 개수 구하기.. 
  total_issue_avg = mean(issue_cnt_week$count)
  total_issue_sd = sd(issue_cnt_week$count)
  
  #2020년 5월 1주차 ~ 2021년 3월 1주차 까지만 필터링해서 그래프를 그려보자
  g_issue_cnt_week <- issue_cnt_week %>% filter(Week_WED > from & Week_WED < to)
  
  g <- ggplot(g_issue_cnt_week, aes(x = Week_WED, y = count, group = 1)) + geom_line() + geom_point(size = 1, alpha = 0.5)+geom_smooth()+
    #평균선 그리기...
    geom_hline(yintercept = total_issue_avg, linetype='solid', color='darkorange1', size= .5, alpha = .5) +
    #annotate("text", x = Inf, y=total_issue_avg, label="   Avg.", fontface="bold", color="darkorange1", size=5, alpha = .8) +
    # +1 sigma 선 그릭... 
    geom_hline(yintercept = total_issue_avg + total_issue_sd, linetype='solid', color='red2', size = .5) +
    #annotate("text", x = Inf, y=total_issue_avg + total_issue_sd, label="       +1 sigma", fontface="bold", color="red2", size=5, alpha = .8) +
    # -1 sigma 선 그릭... 
    geom_hline(yintercept = total_issue_avg - total_issue_sd, linetype='solid', color='darkgreen', size = .5) +
    #annotate("text", x = Inf, y=total_issue_avg - total_issue_sd, label="       -1 sigma", fontface="bold", color="darkgreen", size=5, alpha = .8) +
    
    theme_bw() + #그래프 배경을 흰색으로 변경 
    theme(axis.text.x=element_text(angle=90, hjust=1))#x축이 잘 보이도록 90도로 텍스트를 돌린 것..
  
  return (list(graph = g, issue_cnt_week = issue_cnt_week, total_issue_avg = total_issue_avg, total_issue_sd = total_issue_sd))
  
}


# from 과 to는 "20 05/1주" 같은 형태로 전달해야 함
# return 값은 트랜드를 정리한 리스트..
plot_all_issues_daily_trend <- function(data, from, to){
  
  #주별로 전체 이슈 개수 구해보고..
  issue_cnt_days <- data %>% group_by(month) %>% summarise(count = n())
  #평균 이슈 개수 구하기.. 
  total_issue_avg = mean(issue_cnt_days$count)
  total_issue_sd = sd(issue_cnt_days$count)
  
  #2020년 5월 1주차 ~ 2021년 3월 1주차 까지만 필터링해서 그래프를 그려보자
  g_issue_cnt_days <- issue_cnt_days %>% filter(month > from & month < to)
  
  g <- ggplot(g_issue_cnt_days, aes(x = month, y = count, group = 1)) + geom_line() + geom_point(size = 1, alpha = 0.5)+geom_smooth()+
    #평균선 그리기...
    geom_hline(yintercept = total_issue_avg, linetype='solid', color='darkorange1', size= .5, alpha = .5) +
    #annotate("text", x = as.POSIXct(from), y=total_issue_avg, label="   Avg.", fontface="bold", color="darkorange1", size=5, alpha = .8) +
    # +1 sigma 선 그릭... 
    geom_hline(yintercept = total_issue_avg + total_issue_sd, linetype='solid', color='red2', size = .5) +
    #annotate("text", x = as.POSIXct(from), y=total_issue_avg + total_issue_sd, label="       +1 sigma", fontface="bold", color="red2", size=5, alpha = .8) +
    # -1 sigma 선 그릭... 
    geom_hline(yintercept = total_issue_avg - total_issue_sd, linetype='solid', color='darkgreen', size = .5) +
    #annotate("text", x = as.POSIXct(from), y=total_issue_avg - total_issue_sd, label="       -1 sigma", fontface="bold", color="darkgreen", size=5, alpha = .8) +
    theme_bw() + #그래프 배경을 흰색으로 변경 
    theme(axis.text.x=element_text(angle=90, hjust=1)) #x축이 잘 보이도록 90도로 텍스트를 돌린 것..
  
  return (list(graph = g, issue_cnt_days = issue_cnt_days, total_issue_avg = total_issue_avg, total_issue_sd = total_issue_sd))
}

plot_all_issues_daily_trend_by_lcls <- function(data, lcls, from, to){
  
  #주별로 전체 이슈 개수 구해보고..
  issue_cnt_days_by_lcls <- data %>% filter(str_detect(rspo_lcls_nm, lcls))%>% group_by(month) %>% summarise(count = n())
  #평균 이슈 개수 구하기.. 
  total_issue_avg = mean(issue_cnt_days_by_lcls$count)
  total_issue_sd = sd(issue_cnt_days_by_lcls$count)
  
  #2020년 5월 1주차 ~ 2021년 3월 1주차 까지만 필터링해서 그래프를 그려보자
  g_issue_cnt_days <- issue_cnt_days_by_lcls %>% filter(month > from & month < to)
  
  g <- ggplot(g_issue_cnt_days, aes(x = month, y = count, group = 1)) + geom_line() + geom_point(size = 1, alpha = 0.5)+geom_smooth()+
    #평균선 그리기...
    geom_hline(yintercept = total_issue_avg, linetype='solid', color='darkorange1', size= .5, alpha = .5) +
    #annotate("text", x = as.POSIXct(from), y=total_issue_avg, label="   Avg.", fontface="bold", color="darkorange1", size=5, alpha = .8) +
    # +1 sigma 선 그릭... 
    geom_hline(yintercept = total_issue_avg + total_issue_sd, linetype='solid', color='red2', size = .5) +
    #annotate("text", x = as.POSIXct(from), y=total_issue_avg + total_issue_sd, label="       +1 sigma", fontface="bold", color="red2", size=5, alpha = .8) +
    # -1 sigma 선 그릭... 
    geom_hline(yintercept = total_issue_avg - total_issue_sd, linetype='solid', color='darkgreen', size = .5) +
    #annotate("text", x = as.POSIXct(from), y=total_issue_avg - total_issue_sd, label="       -1 sigma", fontface="bold", color="darkgreen", size=5, alpha = .8) +
    theme_bw() + #그래프 배경을 흰색으로 변경 
    theme(axis.text.x=element_text(angle=90, hjust=1)) #x축이 잘 보이도록 90도로 텍스트를 돌린 것..
  
  return (list(graph = g, issue_cnt_days_by_lcls = issue_cnt_days_by_lcls, total_issue_avg = total_issue_avg, total_issue_sd = total_issue_sd))
}

plot_all_issues_weekly_trend_by_lcls <- function(data, lcls, from, to){
  #주별로 전체 이슈 개수 구해보고..
  issue_cnt_week <- data %>% filter(str_detect(rspo_lcls_nm, lcls)) %>% group_by(Week_WED) %>% summarise(count = n())
  #평균 이슈 개수 구하기.. 
  total_issue_avg = mean(issue_cnt_week$count)
  total_issue_sd = sd(issue_cnt_week$count)
  
  #2020년 5월 1주차 ~ 2021년 3월 1주차 까지만 필터링해서 그래프를 그려보자
  g_issue_cnt_week <- issue_cnt_week %>% filter(Week_WED > from & Week_WED < to)
  
  g <- ggplot(g_issue_cnt_week, aes(x = Week_WED, y = count, group = 1)) + geom_line() + geom_point(size = 1, alpha = 0.5)+geom_smooth()+
    #평균선 그리기...
    geom_hline(yintercept = total_issue_avg, linetype='solid', color='darkorange1', size= .5, alpha = .5) +
    #annotate("text", x = Inf, y=total_issue_avg, label="   Avg.", fontface="bold", color="darkorange1", size=5, alpha = .8) +
    # +1 sigma 선 그릭... 
    geom_hline(yintercept = total_issue_avg + total_issue_sd, linetype='solid', color='red2', size = .5) +
    #annotate("text", x = Inf, y=total_issue_avg + total_issue_sd, label="       +1 sigma", fontface="bold", color="red2", size=5, alpha = .8) +
    # -1 sigma 선 그릭... 
    geom_hline(yintercept = total_issue_avg - total_issue_sd, linetype='solid', color='darkgreen', size = .5) +
    #annotate("text", x = Inf, y=total_issue_avg - total_issue_sd, label="       -1 sigma", fontface="bold", color="darkgreen", size=5, alpha = .8) +
    
    theme_bw() + #그래프 배경을 흰색으로 변경 
    theme(axis.text.x=element_text(angle=90, hjust=1))#x축이 잘 보이도록 90도로 텍스트를 돌린 것..
  
  return (list(graph = g, issue_cnt_week = issue_cnt_week, total_issue_avg = total_issue_avg, total_issue_sd = total_issue_sd))
}


plot_all_issues_daily_trend_by_mcls <- function(data, lcls, from, to){
  
  #주별로 전체 이슈 개수 구해보고..
  issue_cnt_days_by_mcls <- data %>% filter(str_detect(rspo_lcls_nm, lcls))%>% group_by(month, rspo_mcls_nm) %>% summarise(count = n())
  #평균 이슈 개수 구하기.. 
  issue_summary = issue_cnt_days_by_mcls %>% group_by(rspo_mcls_nm) %>% summarise(avg = mean(count), sd = sd(count))
  
  #2020년 5월 1주차 ~ 2021년 3월 1주차 까지만 필터링해서 그래프를 그려보자
  g_issue_cnt_days <- issue_cnt_days_by_mcls %>% filter(month > from & month < to)
  
  g <- ggplot(g_issue_cnt_days, aes(x = month, y = count, color = rspo_mcls_nm)) + 
    geom_line() + 
    geom_point(size = 1, alpha = 0.5)+
    theme_bw() + #그래프 배경을 흰색으로 변경 
    theme(axis.text.x=element_text(angle=90, hjust=1)) #x축이 잘 보이도록 90도로 텍스트를 돌린 것..
  
  return (list(graph = g, issue_cnt_days_by_mcls = issue_cnt_days_by_mcls, issue_summary = issue_summary))
}


plot_all_issues_weekly_trend_by_mcls <- function(data, lcls, from, to){
  
  #주별로 전체 이슈 개수 구해보고..
  issue_cnt_weekly_by_mcls <- data %>% filter(str_detect(rspo_lcls_nm, lcls))%>% group_by(Week_WED, rspo_mcls_nm) %>% summarise(count = n())
  #평균 이슈 개수 구하기.. 
  issue_summary = issue_cnt_weekly_by_mcls %>% group_by(rspo_mcls_nm) %>% summarise(avg = mean(count), sd = sd(count))
  
  #2020년 5월 1주차 ~ 2021년 3월 1주차 까지만 필터링해서 그래프를 그려보자
  g_issue_cnt_weekly <- issue_cnt_weekly_by_mcls %>% filter(Week_WED > from & Week_WED < to)
  
  g <- ggplot(g_issue_cnt_weekly, aes(x = Week_WED, y = count, group = rspo_mcls_nm, color = rspo_mcls_nm)) + 
    geom_line() + 
    geom_point(size = 1, alpha = 0.5)+
    theme_bw() + #그래프 배경을 흰색으로 변경 
    theme(axis.text.x=element_text(angle=90, hjust=1)) #x축이 잘 보이도록 90도로 텍스트를 돌린 것..
  
  return (list(graph = g, issue_cnt_weekly_by_mcls = issue_cnt_weekly_by_mcls, issue_summary = issue_summary))
}