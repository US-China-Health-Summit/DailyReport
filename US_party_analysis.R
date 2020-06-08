##### Filter countries #####
## Set template_input to TRUE if filter country using template file provided; 
## Otherwise, auto select countries/State by Top N
template_input = FALSE
top_n = 5

##### FOR WEEKLY REPORT #####
weekly_summary = FALSE
start_date_wr = NULL
end_date_wr = NULL

# weekly_summary = TRUE
# start_date_wr = "2020-04-25"
# end_date_wr = "2020-05-01"
# The thresholds are used for weekly report to filter countries based on confirmed or deaths numbers of the most recent day
## Values can be changed as needed.
end_date_confirmed_threshold = 10000
end_date_deaths_threshold = 1000
end_date_us_confirmed_threshold = 1000
end_date_us_deaths_threshold = 100


##### filter date #####
# date filter input format: "yyyy-mm-dd" ; set as NULL if not used
# start_date/end_date control the start and end date for global plots 
# start_date_US/end_date_US control the start and end date for US plots 

start_date = NULL
end_date = NULL
start_date_US = "2020-03-01" 
end_date_US = NULL


time_series_url <<- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
web_data_url <<- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/data/"

web_data = TRUE
############################################################

list.of.packages <- c("ggplot2", "RCurl", "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) {install.packages(new.packages)}
if (packageVersion("tidyverse") != "1.3.0") {install.packages("tidyverse")}


library(ggplot2)
library(tidyverse)
library(scales)
library(RCurl)

##################
#### function ####
##################
convert_date = function(date_label){
  ##C by HS: get the date label-> turn into character-> turn into date-> format the date
  date_label %>% as.character() %>% as.Date("%m/%d/%y") %>% format("%Y-%m-%d")
}

read_data_us = function(label){
  # read us time series data
  filename = paste("time_series_covid19_", tolower(label), "_US.csv", sep = "")
  fileurl = paste(time_series_url, filename, sep = "")
  data_wide = getURL(fileurl) %>% read_csv() %>% 
    dplyr::select(-c("UID", "iso2",  "iso3",  "code3",  "FIPS", "Admin2" ,"Lat","Long_","Combined_Key")) %>% 
    filter(`Country_Region` == "US")
  if ("Population" %in% colnames(data_wide)) data_wide = data_wide %>% select(-"Population")
  data_wide = data_wide %>% select(-"Country_Region") %>% group_by(`Province_State`) %>% summarise_all(sum)
  
  date_label = data_wide %>% dplyr::select(-c("Province_State")) %>% colnames()
  date_label_fixed = convert_date(date_label) %>% as.character()
  #  make date into desirable form and rename the variable names
  data_wide = data_wide %>% rename_at(vars(date_label), ~ date_label_fixed)
  # if web data used, and if last column is today's date, remove last column and use web data for latest day
  if (web_data & (date_today %>% as.character() == max(date_label_fixed))) {
    data_wide = data_wide %>% select(-last_col())
  }
  
  # get web data and append latest cases
  if (web_data) {
    fileurl = paste(web_data_url, "cases_state.csv", sep = "")
    wdata = getURL(fileurl) %>% read_csv() %>% filter(`Country_Region` == "US") %>% 
      select("Province/State" = Province_State, label) %>% 
      rename(`Province_State` = `Province/State`)
    data_wide = left_join(x = data_wide,y = wdata, by = "Province_State") %>% rename_at(vars(label),~date_today %>% as.character() )
    data_wide[is.na(data_wide)] = 0
  }
  
  # build incremental data
  temp = data_wide[,-1]
  # Change of each day
  data_incremental=(temp[,-1, drop = F]-temp[,-ncol(temp)])%>%cbind(data_wide[,1:2],.)
  # First day has no change
  data_incremental[,2] =0
  #Reformat the data to produce a summary
  a = data_wide%>%pivot_longer(names_to = "Date", values_to = "Counts", cols = contains("-"))
  b = data_incremental%>%pivot_longer(names_to = "Date", values_to = "Counts_incremental", cols = contains("-"))
  data = left_join(x =a, y = b)%>%mutate(Date = as.Date(Date))%>%arrange(Date)
  colnames(data)[3:4]=c(label, paste(label,"_incremental", sep=""))
  
  # reverse column order of everything but the first
  data_wide = data_wide[, ncol(data_wide):1]%>%select(last_col(),everything())
  data_incremental = data_incremental[, ncol(data_incremental):1]%>%select(last_col(),everything())
  
  return(list(data=data, 
              data_wide=data_wide %>% as.data.frame(), 
              data_incremental_wide=data_incremental %>% as.data.frame()))
  
}

adjust_y_interval = function(y_max){
  temp_interval = y_max / 6
  if (temp_interval < 15) {
    y_interval = ceiling((temp_interval/20))*20
  } else if (temp_interval < 30) {
    y_interval = ceiling((temp_interval/25))*25
  } else if (temp_interval < 50) {
    y_interval = ceiling((temp_interval/50))*50
  } else if (temp_interval < 500) {
    y_interval = ceiling((temp_interval/100))*100
  } else {
    y_interval = ceiling((temp_interval/1000))*1000
  }
  return(y_interval)
}


######################
#### US by states ####
######################

state_population = read.csv("input_state_population.csv", stringsAsFactors = F)

date_today = Sys.Date()

data_confirmed = read_data_us('Confirmed')
data_deaths = read_data_us('Deaths')

case_confirmed = data_confirmed$data
case_deaths = data_deaths$data

case_confirmed_wide = data_confirmed$data_wide
case_confirmed_incremental_wide = data_confirmed$data_incremental_wide
case_deaths_wide = data_deaths$data_wide
case_deaths_incremental_wide = data_deaths$data_incremental_wide

data_us_states = Reduce(function(x, y) merge(x, y, all = TRUE), list(case_confirmed, case_deaths))
colnames(data_us_states)[grep("Province_State", colnames(data_us_states))] = "state"


##################
#### US Hospitalize and Test Results Detail View ####
##################

library(httr)
library(jsonlite)

state_detail_url = 'https://covidtracking.com/api/states/daily'
r = GET(state_detail_url)
if (r$status != 200) stop(paste("BAD API RETURN!"))
data_us_states_detailed = fromJSON(paste(rawToChar(r$content), collapse = ""))
data_us_states_detailed$date = as.Date(as.character(data_us_states_detailed$date), format = "%Y%m%d")
data_us_states_detailed$state = state_population$State_Full[match(data_us_states_detailed$state, state_population$State)]
data_us_states_detailed$hospitalized_pct = data_us_states_detailed$hospitalized/data_us_states_detailed$positive

# write to table
# data_us_states_detailed
report_date_us = as.character(max(data_us_states_detailed$date))
write_excel_csv(data_us_states_detailed, paste(report_date_us,"table_US_details_data_all.csv"))

# sort by latest day
Confirmed = case_confirmed_wide[order(case_confirmed_wide[,report_date], decreasing = T),]
Confirmed_incremental = case_confirmed_incremental_wide[order(case_confirmed_incremental_wide[,report_date], decreasing = T),]
Deaths = case_deaths_wide[order(case_deaths_wide[,report_date], decreasing = T),]
Deaths_Incremental = case_deaths_incremental_wide[order(case_deaths_incremental_wide[,report_date], decreasing = T),]
write_excel_csv(Confirmed, paste(report_date,"table_case_confirmed_US.csv" ))
write_excel_csv(Confirmed_incremental, paste(report_date,"table_case_confirmed_incremental_US.csv" ))
write_excel_csv(Deaths, paste(report_date,"table_case_deaths_US.csv" ))
write_excel_csv(Deaths_Incremental, paste(report_date,"table_case_deaths_incremental_US.csv" ))

# add crude_incidence_rate
data_us_states$Population = state_population$Population[match(data_us_states$state, state_population$State_Full)]
data_us_states$Crude_Incidence_Rate = round(as.numeric(data_us_states$Confirmed)/as.numeric(data_us_states$Population) * 100000, 0)
# Fatality_rate
data_us_states$Fatality_rate = round(data_us_states$Deaths/data_us_states$Confirmed*100, 1)
data_us_states[is.na(data_us_states)] = 0

# merge data_us_states_detailed into data_us_states																									 
data_us_states_detailed_keep = data_us_states_detailed[, c("state", "date","totalTestResults", "totalTestResultsIncrease", "deathIncrease")]
colnames(data_us_states_detailed_keep)[2]="Date"
data_us_states = merge(data_us_states, data_us_states_detailed_keep, by = c("state","Date"), all.x=TRUE)
data_us_states$positive_rate = round(data_us_states$Confirmed/data_us_states$totalTestResults, 2) * 100
data_us_states$pct_test = round(data_us_states$totalTestResults/data_us_states$Population * 100000, 0)

## correct negative death increase
data_us_states$Deaths_incremental[data_us_states$Deaths_incremental<0] = data_us_states$deathIncrease[data_us_states$Deaths_incremental<0]
data_us_states$Deaths_incremental[data_us_states$Deaths_incremental<0] = 0



##############
#### Plot ####
##############

library(readxl)
color_list = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628", "#F781BF","#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")


raw_governors <- read_excel("governors.xlsx")
governors <- select(raw_governors, State,Party)

us_party = merge(data_us_states, governors, by.x = "state", by.y = "State")
us_party$test_rate = us_party$totalTestResults/us_party$Population

#plot 1
temp = us_party %>% 
  filter(Date >="2020-03-15") %>% 
  select(state,Date,Party,test_rate) %>% 
  group_by(Date,Party) %>% 
  summarise(test_rate = mean(test_rate))

temp_sum = temp %>% 
  group_by(Date) %>% 
  summarise(Party = 'Both',test_rate =  mean(test_rate)) 

temp_final <- bind_rows(temp,temp_sum)


plot1 = ggplot(temp_final, aes(x=Date, y=test_rate, group=Party, colour = Party,  shape = Party)) + 
  geom_point(size=2) + 
  geom_line(size=1) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
  theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
  theme(legend.position = c(0.2, 0.8)) + 
  theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 24,face="italic")) +
  scale_x_date(breaks = '5 day',date_labels = "%m-%d") +
  scale_color_manual(values=color_list[1:3]) +
  xlab("") +
  ylab("test rate")

ggsave(filename=paste("plot1", ".png"), plot = plot1, width = 10, height = 8 )



#plot 2
temp = us_party %>% 
  filter(Date >="2020-03-01") %>% 
  select(state,Date,Party,Confirmed) %>% 
  group_by(Date,Party) %>% 
  summarise(Confirmed = sum(Confirmed))

temp_sum = temp %>% 
  group_by(Date) %>% 
  summarise(Party = 'Both',Confirmed =  sum(Confirmed)) 

temp_final <- bind_rows(temp,temp_sum)

y_max=(round(max(temp_final$Confirmed)/500)+1)*500
y_interval = adjust_y_interval(y_max)
plot2 = ggplot(temp_final, aes(x=Date, y=Confirmed, group=Party, colour = Party,  shape = Party)) + 
  geom_point(size=2) + 
  geom_line(size=1) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
  theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
  theme(legend.position = c(0.2, 0.8)) + 
  theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 24,face="italic")) +
  scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
  scale_x_date(breaks = '5 day',date_labels = "%m-%d") +
  scale_color_manual(values=color_list[1:3]) +
  xlab("") +
  ylab("Confirmed Cases")

ggsave(filename=paste("plot2", ".png"), plot = plot2, width = 10, height = 8 )

# plot 3
temp = us_party %>% 
  filter(Date >="2020-03-01") %>% 
  select(state,Date,Party,Confirmed_incremental) %>% 
  group_by(Date,Party) %>% 
  summarise(Confirmed_incremental = sum(Confirmed_incremental))

temp_sum = temp %>% 
  group_by(Date) %>% 
  summarise(Party = 'Both',Confirmed_incremental =  sum(Confirmed_incremental)) 

temp_final <- bind_rows(temp,temp_sum)

y_max=(round(max(temp_final$Confirmed_incremental)/500)+1)*500
y_interval = adjust_y_interval(y_max)
plot3 = ggplot(temp_final, aes(x=Date, y=Confirmed_incremental, group=Party, colour = Party,  shape = Party)) + 
  geom_point(size=2) + 
  geom_line(size=1) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
  theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
  theme(legend.position = c(0.2, 0.8)) + 
  theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 24,face="italic")) +
  scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
  scale_x_date(breaks = '5 day',date_labels = "%m-%d") +
  scale_color_manual(values=color_list[1:3]) +
  xlab("") +
  ylab("Incremental Cases")

ggsave(filename=paste("plot3", ".png"), plot = plot3, width = 10, height = 8 )

# plot 4
temp = us_party %>% 
  filter(Date >="2020-03-01") %>% 
  select(state,Date,Party,Deaths) %>% 
  group_by(Date,Party) %>% 
  summarise(Deaths = sum(Deaths))

temp_sum = temp %>% 
  group_by(Date) %>% 
  summarise(Party = 'Both',Deaths =  sum(Deaths)) 

temp_final <- bind_rows(temp,temp_sum)

y_max=(round(max(temp_final$Deaths)/500)+1)*500
y_interval = adjust_y_interval(y_max)
plot4 = ggplot(temp_final, aes(x=Date, y=Deaths, group=Party, colour = Party,  shape = Party)) + 
  geom_point(size=2) + 
  geom_line(size=1) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
  theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
  theme(legend.position = c(0.2, 0.8)) + 
  theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 24,face="italic")) +
  scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
  scale_x_date(breaks = '5 day',date_labels = "%m-%d") +
  scale_color_manual(values=color_list[1:3]) +
  xlab("") +
  ylab("Deaths Cases")

ggsave(filename=paste("plot4", ".png"), plot = plot4, width = 10, height = 8 )


# plot5

temp = us_party %>% 
  filter(Date >="2020-03-01") %>% 
  select(state,Date,Party,Deaths_incremental) %>% 
  group_by(Date,Party) %>% 
  summarise(Deaths_incremental = sum(Deaths_incremental))

temp_sum = temp %>% 
  group_by(Date) %>% 
  summarise(Party = 'Both',Deaths_incremental =  sum(Deaths_incremental)) 

temp_final <- bind_rows(temp,temp_sum)

y_max=(round(max(temp_final$Deaths_incremental)/500)+1)*500
y_interval = adjust_y_interval(y_max)
plot5 = ggplot(temp_final, aes(x=Date, y=Deaths_incremental, group=Party, colour = Party,  shape = Party)) + 
  geom_point(size=2) + 
  geom_line(size=1) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
  theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
  theme(legend.position = c(0.2, 0.8)) + 
  theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 24,face="italic")) +
  scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
  scale_x_date(breaks = '5 day',date_labels = "%m-%d") +
  scale_color_manual(values=color_list[1:3]) +
  xlab("") +
  ylab("Incremental Cases")

ggsave(filename=paste("plot5", ".png"), plot = plot5, width = 10, height = 8 )

#plot 6
temp = us_party %>% 
  filter(Date >="2020-03-18") %>% 
  mutate(fatality_rate = Deaths/Confirmed) %>% 
  select(state,Date,Party,fatality_rate) %>% 
  group_by(Date,Party) %>% 
  summarise(fatality_rate = mean(fatality_rate))

temp_sum = temp %>% 
  group_by(Date) %>% 
  summarise(Party = 'Both',fatality_rate =  mean(fatality_rate)) 

temp_final <- bind_rows(temp,temp_sum)

plot6 = ggplot(temp_final, aes(x=Date, y=fatality_rate, group=Party, colour = Party,  shape = Party)) + 
  geom_point(size=2) + 
  geom_line(size=1) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
  theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
  theme(legend.position = c(0.2, 0.8)) + 
  theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 24,face="italic")) +
  scale_x_date(breaks = '5 day',date_labels = "%m-%d") +
  scale_color_manual(values=color_list[1:3]) +
  xlab("") +
  ylab("Fatality Rate")

ggsave(filename=paste("plot6", ".png"), plot = plot6, width = 10, height = 8 )


#############################################
#############TOP N state plots ##############
#############################################

# or "Republican" or "both"
p = "both"
# later change p_date to 2020-05-22
p_date = "2020-05-20"

temp = us_party
if (p == "Democratic" | p == "Republican"){
  top_n = 5
  temp = temp[temp$Date == p_date & temp$Party == p ,] 

  temp_test_rate = temp[order(temp$test_rate, decreasing = T),]
  filter_test_rate = temp_test_rate$state[1:top_n]

  temp_confirmed = temp[order(temp$Confirmed, decreasing = T),]
  filter_confirmed = temp_confirmed$state[1:top_n]
  
  
  temp_confirmed_incre = temp[order(temp$Confirmed_incremental, decreasing = T),]
  filter_confirmed_incre = temp_confirmed_incre$state[1:top_n]
  
  
  temp_Death = temp[order(temp$Deaths, decreasing = T),]
  filter_Death = temp_Death$state[1:top_n]
  
  
  
  temp_Death_incre = temp[order(temp$Deaths_incremental, decreasing = T),]
  filter_Death_incre =temp_Death_incre$state[1:top_n]
  
  
  temp$mortality_rate = temp$Deaths/temp$Confirmed
  temp_mortality = temp[order(temp$mortality_rate, decreasing = T),]
  filter_mortality = temp_mortality$state[1:top_n]
  
}else {
  # to find both party
  top_n = 10
  temp = temp[temp$Date == p_date ,] 
  
  temp_test_rate = temp[order(temp$test_rate, decreasing = T),]
  filter_test_rate = temp_test_rate$state[1:top_n]
  
  temp_confirmed = temp[order(temp$Confirmed, decreasing = T),]
  filter_confirmed = temp_confirmed$state[1:top_n]
  
  
  temp_confirmed_incre = temp[order(temp$Confirmed_incremental, decreasing = T),]
  filter_confirmed_incre = temp_confirmed_incre$state[1:top_n]
  
  
  temp_Death = temp[order(temp$Deaths, decreasing = T),]
  filter_Death = temp_Death$state[1:top_n]
  
  
  
  temp_Death_incre = temp[order(temp$Deaths_incremental, decreasing = T),]
  filter_Death_incre =temp_Death_incre$state[1:top_n]
  
  
  temp$mortality_rate = temp$Deaths/temp$Confirmed
  temp_mortality = temp[order(temp$mortality_rate, decreasing = T),]
  filter_mortality = temp_mortality$state[1:top_n]
}


 
####### plots  #########

# for seperate party
if (p == "Democratic" | p == "Republican"){
  data_to_plot = us_party[us_party$state %in% filter_test_rate, ]
  data_to_plot = data_to_plot[data_to_plot$Date >= "2020-03-15" & data_to_plot$Date <=p_date,]
  
  # reorder factor levels by country filter order
  temp = data_to_plot[data_to_plot$Date == max(data_to_plot$Date),]
  temp = temp[order(temp$test_rate,decreasing = T),]
  state_order = temp$state
  data_to_plot$state <- factor(data_to_plot$state, levels = state_order)
  
  # y_max=(round(max(data_to_plot$Confirmed)/500)+1)*500
  # y_interval = adjust_y_interval(y_max)
  p1 = ggplot(data_to_plot , aes(x=Date, y=test_rate, group=state, colour = state)) + 
    # geom_point(size=2) + 
    geom_line(size=1) +
    theme_bw() + 
    theme(panel.border = element_blank()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.line = element_line(colour = "black")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
    theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
    theme(legend.position ="right") + 
    theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 12,face="italic")) +
    scale_x_date(breaks = '3 day',date_labels = "%m-%d") +
    scale_color_manual(values=color_list[1:10]) +
    xlab("") +
    ylab("test rate")
  
  ggsave(filename=paste("p1",p, ".png"), plot = p1, width = 10, height = 8 )
  
  
  # p2
  data_to_plot = us_party[us_party$state %in% filter_confirmed, ]
  data_to_plot = data_to_plot[data_to_plot$Date >= "2020-03-15" & data_to_plot$Date <=p_date,]
  
  # reorder factor levels by country filter order
  temp = data_to_plot[data_to_plot$Date == max(data_to_plot$Date),]
  temp = temp[order(temp$Confirmed,decreasing = T),]
  state_order = temp$state
  data_to_plot$state <- factor(data_to_plot$state, levels = state_order)
  
  y_max=(round(max(data_to_plot$Confirmed)/500)+1)*500
  y_interval = adjust_y_interval(y_max)
  p2 = ggplot(data_to_plot , aes(x=Date, y=Confirmed, group=state, colour = state)) + 
    # geom_point(size=2) + 
    geom_line(size=1) +
    theme_bw() + 
    theme(panel.border = element_blank()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.line = element_line(colour = "black")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
    theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
    theme(legend.position ="right") + 
    theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 12,face="italic")) +
    scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
    scale_x_date(breaks = '3 day',date_labels = "%m-%d") +
    scale_color_manual(values=color_list[1:10]) +
    xlab("") +
    ylab("Confirmed Cases")
  
  ggsave(filename=paste("p2",p, ".png"), plot = p2, width = 10, height = 8 )
  
  
  # p3
  data_to_plot = us_party[us_party$state %in% filter_confirmed_incre, ]
  data_to_plot = data_to_plot[data_to_plot$Date >= "2020-03-15" & data_to_plot$Date <=p_date,]
  
  # reorder factor levels by country filter order
  temp = data_to_plot[data_to_plot$Date == max(data_to_plot$Date),]
  temp = temp[order(temp$Confirmed_incremental,decreasing = T),]
  state_order = temp$state
  data_to_plot$state <- factor(data_to_plot$state, levels = state_order)
  
  y_max=(round(max(data_to_plot$Confirmed_incremental)/500)+1)*500
  y_interval = adjust_y_interval(y_max)
  p3 = ggplot(data_to_plot , aes(x=Date, y=Confirmed_incremental, group=state, colour = state)) + 
    # geom_point(size=2) + 
    geom_line(size=1) +
    theme_bw() + 
    theme(panel.border = element_blank()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.line = element_line(colour = "black")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
    theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
    theme(legend.position ="right") + 
    theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 12,face="italic")) +
    scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
    scale_x_date(breaks = '3 day',date_labels = "%m-%d") +
    scale_color_manual(values=color_list[1:10]) +
    xlab("") +
    ylab("Confirmed Cases")
  
  ggsave(filename=paste("p3",p, ".png"), plot = p3, width = 10, height = 8 )
  
  # p4
  data_to_plot = us_party[us_party$state %in% filter_Death, ]
  data_to_plot = data_to_plot[data_to_plot$Date >= "2020-03-15" & data_to_plot$Date <=p_date,]
  
  # reorder factor levels by country filter order
  temp = data_to_plot[data_to_plot$Date == max(data_to_plot$Date),]
  temp = temp[order(temp$Deaths,decreasing = T),]
  state_order = temp$state
  data_to_plot$state <- factor(data_to_plot$state, levels = state_order)
  
  y_max=(round(max(data_to_plot$Deaths)/500)+1)*500
  y_interval = adjust_y_interval(y_max)
  p4 = ggplot(data_to_plot , aes(x=Date, y=Deaths, group=state, colour = state)) + 
    # geom_point(size=2) + 
    geom_line(size=1) +
    theme_bw() + 
    theme(panel.border = element_blank()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.line = element_line(colour = "black")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
    theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
    theme(legend.position ="right") + 
    theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 12,face="italic")) +
    scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
    scale_x_date(breaks = '3 day',date_labels = "%m-%d") +
    scale_color_manual(values=color_list[1:10]) +
    xlab("") +
    ylab("Deaths Cases")
  
  ggsave(filename=paste("p4",p, ".png"), plot = p4, width = 10, height = 8 )
  
  
  # p5
  data_to_plot = us_party[us_party$state %in% filter_Death_incre, ]
  data_to_plot = data_to_plot[data_to_plot$Date >= "2020-03-15" & data_to_plot$Date <=p_date,]
  
  # reorder factor levels by country filter order
  temp = data_to_plot[data_to_plot$Date == max(data_to_plot$Date),]
  temp = temp[order(temp$Deaths_incremental,decreasing = T),]
  state_order = temp$state
  data_to_plot$state <- factor(data_to_plot$state, levels = state_order)
  
  y_max=(round(max(data_to_plot$Deaths_incremental)/500)+1)*500
  y_interval = adjust_y_interval(y_max)
  p5 = ggplot(data_to_plot , aes(x=Date, y=Deaths_incremental, group=state, colour = state)) + 
    # geom_point(size=2) + 
    geom_line(size=1) +
    theme_bw() + 
    theme(panel.border = element_blank()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.line = element_line(colour = "black")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
    theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
    theme(legend.position ="right") + 
    theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 12,face="italic")) +
    scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
    scale_x_date(breaks = '3 day',date_labels = "%m-%d") +
    scale_color_manual(values=color_list[1:10]) +
    xlab("") +
    ylab("Deaths Cases")
  
  ggsave(filename=paste("p5",p, ".png"), plot = p5, width = 10, height = 8 )
  
  # p6
  data_to_plot = us_party[us_party$state %in% filter_mortality, ]
  data_to_plot = data_to_plot[data_to_plot$Date >= "2020-03-15" & data_to_plot$Date <=p_date,]
  data_to_plot$mortality = data_to_plot$Deaths/data_to_plot$Confirmed
  
  # reorder factor levels by country filter order
  temp = data_to_plot[data_to_plot$Date == max(data_to_plot$Date),]
  temp = temp[order(temp$mortality,decreasing = T),]
  state_order = temp$state
  data_to_plot$state <- factor(data_to_plot$state, levels = state_order)
  
  p6 = ggplot(data_to_plot , aes(x=Date, y=mortality, group=state, colour = state)) + 
    # geom_point(size=2) + 
    geom_line(size=1) +
    theme_bw() + 
    theme(panel.border = element_blank()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.line = element_line(colour = "black")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
    theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
    theme(legend.position ="right") + 
    theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 12,face="italic")) +
    scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
    scale_x_date(breaks = '3 day',date_labels = "%m-%d") +
    scale_color_manual(values=color_list[1:10]) +
    xlab("") +
    ylab("mortality rate")
  
  ggsave(filename=paste("p6",p, ".png"), plot = p6, width = 10, height = 8 )
  
}else{
  # for total top 10 states
  data_to_plot = us_party[us_party$state %in% filter_test_rate, ]
  data_to_plot = data_to_plot[data_to_plot$Date >= "2020-03-15" & data_to_plot$Date <=p_date,]
  
  # reorder factor levels by country filter order
  temp = data_to_plot[data_to_plot$Date == max(data_to_plot$Date),]
  temp = temp[order(temp$test_rate,decreasing = T),]
  state_order = temp$state
  data_to_plot$state <- factor(data_to_plot$state, levels = state_order)
  
  # y_max=(round(max(data_to_plot$Confirmed)/500)+1)*500
  # y_interval = adjust_y_interval(y_max)
  p1 = ggplot(data_to_plot , aes(x=Date, y=test_rate, group=state, colour = state,linetype = Party)) + 
    # geom_point(size=2) + 
    geom_line(size=1) +
    theme_bw() + 
    theme(panel.border = element_blank()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.line = element_line(colour = "black")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
    theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
    theme(legend.position ="right") + 
    theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 12,face="italic")) +
    scale_x_date(breaks = '3 day',date_labels = "%m-%d") +
    scale_color_manual(values=color_list[1:10]) +
    xlab("") +
    ylab("test rate")
  
  ggsave(filename=paste("p1", ".png"), plot = p1, width = 10, height = 8 )
 
  
  # p2
  data_to_plot = us_party[us_party$state %in% filter_confirmed, ]
  data_to_plot = data_to_plot[data_to_plot$Date >= "2020-03-15" & data_to_plot$Date <=p_date,]
  
  # reorder factor levels by country filter order
  temp = data_to_plot[data_to_plot$Date == max(data_to_plot$Date),]
  temp = temp[order(temp$Confirmed,decreasing = T),]
  state_order = temp$state
  data_to_plot$state <- factor(data_to_plot$state, levels = state_order)
  
  y_max=(round(max(data_to_plot$Confirmed)/500)+1)*500
  y_interval = adjust_y_interval(y_max)
  p2 = ggplot(data_to_plot , aes(x=Date, y=Confirmed, group=state, colour = state,linetype = Party)) + 
    # geom_point(size=2) + 
    geom_line(size=1) +
    theme_bw() + 
    theme(panel.border = element_blank()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.line = element_line(colour = "black")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
    theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
    theme(legend.position ="right") + 
    theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 12,face="italic")) +
    scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
    scale_x_date(breaks = '3 day',date_labels = "%m-%d") +
    scale_color_manual(values=color_list[1:10]) +
    xlab("") +
    ylab("Confirmed Cases")
  
  ggsave(filename=paste("p2", ".png"), plot = p2, width = 10, height = 8 )
  
  
  # p3
  data_to_plot = us_party[us_party$state %in% filter_confirmed_incre, ]
  data_to_plot = data_to_plot[data_to_plot$Date >= "2020-03-15" & data_to_plot$Date <=p_date,]
  
  # reorder factor levels by country filter order
  temp = data_to_plot[data_to_plot$Date == max(data_to_plot$Date),]
  temp = temp[order(temp$Confirmed_incremental,decreasing = T),]
  state_order = temp$state
  data_to_plot$state <- factor(data_to_plot$state, levels = state_order)
  
  y_max=(round(max(data_to_plot$Confirmed_incremental)/500)+1)*500
  y_interval = adjust_y_interval(y_max)
  p3 = ggplot(data_to_plot , aes(x=Date, y=Confirmed_incremental, group=state, colour = state,linetype = Party)) + 
    # geom_point(size=2) + 
    geom_line(size=1) +
    theme_bw() + 
    theme(panel.border = element_blank()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.line = element_line(colour = "black")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
    theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
    theme(legend.position ="right") + 
    theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 12,face="italic")) +
    scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
    scale_x_date(breaks = '3 day',date_labels = "%m-%d") +
    scale_color_manual(values=color_list[1:10]) +
    xlab("") +
    ylab("Confirmed Cases")
  
  ggsave(filename=paste("p3",".png"), plot = p3, width = 10, height = 8 )
  
  # p4
  data_to_plot = us_party[us_party$state %in% filter_Death, ]
  data_to_plot = data_to_plot[data_to_plot$Date >= "2020-03-15" & data_to_plot$Date <=p_date,]
  
  # reorder factor levels by country filter order
  temp = data_to_plot[data_to_plot$Date == max(data_to_plot$Date),]
  temp = temp[order(temp$Deaths,decreasing = T),]
  state_order = temp$state
  data_to_plot$state <- factor(data_to_plot$state, levels = state_order)
  
  y_max=(round(max(data_to_plot$Deaths)/500)+1)*500
  y_interval = adjust_y_interval(y_max)
  p4 = ggplot(data_to_plot , aes(x=Date, y=Deaths, group=state, colour = state,linetype = Party)) + 
    # geom_point(size=2) + 
    geom_line(size=1) +
    theme_bw() + 
    theme(panel.border = element_blank()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.line = element_line(colour = "black")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
    theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
    theme(legend.position ="right") + 
    theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 12,face="italic")) +
    scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
    scale_x_date(breaks = '3 day',date_labels = "%m-%d") +
    scale_color_manual(values=color_list[1:10]) +
    xlab("") +
    ylab("Deaths Cases")
  
  ggsave(filename=paste("p4",".png"), plot = p4, width = 10, height = 8 )
  
  
  # p5
  data_to_plot = us_party[us_party$state %in% filter_Death_incre, ]
  data_to_plot = data_to_plot[data_to_plot$Date >= "2020-03-15" & data_to_plot$Date <=p_date,]
  
  # reorder factor levels by country filter order
  temp = data_to_plot[data_to_plot$Date == max(data_to_plot$Date),]
  temp = temp[order(temp$Deaths_incremental,decreasing = T),]
  state_order = temp$state
  data_to_plot$state <- factor(data_to_plot$state, levels = state_order)
  
  y_max=(round(max(data_to_plot$Deaths_incremental)/500)+1)*500
  y_interval = adjust_y_interval(y_max)
  p5 = ggplot(data_to_plot , aes(x=Date, y=Deaths_incremental, group=state, colour = state,linetype = Party)) + 
    # geom_point(size=2) + 
    geom_line(size=1) +
    theme_bw() + 
    theme(panel.border = element_blank()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.line = element_line(colour = "black")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
    theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
    theme(legend.position ="right") + 
    theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 12,face="italic")) +
    scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
    scale_x_date(breaks = '3 day',date_labels = "%m-%d") +
    scale_color_manual(values=color_list[1:10]) +
    xlab("") +
    ylab("Deaths Cases")
  
  ggsave(filename=paste("p5", ".png"), plot = p5, width = 10, height = 8 )
  
  # p6
  data_to_plot = us_party[us_party$state %in% filter_mortality, ]
  data_to_plot = data_to_plot[data_to_plot$Date >= "2020-03-15" & data_to_plot$Date <=p_date,]
  data_to_plot$mortality = data_to_plot$Deaths/data_to_plot$Confirmed
  
  # reorder factor levels by country filter order
  temp = data_to_plot[data_to_plot$Date == max(data_to_plot$Date),]
  temp = temp[order(temp$mortality,decreasing = T),]
  state_order = temp$state
  data_to_plot$state <- factor(data_to_plot$state, levels = state_order)
  
  p6 = ggplot(data_to_plot , aes(x=Date, y=mortality, group=state, colour = state,linetype = Party)) + 
    # geom_point(size=2) + 
    geom_line(size=1) +
    theme_bw() + 
    theme(panel.border = element_blank()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.line = element_line(colour = "black")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24)) + 
    theme(axis.text.y = element_text(size = 24), axis.title.y = element_text(size = 24)) + 
    theme(legend.position ="right") + 
    theme(legend.title = element_text(size=24,face="bold.italic"), legend.text = element_text(size = 12,face="italic")) +
    scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
    scale_x_date(breaks = '3 day',date_labels = "%m-%d") +
    scale_color_manual(values=color_list[1:10]) +
    xlab("") +
    ylab("mortality rate")
  
  ggsave(filename=paste("p6", ".png"), plot = p6, width = 10, height = 8 )
}
