##############################
####        README        ####
##############################

### Data source:  https://github.com/CSSEGISandData/COVID-19
### Data source (Not used):  https://covidtracking.com/api/states/daily

### Currently this program will give 6 plots and 4 tables as below:
# plot 1. total confirmed cases sort by countries total
# plot 2. new confirmed cases daily sort by countries total
# plot 3. new confirmed cases daily sort by countries incremental
# plot 4: US only Active / deaths / recovered
# plot 5: total confirmed cases by selected US States
# plot 6: new confirmed cases daily by selected US States
# Table 1: Total confirmed cases sort by the last day worldwide
# Table 2: Confirmed cases Incrementally daily sort by the last day worldwide
# Table 3: Total confirmed cases sort by the last day US
# Table 4: Confirmed cases Incrementally daily sort by the last day US

### Required input templates: (Please save these in the same folder with csse_covid_19_time_series input)
# 'input_country_list.csv' : 
#     Required if use customized country list for plots. (rather than TOP_N); Set "template_input" to TRUE below
# 'input_us_state_list.csv' :
#     Required if use customized US State list for plots. (rather than TOP_N); Set "template_input" to TRUE below
# 'input_plot_titles.csv' : 
#     Always keep this file in the folder. Modify if you need to update plots title / labels. 
# 'input_country_population.csv' : 
#     Always keep this file in the folder. No need to modify. 
# 'input_state_population.csv' : 
#     Always keep this file in the folder. No need to modify. 

############################################################

##############################
####        INPUTS        ####
##############################

##### folder path. In this folder, you should have all template input files mentioned above and these three time series files. 

# getwd()

folder = "YOUR_PATH/csse_covid_19_time_series"

setwd(folder)

##### Filter countries
## Set template_input to TRUE if filter country using template file provided; 
## Otherwise, auto select countries/State by Top N
template_input = FALSE
top_n = 5

##### filter data by start date and end date
# start_date = c(mm, dd, yyyy) ; set as NULL if not used
# end_date = c(mm, dd, yyyy) ; set as NULL if not used
# start/end_date_US control the start and end date of plot 5 and 6

start_date = NULL
end_date = NULL
start_date_US = c(03, 01, 2020)
end_date_US = NULL


#####  sort without China: TRUE, FALSE ; default is TRUE (sort without China)
remove_mainland_china = TRUE
china_label = "China"   
#### If want to review different province,  specify below
#####Currentlt can only calc Hubei Crude incident.
Province_name = "Hubei"

##### web data (TRUE if use additional data file from web data branch; otherwise set to FALSE)
web_data = TRUE

############################################################

list.of.packages <- c("ggplot2", "jsonlite", "httr","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) {install.packages(new.packages)}
if (packageVersion("tidyverse") != "1.3.0") {install.packages("tidyverse")}



library(ggplot2)
library(tidyverse)
library(scales)
library(RCurl)

###############################################################
## RUN THROUGH EVERYTHING BELOW TO GENERATE PLOTS AND TABLES ##
###############################################################

input_plot_titles = read.csv("input_plot_titles.csv", stringsAsFactors = F)
p1_title = input_plot_titles$Input[input_plot_titles$Item == "p1_title"]
p1_1_title = input_plot_titles$Input[input_plot_titles$Item == "p1_1_title"]
p1_2_title = input_plot_titles$Input[input_plot_titles$Item == "p1_2_title"]
p1_xlab = input_plot_titles$Input[input_plot_titles$Item == "p1_xlab"]
p1_ylab = input_plot_titles$Input[input_plot_titles$Item == "p1_ylab"]
p2_title = input_plot_titles$Input[input_plot_titles$Item == "p2_title"]
p2_xlab = input_plot_titles$Input[input_plot_titles$Item == "p2_xlab"]
p2_ylab = input_plot_titles$Input[input_plot_titles$Item == "p2_ylab"]
p3_title = input_plot_titles$Input[input_plot_titles$Item == "p3_title"]
p3_1_title = input_plot_titles$Input[input_plot_titles$Item == "p3_1_title"]
p3_2_title = input_plot_titles$Input[input_plot_titles$Item == "p3_2_title"]
p3_xlab = input_plot_titles$Input[input_plot_titles$Item == "p3_xlab"]
p3_ylab = input_plot_titles$Input[input_plot_titles$Item == "p3_ylab"]
p4_title = input_plot_titles$Input[input_plot_titles$Item == "p4_title"]
p4_xlab = input_plot_titles$Input[input_plot_titles$Item == "p4_xlab"]
p4_ylab = input_plot_titles$Input[input_plot_titles$Item == "p4_ylab"]
p5_title = input_plot_titles$Input[input_plot_titles$Item == "p5_title"]
p5_xlab = input_plot_titles$Input[input_plot_titles$Item == "p5_xlab"]
p5_ylab = input_plot_titles$Input[input_plot_titles$Item == "p5_ylab"]
p6_title = input_plot_titles$Input[input_plot_titles$Item == "p6_title"]
p6_xlab = input_plot_titles$Input[input_plot_titles$Item == "p6_xlab"]
p6_ylab = input_plot_titles$Input[input_plot_titles$Item == "p6_ylab"]
p7_1_title = input_plot_titles$Input[input_plot_titles$Item == "p7_1_title"]
p7_2_title = input_plot_titles$Input[input_plot_titles$Item == "p7_2_title"]
p7_xlab = input_plot_titles$Input[input_plot_titles$Item == "p7_xlab"]
p7_ylab = input_plot_titles$Input[input_plot_titles$Item == "p7_ylab"]
p8_1_title = input_plot_titles$Input[input_plot_titles$Item == "p8_1_title"]
p8_xlab = input_plot_titles$Input[input_plot_titles$Item == "p8_xlab"]
p8_ylab = input_plot_titles$Input[input_plot_titles$Item == "p8_ylab"]
p9_title = input_plot_titles$Input[input_plot_titles$Item == "p9_title"]
p9_xlab = input_plot_titles$Input[input_plot_titles$Item == "p9_xlab"]
p9_ylab = input_plot_titles$Input[input_plot_titles$Item == "p9_ylab"]
# p10_1_title = input_plot_titles$Input[input_plot_titles$Item=="p_1_title"]
# p10_xlab = input_plot_titles$Input[input_plot_titles$Item=="p10_xlab"]
# p10_ylab = input_plot_titles$Input[input_plot_titles$Item=="p10_ylab"]
# p11_title = input_plot_titles$Input[input_plot_titles$Item=="p11_title"]
# p11_xlab = input_plot_titles$Input[input_plot_titles$Item=="p11_xlab"]
# p11_ylab = input_plot_titles$Input[input_plot_titles$Item=="p11_ylab"]

time_series_url <<- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
web_data_url <<- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/data/"


color_list = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF")

input_population = read.csv("input_country_population.csv" , stringsAsFactors = F)

# four functions for translate plots and tables; pls run it

translate_country = function(ut_data) {
  
  # wwqi4realGitHub Apr 4
  # ut_data: untranslated data frame, which is used for plots/tables
  # for the table part, Hubei translation is now supported
  # csv file is modified to support different country names (mainly for table 1 input)
  
  # support data input of 
  # plot: 2,3 (tho ggplot doesnt support Chinses char well)
  # table: 1,2,3
  
  
  
  # read in the CN-EN data
  cn_name = read_csv("./translation/world_cn.csv")
  en_name = read_csv("./translation/world_en.csv")
  
  # combine the two name dataframes
  # trans_name = cbind(cn_name, en_name)
  
  trans_name = full_join(cn_name, en_name, by = "id") %>% 
    janitor::clean_names() %>% 
    select(name_x, name_y) %>% 
    rename(Country_cn = name_x, 
           Country = name_y) %>% 
    unite("Country_bi", Country_cn:Country, sep = " ", remove = F)
  
  t_data = left_join(ut_data, trans_name, by = "Country") %>% 
    # in this step coercing warning will show up but dont worry
    mutate(Country =  as.factor(as.character(Country)), 
           Country_cn = as.factor(as.character(Country_cn)), 
           Country_bi = as.factor(as.character(Country_bi)))
  
  print("[Just if] Ignore the [coercing] warning, already taken care of")
  return(t_data)
}

translate_state = function(ut_data) {
  
  # wwqi4realGitHub Apr 5
  # ut_data: untranslated data frame, which is used for plots/tables
  
  # support data input of 
  # plot: 
  # table: 4,5,7
  
  state_cn = read_csv("./translation/state_translate.txt") %>% 
    rename(state = State_Full) %>% 
    janitor::clean_names()
  
  t_data = left_join(ut_data, state_cn, by = "state") %>% 
    select(state_cn, state, everything()) %>% 
    unite("state_bi", state_cn, state_abb, sep = " ", remove = F)
  
  return(t_data)
  
}

translate_country_colname = function(ut_data, x) {
  
  # wwqi4realGutHub
  # translate column names in table 1, 2, 3
  # x = number of the table
  
  if (x == 1) {
    
    t_data = ut_data %>% 
      select(Country_bi, Confirmed_Cases, Population, Crude_Incidence_Rate) %>% 
      rename("国家" = Country_bi, 
             "累计确诊病例" = Confirmed_Cases, 
             "国家/省份人口" = Population, 
             "粗发病率" = Crude_Incidence_Rate)
    
    return(t_data)
    
  } else if (x == 2) {
    t_data = ut_data %>% 
      select(Country_bi, Confirmed) %>% 
      rename("国家" = Country_bi, 
             "当日新增病例" = Confirmed)
    
    return(t_data)
  } else if (x == 3) {
    
    t_data = ut_data %>% 
      select(Country_bi, Deaths, Deaths_incremental, Fatality_rate) %>% 
      rename("国家" = Country_bi, 
             "累计死亡病例" = Deaths, 
             "较昨日" = Deaths_incremental, 
             "病死率" = Fatality_rate)
    
    return(t_data)
  } else {
    warning("x can only be 1, 2, 3; column name is NOT translated")
    return(ut_data)
  }
}

translate_state_colname = function(ut_data, x) {
  
  # wwqi4realGutHub
  # translate column names in table 4, 5, 6
  # x = number of the table
  
  if (x == 4) {
    
    t_data = ut_data %>% 
      select(state_bi, Confirmed, crude_incidence_rate, positive_rate, totalTestResults, totalTestResultsIncrease, pct_test) %>% 
      rename("州名" = state_bi, 
             "累计确诊" = Confirmed, 
             "粗发病率" = crude_incidence_rate, 
             "阳性率%" = positive_rate, 
             "累计检测" = totalTestResults, 
             "较昨日" = totalTestResultsIncrease, 
             "检测率" = pct_test)
    
    return(t_data)
    
  } else if (x == 5) {
    t_data = ut_data %>% 
      select(state_bi, Confirmed_Incremental, Percentage) %>%
    rename("州名" = state_bi, 
           "当日新增" = Confirmed_Incremental, 
           "全美比率" = Percentage)
    
    return(t_data)
  } else if (x == 6) {
    
    t_data = ut_data %>% 
      select(state_bi, Deaths, Fatality_rate) %>%
    rename("州名" = state_bi, 
           "累计死亡人数" = Deaths, 
           "病死率" = Fatality_rate)
    
    return(t_data)
  } else {
    
    warning("x can only be 4, 5, 6; column name is NOT translated")
    return(ut_data)
  }
}



convert_date = function(date_label){
  ##C by HS: get the date label-> turn into character-> turn into date-> format the date
  date_label %>% as.character() %>% as.Date("%m/%d/%y") %>% format("%Y-%m-%d")
}

filter_by_date = function(ds, date_var, start_date, end_date){
  if (!is.null(start_date)) {
    start_date_converted = as.Date(ISOdate(year = start_date[3], month = start_date[1], day = start_date[2]))
    ds = ds[ds[,date_var] >= start_date_converted, ]
  }
  if (!is.null(end_date)) {
    end_date_converted = as.Date(ISOdate(year = end_date[3], month = end_date[1], day = end_date[2]))
    ds = ds[ds[,date_var]  <= end_date_converted, ]
  }
  ds
}

adjust_y_interval = function(y_max){
  temp_interval = y_max / 10
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
  y_interval
}

read_data = function(label, type, web_data, Province_name = NULL ){ 


  #HS: Rewrote function using tiddyverse to make more readable
  # read time series data
	filename = paste("time_series_covid19_", tolower(label), "_global.csv", sep = "")
	fileurl = paste(time_series_url, filename, sep = "")
	time_series = getURL(fileurl) %>% read_csv()
	
  # Remove the empty column
  time_series = time_series[,!apply(time_series, 2, function(X) all(X == ""))]
  #fix date format in colnames
  # get date label by removing everything else
  date_label = time_series %>% 
    dplyr::select(-c("Province/State", "Country/Region" ,"Lat","Long")) %>% 
    colnames()
  date_label_fixed = convert_date(date_label) %>% as.character()
  #  make date into desirable form and rename the variable names
  time_series = time_series %>% rename_at(vars(date_label), ~ date_label_fixed)
  # if web data used, and if last column is today's date, remove last column and use web data for latest day
  if (web_data &  (date_today %>% as.character() == max(date_label_fixed))) {
    time_series = time_series %>% select(-last_col())
  }
  ##Clear data
  if (type == "Country") {
    # Remove all the unused column, and sum based on country
    data_wide = time_series %>% select(-"Province/State",-Lat,-Long) %>% group_by(`Country/Region`) %>% summarise_all(sum)
    if (web_data) {
			fileurl = paste(web_data_url, "cases_country.csv", sep = "")
			wdata = getURL(fileurl) %>% read_csv() %>% select("Country/Region" = Country_Region ,label)
      data_wide = left_join(x = data_wide,y = wdata, by = "Country/Region") %>% rename_at(vars(label),~date_today %>% as.character() )
      data_wide[is.na(data_wide)] = 0
    }
  } else if (type == "State") {
    # Remove all the unused column, and sum based on country, Select the needed state, get its sum
    data_wide = time_series %>% select(-"Country/Region",-Lat,-Long) %>% filter(`Province/State` == Province_name) %>%
      group_by(`Province/State`) %>% summarise_all(sum)
    if (web_data) {
			fileurl = paste(web_data_url, "cases_state.csv", sep = "")
      wdata = getURL(fileurl) %>% read_csv() %>% select("Province/State" = Province_State ,label) %>% filter(`Province/State` == Province_name)
      data_wide = left_join(x = data_wide,y = wdata, by = "Province/State") %>% rename_at(vars(label),~date_today %>% as.character() )
      data_wide[is.na(data_wide)] = 0
    }
    
  }
  # Data validation : if N is smaller than previous data, assign the number from previous date (Unchanged from original function)
  for (i in 3:ncol(data_wide)) {
    #The first oen is country name,so start with 3-2
    if (any(data_wide[,i] < data_wide[, (i - 1)])) {
      data_wide[data_wide[,i] < data_wide[, (i - 1)], i] = data_wide[data_wide[,i] < data_wide[, (i - 1)], (i - 1)]
    }
  }
  # build incremental data
  temp = data_wide[,-1]
  # Change of each day
  data_incremental = (temp[,-1, drop = F] - temp[,-ncol(temp)]) %>% cbind(data_wide[,1:2],.)
  # First day has no change
  data_incremental[,2] = 0
  #Reformat the data to produce a summary
  a = data_wide %>% pivot_longer(names_to = "Date", values_to = "Counts", cols = contains("-"))
  b = data_incremental %>% pivot_longer(names_to = "Date", values_to = "Counts_incremental", cols = contains("-"))
  data = left_join(x = a, y = b) %>% mutate(Date = as.Date(Date)) %>% arrange(Date)
  colnames(data)[3:4] = c(label, paste(label,"_incremental", sep = ""))
  
  # reverse column order of everything but the first
  data_wide = data_wide[, ncol(data_wide):1] %>% select(last_col(),everything())
  data_incremental = data_incremental[, ncol(data_incremental):1] %>% select(last_col(),everything())
  
  
  return(list(data = data, data_wide = data_wide, data_incremental_wide = data_incremental))
}

create_final_data = function(type = NULL, Province_name = NULL, web_data){ 
  # type: "Country" if by country; "State" if by US states
  if (!type %in% c("Country", "State")) stop("Please specify type as country or state.")
  data_confirmed = read_data("Confirmed", type , web_data,Province_name)
  data_deaths = read_data("Deaths", type, web_data,Province_name)
  data_recovered = read_data("Recovered", type, web_data,Province_name)
  
  case_confirmed = data_confirmed$data
  case_deaths = data_deaths$data
  case_recovered = data_recovered$data
  case_confirmed_wide = data_confirmed$data_wide
  case_confirmed_incremental_wide = data_confirmed$data_incremental_wide
  case_deaths_wide = data_deaths$data_wide
  case_recovered_wide = data_recovered$data_wide
  
  data_all = Reduce(function(x, y) merge(x, y, all = TRUE), list(case_confirmed, case_deaths, case_recovered))
  data_all$Active = data_all$Confirmed - data_all$Deaths - data_all$Recovered
  # Crude_Incidence_Rate
  if (type == "Country") {
    data_all$Population = input_population$Population[match(data_all$Country, input_population$Country)]
    data_all$Crude_Incidence_Rate = as.numeric(data_all$Confirmed)/as.numeric(data_all$Population) * 100000
    data_all$Active_Crude_Incidence_Rate = as.numeric(data_all$Active)/as.numeric(data_all$Population) * 100000
  }
  if (type == "State") { if (Province_name == "Hubei") {
    data_all$Population = 59172000
    data_all$Crude_Incidence_Rate = as.numeric(data_all$Confirmed)/as.numeric(data_all$Population) * 100000
    data_all$Active_Crude_Incidence_Rate = as.numeric(data_all$Active)/as.numeric(data_all$Population) * 100000
  }}
  
  output = list(data_all = data_all,
                case_confirmed_wide = case_confirmed_wide %>% 
                  column_to_rownames(var = colnames(.)[1]) %>% 
                  as.data.frame(), 
                case_confirmed_incremental_wide = case_confirmed_incremental_wide %>% 
                  column_to_rownames(var = colnames(.)[1]) %>% 
                  as.data.frame(),
                case_deaths_wide = case_deaths_wide %>% 
                  column_to_rownames(var = colnames(.)[1]) %>% 
                  as.data.frame(), 
                case_recovered_wide = case_recovered_wide %>% 
                  column_to_rownames(var = colnames(.)[1]) %>% 
                  as.data.frame()
  )
  return(output)  
}


# load data
date_today = Sys.Date()
countries_data = create_final_data(type = "Country", web_data = web_data )
Hubei_data = create_final_data(type = "State",Province_name = "Hubei",web_data = web_data  )


##############################
##        WORLDWIDE         ##
##############################

case_confirmed_wide = countries_data$case_confirmed_wide
case_confirmed_incremental_wide = countries_data$case_confirmed_incremental_wide
case_deaths_wide = countries_data$case_deaths_wide

report_date = max(colnames(case_confirmed_wide))

# time series table
case_confirmed_wide = case_confirmed_wide[order(case_confirmed_wide[,report_date], decreasing = T), ]
case_confirmed_incremental_wide = case_confirmed_incremental_wide[order(case_confirmed_incremental_wide[,report_date], decreasing = T), ]
case_deaths_wide = case_deaths_wide[order(case_deaths_wide[,report_date], decreasing = T), ]
write_csv(case_confirmed_wide, paste(report_date, "table_case_confirmed.csv"))
write_csv(case_confirmed_incremental_wide, paste(report_date,"table_case_confirmed_incremental.csv"))
write_csv(case_deaths_wide, paste(report_date,"table_case_deaths.csv"))

# table 1
crude_incidence_rate = as.data.frame(cbind(row.names(case_confirmed_wide), case_confirmed_wide[, report_date], input_population$Population[match(row.names(case_confirmed_wide), input_population$Country)]), stringsAsFactors = F)
colnames(crude_incidence_rate) = c("Country", "Confirmed_Cases", "Population")
# add Hubei data to crude_incidence_rate
case_confirmed_wide_hubei = Hubei_data$case_confirmed_wide
case_confirmed_wide_hubei = case_confirmed_wide_hubei[, ncol(case_confirmed_wide_hubei):1]
crude_incidence_rate = rbind(c("Hubei", case_confirmed_wide_hubei[, report_date], 59172000), crude_incidence_rate)
crude_incidence_rate$Crude_Incidence_Rate = round(as.numeric(crude_incidence_rate$Confirmed_Cases)/as.numeric(crude_incidence_rate$Population) * 100000, 0)
write_csv(crude_incidence_rate, paste(report_date, "table_1_crude_incidence_rate.csv"))
write_csv(crude_incidence_rate %>% 
            translate_country() %>% 
            translate_country_colname(1), paste(report_date, "table_1_crude_incidence_rate_ch.csv"))

						
# data for plots
data_all_countries = countries_data$data_all
data_all_countries = filter_by_date(data_all_countries, "Date", start_date, end_date)

data_global_latest = data_all_countries[data_all_countries$Date == report_date, ]
data_global_latest$Fatality_rate = round(data_global_latest$Deaths/data_global_latest$Confirmed*100, 1)

# table 2
data_global_latest_confirmed = data_global_latest[,c("Country/Region", "Confirmed")]
data_global_latest_confirmed = data_global_latest_confirmed[order(data_global_latest_confirmed$Confirmed, decreasing = T), ]
rownames(data_global_latest_confirmed) = 1:nrow(data_global_latest_confirmed)
write_csv(data_global_latest_confirmed, paste(report_date, "table_2_case_confirmed_latest_date.csv"))
write_csv(data_global_latest_confirmed %>% 
            rename("Country" = "Country/Region") %>% 
            translate_country() %>% 
            translate_country_colname(2), 
          paste(report_date, "table_2_case_confirmed_latest_date_ch.csv"))

# table 3

data_global_latest_death = data_global_latest[,c("Country/Region", "Deaths", "Deaths_incremental", "Fatality_rate")]
data_global_latest_death = data_global_latest_death[order(data_global_latest_death$Deaths, decreasing = T), ]
rownames(data_global_latest_death) = 1:nrow(data_global_latest_death)
write_csv(data_global_latest_death, paste(report_date, "table_3_case_death_latest_date.csv"))
write_csv(data_global_latest_death %>% 
            rename("Country" = "Country/Region") %>% 
            translate_country() %>% 
            translate_country_colname(3), 
          paste(report_date, "table_3_case_death_latest_date_ch.csv"))

# x label break for plots:
x_min = min(data_all_countries$Date)
x_max = max(data_all_countries$Date)
if (as.numeric(x_max - x_min) < 15 ) {
  break.vec <- seq( x_min, x_max, by = "day")
} else {
  if (as.numeric(x_max - x_min) %% 3 == 2) {
    break.vec <- c(x_min, seq( as.numeric(x_max - x_min) %% 3 + x_min, x_max, by = "3 days"))
  } else {
    break.vec <- c(x_min, seq( as.numeric(x_max - x_min) %% 3 + 3 + x_min, x_max, by = "3 days"))
  }
}

#  specify country_filter
if (template_input) {
  country_list = read.csv("input_country_list.csv", stringsAsFactors = F)
  filter_total <- filter_incremental <- country_list$Countries
} else {
  temp = data_all_countries
  if (remove_mainland_china) temp = temp[!temp$Country %in% china_label, ]
  temp = temp[temp$Date == max(data_all_countries$Date),] 
  
  # filter by total confirmed
  temp_total = temp[order(temp$Confirmed, decreasing = T), ]
  filter_total = temp_total$Country[1:top_n]
  temp_incremental = temp[order(temp$Confirmed_incremental, decreasing = T), ]
  filter_incremental = temp_incremental$Country[1:top_n]
  
  # filter by death
  temp_death = temp[order(temp$Deaths,decreasing = T),]
  filter_death = temp_death$Country[1:top_n]
  # temp_death_incremental = temp[order(temp$Death_incremental, decreasing = T),]
  # filter_death_incremental = temp_death_incremental$Country[1:top_n]
  # 
}


color_list_country = unique(c(filter_total, filter_incremental,filter_death, "China", "Hubei"))

# plot 1. total confirmed cases sort by countries cumulative

# filter by country and cumulative confirmed
data_to_plot_confirmed = data_all_countries[data_all_countries$Country %in% filter_total, ]
# reorder factor levels by country filter order
temp = data_to_plot_confirmed[data_to_plot_confirmed$Date == max(data_to_plot_confirmed$Date), ]
temp = temp[order(temp$Confirmed, decreasing = T),]
country_order = temp$Country
data_to_plot_confirmed$Country <- factor(data_to_plot_confirmed$Country, levels = country_order)

y_max = (round(max(data_to_plot_confirmed$Confirmed)/1000) + 1)*1000
y_interval = adjust_y_interval(y_max)
p1 = ggplot(data_to_plot_confirmed , aes(x = Date, y = Confirmed, group = Country, colour = Country,  shape = Country)) + 
  geom_point(size = 2) + 
  geom_line(size = 1) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size = 19,face = "bold.italic"), legend.text = element_text(size = 18,face = "italic")) +
  scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
  scale_x_date(breaks = break.vec, date_labels = "%m-%d") +
  scale_color_manual(values = color_list[match(country_order, color_list_country)]) +
  xlab("") +
  ylab(p1_ylab) 

ggsave(filename = paste(report_date,"p1",p1_title, ".pdf"), plot = p1, width = 10, height = 8 )


# plot 1-1. total confirmed cases sort by countries cumulative (including China)
filter_total_with_china = c(china_label, filter_total)

# filter by country and cumulative confirmed
data_to_plot_confirmed = data_all_countries[data_all_countries$Country %in% filter_total_with_china, ]

# reorder factor levels by country filter order
temp = data_to_plot_confirmed[data_to_plot_confirmed$Date == max(data_to_plot_confirmed$Date),]
temp = temp[order(temp$Confirmed,decreasing = T),]
country_order = temp$Country
data_to_plot_confirmed$Country <- factor(data_to_plot_confirmed$Country, levels = country_order)

y_max = (round(max(data_to_plot_confirmed$Confirmed)/1000) + 1) * 1000
y_interval = adjust_y_interval(y_max)
p1_1 = ggplot(data_to_plot_confirmed , aes(x = Date, y = Confirmed, group = Country, colour = Country,  shape = Country)) + 
  geom_point(size = 2) + 
  geom_line(size = 1) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size = 19,face = "bold.italic"), legend.text = element_text(size = 18,face = "italic")) +
  scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
  scale_x_date(breaks = break.vec, date_labels = "%m-%d") +
  scale_color_manual(values = color_list[match(country_order, color_list_country)]) +
  xlab("") +
  ylab(p1_ylab)

ggsave(filename = paste(report_date, "p1-1", p1_1_title, ".pdf"), plot = p1_1, width = 10, height = 8 )


# plot 2. incremental cases for top N total confirmed

# filter by country and cumulative confirmed
data_to_plot_confirmed = data_all_countries[data_all_countries$Country %in% filter_total , ]

# reorder factor levels by country filter order
temp = data_to_plot_confirmed[data_to_plot_confirmed$Date == max(data_to_plot_confirmed$Date),]
temp = temp[order(temp$Confirmed_incremental,decreasing = T),]
country_order = temp$Country
data_to_plot_confirmed$Country <- factor(data_to_plot_confirmed$Country, levels = country_order)

y_max = (round(max(data_to_plot_confirmed$Confirmed_incremental)/1000) + 1)*1000
y_interval = adjust_y_interval(y_max)

# plot 2


p2 = ggplot(data_to_plot_confirmed, aes(x = Date, y = Confirmed_incremental, 
                                        group = Country, 
                                        colour = Country, 
                                        shape = Country)) + 
  geom_point(size = 2) + 
  geom_line(size = 1) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size = 19,face = "bold.italic"), legend.text = element_text(size = 18,face = "italic")) +
  scale_y_continuous(breaks = seq(0,y_max, y_interval), label = comma) +
  scale_x_date(breaks = break.vec, date_labels = "%m-%d") +
  scale_color_manual(values = color_list[match(country_order, color_list_country)]) +
  # ggtitle("日新增确诊病例国家趋势图", subtitle = "中国及其他前五位国家") + 
  xlab("") +
  ylab(p2_ylab)

ggsave(filename = paste(report_date,"p2",p2_title, ".pdf"), plot = p2, width = 10, height = 8 )



# plot 3. new confirmed cases daily sort by countries incremental
# filter by country total and date
data_to_plot_confirmed_increment = data_all_countries[data_all_countries$Country %in% filter_incremental ,]
# reorder factor levels by country filter order
temp = data_to_plot_confirmed_increment[data_to_plot_confirmed_increment$Date == max(data_to_plot_confirmed_increment$Date),]
temp = temp[order(temp$Confirmed_incremental,decreasing = T),]
country_order = temp$Country
data_to_plot_confirmed_increment$Country <- factor(data_to_plot_confirmed_increment$Country, levels = country_order)

y_max = (round(max(data_to_plot_confirmed_increment$Confirmed_incremental)/1000) + 1)*1000
y_interval = adjust_y_interval(y_max)
p3 = ggplot(data_to_plot_confirmed_increment, aes(x = Date, y = Confirmed_incremental, 
                                                  group = Country, 
                                                  colour = Country,
                                                  shape = Country)) + 
  geom_point(size = 2) + 
  geom_line(size = 1) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size = 19,face = "bold.italic"), legend.text = element_text(size = 18,face = "italic")) +
  scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
  scale_x_date(breaks = break.vec, date_labels = "%m-%d") +
  scale_color_manual(values = color_list[match(country_order, color_list_country)]) +
  # ggtitle("累计确诊病例国家趋势图", subtitle = "中国及其他前五位国家") + 
  xlab("") +
  ylab(p3_ylab)

ggsave(filename = paste(report_date,"p3",p3_title, ".pdf"), plot = p3, width = 10, height = 8 )



# plot 3-1. new confirmed cases daily sort by countries incremental (including China)
filter_incremental_inc_china = c(china_label, filter_incremental)
# filter by country total and date
data_to_plot_confirmed_increment = data_all_countries[data_all_countries$Country %in% filter_incremental_inc_china,]
# reorder factor levels by country filter order
temp = data_to_plot_confirmed_increment[data_to_plot_confirmed_increment$Date == max(data_to_plot_confirmed_increment$Date),]
temp = temp[order(temp$Confirmed_incremental,decreasing = T),]
country_order = temp$Country
data_to_plot_confirmed_increment$Country <- factor(data_to_plot_confirmed_increment$Country, levels = country_order)

y_max=(round(max(data_to_plot_confirmed_increment$Confirmed_incremental)/1000)+1)*1000
y_interval = adjust_y_interval(y_max)
p3_1 = ggplot(data_to_plot_confirmed_increment , aes(x=Date, y=Confirmed_incremental, group=Country, colour = Country,  shape = Country)) + 
  geom_point(size=2) + 
  geom_line(size=1) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size=19,face = "bold.italic"), legend.text = element_text(size = 18,face = "italic")) +
  scale_y_continuous(breaks=seq(0,y_max, y_interval),label=comma) +
  scale_x_date(breaks = break.vec, date_labels = "%m-%d") +
  scale_color_manual(values=color_list[match(country_order, color_list_country)]) +
  xlab("") +
  ylab(p3_ylab)

ggsave(filename=paste(report_date,"p3_1",p3_1_title, ".pdf"), plot = p3_1, width = 10, height = 8 )


# plot 7-1 crude incidence rate VS Hubei
# filter by country and cumulative confirmed

Hubei_data_plot = Hubei_data$data_all
Hubei_data_plot = filter_by_date(Hubei_data_plot, "Date", start_date, end_date)
names(Hubei_data_plot)[1] = "Region"

data_to_plot_IR = data_all_countries[data_all_countries$Country %in% filter_total_with_china, ]
names(data_to_plot_IR)[1] = "Region"

data_to_plot_IR = rbind(Hubei_data_plot, data_to_plot_IR)

# reorder factor levels by Crude_Incidence_Rate
temp = data_to_plot_IR[data_to_plot_IR$Date == max(data_to_plot_IR$Date),]
temp = temp[order(temp$Crude_Incidence_Rate,decreasing = T),]
region_order=temp$Region
data_to_plot_IR$Region <- factor(data_to_plot_IR$Region, levels = region_order)

y_max=(round(max(data_to_plot_IR$Crude_Incidence_Rate)/10)+1)*10
y_interval = adjust_y_interval(y_max)

p7_1 = ggplot(data_to_plot_IR , aes(x=Date, y=Crude_Incidence_Rate, group=Region, colour = Region,  shape = Region)) + 
  geom_point(size=2) + 
  geom_line(size=1) +
  scale_shape_manual(values=1:nlevels(data_to_plot_IR$Region)) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size=19,face="bold.italic"), legend.text = element_text(size = 18,face="italic")) +
  scale_y_continuous(breaks=seq(0,y_max, y_interval),label=comma) +
  scale_x_date(breaks = break.vec, date_labels = "%m-%d") +
  scale_color_manual(values=color_list[match(region_order, color_list_country)]) +
  xlab("") +
  ylab(p7_ylab)

ggsave(filename=paste(report_date,"p7-1",p7_1_title, ".pdf"), plot = p7_1, width = 10, height = 8 )

# plot 8-1. cumulative death cases sort by countries cumulative (including China)
filter_total_with_china = c(china_label, filter_death)

# filter by country and cumulative confirmed
data_to_plot_death_total = data_all_countries[data_all_countries$Country %in% filter_total_with_china, ]
# reorder factor levels by country filter order
temp = data_to_plot_death_total[data_to_plot_death_total$Date == max(data_to_plot_death_total$Date),]
temp = temp[order(temp$Deaths,decreasing = T),]
country_order = temp$Country
data_to_plot_death_total$Country <- factor(data_to_plot_death_total$Country, levels = country_order)

y_max=(round(max(data_to_plot_death_total$Deaths)/1000)+1)*1000
y_interval = adjust_y_interval(y_max)
p8_1 = ggplot(data_to_plot_death_total , aes(x=Date, y= Deaths, group=Country, colour = Country,  shape = Country)) + 
  geom_point(size=2) + 
  geom_line(size=1) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size=19,face="bold.italic"), legend.text = element_text(size = 18,face="italic")) +
  scale_y_continuous(breaks=seq(0,y_max, y_interval),label=comma) +
  scale_x_date(breaks = break.vec, date_labels = "%m-%d") +
  scale_color_manual(values=color_list[match(country_order, color_list_country)]) +
  xlab("") +
  ylab(p8_ylab)

ggsave(filename=paste(report_date,"p8-1",p8_1_title, ".pdf"), plot = p8_1, width = 10, height = 8 )



##############################
## US Active Deaths Recovered
##############################

### US Active Deaths Recovered Table
case_confirmed_wide = countries_data$case_confirmed_wide
case_deaths_wide = countries_data$case_deaths_wide

US_Confirmed = case_confirmed_wide[rownames(case_confirmed_wide) == "US",]
US_Deaths = case_deaths_wide[rownames(case_deaths_wide) == "US",]
# US_Recoverd = case_recovered_wide[rownames(case_recovered_wide) == "US",]
# US_Active = US_Confirmed - US_Deaths - US_Recoverd
# US_tbl = rbind(US_Active, US_Deaths, US_Recoverd)
US_tbl = rbind(US_Confirmed, US_Deaths)
rownames(US_tbl) = c("Total Confirmed","Deaths")
# reverse column order 
US_tbl = US_tbl[, ncol(US_tbl):1]
write_csv(US_tbl, paste(report_date,"table_active_deaths_recovered_US.csv"))

# plot 4: US only Active Deaths Recovered
temp = data_all_countries[data_all_countries$Country =='US' , c("Country/Region", "Date", "Confirmed", "Deaths")]
data_to_plot_us = reshape(data=temp, 
                          idvar=c("Country/Region", "Date"),
                          varying = names(temp)[-(1:2)],
                          v.name=c("Cases"),
                          times=names(temp)[-(1:2)],
                          new.row.names = 1:(nrow(temp)*(ncol(temp)-2)),
                          direction="long")
colnames(data_to_plot_us)[grep("time", names(data_to_plot_us))] = "Status"

# reorder label 
temp = data_to_plot_us[data_to_plot_us$Date == max(data_to_plot_us$Date),]
temp = temp[order(temp$Cases,decreasing = T),]
status_order = temp$Status
data_to_plot_us$Status <- factor(data_to_plot_us$Status, levels = status_order)

y_max=(round(max(data_to_plot_us$Cases)/1000)+1)*1000
y_interval = adjust_y_interval(y_max)
p4 = ggplot(data_to_plot_us , aes(x=Date, y=Cases, group=Status, colour = Status,  shape = Status)) + 
  geom_point(size=2) + 
  geom_line(size=1) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size=19,face="bold.italic"), legend.text = element_text(size = 18,face="italic")) +
  scale_y_continuous(breaks=seq(0,y_max, y_interval),label=comma) +
  scale_x_date(breaks = break.vec, date_labels = "%m-%d") +
  xlab("") +
  ylab(p4_ylab)

ggsave(filename=paste(report_date,"p4",p4_title, ".pdf"), plot = p4, width = 10, height = 8 )



##############################
## US by states
##############################

state_population = read.csv("input_state_population.csv", stringsAsFactors = F)

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
  colnames(data)[3:4]=c(label, paste(label,"_Incremental", sep=""))
  
  # reverse column order of everything but the first
  data_wide = data_wide[, ncol(data_wide):1]%>%select(last_col(),everything())
  data_incremental = data_incremental[, ncol(data_incremental):1]%>%select(last_col(),everything())
  
  return(list(data=data, 
				data_wide=data_wide%>%column_to_rownames(var=colnames(.)[1])%>%as.data.frame(), 
				data_incremental_wide=data_incremental%>%column_to_rownames(var=colnames(.)[1])%>%as.data.frame()))
	
}

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
# US Hospitalize and Test Results Detail View
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
write_csv(data_us_states_detailed, paste(report_date_us,"table_US_details_data_all.csv"))

# sort by latest day
Confirmed = case_confirmed_wide[order(case_confirmed_wide[,report_date], decreasing = T),]
Confirmed_Incremental = case_confirmed_incremental_wide[order(case_confirmed_incremental_wide[,report_date], decreasing = T),]
Deaths = case_deaths_wide[order(case_deaths_wide[,report_date], decreasing = T),]
Deaths_Incremental = case_deaths_incremental_wide[order(case_deaths_incremental_wide[,report_date], decreasing = T),]
write_csv(Confirmed, paste(report_date,"table_case_confirmed_US.csv" ))
write_csv(Confirmed_Incremental, paste(report_date,"table_case_confirmed_incremental_US.csv" ))
write_csv(Deaths, paste(report_date,"table_case_deaths_US.csv" ))
write_csv(Deaths_Incremental, paste(report_date,"table_case_deaths_incremental_US.csv" ))

# add crude_incidence_rate
data_us_states$population = state_population$Population[match(data_us_states$state, state_population$State_Full)]
data_us_states$crude_incidence_rate = round(as.numeric(data_us_states$Confirmed)/as.numeric(data_us_states$population) * 100000, 0)
# Fatality_rate
data_us_states$Fatality_rate = round(data_us_states$Deaths/data_us_states$Confirmed*100, 1)
data_us_states[is.na(data_us_states)] = 0

# filter by latest date
data_us_latest = data_us_states[data_us_states$Date == max(data_us_states$Date),]

if (data_us_latest$Date[1] == max(data_us_states_detailed$date)) {
  data_us_states_detailed_latest = data_us_states_detailed[data_us_states_detailed$date == max(data_us_states_detailed$date),]
  data_us_states_detailed_latest_keep = data_us_states_detailed_latest[, c("state", "totalTestResults", "totalTestResultsIncrease")]
  data_us_latest = merge(data_us_latest, data_us_states_detailed_latest_keep, by = "state")
  data_us_latest$positive_rate = round(data_us_latest$Confirmed/data_us_latest$totalTestResults, 2) * 100
  data_us_latest$pct_test = round(data_us_latest$totalTestResults/data_us_latest$population * 100000, 0)
  data_us_latest_confirm = data_us_latest[, c("state", "Confirmed", "crude_incidence_rate","positive_rate", "totalTestResults", "totalTestResultsIncrease", "pct_test")]
} else {  
  print("US test data hasn't been updated yet. Try later.")
  data_us_latest_confirm = data_us_latest[, c("state", "Confirmed", "crude_incidence_rate")]
}
# table 4 

data_us_latest_confirm = data_us_latest_confirm[order(data_us_latest_confirm$Confirmed, decreasing = T),]
rownames(data_us_latest_confirm) = 1:nrow(data_us_latest_confirm)
write_csv(data_us_latest_confirm, paste(report_date,"table_4_confirmed_cases_and_incidence_rate_US.csv"))
write_csv(data_us_latest_confirm %>% 
            translate_state() %>% 
            translate_state_colname(4), paste(report_date,"table_4_confirmed_cases_and_incidence_rate_US_ch.csv"))

# table 5
data_us_latest_incremental = data_us_latest[, c("state", "Confirmed_Incremental")]
data_us_latest_incremental = data_us_latest_incremental[order(data_us_latest_incremental$Confirmed_Incremental, decreasing = T),]
data_us_latest_incremental$Percentage = round(data_us_latest_incremental$Confirmed_Incremental/sum(data_us_latest_incremental$Confirmed_Incremental)*100,0)
rownames(data_us_latest_incremental) = 1:nrow(data_us_latest_incremental)
write_csv(data_us_latest_incremental, paste(report_date,"table_5_confirmed_incremental_US.csv"))
write_csv(data_us_latest_incremental %>% 
            translate_state() %>% 
            translate_state_colname(5), paste(report_date,"table_5_confirmed_incremental_US_ch.csv"))
						
# table 6
data_us_latest_fatality = data_us_latest[, c("state", "Deaths", "Fatality_rate")]
data_us_latest_fatality = data_us_latest_fatality[order(data_us_latest_fatality$Deaths, decreasing = T),]
rownames(data_us_latest_fatality) = 1:nrow(data_us_latest_fatality)
write_csv(data_us_latest_fatality, paste(report_date,"table_6_fatality_US.csv"))
write_csv(data_us_latest_fatality %>% 
            translate_state() %>% 
            translate_state_colname(6), paste(report_date,"table_6_fatality_US_ch.csv"))


# data for plots

# filter by date
data_us_states = filter_by_date(data_us_states, "Date", start_date_US, end_date_US)

if (template_input) {
  state_list = read.csv("input_us_state_list.csv", stringsAsFactors = F)
  filter_total <- filter_incremental <- state_list$States
}else{
  temp = data_us_states
  temp = temp[temp$Date == max(data_us_states$Date),] 
  temp_total = temp[order(temp$Confirmed, decreasing = T), ]
  filter_total = temp_total$state[1:top_n]
  temp_incremental = temp[order(temp$Confirmed_Incremental, decreasing = T), ]
  filter_incremental = temp_incremental$state[1:top_n]
	temp_death = temp[order(temp$Deaths, decreasing = T),]
  filter_death = temp_death$state[1:top_n]				
}

# x label break for all plots:
x_min = min(data_us_states$Date)
x_max = max(data_us_states$Date)
if (as.numeric(x_max - x_min) < 15) {
  break.vec <- seq( x_min, x_max, by = "day")
}else{
  if (as.numeric(x_max - x_min) %% 3 == 2) {
    break.vec <- c(x_min, seq( as.numeric(x_max - x_min) %% 3 + x_min, x_max, by = "3 days"))
  }else{
    break.vec <- c(x_min, seq( as.numeric(x_max - x_min) %% 3 + 3 + x_min, x_max, by = "3 days"))
  }
}

color_list_state = unique(c(filter_total, filter_incremental, filter_death))

# plot 5: total confirmed cases by US States sort by Cumulative 
data_to_plot = data_us_states[data_us_states$state %in% filter_total, ]

# reorder factor levels by country filter order
temp = data_to_plot[data_to_plot$Date == max(data_to_plot$Date),]
temp = temp[order(temp$Confirmed,decreasing = T),]
state_order = temp$state
data_to_plot$state <- factor(data_to_plot$state, levels = state_order)

y_max=(round(max(data_to_plot$Confirmed)/500)+1)*500
y_interval = adjust_y_interval(y_max)
p5 = ggplot(data_to_plot , aes(x=Date, y=Confirmed, group=state, colour = state,  shape = state)) + 
  geom_point(size=2) + 
  geom_line(size=1) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size=19,face="bold.italic"), legend.text = element_text(size = 18,face="italic")) +
  scale_y_continuous(breaks=seq(0,y_max, y_interval),label=comma) +
  scale_x_date(breaks = break.vec, date_labels = "%m-%d") +
  scale_color_manual(values=color_list[match(state_order, color_list_state)]) +
  xlab("") +
  ylab(p5_ylab)


ggsave(filename=paste(report_date,"p5",p5_title, ".pdf"), plot = p5, width = 10, height = 8 )


# plot 6: new confirmed cases daily by US States sort by incremental
data_to_plot_incremental = data_us_states[data_us_states$state %in% filter_incremental , ]

# reorder factor levels by country filter order
temp = data_to_plot_incremental[data_to_plot_incremental$Date == max(data_to_plot_incremental$Date),]
temp = temp[order(temp$Confirmed_Incremental,decreasing = T),]
state_order = temp$state
data_to_plot_incremental$state <- factor(data_to_plot_incremental$state, levels = state_order)

y_max=(round(max(data_to_plot_incremental$Confirmed_Incremental)/100)+1)*100
y_interval = adjust_y_interval(y_max)
p6 = ggplot(data_to_plot_incremental, aes(x = Date, y = Confirmed_Incremental, 
                                          group = state, 
                                          colour = state, 
                                          shape = state)) + 
  geom_point(size = 2) + 
  geom_line(size = 1) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), 
        axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size = 19,face = "bold.italic"), 
        legend.text = element_text(size = 18,face = "italic")) +
  scale_y_continuous(breaks = seq(0,y_max, y_interval),label = comma) +
  scale_x_date(breaks = break.vec, date_labels = "%m-%d") +
  scale_color_manual(values = color_list[match(state_order, color_list_state)]) +
  xlab("") +
  ylab(p6_ylab)

ggsave(filename=paste(report_date,"p6",p6_title, ".pdf"), plot = p6, width = 10, height = 8 )


# plot9: total death cases by US States sorted by cumulative
data_to_plot_death = data_us_states[data_us_states$state %in% filter_death,]
temp = data_to_plot_death[data_to_plot_death$Date == max(data_to_plot_death$Date),]
temp = temp[order(temp$Deaths,decreasing = T),]
state_order = temp$state
data_to_plot_death$state <- factor(data_to_plot_death$state, levels = state_order)

y_max=(round(max(data_to_plot_death$Deaths)/500)+1)*500
y_interval = adjust_y_interval(y_max)
p9 = ggplot(data_to_plot_death , aes(x=Date, y=Deaths, group=state, colour = state,  shape = state)) + 
  geom_point(size=2) + 
  geom_line(size=1) +
  theme_bw() + 
  theme(panel.border = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(colour = "black")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size=19,face="bold.italic"), legend.text = element_text(size = 18,face="italic")) +
  scale_y_continuous(breaks=seq(0,y_max, y_interval),label=comma) +
  scale_x_date(breaks = break.vec, date_labels = "%m-%d") +
  scale_color_manual(values=color_list[match(state_order, color_list_state)]) +
  xlab("") +
  ylab(p9_ylab)

ggsave(filename=paste(report_date,"p9",p9_title, ".pdf"), plot = p9, width = 10, height = 8 )





