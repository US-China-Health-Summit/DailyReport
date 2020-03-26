##############################
####        README        ####
##############################

### Data source (by country):  https://github.com/CSSEGISandData/COVID-19
### Data source (by us state):  https://covidtracking.com/api/states/daily

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
# 'input_US_State_Abbr.csv' : 
#     Always keep this file in the folder. No need to modify. 

############################################################

##############################
####        INPUTS        ####
##############################

##### folder path. In this folder, you should have all template input files mentioned above and these three time series files. 
folder = "YOUR_PATH/csse_covid_19_time_series"

##### Filter countries
## Set template_input to TRUE if filter country using template file provided; 
## Otherwise, auto select countries/State by Top N
template_input = FALSE
top_n = 5

##### filter data by start date and end date
# start_date = c(mm, dd, yyyy) ; set as NULL if not used
# end_date = c(mm, dd, yyyy) ; set as NULL if not used
start_date = NULL
end_date = NULL

#####  sort without China: TRUE, FALSE ; default is TRUE (sort without China)
remove_mainland_china = TRUE
china_label = "China"   

##### web data (TRUE if use additional data file from web data branch; otherwise set to FALSE)
web_data = TRUE

############################################################

library(ggplot2)
library(jsonlite)
library(httr)

setwd(folder)

###############################################################
## RUN THROUGH EVERYTHING BELOW TO GENERATE PLOTS AND TABLES ##
###############################################################

input_plot_titles = read.csv("input_plot_titles.csv", stringsAsFactors = F)
p1_title = input_plot_titles$Input[input_plot_titles$Item=="p1_title"]
p1_1_title = input_plot_titles$Input[input_plot_titles$Item=="p1_1_title"]
p1_2_title = input_plot_titles$Input[input_plot_titles$Item=="p1_2_title"]
p1_xlab = input_plot_titles$Input[input_plot_titles$Item=="p1_xlab"]
p1_ylab = input_plot_titles$Input[input_plot_titles$Item=="p1_ylab"]
p2_title = input_plot_titles$Input[input_plot_titles$Item=="p2_title"]
p2_xlab = input_plot_titles$Input[input_plot_titles$Item=="p2_xlab"]
p2_ylab = input_plot_titles$Input[input_plot_titles$Item=="p2_ylab"]
p3_title = input_plot_titles$Input[input_plot_titles$Item=="p3_title"]
p3_1_title = input_plot_titles$Input[input_plot_titles$Item=="p3_1_title"]
p3_2_title = input_plot_titles$Input[input_plot_titles$Item=="p3_2_title"]
p3_xlab = input_plot_titles$Input[input_plot_titles$Item=="p3_xlab"]
p3_ylab = input_plot_titles$Input[input_plot_titles$Item=="p3_ylab"]
p4_title = input_plot_titles$Input[input_plot_titles$Item=="p4_title"]
p4_xlab = input_plot_titles$Input[input_plot_titles$Item=="p4_xlab"]
p4_ylab = input_plot_titles$Input[input_plot_titles$Item=="p4_ylab"]
p5_title = input_plot_titles$Input[input_plot_titles$Item=="p5_title"]
p5_xlab = input_plot_titles$Input[input_plot_titles$Item=="p5_xlab"]
p5_ylab = input_plot_titles$Input[input_plot_titles$Item=="p5_ylab"]
p6_title = input_plot_titles$Input[input_plot_titles$Item=="p6_title"]
p6_xlab = input_plot_titles$Input[input_plot_titles$Item=="p6_xlab"]
p6_ylab = input_plot_titles$Input[input_plot_titles$Item=="p6_ylab"]
p7_1_title = input_plot_titles$Input[input_plot_titles$Item=="p7_1_title"]
p7_2_title = input_plot_titles$Input[input_plot_titles$Item=="p7_2_title"]
p7_xlab = input_plot_titles$Input[input_plot_titles$Item=="p7_xlab"]
p7_ylab = input_plot_titles$Input[input_plot_titles$Item=="p7_ylab"]

input_population = read.csv("input_country_population.csv" , stringsAsFactors = F)

convert_date=function(date_label){
  temp =unlist(lapply(as.character(date_label), function(X) {
    a = strsplit(X, "/")[[1]]
		if (nchar(a[3]) == 2) a[3] = 2000+as.numeric(a[3])
    a  }))
  month = temp[1:length(date_label)*3-2]
  day = temp[1:length(date_label)*3-1]
  year = temp[1:length(date_label)*3]
  
  as.Date(ISOdate(year = year, month = month, day = day))
}

adjust_y_interval = function(y_max){
  temp_interval = y_max / 10
  if (temp_interval<15){
    y_interval = ceiling((temp_interval/20))*20
  }else if (temp_interval<30){
    y_interval = ceiling((temp_interval/25))*25
  }else if (temp_interval<50){
    y_interval = ceiling((temp_interval/50))*50
  }else if (temp_interval<500){
    y_interval = ceiling((temp_interval/100))*100
  }else{
    y_interval = ceiling((temp_interval/1000))*1000
  }
  y_interval
}

read_data = function(label, type, web_data){
  # read time series data

	if (label == "Confirmed")
		time_series=read.csv("time_series_covid19_confirmed_global.csv", head = F, stringsAsFactors = F)
	if (label == "Deaths")
		time_series=read.csv("time_series_covid19_deaths_global.csv", head = F, stringsAsFactors = F)
	time_series=time_series[,!apply(time_series[-1, ], 2, function(X) all(X==""))]
	# extract data label
  date_label = time_series[1, -(1:4)]
  ds=time_series[-1, ]
	
	# if web data used, and if last column is today's date, remove last column and use web data for latest day
	if (web_data &  (Sys.Date() == convert_date(date_label[length(date_label)]))){
		ds = ds[, -ncol(ds)]
		date_label = date_label[-length(date_label)]
	}

	# clean data
	if (type == "Country"){
		data_wide=as.data.frame(apply(ds[,5:ncol(ds)], 2, function(x) tapply(x, ds$V2, function(X) sum(as.numeric(X)))))
		if (web_data){
			wdata = read.csv("cases_country.csv", stringsAsFactors = F)
			temp = wdata[,label, drop=F]
			rownames(temp ) = wdata[, 1]
			data_wide = merge(data_wide, temp, by=0, all.x = TRUE, all.y = FALSE) 
			data_wide[is.na(data_wide)] = 0
			rownames(data_wide) = data_wide[, "Row.names"]
			data_wide = data_wide[, -grep("Row.names", names(data_wide))]
			date_today = paste(strsplit(as.character(Sys.Date()), "-")[[1]][c(2,3,1)], collapse = "/")
			date_label =  unlist(c(date_label, date_today))
		}
	}else if (type == "Hubei"){
		data_wide=ds[ds$V1 == "Hubei", -(1:4)]
		data_wide = as.data.frame(t(apply(data_wide, 1, as.numeric)))
		rownames(data_wide) = "Hubei"
		if (web_data){
			wdata = read.csv("cases_state.csv", stringsAsFactors = F)
			wdata = wdata[wdata$Province_State == "Hubei",]
			temp = wdata[,label, drop=F]
			data_wide = cbind(data_wide, temp) 
			date_today = paste(strsplit(as.character(Sys.Date()), "-")[[1]][c(2,3,1)], collapse = "/")
			date_label =  unlist(c(date_label, date_today))
		}
	}
  colnames(data_wide)=date_label
	
	# Data validation : if N is smaller than previous data, assign the number from previous date
	for (i in 2:ncol(data_wide)){
		if (any(data_wide[,i] < data_wide[, (i-1)])) data_wide[data_wide[,i] < data_wide[, (i-1)], i] = data_wide[data_wide[,i] < data_wide[, (i-1)], (i-1)]
	}
	
  data_incremental=data_wide[,-1, drop = F]-data_wide[,-ncol(data_wide)]
  data_incremental=cbind(0, data_incremental)
  colnames(data_incremental)=date_label
  
  Counts = unlist(c(data_wide))
  Counts_incremental = unlist(c(data_incremental))
  Date = unlist(lapply(date_label, function(X) rep(X, nrow(data_wide))))
  Region = rep(row.names(data_wide), ncol(data_wide))
  
  data = data.frame(Region = Region, Date = Date, Counts = Counts, Counts_incremental=Counts_incremental,row.names=NULL, stringsAsFactors = F)
  colnames(data)[3:4]=c(label, paste(label,"_incremental", sep=""))
	colnames(data)[1]=type
  
  return(list(data=data, data_wide=data_wide, data_incremental_wide=data_incremental))
}

create_final_data=function(type = NULL){ 
  # type: "Country" if by country; "State" if by US states
  if (!type %in% c("Country", "Hubei")) stop("Please specify type as country or state.")
	
	data_confirmed = read_data("Confirmed", type, web_data)
	data_deaths = read_data("Deaths", type, web_data)
	# data_recovered = read_data("Recovered", type, web_data)
	
	case_confirmed = data_confirmed$data
	case_deaths = data_deaths$data
	# case_recovered = data_recovered$data
	case_confirmed_wide = data_confirmed$data_wide
	case_confirmed_incremental_wide = data_confirmed$data_incremental_wide
	case_deaths_wide = data_deaths$data_wide
	# case_recovered_wide = data_recovered$data_wide

  data_all = Reduce(function(x, y) merge(x, y, all=TRUE), list(case_confirmed, case_deaths)) # ,  case_recovered))
	# data_all$Active = data_all$Confirmed - data_all$Deaths - data_all$Recovered
  data_all$Date = convert_date(data_all$Date)
	
	# Crude_Incidence_Rate
	if (type == "Country"){
		data_all$Population = input_population$Population[match(data_all$Country, input_population$Country)]
		data_all$Crude_Incidence_Rate=as.numeric(data_all$Confirmed)/as.numeric(data_all$Population) * 100000
		# data_all$Active_Crude_Incidence_Rate=as.numeric(data_all$Active)/as.numeric(data_all$Population) * 100000
	}
	if (type == "Hubei"){
		data_all$Population = 59172000
		data_all$Crude_Incidence_Rate=as.numeric(data_all$Confirmed)/as.numeric(data_all$Population) * 100000
		# data_all$Active_Crude_Incidence_Rate=as.numeric(data_all$Active)/as.numeric(data_all$Population) * 100000
	}
  return(list(data_all=data_all, case_confirmed_wide= case_confirmed_wide, case_confirmed_incremental_wide = case_confirmed_incremental_wide, case_deaths_wide = case_deaths_wide))  # , case_recovered_wide=case_recovered_wide
}


##############################
## WORLDWIDE
##############################

# load data
countries_data = create_final_data(type = "Country")
Hubei_data = create_final_data(type = "Hubei")

case_confirmed_wide= countries_data$case_confirmed_wide
case_confirmed_incremental_wide= countries_data$case_confirmed_incremental_wide
case_deaths_wide = countries_data$case_deaths_wide
report_date = convert_date(colnames(case_confirmed_wide)[ncol(case_confirmed_wide)])

# table2
tb2 = cbind(case_confirmed_wide[, (ncol(case_confirmed_wide)-1):ncol(case_confirmed_wide)])
Incremental = case_confirmed_incremental_wide[, ncol(case_confirmed_incremental_wide)]
tb2 = cbind(tb2,Incremental)
tb2 = tb2[order(tb2$Incremental, decreasing = T), ]
write.csv(tb2, paste(report_date,"table_2_incremental_and_latest_2_day_confirmed.csv"))

# time series table
case_confirmed_wide = case_confirmed_wide[order(case_confirmed_wide[ncol(case_confirmed_wide)], decreasing = T), ]
case_confirmed_incremental_wide = case_confirmed_incremental_wide[order(case_confirmed_incremental_wide[ncol(case_confirmed_incremental_wide)], decreasing = T), ]
case_deaths_wide = case_deaths_wide[order(case_deaths_wide[ncol(case_deaths_wide)], decreasing = T), ]
diff_deaths = case_deaths_wide[ncol(case_deaths_wide)]-case_deaths_wide[ncol(case_deaths_wide)-1]
colnames(diff_deaths) = "Difference"
case_deaths_wide = cbind(case_deaths_wide, diff_deaths)

# reverse column order 
case_confirmed_wide = case_confirmed_wide[, ncol(case_confirmed_wide):1]
case_confirmed_incremental_wide = case_confirmed_incremental_wide[, ncol(case_confirmed_incremental_wide):1]
case_deaths_wide = case_deaths_wide[, ncol(case_deaths_wide):1]
write.csv(case_confirmed_wide, paste(report_date, "table_case_confirmed.csv"))
write.csv(case_confirmed_incremental_wide, paste(report_date,"table_case_confirmed_incremental.csv"))
write.csv(case_deaths_wide, paste(report_date,"table_case_deaths.csv"))

# crude incidence rate order by latest confirmed 
crude_incidence_rate = as.data.frame(cbind(row.names(case_confirmed_wide), case_confirmed_wide[, ncol(case_confirmed_wide)], input_population$Population[match(row.names(case_confirmed_wide), input_population$Country)]), stringsAsFactors = F)
colnames(crude_incidence_rate) = c("Region", "Confirmed_Cases", "Population")
# add Hubei data to crude_incidence_rate
crude_incidence_rate=rbind(c("Hubei", Hubei_data$case_confirmed_wide[, ncol(Hubei_data$case_confirmed_wide)], 59172000), crude_incidence_rate)
crude_incidence_rate$Crude_Incidence_Rate = round(as.numeric(crude_incidence_rate$Confirmed_Cases)/as.numeric(crude_incidence_rate$Population) * 100000, 2)
write.csv(crude_incidence_rate, paste(report_date, "table_crude_incidence_rate.csv"), row.names = F)

# data for plots
data_all_countries = countries_data$data_all

# filter by start date and end date
if (!is.null(start_date)){
  start_date_converted = as.Date(ISOdate(year = start_date[3], month = start_date[1], day = start_date[2]))
  data_all_countries = data_all_countries[data_all_countries$date > start_date_converted, ]
}
if (!is.null(end_date)){
  end_date_converted = as.Date(ISOdate(year = end_date[3], month = end_date[1], day = end_date[2]))
  data_all_countries = data_all_countries[data_all_countries$date < end_date_converted, ]
}
# x label break for all plots:
x_min = min(data_all_countries$Date)
x_max = max(data_all_countries$Date)
if (as.numeric(x_max - x_min)%%3 == 2){
	break.vec <- c(x_min, seq( as.numeric(x_max - x_min)%%3+x_min, x_max, by = "3 days"))
}else{
	break.vec <- c(x_min, seq( as.numeric(x_max - x_min)%%3+3+x_min, x_max, by = "3 days"))
}

#  specify country_filter
if (template_input){
  country_list = read.csv("input_country_list.csv", stringsAsFactors = F)
  filter_total <- filter_incremental <- country_list$Countries
}else{
  temp = data_all_countries
  if (remove_mainland_china) temp = temp[!temp$Country %in% china_label, ]
  temp=temp[temp$Date==max(data_all_countries$Date),] 
  # filter by total confired
  temp_total=temp[order(temp$Confirmed, decreasing = T), ]
  filter_total = temp_total$Country[1:top_n]
  temp_incremental=temp[order(temp$Confirmed_incremental, decreasing = T), ]
  filter_incremental = temp_incremental$Country[1:top_n]
}

# plot 1. total confirmed cases sort by countries cumulative

# filter by country and cumulative confirmed
data_to_plot_confirmed = data_all_countries[data_all_countries$Country %in% filter_total , ]
# reorder factor levels by country filter order
temp = data_to_plot_confirmed[data_to_plot_confirmed$Date == max(data_to_plot_confirmed$Date),]
temp = temp[order(temp$Confirmed,decreasing = T),]
country_order = temp$Country
data_to_plot_confirmed$Country <- factor(data_to_plot_confirmed$Country, levels = country_order)

y_max=(round(max(data_to_plot_confirmed$Confirmed)/1000)+1)*1000
y_interval = adjust_y_interval(y_max)
p1 = ggplot(data_to_plot_confirmed , aes(x=Date, y=Confirmed, group=Country, colour = Country,  shape = Country)) + 
  geom_point() + 
  geom_line() +
  theme_bw() + 
  # theme(plot.title = element_text(size = 20)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
	theme(legend.position = c(0.15, 0.8)) + 
	theme(legend.title = element_text(size=25), legend.text = element_text(size = 21)) +
  scale_y_continuous(breaks=seq(0,y_max, y_interval)) + 
  scale_x_date(breaks = break.vec, date_labels = "%Y-%m-%d") +
  #ggtitle(p1_title) + 
  xlab("") +
  ylab(p1_ylab)

ggsave(filename=paste(report_date,"p1",p1_title, ".pdf"), plot = p1, width = 10, height = 8 )


# plot 1-1. total confirmed cases sort by countries cumulative (including China)
filter_total_with_china = c(china_label, filter_total)

# filter by country and cumulative confirmed
data_to_plot_confirmed = data_all_countries[data_all_countries$Country %in% filter_total_with_china, ]
# reorder factor levels by country filter order
temp = data_to_plot_confirmed[data_to_plot_confirmed$Date == max(data_to_plot_confirmed$Date),]
temp = temp[order(temp$Confirmed,decreasing = T),]
country_order = temp$Country
data_to_plot_confirmed$Country <- factor(data_to_plot_confirmed$Country, levels = country_order)

y_max=(round(max(data_to_plot_confirmed$Confirmed)/1000)+1)*1000
y_interval = adjust_y_interval(y_max)
p1_1 = ggplot(data_to_plot_confirmed , aes(x=Date, y=Confirmed, group=Country, colour = Country,  shape = Country)) + 
  geom_point() + 
  geom_line() +
  theme_bw() + 
  # theme(plot.title = element_text(size = 20)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
	theme(legend.position = c(0.15, 0.8)) + 
	theme(legend.title = element_text(size=25), legend.text = element_text(size = 21)) +
  scale_y_continuous(breaks=seq(0,y_max, y_interval)) + 
  scale_x_date(breaks = break.vec, date_labels = "%Y-%m-%d") +
  #ggtitle(p1_title) +
  xlab("") +
  ylab(p1_ylab)

ggsave(filename=paste(report_date,"p1-1",p1_1_title, ".pdf"), plot = p1_1, width = 10, height = 8 )


# plot 1-2. total confirmed cases sort by countries cumulative (China Only)
filter_total_china = china_label

# filter by country and cumulative confirmed
data_to_plot_confirmed = data_all_countries[data_all_countries$Country %in% filter_total_china, ]
# reorder factor levels by country filter order
temp = data_to_plot_confirmed[data_to_plot_confirmed$Date == max(data_to_plot_confirmed$Date),]
temp = temp[order(temp$Confirmed,decreasing = T),]
country_order = temp$Country
data_to_plot_confirmed$Country <- factor(data_to_plot_confirmed$Country, levels = country_order)

y_max=(round(max(data_to_plot_confirmed$Confirmed)/1000)+1)*1000
y_interval = adjust_y_interval(y_max)
p1_2 = ggplot(data_to_plot_confirmed , aes(x=Date, y=Confirmed, group=Country, colour = Country,  shape = Country)) + 
  geom_point() + 
  geom_line() +
  theme_bw() + 
  # theme(plot.title = element_text(size = 20)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
	theme(legend.position = c(0.15, 0.8)) +	
	theme(legend.title = element_text(size=25), legend.text = element_text(size = 21)) +
  scale_y_continuous(breaks=seq(0,y_max, y_interval)) + 
  scale_x_date(breaks = break.vec, date_labels = "%Y-%m-%d") +
  #ggtitle(p1_title) +
  xlab("") +
  ylab(p1_ylab)

ggsave(filename=paste(report_date,"p1-2",p1_2_title, ".pdf"), plot = p1_2, width = 10, height = 8 )


# plot 2. incremental cases for top N total confirmed

# filter by country and cumulative confirmed
data_to_plot_confirmed = data_all_countries[data_all_countries$Country %in% filter_total , ]

# reorder factor levels by country filter order
temp = data_to_plot_confirmed[data_to_plot_confirmed$Date == max(data_to_plot_confirmed$Date),]
temp = temp[order(temp$Confirmed_incremental,decreasing = T),]
country_order = temp$Country
data_to_plot_confirmed$Country <- factor(data_to_plot_confirmed$Country, levels = country_order)

y_max=(round(max(data_to_plot_confirmed$Confirmed_incremental)/1000)+1)*1000
y_interval = adjust_y_interval(y_max)
p2 = ggplot(data_to_plot_confirmed , aes(x=Date, y=Confirmed_incremental, group=Country, colour = Country,  shape = Country)) + 
  geom_point() + 
  geom_line() +
  theme_bw() + 
  # theme(plot.title = element_text(size = 20)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size=25), legend.text = element_text(size = 21)) +
  scale_y_continuous(breaks=seq(0,y_max, y_interval)) + 
  scale_x_date(breaks = break.vec, date_labels = "%Y-%m-%d") +
  # ggtitle(p2_title) +
  xlab("") +
  ylab(p2_ylab)

ggsave(filename=paste(report_date,"p2",p2_title, ".pdf"), plot = p2, width = 10, height = 8 )



# plot 3. new confirmed cases daily sort by countries incremental
# filter by country total and date
data_to_plot_confirmed_increment = data_all_countries[data_all_countries$Country %in% filter_incremental ,]
# reorder factor levels by country filter order
temp = data_to_plot_confirmed_increment[data_to_plot_confirmed_increment$Date == max(data_to_plot_confirmed_increment$Date),]
temp = temp[order(temp$Confirmed_incremental,decreasing = T),]
country_order = temp$Country
data_to_plot_confirmed_increment$Country <- factor(data_to_plot_confirmed_increment$Country, levels = country_order)

y_max=(round(max(data_to_plot_confirmed_increment$Confirmed_incremental)/1000)+1)*1000
y_interval = adjust_y_interval(y_max)
p3 = ggplot(data_to_plot_confirmed_increment , aes(x=Date, y=Confirmed_incremental, group=Country, colour = Country,  shape = Country)) + 
  geom_point() + 
  geom_line() +
  theme_bw() + 
  # theme(plot.title = element_text(size = 20)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size=25), legend.text = element_text(size = 21)) +
  scale_y_continuous(breaks=seq(0,y_max, y_interval)) + 
  scale_x_date(breaks = break.vec, date_labels = "%Y-%m-%d") +
  # ggtitle(p2_title) +
  xlab("") +
  ylab(p3_ylab)

ggsave(filename=paste(report_date,"p3",p3_title, ".pdf"), plot = p3, width = 10, height = 8 )



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
  geom_point() + 
  geom_line() +
  theme_bw() + 
  # theme(plot.title = element_text(size = 20)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size=25), legend.text = element_text(size = 21)) +
  scale_y_continuous(breaks=seq(0,y_max, y_interval)) + 
  scale_x_date(breaks = break.vec, date_labels = "%Y-%m-%d") +
  # ggtitle(p2_title) +
  xlab("") +
  ylab(p3_ylab)

ggsave(filename=paste(report_date,"p3_1",p3_1_title, ".pdf"), plot = p3_1, width = 10, height = 8 )




# plot 3-2. new confirmed cases daily sort by countries incremental (China Only)
filter_incremental_china = china_label
# filter by country total and date
data_to_plot_confirmed_increment = data_all_countries[data_all_countries$Country %in% filter_incremental_china,]
# reorder factor levels by country filter order
temp = data_to_plot_confirmed_increment[data_to_plot_confirmed_increment$Date == max(data_to_plot_confirmed_increment$Date),]
temp = temp[order(temp$Confirmed_incremental,decreasing = T),]
country_order = temp$Country
data_to_plot_confirmed_increment$Country <- factor(data_to_plot_confirmed_increment$Country, levels = country_order)

y_max=(round(max(data_to_plot_confirmed_increment$Confirmed_incremental)/1000)+1)*1000
y_interval = adjust_y_interval(y_max)
p3_2 = ggplot(data_to_plot_confirmed_increment , aes(x=Date, y=Confirmed_incremental, group=Country, colour = Country,  shape = Country)) + 
  geom_point() + 
  geom_line() +
  theme_bw() + 
  # theme(plot.title = element_text(size = 20)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size=25), legend.text = element_text(size = 21)) +
  scale_y_continuous(breaks=seq(0,y_max, y_interval)) + 
  scale_x_date(breaks = break.vec, date_labels = "%Y-%m-%d") +
  # ggtitle(p2_title) +
  xlab("") +
  ylab(p3_ylab)

ggsave(filename=paste(report_date,"p3_2",p3_2_title, ".pdf"), plot = p3_2, width = 10, height = 8 )



# plot 7-1 crude incidence rate VS Hubei
# filter by country and cumulative confirmed

Hubei_data_plot = Hubei_data$data_all
names(Hubei_data_plot)[1] = "Region"

data_to_plot_IR = data_all_countries[data_all_countries$Country %in% filter_total_with_china, ]
names(data_to_plot_IR)[1] = "Region"

data_to_plot_IR = rbind(Hubei_data_plot, data_to_plot_IR)

# reorder factor levels by Crude_Incidence_Rate
temp = data_to_plot_IR[data_to_plot_IR$Date == max(data_to_plot_IR$Date),]
temp = temp[order(temp$Crude_Incidence_Rate,decreasing = T),]
data_to_plot_IR$Region <- factor(data_to_plot_IR$Region, levels = temp$Region)

y_max=(round(max(data_to_plot_IR$Crude_Incidence_Rate)/10)+1)*10
y_interval = adjust_y_interval(y_max)

p7_1 = ggplot(data_to_plot_IR , aes(x=Date, y=Crude_Incidence_Rate, group=Region, colour = Region,  shape = Region)) + 
  geom_point() + 
  geom_line() +
	scale_shape_manual(values=1:nlevels(data_to_plot_IR$Region)) +
  theme_bw() + 
  # theme(plot.title = element_text(size = 20)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
	theme(legend.position = c(0.15, 0.75)) +	
	theme(legend.title = element_text(size=25), legend.text = element_text(size = 21)) +
  scale_y_continuous(breaks=seq(0,y_max, y_interval)) + 
  scale_x_date(breaks = break.vec, date_labels = "%Y-%m-%d") +
  #ggtitle(p1_title) +
  xlab("") +
  ylab(p7_ylab)

ggsave(filename=paste(report_date,"p7-1",p7_1_title, ".pdf"), plot = p7_1, width = 10, height = 8 )



##############################
## US Active Deaths Recovered
##############################

### US Active Deaths Recovered Table
case_confirmed_wide= countries_data$case_confirmed_wide
case_deaths_wide = countries_data$case_deaths_wide

US_Confirmed = case_confirmed_wide[rownames(case_confirmed_wide) == "US",]
US_Deaths = case_deaths_wide[rownames(case_deaths_wide) == "US",]
# US_Recoverd = case_recovered_wide[rownames(case_recovered_wide) == "US",]
# US_Active = US_Confirmed - US_Deaths - US_Recoverd
# US_tbl = rbind(US_Active, US_Deaths, US_Recoverd)
US_tbl = rbind(US_Confirmed, US_Deaths)
rownames(US_tbl) = c("Total Confirmed"， "Deaths")
# reverse column order 
US_tbl = US_tbl[, ncol(US_tbl):1]
write.csv(US_tbl, paste(report_date,"table_active_deaths_recovered_US.csv"))
	
# plot 4: US only Active Deaths Recovered
temp = data_all_countries[data_all_countries$Country =='US' , c("Country", "Date", "Confirmed", "Deaths")]
data_to_plot_us = reshape(data=temp, 
													idvar=c("Country", "Date"),
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
  geom_point() + 
  geom_line() +
  theme_bw() + 
  # theme(plot.title = element_text(size = 20)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size=25), legend.text = element_text(size = 21)) +
  scale_y_continuous(breaks=seq(0,y_max, y_interval)) + 
  scale_x_date(breaks = break.vec, date_labels = "%Y-%m-%d") +
  # ggtitle(p4_title) +
  xlab("") +
  ylab(p4_ylab)

ggsave(filename=paste(report_date,"p4",p4_title, ".pdf"), plot = p4, width = 10, height = 8 )



##############################
## US by states
##############################

# get data
state_detail_url = 'https://covidtracking.com/api/states/daily'
r = GET(state_detail_url)
if (r$status != 200) stop(paste("BAD API RETURN!"))
data_us_states_orig = fromJSON(paste(rawToChar(r$content), collapse=""))
state_population = read.csv("input_state_population.csv", stringsAsFactors=F)

convert_date_us=function(date_label){
  year =unlist(lapply(as.character(date_label), function(X) substr(X,1,4) ))
	month =unlist(lapply(as.character(date_label), function(X) substr(X,5,6) ))
	day =unlist(lapply(as.character(date_label), function(X) substr(X,7,8) ))
  
  as.Date(ISOdate(year = year, month = month, day = day))
}

data_us_states = data_us_states_orig
data_us_states$date = convert_date_us(data_us_states$date)
# data_us_states = data_us_states[order(data_us_states$date),]
data_us_states[is.na(data_us_states)] = 0
data_us_states$population = state_population$Population[match(data_us_states$state, state_population$State)]
data_us_states$crude_incidence_rate = as.numeric(data_us_states$positive)/as.numeric(data_us_states$population) * 100000
report_date = as.character( max(data_us_states$date))

### time series table - reshape from long to wide example
data_us_states_confirmed = data_us_states[, c("date", "state", "positive")]
case_confirmed_wide <- reshape(data=data_us_states_confirmed,idvar="state",
                          v.names = "positive",
                          timevar = "date",
                          direction="wide")
case_confirmed_wide[is.na(case_confirmed_wide)] = 0
													
data_us_states_incremental = data_us_states[, c("date", "state", "positiveIncrease")]
case_confirmed_incremental_wide <- reshape(data=data_us_states_incremental,idvar="state",
                          v.names = "positiveIncrease",
                          timevar = "date",
                          direction="wide")
case_confirmed_incremental_wide[is.na(case_confirmed_incremental_wide)] = 0
													
write.csv(case_confirmed_wide, paste(report_date,"table_case_confirmed_US.csv"), row.names = F)
write.csv(case_confirmed_incremental_wide, paste(report_date,"table_case_confirmed_incremental_US.csv" ), row.names = F)

### crude_incidence_rate
data_latest = data_us_states[data_us_states$date == report_date,c( "date", "state", "positive", "crude_incidence_rate", "death", "hospitalized", "totalTestResults", "deathIncrease", "hospitalizedIncrease", "negativeIncrease")]
data_latest = data_latest[order(data_latest$positive, decreasing = T),]
write.csv(data_latest, paste(report_date,"table_latest_cases_and_incidence_rate_US.csv"), row.names = F)

# data for plots

# filter by date
if (!is.null(start_date)){
  start_date_converted = as.Date(ISOdate(year = start_date[3], month = start_date[1], day = start_date[2]))
  data_us_states = data_us_states[data_us_states$Date > start_date_converted, ]
}
if (!is.null(end_date)){
  end_date_converted = as.Date(ISOdate(year = end_date[3], month = end_date[1], day = end_date[2]))
  data_us_states = data_us_states[data_us_states$Date < end_date_converted, ]
}

# filter by date
if (template_input){
  state_list = read.csv("input_us_state_list.csv", stringsAsFactors = F)
  filter_total <- filter_incremental <- state_list$States
}else{
  temp = data_us_states
  temp=temp[temp$date==max(data_us_states$date),] 
  temp_total=temp[order(temp$positive, decreasing = T), ]
  filter_total = temp_total$state[1:top_n]
  temp_incremental=temp[order(temp$positiveIncrease, decreasing = T), ]
  filter_incremental = temp_incremental$state[1:top_n]
}


# x label break for all plots:
x_min = min(data_us_states$date)
x_max = max(data_us_states$date)
if (as.numeric(x_max - x_min)%%3 == 2){
	break.vec <- c(x_min, seq( as.numeric(x_max - x_min)%%3+x_min, x_max, by = "3 days"))
}else{
	break.vec <- c(x_min, seq( as.numeric(x_max - x_min)%%3+3+x_min, x_max, by = "3 days"))
}

# plot 5: total confirmed cases by US States sort by Cumulative 
data_to_plot = data_us_states[data_us_states$state %in% filter_total, ]

# reorder factor levels by country filter order
temp = data_to_plot[data_to_plot$date == max(data_to_plot$date),]
temp = temp[order(temp$positive,decreasing = T),]
state_order = temp$state
data_to_plot$state <- factor(data_to_plot$state, levels = state_order)

y_max=(round(max(data_to_plot$positive)/500)+1)*500
y_interval = adjust_y_interval(y_max)
p5 = ggplot(data_to_plot , aes(x=date, y=positive, group=state, colour = state,  shape = state)) + 
  geom_point() + 
  geom_line() +
  theme_bw() + 
  # theme(plot.title = element_text(size = 20)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size=25), legend.text = element_text(size = 21)) +
  scale_y_continuous(breaks=seq(0,y_max, y_interval)) + 
  scale_x_date(breaks = break.vec, date_labels = "%Y-%m-%d") +
  # ggtitle(p5_title) +
  xlab("") +
  ylab(p5_ylab)

ggsave(filename=paste(report_date,"p5",p5_title, ".pdf"), plot = p5, width = 10, height = 8 )


# plot 6: new confirmed cases daily by US States sort by incremental
data_to_plot_incremental = data_us_states[data_us_states$state %in% filter_incremental , ]

# reorder factor levels by country filter order
temp = data_to_plot_incremental[data_to_plot_incremental$date == max(data_to_plot_incremental$date),]
temp = temp[order(temp$positiveIncrease,decreasing = T),]
state_order = temp$state
data_to_plot_incremental$state <- factor(data_to_plot_incremental$state, levels = state_order)

y_max=(round(max(data_to_plot_incremental$positiveIncrease)/100)+1)*100
y_interval = adjust_y_interval(y_max)
p6 = ggplot(data_to_plot_incremental , aes(x=date, y=positiveIncrease, group=state, colour = state,  shape = state)) + 
  geom_point() + 
  geom_line() +
  theme_bw() + 
  # theme(plot.title = element_text(size = 20)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18)) + 
  theme(axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 18)) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(legend.title = element_text(size=25), legend.text = element_text(size = 21)) +
  scale_y_continuous(breaks=seq(0,y_max, y_interval)) + 
  scale_x_date(breaks = break.vec, date_labels = "%Y-%m-%d") +
  # ggtitle(p6_title) +
  xlab("") +
  ylab(p6_ylab)

ggsave(filename=paste(report_date,"p6",p6_title, ".pdf"), plot = p6, width = 10, height = 8 )




																																																			