library(tidyverse)
#2007-2019

#setwd("~/Documents/neonate-wonder")

#CDC Guidelines
#Do not present or publish statistics representing nine or fewer births or deaths, including rates based on counts of nine or fewer births or deaths, in figures, graphs, maps, tables, etc.


sizing_theme <- theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=12), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5)) 


panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())


readData <- function(filepath, gender_label){

	lines <- readLines(filepath)
	index <- which(lines == '"---"')

	if (length(index) > 0) {
	  lines <- lines[1:(index[1] - 1)]
	}

	list_of_vectors <- lapply(lines, function(x) unlist(strsplit(x, "\t")))
	df <- do.call(rbind, lapply(list_of_vectors, function(x) as.data.frame(t(x))))

	df <- df %>% as_tibble()

	df[,1:11] <- lapply(df[,1:11], function(x) gsub('\\"', "", x))

	colnames(df) <- df[1,]

	df <- df[-1,]

	df <- df %>% select(-Notes)

	df <- df %>% select(c(`Mother's Bridged Race`, `ICD-10 130 Cause List (Infants)`, `Age of Mother Code`, `Year of Death`, State, Deaths, Births)) %>% magrittr::set_colnames(c("race", "cause", "age_mother", "year", "state", "deaths", "births")) %>% mutate(gender = gender_label)

	df

}



files <- list.files(pattern = "*.txt", full.names = TRUE)
gender_vector <- lapply(files, function(x) if(grepl("female", x)) "female" else "male") %>% unlist()

data <- lapply(seq_along(files), function(x) readData(files[x], gender_vector[x]))

data <- do.call(rbind, data) %>% as_tibble()

#by year, gender
cbb <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")


byyear <- data %>% type.convert(as.is = TRUE) %>% group_by(race, year, gender) %>% summarize(n.death = sum(deaths), n.births = sum(births)) %>% mutate(rate = (n.death / n.births)*1000 ) %>% ungroup() 

fig1a <- byyear %>% ggplot(aes(x=year, y=rate, color = race, shape = gender)) + geom_point(size = 3) + geom_line(size = 1) + theme_minimal() + sizing_theme + panel_theme + ylim(0, 6) + scale_x_continuous(breaks=seq(2007,2019,1), labels= function(x) ifelse(x %% 2 == 1, x, "")) + ylab("Infant Mortality Rate Per 1000 Births") + xlab("Year") + scale_y_continuous(breaks = scales::pretty_breaks(n = 8), limits = c(0, 6)) + scale_color_manual(values = cbb[1:2])


excess_byyear <- byyear %>% group_by(year, gender) %>% summarize(excess_rate = rate[race!="White"] - rate[race=="White"], number_excess = excess_rate*(1/1000)*n.births[race!="White"])

fig1b <- excess_byyear %>% ggplot(aes(x=year, y=number_excess, color = gender)) + geom_point(size = 3) + geom_line(size = 1) + theme_minimal() + sizing_theme + panel_theme  + scale_x_continuous(breaks=seq(2007,2019,1), labels= function(x) ifelse(x %% 2 == 1, x, "")) + ylab("Excess Infant Deaths") + xlab("Year")  + scale_color_manual(values = cbb[3:4]) + scale_y_continuous(breaks = scales::pretty_breaks(n = 8), limits = c(0, 2500))


#by icd cause over the years
data %>% type.convert(as.is = TRUE) %>% group_by(race, year, gender, cause) %>% summarize(n.death = sum(deaths), n.births = sum(births)) %>% mutate(rate = (n.death / n.births)*1000 ) %>% ungroup() %>% ggplot(aes(x=year, y=rate, color = race, shape = gender)) + geom_point(size = 3) + geom_line(size = 1) + theme_minimal() + sizing_theme + panel_theme + ylim(0, 6) + scale_x_continuous(breaks=seq(2007,2019,1), labels= function(x) ifelse(x %% 2 == 1, x, "")) + ylab("Infant Mortality Rate Per 1000 Births") + xlab("Year") + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + scale_color_manual(values = cbb[1:2]) + facet_wrap(~cause, scales = "free")



data %>% type.convert(as.is = TRUE) %>% group_by(race, year, gender, cause) %>% summarize(n.death = sum(deaths), n.births = sum(births)) %>% mutate(rate = (n.death / n.births)*1000 ) %>% ungroup() %>% group_by(year, gender, cause) %>% summarize(excess_rate = rate[race!="White"] - rate[race=="White"], number_excess = excess_rate*(1/1000)*n.births[race!="White"]) %>% ggplot(aes(x=year, y=number_excess, color = gender)) + geom_point(size = 3) + geom_line(size = 1) + theme_minimal() + sizing_theme + panel_theme  + scale_x_continuous(breaks=seq(2007,2019,1), labels= function(x) ifelse(x %% 2 == 1, x, "")) + ylab("Excess Infant Deaths") + xlab("Year")  + scale_color_manual(values = cbb[3:4]) + facet_wrap(~cause, scales = "free")