library(readr)
library(dplyr)
library(stringr)
dat <- read_csv(file = "NFL Play by Play 2009-2016 (v3).csv")

dat$desc <- tolower(dat$desc)

#new_dat <- dat %>% select(c(Date:DefTwoPoint, PlayType,Timeout_Team, Timeout_Indicator))
new_dat <- dat %>% select(c("Date", "GameID", "posteam","DefensiveTeam","desc", "PlayType"))
new_dat <- new_dat %>% mutate(icing = 0)
new_dat <- new_dat %>% filter(grepl("Field Goal|Timeout", PlayType))
################# Filling in team names ########################

for(i in 1:dim(new_dat)[1]) {
	if(is.na(new_dat$DefensiveTeam[i])) {
		new_dat$DefensiveTeam[i] <- new_dat$DefensiveTeam[i-1]
	}
	
	if(is.na(new_dat$posteam[i])) {
		new_dat$posteam[i] <- new_dat$posteam[i-1]
	}
}

####################################################################

fieldgoal <- new_dat %>% filter(grepl("Field Goal", PlayType))

fieldgoal_timeout <- new_dat %>% filter(grepl("Field Goal|Timeout", PlayType))

timeout <- new_dat %>% filter(grepl("Timeout", PlayType))


############# Matching Time format
pt <- "([0-9]*)\\:([0-9]*)"
fieldgoal_time <- fieldgoal %>% select(desc) %>% str_match_all(pattern = pt)
fieldgoal_time <- fieldgoal_time[[1]][,1]

pt <- "^\\:[0-9]+"
cleaning_time <- fieldgoal_time %>% str_match_all(pt)

pt <- "[1-9]{1}\\:[0-9]+"
cleaning_time2 <- fieldgoal_time %>% str_match_all(pt)
# 997 990

a <- vector()
for(i in 1:length(cleaning_time)) {
	if(length(cleaning_time[[i]]) == 1) {
		cleaning_time[[i]] <- paste("00", cleaning_time[[i]], sep = "")
		a <- append(a, i)
	}
}


for(i in a) {
	fieldgoal_time[i] <- cleaning_time[[i]]
}

b <- vector()

for(i in 1:length(cleaning_time2)) {
	if(length(cleaning_time2[[i]]) == 1){
		cleaning_time2[[i]] <- paste("0", cleaning_time2[[i]], sep = "")
		b <- append(b, i)
	}
}

for(i in b) {
	fieldgoal_time[i] <- cleaning_time2[[i]]
}

rm(a)
rm(b)
rm(cleaning_time)
rm(cleaning_time2)
a <- vector()
for(i in 1:dim(fieldgoal_timeout)[1]) {
	if(str_detect(fieldgoal_timeout$desc[i], "field goal")) {
		a <- append(a, i)
	}
}
j = 1
pt <- "[0-9]*\\:[0-9]+"
for(i in a){
	fieldgoal_timeout$desc[i] <- str_replace(fieldgoal_timeout$desc[i],
																					 pt, fieldgoal_time[j])
	j = j + 1
}

rm(a)
rm(j)
rm(i)
################# End of formatting time ######################

fieldgoal_timeout <- fieldgoal_timeout[-22289,]

pt = "([0-9]*)\\:([0-9]*)"
for(i in 1:dim(fieldgoal_timeout)[1]) {
	if(str_detect(fieldgoal_timeout$desc[i], "timeout") && 
		 str_detect(fieldgoal_timeout$desc[i+1], "field goal")){
		if(str_match_all(fieldgoal_timeout$desc[i], pt)[1] %in% 
			 str_match_all(fieldgoal_timeout$desc[i+1], pt)[1]){
			fieldgoal_timeout$icing[i+1] <- 1
		}
	}
}


