data <- read.csv("C:/SelfLearn/Stats/CKME132/set1.csv", na.strings = c("","NA"), stringsAsFactors = F, header = F)
df <- data.frame(data)


# name columns
names(df) <- c('ID','Fname', 'Lname', 'Email', 'Gender', 'Country', 'Amount', 'Date')

# examine the contents
summary(df)

# How many different countries are out there?
unique(df$Country)

# count number of Females, Males and NA (2 versions are provided)
length(which(df$Gender == 'Female'))
sum(df$Gender == 'Female', na.rm = T)

length(which(df$Gender == 'Male'))
sum(df$Gender == 'Male', na.rm = T)

length(which(is.na(df$Gender)))
sum(is.na(df$Gender))

# Cleaning 
df <- df[!is.na(df$Country),]
df$Amount <- as.numeric(gsub("[$,]","", df$Amount))
df$Date <- as.Date(df$Date, "%m/%d/%Y")

# Create new data
df$days <- as.numeric(df$Date - min(df$Date))

# Identify people of interest
df$IndEmail <- 0 
df$IndEmail[grep("gov", df$Email)] <- 1
df$IndEmail[grep("org", df$Email)] <- 1

# Optional
lma <- lm(df$Amount ~ df$days + df$IndEmail)
summary(lma)
