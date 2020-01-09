#### Preparing data
#### Diego Mazzotti
#### January 2020

library(dplyr)

# Load original data
data <- read.csv("data/Data_symptoms.csv", stringsAsFactors = F)

# Apply filters 
data_osa <- data %>%
        filter(psg==1, osa==1) %>% # (only psg==1, only osa==1)
        select(-psg, -osa, -s11)
        
# Fix snoring questions
data_osa$s17[data_osa$s17==9] <- NA # set as missing
data_osa$s17[data_osa$s17==3] <- 2 # set as doesn't bother (2)

# Convert to factor and name levels
data_osa_factors <- data_osa
data_osa_factors <- as.data.frame(sapply(data_osa_factors, as.character))

for (c in c(2:13, 15)) {
        
        levels(data_osa_factors[,c]) <- c("No", "Yes")
        
}

levels(data_osa_factors$s17) <- c("No", "Yes and bothers", "Yes and doesn't bother")
data_osa_factors$s17 <- factor(data_osa_factors$s17, levels =  c("No", "Yes and doesn't bother", "Yes and bothers"))

levels(data_osa_factors$s19) <- c("0 to =5", ">5 to =10", ">10 to =15", ">15")

# Check for missing 
data_osa_factors$nmiss <- rowSums(is.na(data_osa_factors))

# Select only with less than 5 missing
data_osa_factors <- data_osa_factors%>%
        filter(nmiss<5) %>%
        select(-nmiss)

# Save data ready to analyze
write.csv(data_osa_factors, file = "data/data_ready.csv", row.names = F)



