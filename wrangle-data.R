

#loading the file into R
research_funding_rates <- read.csv("data/research_funding_rates.csv") 

save(research_funding_rates, file = "rdas/research_funding_rates.rda")

# loading file in to R
geHTdata <- read.csv("data/geHTdata.csv")
save(geHTdata, file = "rdas/geHTdata.rda")

# loading file in to R
protStruct <- read.csv("data/protStruct.csv")
save(protStruct, file = "rdas/protStruct.rda")

# loading file in to R
medrank <- read.csv("data/pain_medication_rank.csv")
save(medrank, file = "rdas/medrank.rda")

# loading file in to R
my_data <- read.csv("data/my_data.csv")
save(my_data, file = "rdas/my_data.rda")

# loading file in to R
reactionR <- read.csv("data/ReactionTime.csv")
save(reactionR, file = "rdas/reactionR.rda")

# loading file in to R
Data <- read.csv("data/instructor_rating.csv")
save(Data, file= "rdas/Data.rda")
