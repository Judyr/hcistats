### File to generate the data used in the examples. See also: Data_overview.docx

library(xtable)

### Libraries:
library(digest)		# for MD5 hashes for userID's
library(psych)		# for simulations
library(plyr)

### Data generating functions
source("functions.R")

### Set the seed for replication
set.seed(11)

# Settings
numberOfSalesPersons <- 200


# Generate the UserID's
uid <- generateUIDs(numberOfSalesPersons)


# assign to Teams
base <- assignTeam(c("Team A", "Team B"), data.frame(uid))
names(base) <- c("ID", "Team")
xtable(head(base))

save(base, file="Base.RData")


# generate monthly scale data 
timepoints <- 3  # time in months
set.seed(11)
scale <- monthlySUSScaleData(base[base$Team=="Team B",]$ID, timepoints)  

xtable(head(scale))
save(scale, file="SuSScaleData.RData")


# generate response time (average) to customer emails ()
email <- emailResponseTime(base, timepoints)

xtable(head(email))
save(email, file="EmailResponseData.RData")


# Please do add your own! We will work it into the description
# Or, contact us with your exact needs at maurits \@ mauritskaptein [dot] com





