library(data.table)

set.seed(4261998)

# we are going to procedurally read in every csv in the raw directory.
# first we must produce a list of all of the files in the directory,
# obtained recursively.
filelist = list.files("project/volume/data/raw/", recursive = T)
# to see if it worked correctly:
head(filelist, n=5)

# we will loop over every file in the list, extract the file name
# without the file extension
for (file in filelist) {
  print(paste0("Reading ", file))
  assign(strsplit(tail(strsplit(file, "/")[[1]], n=1), # xxx.csv
                  ".csv")[[1]], # xxx
         fread(paste0("project/volume/data/raw/",
                      file)
         )
  )
}
rm("file", "filelist")

# preprocessing
strsplit(example_sub$id, "_")
unlist(strsplit(example_sub$id, "_"))
  
examp_sub <- data.table(matrix(unlist(strsplit(example_sub$id, "_")), ncol = 4, byrow = TRUE))
head(examp_sub, n=5)
setnames(examp_sub, c("V1", "V2", "V3", "V4"), c("season", "daynum", "team1", "team2"))
head(examp_sub, n=5)
examp_sub[, result := 0.5]
head(examp_sub, n=5)

# make train
train <- rbind(RegularSeasonDetailedResults, NCAATourneyDetailedResults)
train
train <- train[,.(WTeamID, LTeamID, Season, DayNum)]
setnames(train,c("WTeamID","LTeamID"),c("team_1","team_2"))
train

train$result <- 1

# make master data file

master <- rbind(train,examp_sub)
master$team_1 <- as.character(master$team_1)
master$team_2 <- as.character(master$team_2)

MasseyOrdinals_thru_2019_day_128

MasseyOrdinals_thru_2019_day_128$DayNum <- MasseyOrdinals_thru_2019_day_128$RankingDayNum + 1
MasseyOrdinals_thru_2019_day_128
pom_ranks <- MasseyOrdinals_thru_2019_day_128[SystemName == "POM", .(Season, DayNum, TeamID, OrdinalRank)]
pom_ranks
setnames(pom_ranks, "TeamID", "team_1")

pom_ranks
pom_ranks$team_1 <- as.character(pom_ranks$team_1)

setkey(master, Season,team_1, DayNum)
setkey(pom_ranks, Season, team_1, DayNum)

master
pom_ranks

master <- pom_ranks[master, roll = T]
master
setnames(master,"OrdinalRank", "team_1_POM")


setnames(pom_ranks,"team_1","team_2")
setkey(master,Season,team_2,DayNum)
setkey(pom_ranks,Season,team_2,DayNum)

master <- pom_ranks[master,roll = T]

setnames(master,"OrdinalRank","team_2_POM")

master
master[, POM_dif := team_2_POM - team_1_POM]
master
master <- master[order(Season, DayNum)]

master <- master[,.(team_1,team_2, POM_dif, result)]


master <- master[!is.na(master$POM_dif)]

examp_sub <- master[result == 0.5]
train <- master[result == 1]

rand_idx <- sample(1:nrow(train), nrow(train) * 0.5)
train_a <- train[rand_idx, ]
train_b <- train[!rand_idx, ]

train_b$result <- 0
train_b$POM_dif <- -1 * train_b$POM_dif

train <- rbind(train_a, train_b)

train
examp_sub
fwrite(examp_sub, "project/volume/data/interim/test.csv")
fwrite(train, "project/volume/data/interim/train.csv")
