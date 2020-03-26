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
                      file) # xxx = read data
         )
  )
}
rm("file", "filelist")

# preprocessing
strsplit(example_sub$id, "_")
unlist(strsplit(example_sub$id, "_"))
  
examp_sub <- data.table(matrix(as.numeric(unlist(strsplit(example_sub$id, "_"))), ncol = 4, byrow = TRUE))
head(examp_sub, n=5)
setnames(examp_sub,
         c("V1", "V2", "V3", "V4"),
         c("Season", "DayNum", "Team1", "Team2"))
head(examp_sub, n=5)
examp_sub[, result := 0.5]
head(examp_sub, n=5)

# make train
train <- rbind(season, tourney)
head(train, n=5)
train <- train[, .(WTeamID, LTeamID, Season, DayNum)]
setnames(train, c("WTeamID","LTeamID"), c("Team1","Team2"))
head(train, n=5)

train$result <- 1
train = train[, c(3,1,2,4,5)] # reorder columns so season is first
head(train, n=5)

# make master data
master <- rbind(train,examp_sub)
head(master)
master[,Team1:=as.character(Team1)]
master[,Team2:=as.character(Team2)]
head(master)

head(massey, n=5)

massey$DayNum <- massey$RankingDayNum + 1
head(massey, n=5)
pom_ranks <- massey[SystemName == "POM", .(Season, DayNum, TeamID, OrdinalRank)]
head(pom_ranks, n=5)
setnames(pom_ranks, "TeamID", "Team1")
head(pom_ranks)
pom_ranks[,Team1:=as.character(Team1)]
head(pom_ranks)

pom_ranks = pom_ranks[,c(1,3,2,4)]

setkey(master, Season, Team1, DayNum)
setkey(pom_ranks, Season, Team1, DayNum)

head(master)
head(pom_ranks)

master <- pom_ranks[master, roll = T]
head(master)
setnames(master,"OrdinalRank", "team_1_POM")

setnames(pom_ranks,"Team1","Team2")
setkey(master, Season, Team2, DayNum)
setkey(pom_ranks, Season, Team2, DayNum)

master <- pom_ranks[master, roll = T]
head(master)

setnames(master, "OrdinalRank", "team_2_POM")

master
master[, POM_dif := team_2_POM - team_1_POM]
master
master = na.omit(master, cols=c("team_1_POM", "team_2_POM", "POM_dif"), invert=FALSE)
master
master[is.na(POM_dif)==T, .N]
master <- master[order(Season, DayNum)]
master

master <- master[, .(Season, Team1, Team2, POM_dif, result)]
master

examp_sub <- master[result==0.5]
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
