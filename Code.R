# Libraries required for the rest of the code is located here
if(!require(pbapply)) install.packages("pbapply", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repo = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repo = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repo = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repo = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repo = "http://cran.us.r-project.org")
if(!require(sjstats)) install.packages("sjstats", repos = "http://cran.us.r-project.org")
if(!require(mice)) install.packages("mice", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(RSNNS)) install.packages("RSNNS", repos = "http://cran.us.r-project.org")
if(!require(neuralnet)) install.packages("neuralnet", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

# Load dataframes
atp <- do.call(rbind, lapply(list.files(pattern = "atp_matches_201\\d.csv"), fread))

# Convert player name to character string and rename
dna <- dna %>% 
  mutate(Player, player = as.character(dna$Player)) %>%
  select(-Player)

# Check player names between atp and dna datasets, pull players who don't match then split out the names to find in main dataset
noNames <- atp %>%
  select(winner_name, loser_name) %>%
  gather(key = result, value = player) %>%
  group_by(player) %>%
  summarise(wins = sum(result == "winner_name"),
            games = n()) %>%
  right_join(dna, by = "player") %>%
  filter(is.na(games)) %>%
  separate_rows(player, sep = "\\ ") %>%
  pull(player)

# Check names in atp dataset
atp %>%
  select(winner_name, loser_name) %>%
  gather(key = result, value = player) %>%
  group_by(player) %>%
  filter(grepl(paste(noNames, collapse = "|"), player)) %>%
  summarise(count = n())

# Replace player names which don't match
dna$player <- dna$player %>%
  str_replace("-", " ") %>%
  str_replace("Stan Wawrinka", "Stanislas Wawrinka") %>%
  str_replace("Albert Ramos Vinolas", "Albert Ramos")

# Remove unused values
rm(noNames)

# Remove unnecessary variables & Convert factors
atp <- atp %>%
  select(-c(draw_size, match_num, winner_id, winner_seed, winner_entry, winner_ioc, loser_id, loser_seed, loser_entry, loser_ioc, 
            w_SvGms, l_SvGms, winner_ht, loser_ht, score)) %>%
  mutate_at(vars(surface, tourney_level, winner_hand, loser_hand, round, best_of), as.factor)

# Adjust factors with no values to unknown values
atp$surface[which(atp$surface == "")] <- "None"
atp$surface <- factor(atp$surface)
atp$winner_hand[which(atp$winner_hand == "")] <- "U"
atp$winner_hand <- factor(atp$winner_hand)
atp$loser_hand[which(atp$loser_hand == "")] <- "U"
atp$loser_hand <- factor(atp$loser_hand)

# Remove NA's
atp <- na.omit(atp)

# Set seed
set.seed(17)

# Using dataset, replace winner & losers with Player 0 or 1 by random. Record who won in another column
# Ensure all stats assigned to players are recorded by the correct column rows
# Remove Height, which has lots of na's
atp <- atp %>%
  mutate(winner = as.factor(sample(0:1, n(), replace = T))) %>%
  mutate(name.p0 = if_else(winner == 0, winner_name, loser_name),
         name.p1 = if_else(winner == 0, loser_name, winner_name),
         age.p0 = if_else(winner == 0, winner_age, loser_age),
         age.p1 = if_else(winner == 0, loser_age, winner_age),
         hand.p0 = if_else(winner == 0, winner_hand, loser_hand),
         hand.p1 = if_else(winner == 0, loser_hand, winner_hand),
         aceRate.p0 = if_else(winner == 0, w_ace/w_svpt, l_ace/l_svpt),
         aceRate.p1 = if_else(winner == 0, l_ace/l_svpt, w_ace/w_svpt),
         dfRate.p0 = if_else(winner == 0, w_df/w_svpt, l_df/l_svpt),
         dfRate.p1 = if_else(winner == 0, l_df/l_svpt, w_df/w_svpt),
         fSrvInRate.p0 = if_else(winner == 0, w_1stIn/w_svpt, l_1stIn/l_svpt),
         fSrvInRate.p1 = if_else(winner == 0, l_1stIn/l_svpt, w_1stIn/w_svpt),
         fSrvWonRate.p0 = if_else(winner == 0, w_1stWon/w_1stIn, l_1stWon/l_1stIn),
         fSrvWonRate.p1 = if_else(winner == 0, l_1stWon/l_1stIn, w_1stWon/w_1stIn),
         SSrvWonRate.p0 = if_else(winner == 0, w_2ndWon/(w_svpt-w_1stIn), l_2ndWon/(l_svpt-l_1stIn)),
         SSrvWonRate.p1 = if_else(winner == 0, l_2ndWon/(l_svpt-l_1stIn), w_2ndWon/(w_svpt-w_1stIn)),
         bpSavedRate.p0 = if_else(winner == 0, w_bpSaved/w_bpFaced, l_bpSaved/l_bpFaced),
         bpSavedRate.p1 = if_else(winner == 0, l_bpSaved/l_bpSaved, w_bpSaved/w_bpSaved),
         rankPts.p0 = if_else(winner == 0, winner_rank_points, loser_rank_points),
         rankPts.p1 = if_else(winner == 0, loser_rank_points, winner_rank_points),
         rank.p0 = if_else(winner == 0, winner_rank, loser_rank),
         rank.p1 = if_else(winner == 0, loser_rank, winner_rank)) %>%
  select(-c(winner_name, winner_hand, winner_age, 
            loser_name, loser_hand, loser_age,
            w_ace, w_df, w_svpt, w_1stIn, w_1stWon, w_2ndWon, w_bpSaved, w_bpFaced, winner_rank_points, winner_rank,
            l_ace, l_df, l_svpt, l_1stIn, l_1stWon, l_2ndWon, l_bpSaved, l_bpFaced, loser_rank_points, loser_rank))

# Court type win rate
# Games, wins, win% by court type dataframe
courtWinRate <- atp %>%
  select(name.p0, name.p1, surface, tourney_date, winner) %>%
  gather(key = label, value = name, -c(surface, tourney_date, winner)) %>%
  mutate(win = str_extract(label, '\\d') == winner)

# Create new variables giving players winning % for the last 3 months and winning percentage on court surface for the last 3 years 
atp <- atp %>%
  rowwise() %>%
  mutate(surfaceWinRate.p0 = sum((courtWinRate$name == name.p0) & 
                                   (surface == courtWinRate$surface) & 
                                   ((courtWinRate$tourney_date < tourney_date) & (courtWinRate$tourney_date + 30000 > tourney_date)) &
                                   (courtWinRate$win == TRUE)) /
           sum((courtWinRate$name == name.p0) & 
                 (surface == courtWinRate$surface) & 
                 ((courtWinRate$tourney_date < tourney_date) & (courtWinRate$tourney_date + 30000 > tourney_date))), 
         surfaceWinRate.p1 = sum((courtWinRate$name == name.p1) & 
                                   (surface == courtWinRate$surface) & 
                                   ((courtWinRate$tourney_date < tourney_date) & (courtWinRate$tourney_date + 30000 > tourney_date)) &
                                   (courtWinRate$win == TRUE)) /
           sum((courtWinRate$name == name.p1) & 
                 (surface == courtWinRate$surface) & 
                 ((courtWinRate$tourney_date < tourney_date) & (courtWinRate$tourney_date + 30000 > tourney_date))),
         form.p0 = sum((courtWinRate$name == name.p0) &
                         (courtWinRate$win == TRUE) &
                         ((courtWinRate$tourney_date < tourney_date) & (courtWinRate$tourney_date + 3000 > tourney_date))) /
           sum((courtWinRate$name == name.p0) &
                 ((courtWinRate$tourney_date < tourney_date) & (courtWinRate$tourney_date + 3000 > tourney_date))),
         form.p1 = sum((courtWinRate$name == name.p1) &
                         (courtWinRate$win == TRUE) &
                         ((courtWinRate$tourney_date < tourney_date) & (courtWinRate$tourney_date + 3000 > tourney_date))) /
           sum((courtWinRate$name == name.p1) &
                 ((courtWinRate$tourney_date < tourney_date) & (courtWinRate$tourney_date + 3000 > tourney_date))))

# Create Fatigue Dataframe
fatigue <- atp %>%
  select(name.p0, name.p1, tourney_date, minutes) %>%
  gather(key = label, value = name, -c(tourney_date, minutes))

# Get fatigue for player from last month
atp <- atp %>%
  rowwise() %>%
  mutate(fatigue.p0 = sum(fatigue$minutes[((fatigue$name == name.p0) &
                                             ((fatigue$tourney_date < tourney_date) & (fatigue$tourney_date + 100 > tourney_date)))]),
         fatigue.p1 = sum(fatigue$minutes[((fatigue$name == name.p1) &
                                             ((fatigue$tourney_date < tourney_date) & (fatigue$tourney_date + 100 > tourney_date)))]))

# Head to Head Balance
h2h <- atp %>%
  select(name.p0, name.p1, winner, tourney_date) %>%
  mutate(win = str_extract("name.p0", '\\d') == winner)

# For every match find culmulative head to head to that date
# Negative values give p1 the advantage
atp <- atp %>% rowwise() %>%
  mutate(h2h = 2*(sum((((h2h$name.p0 == name.p0) & (h2h$name.p1 == name.p1)) &
                         (h2h$tourney_date < tourney_date) &
                         (h2h$winner == 0)) |
                        (((h2h$name.p0 == name.p1) & (h2h$name.p1 == name.p0)) &
                           (h2h$tourney_date < tourney_date) &
                           (h2h$winner == 1)))) -
           sum((((h2h$name.p0 == name.p0) & (h2h$name.p1 == name.p1)) |
                  ((h2h$name.p0 == name.p1) & (h2h$name.p1 == name.p0))) &
                 (h2h$tourney_date < tourney_date)))


# Stats Record dataframe
stats <- atp %>%
  select(name.p0, name.p1, tourney_date, aceRate.p0, aceRate.p1, dfRate.p0, dfRate.p1, 
         fSrvInRate.p0, fSrvInRate.p1, fSrvWonRate.p0, fSrvWonRate.p1,
         SSrvWonRate.p0, SSrvWonRate.p1, bpSavedRate.p0, bpSavedRate.p1) %>%
  gather(key = label, value = name, c(name.p0, name.p1)) %>%
  mutate(aceRate = if_else(str_extract(label, '\\d') == 0, aceRate.p0, aceRate.p1),
         dfRate = if_else(str_extract(label, '\\d') == 0, dfRate.p0, dfRate.p1),
         fSrvInRate = if_else(str_extract(label, '\\d') == 0, fSrvInRate.p0, fSrvInRate.p1),
         fSrvWonRate = if_else(str_extract(label, '\\d') == 0, fSrvWonRate.p0, fSrvWonRate.p1),
         SSrvWonRate = if_else(str_extract(label, '\\d') == 0, SSrvWonRate.p0, SSrvWonRate.p1),
         bpSavedRate = if_else(str_extract(label, '\\d') == 0, bpSavedRate.p0, bpSavedRate.p1)) %>%
  select(-c(aceRate.p0, aceRate.p1, dfRate.p0, dfRate.p1, fSrvInRate.p0, fSrvInRate.p1,
            fSrvWonRate.p0, fSrvWonRate.p1, SSrvWonRate.p0, SSrvWonRate.p1, bpSavedRate.p0, bpSavedRate.p1,
            label))

# Na's are from dividing by 0, replace with 0
stats[is.na(stats)] <- 0

# Add stats for the players from the last two year as variables
atp <- atp %>% rowwise() %>%
  mutate(aceRate.p0 = sum(stats$aceRate[((stats$name == name.p0) & 
                                           ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
           sum((stats$name == name.p0) &
                 ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         aceRate.p1 = sum(stats$aceRate[((stats$name == name.p1) & 
                                           ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
           sum((stats$name == name.p1) &
                 ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         dfRate.p0 = sum(stats$dfRate[((stats$name == name.p0) & 
                                         ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
           sum((stats$name == name.p0) &
                 ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         dfRate.p1 = sum(stats$dfRate[((stats$name == name.p1) & 
                                         ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
           sum((stats$name == name.p1) &
                 ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         fSrvInRate.p0 = sum(stats$fSrvInRate[((stats$name == name.p0) & 
                                                 ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
           sum((stats$name == name.p0) &
                 ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         fSrvInRate.p1 = sum(stats$fSrvInRate[((stats$name == name.p1) & 
                                                 ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
           sum((stats$name == name.p1) &
                 ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         fSrvWonRate.p0 = sum(stats$fSrvWonRate[((stats$name == name.p0) & 
                                                   ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
           sum((stats$name == name.p0) &
                 ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         fSrvWonRate.p1 = sum(stats$fSrvWonRate[((stats$name == name.p1) & 
                                                   ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
           sum((stats$name == name.p1) &
                 ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         SSrvWonRate.p0 = sum(stats$SSrvWonRate[((stats$name == name.p0) & 
                                                   ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
           sum((stats$name == name.p0) &
                 ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         SSrvWonRate.p1 = sum(stats$SSrvWonRate[((stats$name == name.p1) & 
                                                   ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
           sum((stats$name == name.p1) &
                 ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         bpSavedRate.p0 = sum(stats$bpSavedRate[((stats$name == name.p0) & 
                                                   ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
           sum((stats$name == name.p0) &
                 ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))),
         bpSavedRate.p1 = sum(stats$bpSavedRate[((stats$name == name.p1) & 
                                                   ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date)))]) /
           sum((stats$name == name.p1) &
                 ((stats$tourney_date < tourney_date) & (stats$tourney_date + 20000 > tourney_date))))

# Remove unneeded dataframes
rm(courtWinRate, fatigue, h2h, stats)
# Convert Na's to 0
atp[is.na(atp)] <- 0
# Remove variables not to be used as predictors
atp <- atp %>% select(-c(tourney_id, tourney_name, tourney_level, best_of, minutes,
                         round, name.p0, name.p1, hand.p0, hand.p1, surface))

# Use 2019 for validation set
validation <- atp %>%
  filter(tourney_date > 20190000)

# Use 2018 for test set
testing <- atp %>%
  filter(tourney_date > 20180000 & tourney_date < 20190000)

# Any matches before 2018 is inital training data
training <- atp %>%
  filter(tourney_date < 20180000)

# Model 1 - Baseline; if a player is has a lower rank predict win
baselineModel <- testing %>%
  select(winner, rank.p0, rank.p1) %>%
  filter(!is.na(rank.p0) & !is.na(rank.p1)) %>%
  mutate(baselinePred = as.integer(ifelse(rank.p0 < rank.p1, 0, 1))) %>%
  mutate(correct = ifelse(baselinePred == winner, 1, 0))

# Table of correct predictions
table(baselineModel$correct)
# Create dataframe to capture all model accuracies
accuracy <- data_frame(Method = "baseline", Accuracy = mean(baselineModel$correct))
# Present table
accuracy %>% knitr::kable()

# Model 2 - Logistic Regression
# Train model  
logitModel <- caret::train(winner ~ . -tourney_date,
                           data = training,
                           method = 'glm',
                           family = "binomial",
                           na.action = NULL)

summary(logitModel)
# All significant variables are match stats

# Predict on testing set
logitPred <- predict(logitModel, newdata = testing)

# Look at predicted data
summary(logitPred)

# Compare values
logitCorrect <- (logitPred == testing$winner)
table(logitCorrect)

# Work out accuracy
accuracy <- bind_rows(accuracy,
                      data_frame(Method = "Logistic Regression", 
                                 Accuracy = sum(logitCorrect) / length(logitCorrect)))
accuracy %>% knitr::kable()

table(logitPred == testing$winner, testing$winner)

# Model 2A - Logistic Regression remove more variables
# Train model  
logitModel2B <- caret::train(winner ~ . -tourney_date - dfRate.p0 - dfRate.p1
                             - fSrvInRate.p0 - fSrvInRate.p1 - fSrvWonRate.p0
                             - fSrvWonRate.p1 - SSrvWonRate.p0 - SSrvWonRate.p1
                             - bpSavedRate.p0 - bpSavedRate.p1,
                             data = training,
                             method = 'glm',
                             na.action = NULL)

summary(logitModel2B)

# Predict on testing set
logitPred2B <- predict(logitModel2B, newdata = testing)

summary(logitPred2B)
# Compare values
logitCorrect2B <- (logitPred2B == testing$winner)
table(logitCorrect2B)
# Work out accuracy
accuracy <- bind_rows(accuracy,
                      data_frame(Method = "Logistic Regression no serve Stats", 
                                 Accuracy = sum(logitCorrect2B) / length(logitCorrect2B)))
accuracy %>% knitr::kable()

Model 3 - CART Model
# Train model  
cartModel <- caret::train(winner ~ . -tourney_date - dfRate.p0 - dfRate.p1
                          - fSrvInRate.p0 - fSrvInRate.p1 - fSrvWonRate.p0
                          - fSrvWonRate.p1 - SSrvWonRate.p0 - SSrvWonRate.p1
                          - bpSavedRate.p0 - bpSavedRate.p1,
                          data = training,
                          method = 'rpart',
                          cp = 0.9,
                          minsplit = 5,
                          minbucket = 6)

plot(cartModel$finalModel)
text(cartModel$finalModel)

# Predict on testing set
cartPred <- predict(cartModel, newdata = testing)

summary(cartPred)
# Compare values
cartCorrect <- (cartPred == testing$winner)
table(cartCorrect)

# Work out accuracy
accuracy <- bind_rows(accuracy,
                      data_frame(Method = "CART Model", 
                                 Accuracy = sum(cartCorrect) / length(cartCorrect)))
accuracy %>% knitr::kable()

# Model 4 - Random Forest Model - Warning takes a while to train
# Train model
trainctrl <- trainControl(verboseIter = FALSE)
rfModel <- caret::train(winner ~ . -tourney_date - dfRate.p0 - dfRate.p1
                        - fSrvInRate.p0 - fSrvInRate.p1 - fSrvWonRate.p0
                        - fSrvWonRate.p1 - SSrvWonRate.p0 - SSrvWonRate.p1
                        - bpSavedRate.p0 - bpSavedRate.p1,
                        data = training,
                        method = 'Rborist',
                        trControl = trainctrl)

# Predict on testing set
rfPred <- predict(rfModel, newdata = testing)

summary(rfPred)
# Compare values
rfCorrect <- (rfPred == testing$winner)
table(rfCorrect)

# Work out accuracy
accuracy <- bind_rows(accuracy,
                      data_frame(Method = "Random Forest Model", 
                                 Accuracy = sum(rfCorrect) / length(rfCorrect)))
accuracy %>% knitr::kable()

# Model 5 - Ensemble Models
# Models to implement - Please install libraries if prompted
models <- c("bayesglm", "lda", "naive_bayes", "glmnet", "gamLoess", "pda", "xgbLinear")

# Function to apply each of the models into the caret train function
fits <- pblapply(models, function(model){ 
  print(model)
  caret::train(winner ~ . -tourney_date - dfRate.p0 - dfRate.p1
               - fSrvInRate.p0 - fSrvInRate.p1 - fSrvWonRate.p0
               - fSrvWonRate.p1 - SSrvWonRate.p0 - SSrvWonRate.p1
               - bpSavedRate.p0 - bpSavedRate.p1, 
               method = model, 
               data = training)
})

# Apply name of models to each of the model
names(fits) <- models

# Function to create a predicition for each model
pred <- pbsapply(fits, function(object) 
  predict(object, newdata = testing))
dim(pred)

# Work out accuracy of each model
accEnsem <- colMeans(pred == testing$winner)
accEnsem
mean(accEnsem)

# Implement voting system
votes <- rowMeans(pred == "0")
y_hat <- ifelse(votes > 0.5, "0", "1")

# Add to accuracy metric
accuracy <- bind_rows(accuracy,
                      data_frame(Method = "Ensemble Models", 
                                 Accuracy = mean(y_hat == testing$winner)))
accuracy %>% knitr::kable()

# See how many predictions from the ensemble model matches the pruned logistic regression model
mean(y_hat == logitPred2B)

# PCA Logistic Regression Model
# Complete PCA
pcomp <- prcomp(training %>% select(-c(tourney_date, winner)), scale. = T)

# Training Data frame for the first 13 principle components
trainPCA <- as.data.frame(pcomp$x)[,1:13] %>%
  cbind(training$winner) %>% rename(winner = `training$winner`)

# Testing Data frame for the first 13 principle components, transformations according to pcomp
testPCA <- as.data.frame(predict(pcomp, testing))[,1:13] %>%
  cbind(testing$winner) %>% rename(winner = `testing$winner`)

# Validation Data frame for the first 13 principle components, transformations according to pcomp
validationPCA <- as.data.frame(predict(pcomp, validation))[,1:13] %>%
  cbind(validation$winner) %>% rename(winner = `validation$winner`)

# Logistic Model using PCA
logitPCA <- caret::train(winner ~ .,
                         data = trainPCA,
                         method = 'glm',
                         na.action = NULL)

summary(logitPCA)


# Predict on testing set
logitPCAPred <- predict(logitPCA, newdata = testPCA)

summary(logitPCAPred)
# Compare values
logitPCACorrect <- (logitPCAPred == testing$winner)
table(logitPCACorrect)
# Work out accuracy
accuracy <- bind_rows(accuracy,
                      data_frame(Method = "Logistic Regression with PCA", 
                                 Accuracy = sum(logitPCACorrect) / length(logitPCACorrect)))
accuracy %>% knitr::kable()

# Baseline; if a player is has a lower rank predict win on validation set
baselineFinalModel <- validation %>%
  select(winner, rank.p0, rank.p1) %>%
  filter(!is.na(rank.p0) & !is.na(rank.p1)) %>%
  mutate(baselinePred = as.integer(ifelse(rank.p0 < rank.p1, 0, 1))) %>%
  mutate(correct = ifelse(baselinePred == winner, 1, 0))

# Table of correct predictions
table(baselineFinalModel$correct)
# Work out accuracy
accuracy <- bind_rows(accuracy,
                      data_frame(Method = "Final Baseline Model", Accuracy = mean(baselineFinalModel$correct)))

# Final Model - Logistic Regression remove more variables
# Train model  
logitModelFinal <- caret::train(winner ~ . -tourney_date - dfRate.p0 - dfRate.p1
                                - fSrvInRate.p0 - fSrvInRate.p1 - fSrvWonRate.p0
                                - fSrvWonRate.p1 - SSrvWonRate.p0 - SSrvWonRate.p1
                                - bpSavedRate.p0 - bpSavedRate.p1,
                                data = training %>%
                                  rbind(testing),
                                method = 'glm',
                                na.action = NULL)

summary(logitModelFinal)

# Predict on testing set
logitPredFinal <- predict(logitModelFinal, newdata = validation)

summary(logitPredFinal)
# Compare values
logitCorrectFinal <- (logitPredFinal == validation$winner)
table(logitCorrectFinal)
# Work out accuracy
accuracy <- bind_rows(accuracy,
                      data_frame(Method = "Final Logistic Regression Model on Validation", 
                                 Accuracy = sum(logitCorrectFinal) / length(logitCorrectFinal)))
accuracy %>% knitr::kable()

Final Ensemble Model
# Models to implement
models <- c("bayesglm", "lda", "naive_bayes", "glmnet", "gamLoess", "pda", "xgbLinear")

# Function to apply each of the models into the caret train function
finalFits <- pblapply(models, function(model){ 
  print(model)
  caret::train(winner ~ . -tourney_date - dfRate.p0 - dfRate.p1
               - fSrvInRate.p0 - fSrvInRate.p1 - fSrvWonRate.p0
               - fSrvWonRate.p1 - SSrvWonRate.p0 - SSrvWonRate.p1
               - bpSavedRate.p0 - bpSavedRate.p1, 
               method = model, 
               data = training %>%
                 rbind(testing))
})

# Apply name of models to each of the model
names(finalFits) <- models

# Function to create a predicition for each model
predFinal <- pbsapply(finalFits, function(object) 
  predict(object, newdata = validation))

# Work out accuracy of each model
accFinEnsem <- colMeans(predFinal == validation$winner)
accFinEnsem
mean(accFinEnsem)

# Implement voting system
votesFinal <- rowMeans(predFinal == "0")
y_hatFinal <- ifelse(votesFinal > 0.5, "0", "1")

# Add to accuracy metric
accuracy <- bind_rows(accuracy,
                      data_frame(Method = "Final Ensemble Models", 
                                 Accuracy = mean(y_hatFinal == validation$winner)))
accuracy %>% knitr::kable()
