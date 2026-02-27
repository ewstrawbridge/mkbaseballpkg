#' A Data Cleaning Function
#'
#' This function cleans files downloaded directly from a statcast search on baseballsavant.mlb.com. Example search: https://baseballsavant.mlb.com/statcast_search?hfPT=&hfAB=&hfGT=R%7C&hfPR=&hfZ=&hfStadium=&hfBBL=&hfNewZones=&hfPull=&hfC=&hfSea=2025%7C&hfSit=&player_type=pitcher&hfOuts=&home_road=&pitcher_throws=&batter_stands=&hfSA=&hfEventOuts=&hfEventRuns=&game_date_gt=&game_date_lt=&hfMo=&hfTeam=&hfOpponent=&hfRO=&position=&hfInfield=&hfOutfield=&hfInn=&hfBBT=&hfFlag=&metric_1=&group_by=name&min_pitches=2000&min_results=25&min_pas=0&sort_col=xwoba&player_event_sort=api_p_release_speed&sort_order=asc&chk_stats_abs=on&chk_stats_hits=on&chk_stats_xwoba=on#results where for each player, click "Graphs" (far right) and then click "Download as CSV". Splits sequences by at-bat.
#' @param file filename from your wokring directory, ex '/data/wheeler.csv'.
#' @keywords clean
#' @export data_clean

data_clean <- function(file) {
  df <- read.csv(file) %>%
    select(player_name, batter, game_type, pitch_type, balls, strikes) %>%
    # we only want regular season games
    filter(game_type == "R") %>%
    # getting my variables in order
    mutate(ct = paste(balls, strikes, sep = "_"),
           ct_red = case_when(
             ct == "1_0" | ct == "2_0" | ct == "3_0" |
             ct ==  "2_1" | ct ==  "3_1" | ct ==  "3_2" ~ "pitchers_ct",
             ct == "0_1" | ct == "0_2" | ct == "1_2"  ~ "hitters_ct",
             ct == "1_1" | ct == "2_2" ~ "even_ct",
             .default = as.character(ct)),
           pitch = pitch_type,
           pitch_red1 = case_when(
             pitch == "FF" ~ "fast",
             pitch == "SI" | pitch == "FS" | pitch == "CH" ~ "sink_split",
             pitch == "CU" | pitch == "KC" ~ "curve",
             pitch == "ST" | pitch == "SL" | pitch == "FC" ~ "cut_sweep",
             .default = as.character(pitch)),
           pitch_red2 = ifelse(pitch=="FF", "fast", "offspeed"),
           aber = 0
    )
  # re-order dataset the other way so i'm counting correctly
  df <- df[order(nrow(df):1),]
  # index for each at-bat
  j <- 0
  for(i in 2:nrow(df)) {
    if(df$batter[i] == df$batter[i-1]) {
      df$aber[i] <- j
    } else {
      j <- j+1
      df$aber[i] <- j
    }
  }
  df <- df %>%
    # pitchcount refers to count of pitches through an at-bat
    mutate(pitchcount = ave(as.character(aber), as.character(aber), FUN=seq_along)) %>%
    filter(pitch_type != "") %>%
    filter(pitch_type != "IN") %>% # unsure what these are, but we're removing them.
    filter(pitch_type != "PO")
  # checking to make sure that we don't have pitches that are only thrown a couple of times
  # usually it's not an issue
  dfcts <- df %>%
    select(pitch) %>%
    group_by(pitch) %>%
    summarize(pitches=n()) %>%
    filter(pitches <= 10)

  named <- dfcts$pitch

  # if it's an issue, we remove the pitches and tell the user what was cut out
  if(nrow(dfcts) != 0 ) {
    df <- df %>%
      filter(pitch != named)
    print(paste0("cut pitch ", named))
  }

  return(df)
}

#' A Data Cleaning Function FOR MATCH SIDES
#'
#' This function cleans files downloaded directly from a statcast search on baseballsavant.mlb.com. Example search: https://baseballsavant.mlb.com/statcast_search?hfPT=&hfAB=&hfGT=R%7C&hfPR=&hfZ=&hfStadium=&hfBBL=&hfNewZones=&hfPull=&hfC=&hfSea=2025%7C&hfSit=&player_type=pitcher&hfOuts=&home_road=&pitcher_throws=&batter_stands=&hfSA=&hfEventOuts=&hfEventRuns=&game_date_gt=&game_date_lt=&hfMo=&hfTeam=&hfOpponent=&hfRO=&position=&hfInfield=&hfOutfield=&hfInn=&hfBBT=&hfFlag=&metric_1=&group_by=name&min_pitches=2000&min_results=25&min_pas=0&sort_col=xwoba&player_event_sort=api_p_release_speed&sort_order=asc&chk_stats_abs=on&chk_stats_hits=on&chk_stats_xwoba=on#results where for each player, click "Graphs" (far right) and then click "Download as CSV". Splits sequences by at-bat.
#' @param file filename from your wokring directory, ex '/data/wheeler.csv'.
#' @keywords clean
#' @export data_clean_match

data_clean_match <- function(file) {
  df <- read.csv(file) %>%
    select(player_name, batter, game_type, pitch_type, balls, strikes, stand, p_throws) %>%
    # we only want regular season games and match sides
    filter(game_type == "R", stand == p_throws) %>%
    # getting my variables in order
    mutate(ct = paste(balls, strikes, sep = "_"),
           ct_red = case_when(
             ct == "1_0" | ct == "2_0" | ct == "3_0" |
               ct ==  "2_1" | ct ==  "3_1" | ct ==  "3_2" ~ "pitchers_ct",
             ct == "0_1" | ct == "0_2" | ct == "1_2"  ~ "hitters_ct",
             ct == "1_1" | ct == "2_2" ~ "even_ct",
             .default = as.character(ct)),
           pitch = pitch_type,
           pitch_red1 = case_when(
             pitch == "FF" ~ "fast",
             pitch == "SI" | pitch == "FS" | pitch == "CH" ~ "sink_split",
             pitch == "CU" | pitch == "KC" ~ "curve",
             pitch == "ST" | pitch == "SL" | pitch == "FC" ~ "cut_sweep",
             .default = as.character(pitch)),
           pitch_red2 = ifelse(pitch=="FF", "fast", "offspeed"),
           aber = 0
    )
  # re-order dataset the other way so i'm counting correctly
  df <- df[order(nrow(df):1),]
  # index for each at-bat
  j <- 0
  for(i in 2:nrow(df)) {
    if(df$batter[i] == df$batter[i-1]) {
      df$aber[i] <- j
    } else {
      j <- j+1
      df$aber[i] <- j
    }
  }
  df <- df %>%
    # pitchcount refers to count of pitches through an at-bat
    mutate(pitchcount = ave(as.character(aber), as.character(aber), FUN=seq_along)) %>%
    filter(pitch_type != "") %>%
    filter(pitch_type != "IN") %>% # unsure what these are, but we're removing them.
    filter(pitch_type != "PO")
  # checking to make sure that we don't have pitches that are only thrown a couple of times
  # usually it's not an issue
  dfcts <- df %>%
    select(pitch) %>%
    group_by(pitch) %>%
    summarize(pitches=n()) %>%
    filter(pitches <= 10)

  named <- dfcts$pitch

  # if it's an issue, we remove the pitches and tell the user what was cut out
  if(nrow(dfcts) != 0 ) {
    df <- df %>%
      filter(pitch != named)
    print(paste0("cut pitch ", named))
  }

  return(df)
}

#' A Data Cleaning Function FOR OFF SIDES
#'
#' This function cleans files downloaded directly from a statcast search on baseballsavant.mlb.com. Example search: https://baseballsavant.mlb.com/statcast_search?hfPT=&hfAB=&hfGT=R%7C&hfPR=&hfZ=&hfStadium=&hfBBL=&hfNewZones=&hfPull=&hfC=&hfSea=2025%7C&hfSit=&player_type=pitcher&hfOuts=&home_road=&pitcher_throws=&batter_stands=&hfSA=&hfEventOuts=&hfEventRuns=&game_date_gt=&game_date_lt=&hfMo=&hfTeam=&hfOpponent=&hfRO=&position=&hfInfield=&hfOutfield=&hfInn=&hfBBT=&hfFlag=&metric_1=&group_by=name&min_pitches=2000&min_results=25&min_pas=0&sort_col=xwoba&player_event_sort=api_p_release_speed&sort_order=asc&chk_stats_abs=on&chk_stats_hits=on&chk_stats_xwoba=on#results where for each player, click "Graphs" (far right) and then click "Download as CSV". Splits sequences by at-bat.
#' @param file filename from your wokring directory, ex '/data/wheeler.csv'.
#' @keywords clean
#' @export data_clean_off

data_clean_off <- function(file) {
  df <- read.csv(file) %>%
    select(player_name, batter, game_type, pitch_type, balls, stand, p_throws) %>%
    # we only want regular season games and off sides
    filter(game_type == "R", stand == p_throws) %>%
    # getting my variables in order
    mutate(ct = paste(balls, strikes, sep = "_"),
           ct_red = case_when(
             ct == "1_0" | ct == "2_0" | ct == "3_0" |
               ct ==  "2_1" | ct ==  "3_1" | ct ==  "3_2" ~ "pitchers_ct",
             ct == "0_1" | ct == "0_2" | ct == "1_2"  ~ "hitters_ct",
             ct == "1_1" | ct == "2_2" ~ "even_ct",
             .default = as.character(ct)),
           pitch = pitch_type,
           pitch_red1 = case_when(
             pitch == "FF" ~ "fast",
             pitch == "SI" | pitch == "FS" | pitch == "CH" ~ "sink_split",
             pitch == "CU" | pitch == "KC" ~ "curve",
             pitch == "ST" | pitch == "SL" | pitch == "FC" ~ "cut_sweep",
             .default = as.character(pitch)),
           pitch_red2 = ifelse(pitch=="FF", "fast", "offspeed"),
           aber = 0
    )
  # re-order dataset the other way so i'm counting correctly
  df <- df[order(nrow(df):1),]
  # index for each at-bat
  j <- 0
  for(i in 2:nrow(df)) {
    if(df$batter[i] == df$batter[i-1]) {
      df$aber[i] <- j
    } else {
      j <- j+1
      df$aber[i] <- j
    }
  }
  df <- df %>%
    # pitchcount refers to count of pitches through an at-bat
    mutate(pitchcount = ave(as.character(aber), as.character(aber), FUN=seq_along)) %>%
    filter(pitch_type != "") %>%
    filter(pitch_type != "IN") %>% # unsure what these are, but we're removing them.
    filter(pitch_type != "PO")
  # checking to make sure that we don't have pitches that are only thrown a couple of times
  # usually it's not an issue
  dfcts <- df %>%
    select(pitch) %>%
    group_by(pitch) %>%
    summarize(pitches=n()) %>%
    filter(pitches <= 10)

  named <- dfcts$pitch

  # if it's an issue, we remove the pitches and tell the user what was cut out
  if(nrow(dfcts) != 0 ) {
    df <- df %>%
      filter(pitch != named)
    print(paste0("cut pitch ", named))
  }

  return(df)
}


#' A Shuffler
#'
#' This function takes in a dataframe cleaned using `data_clean()` and simply shuffles the orders of the at-bats.
#' @param df dataframe cleaned using `data_clean()`.
#' @keywords shuffle
#' @export split_shuffle

split_shuffle <- function(df) {
  # shuffling the at bats
  splitdf <- split(df, f=df$aber)
  shuffled_data <- list_shuffle(splitdf, seed=420)
  return(shuffled_data)
}

#' A Training Setter
#'
#' This function takes in a shuffled large list from `split_shuffle()` and takes 70% of the small lists into a training dataset.
#' @param shuffs large list shuffled using `split_shuffle()`.
#' @keywords train
#' @export train_df

train_df <- function(shuffs) {
  num <- length(shuffs)
  train_num <- ceiling(num * 0.3)
  test_num <- num-train_num
  trainer <- shuffs[1:test_num]
  return(trainer)
}

#' A Testing Setter
#'
#' This function takes in a shuffled large list from `split_shuffle()` and takes 30% of the small lists into a testing dataset.
#' @param shuffs large list shuffled using `split_shuffle()`.
#' @keywords test
#' @export test_df

test_df <- function(shuffs) {
  num <- length(shuffs)
  train_num <- ceiling(num * 0.3)
  test_num <- num-train_num
  tester <- shuffs[(test_num+1):num]
  return(tester)
}

#' A Train/Test Splitter
#'
#' This function takes in a file downloaded directly from a statcast search on baseballsavant.mlb.com, per instructions in `data_clean()`, and splits into 70% training and 30% testing data. Results in large list containing player name, cleaned df, training list, and testing list.
#' @param filename large list shuffled using `split_shuffle()`.
#' @keywords split
#' @export train_test_split

train_test_split <- function(filename) {
  babylist <- list()
  clean_df <- data_clean(filename)
  shuffled <- split_shuffle(clean_df)
  trained_df <- train_df(shuffled)
  tester_df <- test_df(shuffled)
  base_name <- clean_df$player_name[1]
  list_names <- c("base_name", "clean_df", "trained_df", "tester_df")

  for(j in 1:length(list_names)) {
    babylist[[j]] <- get(list_names[j])
  }
  print(base_name)
  return(babylist)
}

#' A Train/Test Splitter FOR MATCH SIDES
#'
#' This function takes in a file downloaded directly from a statcast search on baseballsavant.mlb.com, per instructions in `data_clean()`, and splits into 70% training and 30% testing data. Results in large list containing player name, cleaned df, training list, and testing list.
#' @param filename large list shuffled using `split_shuffle()`.
#' @keywords split
#' @export train_test_split_match

train_test_split_match <- function(filename) {
  babylist <- list()
  clean_df <- data_clean_match(filename)
  shuffled <- split_shuffle(clean_df)
  trained_df <- train_df(shuffled)
  tester_df <- test_df(shuffled)
  base_name <- clean_df$player_name[1]
  list_names <- c("base_name", "clean_df", "trained_df", "tester_df")

  for(j in 1:length(list_names)) {
    babylist[[j]] <- get(list_names[j])
  }
  print(base_name)
  return(babylist)
}

#' A Train/Test Splitter FOR OFF SIDES
#'
#' This function takes in a file downloaded directly from a statcast search on baseballsavant.mlb.com, per instructions in `data_clean()`, and splits into 70% training and 30% testing data. Results in large list containing player name, cleaned df, training list, and testing list.
#' @param filename large list shuffled using `split_shuffle()`.
#' @keywords split
#' @export train_test_split_off

train_test_split_off <- function(filename) {
  babylist <- list()
  clean_df <- data_clean_off(filename)
  shuffled <- split_shuffle(clean_df)
  trained_df <- train_df(shuffled)
  tester_df <- test_df(shuffled)
  base_name <- clean_df$player_name[1]
  list_names <- c("base_name", "clean_df", "trained_df", "tester_df")

  for(j in 1:length(list_names)) {
    babylist[[j]] <- get(list_names[j])
  }
  print(base_name)
  return(babylist)
}

#' A Pitch Arsenal Finder
#'
#' This function identifies the pitch arsenal used by any specific pitcher and returns a vector of alphabetized pitch types with standard abbreviations.
#' @param df dataframe cleaned using `data_clean()`, can be found in `[[2]]` of the large list.
#' @param pitchchoice string indicating whether to use all unique pitches "pitch", pitches grouped "pitch_red1", or fastball/offspeed only "pitch_red2".
#' @keywords arsenal
#' @export fullpitch

fullpitch <- function(df, pitchchoice) {
  sortdf <- arrange(df, .data[[pitchchoice]])
  fullpitch = unique(sortdf[[pitchchoice]])
  return(fullpitch)
}

#' A First Pitch Probability Finder
#'
#' This function finds the probabilities for each pitch type being thrown on the first pitch of any given at-bat by a particular pitcher and returns a dataframe with pitch frequencies to be used in the HMM.
#' @param df dataframe cleaned using `data_clean()`, can be found in `[[2]]` of the large list.
#' @param stats vector pitch arsenal from `fullpitch()`.
#' @param pitchchoice string indicating whether to use all unique pitches "pitch", pitches grouped "pitch_red1", or fastball/offspeed only "pitch_red2". Must match pitchchoice used in `fullpitch()` for `stats`.
#' @keywords first
#' @export firstpitches

firstpitches <- function(df, stats, pitchchoice) {
  statvect <- data.frame(A=stats, tots=0) %>%
    rename(!!pitchchoice := A)
  dffirst <- df %>%
    filter(pitchcount == 1) %>%
    group_by(.data[[pitchchoice]]) %>%
    summarise(tots=n()) %>%
    rbind(statvect) %>%
    group_by(.data[[pitchchoice]]) %>%
    summarise(count=sum(tots))

  allfirst <- sum(dffirst$count)

  dffirst <- dffirst %>%
    mutate(pct = count/allfirst)

  return(dffirst)
}

#' A Training Data Setter Lister
#'
#' This function takes the training data from dataframe form into lists of at-bats with only the necessary information.
#' @param lst_train list of training data made using `train_test_split()`, can be found in `[[3]]` of the large list.
#' @param varb hidden variable to be used in the HMM, in this case "pitch", "pitch_red1", or "pitch_red2".
#' @keywords train
#' @export get_listg

get_listg <- function(lst_train, varb) {
  trained = list()
  for(i in 1:length(lst_train)) {
    addit <- pull(lst_train[[i]], .data[[varb]])
    trained[[i]] <- addit
  }
  return(trained)
}

#' A Transition Matrixer
#'
#' This function produces a transition matrix between variables given a list of at-bats to be used in the HMM.
#' @param lst_got training list obtained from `get_listg()`.
#' @keywords transition
#' @export get_tprobg

get_tprobg <- function(lst_got) {
  mkfit <- markovchainFit(lst_got)
  tprob <- mkfit$estimate@transitionMatrix
  return(tprob)
}

#' An Emission Matrix Preformatter
#'
#' This function preformats the data for an emission matrix to be used in the HMM. Unused on its own unless for debugging, because it always needs debugging.
#' @param dfclean dataframe cleaned using `data_clean()`, can be found in `[[2]]` of the large list.
#' @param varhide string name for hidden variable, one of "pitch", "pitch_red1", "pitch_red2".
#' @param varobs string name for observed variable, one of "ct" or "ct_red".
#' @param stats vector pitch arsenal from `fullpitch()`.
#' @param symbs vector count list from either `countfull` corresponding to `varobs`="ct" or `counthalf` corresponding to `varobs`="ct_red".
#' @keywords emission
#' @export counter_indicesg

counter_indicesg <- function(dfclean, varhide, varobs, stats, symbs) {
  filter_df <- dfclean %>%
    group_by(.data[[varhide]], .data[[varobs]]) %>% #thx casey
    summarise(pitch_tries=n(), .groups = "drop")

  totalpitchtries <- filter_df %>%
    group_by(.data[[varhide]]) %>%
    summarize(total_tries=sum(pitch_tries))

  hide_digit <- paste0(varhide, "_digit")
  obs_digit <- paste0(varobs, "_digit")

  symbnumbs <- data.frame(A=symbs, B=1:length(symbs)) %>%
    rename(!!varobs := A, !!obs_digit := B)
  statenumbs <- data.frame(C=stats, D=1:length(stats)) %>%
    rename(!!varhide := C, !!hide_digit := D)
  fnewpct <- left_join(filter_df, totalpitchtries) %>%
    mutate(pct = pitch_tries/total_tries)
  fnewpct <- left_join(fnewpct, symbnumbs)
  fnewpct <- left_join(fnewpct, statenumbs)

  return(fnewpct)
}

#' An Emission Probability Matrixer
#'
#' This function produces an emission matrix containing the emission probabilities for the counts from each pitch (probability of each pitch-count pair) to be used in the HMM.
#' @param ctindex preformatted emission data from `counter_indicesg()`.
#' @param varhide string name for hidden variable, one of "pitch", "pitch_red1", "pitch_red2".
#' @param varobs string name for observed variable, one of "ct" or "ct_red".
#' @keywords emission
#' @export emissionprob

emissionprob <- function(ctindex, varhide, varobs) {

  hide_l <- length(unique(ctindex[[varhide]]))
  obs_l <- length(unique(ctindex[[varobs]]))

  hide_digit <- paste0(varhide, "_digit")
  obs_digit <- paste0(varobs, "_digit")

  m1 = matrix(, nrow=hide_l, ncol=obs_l)
  for(i in 1:hide_l) {
    temp <- ctindex %>%
      filter(.data[[hide_digit]] == i) %>%
      ungroup() %>%
      select(pct, all_of(obs_digit))
    for(j in 1:obs_l) {
      if(!(j %in% temp[[obs_digit]])) {
        temp <- rbind(temp, c(0,j))

        if(obs_digit=="ct_digit") {
          temp <- temp %>% rename("pct"=1, "ct_digit"=2)
        } else if(obs_digit=="strike_digit") {
          temp <- temp %>% rename("pct"=1, "strike_digit"=2)
        } else {
          temp <- temp %>% rename("pct"=1, "ct_red_digit"=2)
        }

      }
      temp <- temp %>%
        dplyr::arrange(.data[[obs_digit]])

      if(obs_digit=="ct_digit") {
        temp <- temp %>% rename("pct"=1, "ct_digit"=2)
      } else if(obs_digit=="strike_digit") {
        temp <- temp %>% rename("pct"=1, "strike_digit"=2)
      } else {
        temp <- temp %>% rename("pct"=1, "ct_red_digit"=2)
      }
    }
    newguy <- select(temp, pct)
    newerguy <- t(newguy)
    m1[i, ] <- newerguy

  }
  return(m1)
}

#' A Viterbifier
#'
#' This function uses the Viterbi method to produce the HMM estimates for an entire player's training set of data.
#' @param hmm HMM (list of size 5).
#' @param testing_list lst_train list of training data made using `train_test_split()`, can be found in `[[4]]` of the large list.
#' @param varhide string name for hidden variable, one of "pitch", "pitch_red1", "pitch_red2".
#' @param varobs string name for observed variable, one of "ct" or "ct_red".
#' @keywords viterbi
#' @export season_viterbi

season_viterbi <- function(hmm, testing_list, varhide, varobs) {
  viterbi_pred_list = list()
  for(i in 1:length(testing_list)) {
    if((nrow(testing_list[i][[1]])) > 1) {
      hide_vec <- pull(testing_list[[i]], .data[[varhide]])
      obs_vec <- pull(testing_list[[i]], .data[[varobs]])
      pitch_vec <- pull(testing_list[[i]], pitchcount)
      vpred <- viterbi(hmm, obs_vec)

      hide_digit <- paste0(varhide, "_digit")
      hide_digit_pred <- paste0(varhide, "_digit_pred")
      hide_pred <- paste0(varhide, "_pred")
      hide_digit_ac <- paste0(varhide, "_digit_ac")
      obs_digit <- paste0(varobs, "_digit")

      symbnumbs <- data.frame(A=hmm$Symbols, B=1:length(hmm$Symbols)) %>%
        rename(!!varobs := A, !!obs_digit := B)
      statenumbs <- data.frame(C=hmm$States, D=1:length(hmm$States)) %>%
        rename(!!varhide := C, !!hide_digit := D)

      obs_digi <- data.frame(E=obs_vec) %>%
        rename(!!varobs := E)
      obs_digi <- left_join(obs_digi, symbnumbs)

      pred_digi <- data.frame(G=vpred) %>%
        rename(!!varhide := G)
      pred_digi <- left_join(pred_digi, statenumbs) %>%
        rename(!!hide_digit_pred := !!hide_digit, !!hide_pred := !!varhide)


      hide_digi <- data.frame(H=hide_vec) %>%
        rename(!!varhide := H)
      hide_digi <- left_join(hide_digi, statenumbs) %>%
        rename(!!hide_digit_ac := !!hide_digit)

      pred_df <- data.frame(pitchcount=pitch_vec)
      pred_df <- cbind(pred_df, obs_digi, hide_digi, pred_digi)
      viterbi_pred_list[[i]] <- pred_df
    }

  }
  return(viterbi_pred_list)
}

#' Dice-Sorensen Coefficient Finder
#'
#' This function finds the Dice-So/rensen coefficient between two vectors of equal length. This doesn't generalize to other d-s options because I don't need it to.
#' @param x vector of same length as `y`.
#' @param y vector of same length as `x`.
#' @keywords dicesorensen
#' @export dice_sorensen

dice_sorensen <- function(x, y) {
  #https://stackoverflow.com/questions/37798016/r-split-string-to-pairs-of-character
  bigram_x <- substring(x, first = 1:(nchar(x) - 1), last = 2:nchar(x))
  bigram_y <- substring(y, first = 1:(nchar(y) - 1), last = 2:nchar(y))
  n_x <- length(bigram_x)
  n_y <- length(bigram_y) # will be the same in our case, doesn't generalize
  hd_xy <- hamming.distance(bigram_x, bigram_y)
  n_t <- n_x - hd_xy
  dsc <- 2*n_t/(n_x+n_y)
  return(dsc)
}

#' A Viterbi Evaluator
#'
#' This function evaluates the Viterbi estimates from the HMM using Hamming Distance, Dice-Sorensen, and Smith-Waterman methods, returning a dataframe with estimates for each at-bat.
#' @param vpred_list list of Viterbi predictions from `season_viterbi()`.
#' @param varhide string name for hidden variable, one of "pitch", "pitch_red1", "pitch_red2".
#' @param varobs string name for observed variable, one of "ct" or "ct_red".
#' @param typet evaluate for all pitches "all" or for a specific count selected from `countfull` if `varobs`="ct" or from `counthalf` if `varobs`="ct_red".
#' @keywords viterbi
#' @export evaluate_viterbi

evaluate_viterbi <- function(vpred_list, varhide, varobs, typet) {
  df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(df) <- c('outing', 'hd', 'ds', 'sw')

  hide_digit_ac <- paste0(varhide, "_digit_ac")
  hide_digit_pred <- paste0(varhide, "_digit_pred")

  for(i in 1:length(vpred_list)) {

    vpredded <- vpred_list[[i]]
    if(!is.null(vpredded)) {
      if(typet != "all" & varobs == "ct") {
        ezid <- data.frame(ct = countfull, ct_digit = 1:12)
        tofilter <- filter(ezid, ct == typet)$ct_digit
        vpredded <- filter(vpredded, ct_digit == tofilter)
      } else if(typet != "all" & varobs == "ct_red") {
        ezid <- data.frame(ct = counthalf, ct_red_digit = 1:4)
        tofilter <- filter(ezid, ct == typet)$ct_red_digit
        vpredded <- filter(vpredded, ct_red_digit == tofilter)
      }

      actual <- pull(vpredded, .data[[hide_digit_ac]])
      actual <- actual %>% replace(is.na(.), 0)
      actualS <- paste(actual, collapse = '')
      pred <- pull(vpredded, .data[[hide_digit_pred]])
      pred <- pred %>% replace(is.na(.), 0)
      predS <- paste(pred, collapse = '')

      #to get hd into an 0-1 scale
      hd_raw <- hamming.distance(actual, pred)
      hd_raw <- ifelse(is.na(hd_raw), 0, hd_raw)
      full <- length(actual)
      hd <- 1-(hd_raw/full)

      ds <- dice_sorensen(actualS, predS)
      z <- smith_waterman(actualS, predS, type = "characters", match = 1L, mismatch =-1L, gap =-1L)
      sw <- z$similarity

      df <- df %>% add_row(outing=i, hd=hd, ds=ds, sw=sw)
    }
    df <- na.omit(df)
    colnames(df) <- c('outing', 'hd', 'ds', 'sw')
  }

  return(df)
}

#' A Data Reader
#'
#' This function reads data in from a list of filenames, can just be copied by going to finder and selecting the lot of them. From here on out, evaluation of the multiple pitchers is done in clumps and all data, evaluations, models, and results will exist in a large nested list specific to the pairing of hidden/observed variables.
#' @param names_list list of filepath names.
#' @keywords read
#' @export read_data_in

read_data_in <- function(names_list) {
  tts_list <- list()
  for(i in 1:length(names_list)) {
    tts_list[[i]] <- train_test_split(names_list[[i]])
  }
  return(tts_list)
}

#' A Data Reader FOR MATCH SIDES
#'
#' This function reads data in from a list of filenames, can just be copied by going to finder and selecting the lot of them. From here on out, evaluation of the multiple pitchers is done in clumps and all data, evaluations, models, and results will exist in a large nested list specific to the pairing of hidden/observed variables.
#' @param names_list list of filepath names.
#' @keywords read
#' @export read_data_in_match

read_data_in_match <- function(names_list) {
  tts_list <- list()
  for(i in 1:length(names_list)) {
    tts_list[[i]] <- train_test_split(names_list[[i]])
  }
  return(tts_list)
}

#' A Data Reader FOR OFF SIDES
#'
#' This function reads data in from a list of filenames, can just be copied by going to finder and selecting the lot of them. From here on out, evaluation of the multiple pitchers is done in clumps and all data, evaluations, models, and results will exist in a large nested list specific to the pairing of hidden/observed variables.
#' @param names_list list of filepath names.
#' @keywords read
#' @export read_data_in_off

read_data_in_off <- function(names_list) {
  tts_list <- list()
  for(i in 1:length(names_list)) {
    tts_list[[i]] <- train_test_split(names_list[[i]])
  }
  return(tts_list)
}

#' A Data Prepper
#'
#' This function prepares data to be put into a HMM and later evaluated, remains in one large nested list.
#' @param read_list large list of data read using `read_data_in()`.
#' @param varhide string name for hidden variable, one of "pitch", "pitch_red1", "pitch_red2".
#' @param varobs string name for observed variable, one of "ct" or "ct_red".
#' @keywords prep
#' @export prep_model_list

prep_model_list <- function(read_list, varhide, varobs) {

  if(varobs == "ct") {
    symbs <- countfull
  } else if(varobs == "ct_red") {
    symbs <- counthalf
  }

  for(i in 1:length(read_list)) {

    if(varhide == "pitch") {
      stats <- fullpitch(read_list[[i]][[2]], "pitch")
    } else if(varhide == "pitch_red1") {
      stats <- unique(read_list[[i]][[2]][["pitch_red1"]])
    } else if(varhide == "pitch_red2") {
      stats <- unique(read_list[[i]][[2]][["pitch_red2"]])
    }

    starts_full <- firstpitches(read_list[[i]][[2]], stats, varhide)
    starts <- starts_full$pct
    got_list <- get_listg(read_list[[i]][[3]], varhide)
    tprob <- get_tprobg(got_list)
    ctindex <- counter_indicesg(read_list[[i]][[2]], varhide, varobs, stats, symbs)
    emish <- emissionprob(ctindex, varhide, varobs)
    read_list[[i]][[5]] <- varhide
    read_list[[i]][[6]] <- varobs
    read_list[[i]][[7]] <- stats
    read_list[[i]][[8]] <- symbs
    read_list[[i]][[9]] <- starts
    read_list[[i]][[10]] <- tprob
    read_list[[i]][[11]] <- emish
  }
  return(read_list)
}

#' A Model Maker
#'
#' This function creates the HMM for each pitcher given the variable pairs indicated from prep_model_list.
#' @param prepped_list large list prepared using `prep_model_list()`.
#' @keywords model
#' @export big_model

big_model <- function(prepped_list) {
  for(i in 1:length(prepped_list)) {
    model <- initHMM(prepped_list[[i]][[7]], prepped_list[[i]][[8]],
                     prepped_list[[i]][[9]], prepped_list[[i]][[10]], prepped_list[[i]][[11]])
    prepped_list[[i]][[12]] <- model
  }
  return(prepped_list)
}

#' A Model Evaluator
#'
#' This function evaluates each model with respect to all pitches and either 0-0/0-2/3-0/3-2 counts or 0-0/even/hitter/pitcher counts depending on the observed variable specified.
#' @param full_list large list from the output of `big_model()`.
#' @keywords evaluate
#' @export eval_in

eval_in <- function(full_list) {
  for(i in 1:length(full_list)) {
    model <- full_list[[i]][[12]]
    tester <- full_list[[i]][[4]]
    vh <- full_list[[i]][[5]]
    vo <- full_list[[i]][[6]]
    psvit <- season_viterbi(model, tester, vh, vo)
    eval <- evaluate_viterbi(psvit, full_list[[i]][[5]], full_list[[i]][[6]], "all")
    eval_at00 <- evaluate_viterbi(psvit, full_list[[i]][[5]], full_list[[i]][[6]], "0_0")

    if(vo == "ct") {
      eval_at02 <- evaluate_viterbi(psvit, full_list[[i]][[5]], full_list[[i]][[6]], "0_2")
      eval_at30 <- evaluate_viterbi(psvit, full_list[[i]][[5]], full_list[[i]][[6]], "3_0")
      eval_at32 <- evaluate_viterbi(psvit, full_list[[i]][[5]], full_list[[i]][[6]], "3_2")

      full_list[[i]][[15]] <- eval_at02
      full_list[[i]][[16]] <- eval_at30
      full_list[[i]][[17]] <- eval_at32
    } else if(vo == "ct_red") {
      eval_at_even <- evaluate_viterbi(psvit, full_list[[i]][[5]], full_list[[i]][[6]], "even_ct")
      eval_at_hit <- evaluate_viterbi(psvit, full_list[[i]][[5]], full_list[[i]][[6]], "hitters_ct")
      eval_at_pitch <- evaluate_viterbi(psvit, full_list[[i]][[5]], full_list[[i]][[6]], "pitchers_ct")

      full_list[[i]][[15]] <- eval_at_even
      full_list[[i]][[16]] <- eval_at_hit
      full_list[[i]][[17]] <- eval_at_pitch
    }


    full_list[[i]][[13]] <- eval
    full_list[[i]][[14]] <- eval_at00

  }
  return(full_list)
}

#' A Model Assessment Function
#'
#' This function evaluates the models with respect to their average performance across all pitchers. Returns df, *not* large list as other previous functions in list situations.
#' @param final_list large list from `eval_in()`.
#' @param typ evaluate for all pitches "all" or for a specific count selected from `countfull` if `varobs`="ct" or from `counthalf` if `varobs`="ct_red" from original models.
#' @keywords assess
#' @export assess_models

assess_models <- function(final_list, typ) {
  df <- data.frame(matrix(ncol = 4, nrow = 0))
  for(i in 1:length(final_list)) {
    if(typ == "full") {
      evald <- final_list[[i]][[13]]
    } else if(typ == "0_0") {
      evald <- final_list[[i]][[14]]
    } else if(typ == "0_2" | typ == "even") {
      evald <- final_list[[i]][[15]]
    } else if(typ == "3_0" | typ == "hitters_ct") {
      evald <- final_list[[i]][[16]]
    } else if(typ == "3_2" | typ == "pitchers_ct") {
      evald <- final_list[[i]][[17]]
    }
    new_row <- c(pitcher=final_list[[i]][[1]], mean(na.omit(evald$hd)), mean(na.omit(evald$ds)), mean(na.omit(evald$sw)))
    df <- rbind(df, new_row)
  }
  colnames(df) <- c('pitcher', 'hd', 'ds', 'sw')
  df <- df %>% mutate(hd = as.numeric(hd), ds = as.numeric(ds), sw = as.numeric(sw))
  return(df)
}
