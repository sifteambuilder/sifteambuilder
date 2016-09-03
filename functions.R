################################################################################
# Directory of functions in this script                                        #
#                                                                              #
# 1 GetAllSongs()                                                              #
# 2 GetAccount()                                                               #
# 3 GetAccountCards()                                                          #
#   For use in CleanCards() -regexp functions to interpret card.skill_details  #
#   4.1 FillOdds()                                                             #
#   4.2 FillSkillType()                                                        #
#   4.3 FillPoints()                                                           #
#   4.4 FillActivateType()                                                     #
#   4.5 FillActivateNumber()                                                   #
# 4 CleanCards()                                                               #
# 5 CalculateFinalStats()                                                      #
# 6 CheckCards()                                                               #
# 7 FormTeam() -large function; lots of embeded ifelse conditions              #
# 8 TeamImages()                                                               #
################################################################################

GetAllSongs <- function() {
  # Returns a data.frame of all songs in Tomodachi database as well as
  # the calculated 'average songs'
  
  columns.want <- c("name", "romaji_name", "translated_name", "attribute", 
                    "time", "daily_rotation", "easy_notes", "normal_notes", 
                    "hard_notes", "expert_notes", "master_notes", "main_unit")
  
  base.url <- "http://schoolido.lu/api/songs/?page_size=100"
  songs <- data.frame(name = rep(NA, 6),
                      romaji_name = rep(NA, 6),
                      translated_name = rep(NA, 6),
                      attribute = rep(NA, 6),
                      time = rep(NA, 6),
                      daily_rotation = rep(NA, 6),
                      easy_notes = rep(NA, 6),
                      normal_notes = rep(NA, 6),
                      hard_notes = rep(NA, 6),
                      expert_notes = rep(NA, 6),
                      master_notes = rep(NA, 6),
                      main_unit = rep(NA, 6))
  data <- fromJSON(base.url, flatten = TRUE)
  data[[4]] <- data[[4]][, match(columns.want, colnames(data[[4]]))]
  songs <- bind_rows(songs, data[[4]]) # testing dplyr bind_rows
  
  while (!is.null(data[[2]])) {
    data <- fromJSON(data[[2]], flatten = TRUE)
    data[[4]] <- data[[4]][, match(columns.want, colnames(data[[4]]))]
    songs <- bind_rows(songs, data[[4]]) # testing dplyr bind_rows
  }
  
  # Cleans the song titles
  songs$romaji_name <- ifelse(is.na(songs$romaji_name) | songs$romaji_name == "",
                              songs$name, songs$romaji_name)
  songs$translated_name <- ifelse(is.na(songs$translated_name) | songs$translated_name == "",
                                  songs$name, songs$translated_name)
  songs$daily_rotation <- ifelse(!is.na(songs$daily_rotation) & songs$daily_rotation == "",
                                 NA, songs$daily_rotation)
  
  # Adds average Smile Pure Cool songs
  x <- round(apply(songs[which(songs$attribute == "Smile" & songs$main_unit == "\u03BC's"), c(5, 7:11)], 2, mean, na.rm = TRUE))
  y <- round(apply(songs[which(songs$attribute == "Pure" & songs$main_unit == "\u03BC's"), c(5, 7:11)], 2, mean, na.rm = TRUE))
  z <- round(apply(songs[which(songs$attribute == "Cool" & songs$main_unit == "\u03BC's"), c(5, 7:11)], 2, mean, na.rm = TRUE))
  a <- round(apply(songs[which(songs$attribute == "Smile" & songs$main_unit == "Aqours"), c(5, 7:11)], 2, mean, na.rm = TRUE))
  b <- round(apply(songs[which(songs$attribute == "Pure" & songs$main_unit == "Aqours"), c(5, 7:11)], 2, mean, na.rm = TRUE))
  c <- round(apply(songs[which(songs$attribute == "Cool" & songs$main_unit == "Aqours"), c(5, 7:11)], 2, mean, na.rm = TRUE))
  songs[1, 1:4] <- c(rep(enc2utf8("Average u's Smile Song"), 3), "Smile")
  songs[1, 5:11] <- c(x[1], NA, x[2:6])
  songs[1, 12] <- "\u03BC's"
  songs[2, 1:4] <- c(rep(enc2utf8("Average u's Pure Song"), 3), "Pure")
  songs[2, 5:11] <- c(y[1], NA, y[2:6])
  songs[2, 12] <- "\u03BC's"
  songs[3, 1:4] <- c(rep(enc2utf8("Average u's Cool Song"), 3), "Cool")
  songs[3, 5:11] <- c(z[1], NA, z[2:6])
  songs[3, 12] <- "\u03BC's"
  songs[4, 1:4] <- c(rep("Average Aqours Smile Song", 3), "Smile")
  songs[4, 5:11] <- c(a[1], NA, a[2:6])
  songs[4, 12] <- "Aqours"
  songs[5, 1:4] <- c(rep("Average Aqours Pure Song", 3), "Pure")
  songs[5, 5:11] <- c(b[1], NA, b[2:6])
  songs[5, 12] <- "Aqours"
  songs[6, 1:4] <- c(rep("Average Aqours Cool Song", 3), "Cool")
  songs[6, 5:11] <- c(c[1], NA, c[2:6])
  songs[6, 12] <- "Aqours"
  
  songs
}

GetAccount <- function(username) {
  # username is a character vector of length 1 and is the username of the
  # Tomodachi account whose SIF accounts are to be retrieved
  
  base.url <- "http://schoolido.lu/api/accounts/?page_size=100&owner__username="
  data <- fromJSON(paste0(base.url, username))[[4]][, 1:5]
  data$fullname <- paste(data$nickname, data$language)
  accounts <- data
  
  # Checks for duplicate SIF account names in the same server; stops execution
  # if any
  if(anyDuplicated(accounts$fullname)) {
    stop("There are multiple ",
         accounts[anyDuplicated(accounts$fullname), "language"],
         " accounts named ",
         accounts[anyDuplicated(accounts$fullname), "nickname"],
         ". Please rename them differently and try again.")
  }
  
  accounts
}

GetAccountCards <- function(x, accounts) {
  # Retrieves the cards in the selected SIF account from Tomodachi database
  
  columns.want <- c("idolized", "skill", "card.id", "card.idol.main_unit",
                    "card.rarity", "card.attribute", "card.is_promo",
                    "card.non_idolized_maximum_statistics_smile",
                    "card.non_idolized_maximum_statistics_pure",
                    "card.non_idolized_maximum_statistics_cool",
                    "card.idolized_maximum_statistics_smile",
                    "card.idolized_maximum_statistics_pure",
                    "card.idolized_maximum_statistics_cool", "card.skill",
                    "card.skill_details",
                    "card.center_skill", 
                    "card.round_card_image", "card.round_card_idolized_image")
  
  base.url <- "http://schoolido.lu/api/ownedcards/?expand_card&page_size=100&stored=Deck&card__is_special=False&owner_account="
  cards <- data.frame()
  data <- fromJSON(paste0(base.url, accounts[which(accounts$fullname == x), "id"]), flatten = TRUE)
  data[[4]] <- data[[4]][, match(columns.want, colnames(data[[4]]))]
  cards <- rbind(cards, data[[4]], deparse.level = 0)
  
  while (!is.null(data[[2]])) {
    data <- fromJSON(data[[2]], flatten = TRUE)
    data[[4]] <- data[[4]][, match(columns.want, colnames(data[[4]]))]
    cards <- rbind(cards, data[[4]], deparse.level = 0)
  }
  cards <- cards[which(cards$card.rarity != "N"),]
  cards
}


################################################################################
# FOR USE IN CleanCards()

FillOdds <- function(x) {
  
  ifelse(x == "BLANK", 0,
         as.numeric(gsub("[^\\d]+", "", 
                         unlist(regmatches(x, gregexpr("[[:digit:]]*%", x))), 
                         perl=TRUE)) / 100)
  
}

FillSkillType <- function(x) {
  
  ifelse(grepl("HP", x), "Healer",
  ifelse(grepl("into perfects", x), "Plock",
  ifelse(grepl("score by [[:digit:]]*", x), "Scorer", 
               "BLANK")))
  
}

FillPoints <- function(x, y) {
  z <- vector()
  for (i in seq_along(x)) {
    if (y[i] == "Healer") {
      z <- c(z, as.numeric(gsub("[^\\d]+", "", unlist(regmatches(x[i], 
                gregexpr("HP by [[:digit:]]*", x[i]))), perl=TRUE)))
    } else if (y[i] == "Plock") {
      z <- c(z, as.numeric(gsub("[^\\d\\.]+", "", unlist(regmatches(x[i], 
                gregexpr("next [[:digit:]]*.{0,1}[[:digit:]]* seconds", x[i]))), perl=TRUE)))
    } else if (y[i] == "Scorer") {
      z <- c(z, as.numeric(gsub("[^\\d]+", "", unlist(regmatches(x[i], 
                gregexpr("score by [[:digit:]]*", x[i]))), perl=TRUE)))
    } else {z <- c(z, 0)}
  }
  z
}

FillActivateType <- function(x) {
  
  ifelse(grepl("notes", x), "notes",
  ifelse(grepl("[Ee]very [[:digit:]]* seconds", x), "seconds",
  ifelse(grepl("every [[:digit:]]* perfects", x), "perfects",
  ifelse(grepl("hit combo", x), "combo",
  ifelse(grepl("starred", x), "stars",
  ifelse(grepl("points scored", x), "points", 
         "BLANK"))))))
  
}

FillActivateNumber <- function(x, y) {
  z <- vector()
  for (i in seq_along(x)) {
    if (y[i] == "notes") {
      z <- c(z, as.numeric(gsub("[^\\d]+", "", unlist(regmatches(x[i], 
                gregexpr("[Ee]very [[:digit:]]* notes", x[i]))), perl=TRUE)))
    } else if (y[i] == "seconds") {
      z <- c(z, as.numeric(gsub("[^\\d]+", "", unlist(regmatches(x[i], 
                gregexpr("[Ee]very [[[:digit:]]*.{0,1}[[:digit:]]* seconds", x[i]))), perl=TRUE)))
    } else if (y[i] == "perfects") {
      z <- c(z, as.numeric(gsub("[^\\d]+", "", unlist(regmatches(x[i], 
                gregexpr("every [[:digit:]]* perfects", x[i]))), perl=TRUE)))
    } else if (y[i] == "combo") {
      z <- c(z, as.numeric(gsub("[^\\d]+", "", unlist(regmatches(x[i], 
                gregexpr("[[:digit:]]* hit combo", x[i]))), perl=TRUE)))
    } else if (y[i] == "stars") {
      z <- c(z, 1)
    } else if (y[i] == "points") {
      z <- c(z, as.numeric(gsub("[^\\d]+", "", unlist(regmatches(x[i], 
                gregexpr("[[:digit:]]* points scored", x[i]))), perl=TRUE)))
    } else {z <- c(z, 0)}
  }
  z
}

################################################################################

CleanCards <- function(cards, percentPerfect, song.notes, song.length, 
                       song.stars, approx.str) {
  
  bond.unidolized <- c(25, 100, 250, 375, 500)
  names(bond.unidolized) <- c("N", "R", "SR", "SSR", "UR")
  bond.idolized <- c(50, 200, 500, 750, 1000)
  names(bond.idolized) <- c("N", "R", "SR", "SSR", "UR")
  
  cards <- mutate(cards,
                  bond = ifelse(idolized, bond.idolized[card.rarity], 
                                bond.unidolized[card.rarity]),
                  smile = ifelse(idolized, cards$card.idolized_maximum_statistics_smile, 
                                 cards$card.non_idolized_maximum_statistics_smile),
                  pure = ifelse(idolized, cards$card.idolized_maximum_statistics_pure, 
                                cards$card.non_idolized_maximum_statistics_pure),
                  cool = ifelse(idolized, cards$card.idolized_maximum_statistics_cool, 
                                cards$card.non_idolized_maximum_statistics_cool),
                  skillType = FillSkillType(card.skill_details),
                  activateType = FillActivateType(card.skill_details),
                  activateNumber = FillActivateNumber(card.skill_details, activateType),
                  odds = FillOdds(card.skill_details),
                  points = FillPoints(card.skill_details, skillType),
                  contribution = ifelse(activateType == "perfects",
                                        odds * points * floor(percentPerfect * song.notes / activateNumber) / ifelse(skillType == "Scorer", 0.0125 * song.notes, 1),
                                 ifelse(activateType == "combo" | activateType == "notes",
                                        odds * points * song.notes / activateNumber / ifelse(skillType == "Scorer", 0.0125 * song.notes, 1),
                                 ifelse(activateType == "seconds",
                                        odds * points * floor(song.length / activateNumber) / ifelse(skillType == "Scorer", 0.0125 * song.notes, 1),
                                 ifelse(activateType == "stars",
                                        odds * points * floor(percentPerfect * song.stars / activateNumber) / ifelse(skillType == "Scorer", 0.0125 * song.notes, 1),
                                 ifelse(activateType == "points",
                                        odds * points * floor(0.0125 * approx.str * song.notes / activateNumber) / ifelse(skillType == "Scorer", 0.0125 * song.notes, 1),
                                 0))))))
  cards
  
}

CalculateFinalStats <- function(cards, song.unit, is.4.0) {
  
  cards <- mutate(cards,
                  smilePower = (smile * 1.03 + ifelse(card.attribute == "Smile", bond, 0)) * ifelse(card.attribute == "Smile", 1.1, 1) * ifelse(song.unit == card.idol.main_unit & is.4.0, 1.1, 1) + ifelse(skillType == "Scorer", contribution, 0),
                  smileHeart = (smile * 1.06 + ifelse(card.attribute == "Smile", bond, 0)) * ifelse(card.attribute == "Smile", 1.1, 1) * ifelse(song.unit == card.idol.main_unit & is.4.0, 1.1, 1) + ifelse(skillType == "Scorer", contribution, 0),
                  smilePrincess = (smile * 1.09 + ifelse(card.attribute == "Smile", bond, 0)) * ifelse(card.attribute == "Smile", 1.1, 1) * ifelse(song.unit == card.idol.main_unit & is.4.0, 1.1, 1) + ifelse(skillType == "Scorer", contribution, 0),
                  smileAngel = (smile + pure * 0.12 + ifelse(card.attribute == "Smile", bond, 0)) * ifelse(card.attribute == "Smile", 1.1, 1) * ifelse(song.unit == card.idol.main_unit & is.4.0, 1.1, 1) + ifelse(skillType == "Scorer", contribution, 0),
                  smileEmpress = (smile + cool * 0.12 + ifelse(card.attribute == "Smile", bond, 0)) * ifelse(card.attribute == "Smile", 1.1, 1) * ifelse(song.unit == card.idol.main_unit & is.4.0, 1.1, 1) + ifelse(skillType == "Scorer", contribution, 0),
                  purePower = (pure * 1.03 + ifelse(card.attribute == "Pure", bond, 0)) * ifelse(card.attribute == "Pure", 1.1, 1) * ifelse(song.unit == card.idol.main_unit & is.4.0, 1.1, 1) + ifelse(skillType == "Scorer", contribution, 0),
                  pureHeart = (pure * 1.06 + ifelse(card.attribute == "Pure", bond, 0)) * ifelse(card.attribute == "Pure", 1.1, 1) * ifelse(song.unit == card.idol.main_unit & is.4.0, 1.1, 1) + ifelse(skillType == "Scorer", contribution, 0),
                  purePrincess = (pure + smile * 0.12 + ifelse(card.attribute == "Pure", bond, 0)) * ifelse(card.attribute == "Pure", 1.1, 1) * ifelse(song.unit == card.idol.main_unit & is.4.0, 1.1, 1) + ifelse(skillType == "Scorer", contribution, 0),
                  pureAngel = (pure * 1.09 + ifelse(card.attribute == "Pure", bond, 0)) * ifelse(card.attribute == "Pure", 1.1, 1) * ifelse(song.unit == card.idol.main_unit & is.4.0, 1.1, 1) + ifelse(skillType == "Scorer", contribution, 0),
                  pureEmpress = (pure + cool * 0.12 + ifelse(card.attribute == "Pure", bond, 0)) * ifelse(card.attribute == "Pure", 1.1, 1) * ifelse(song.unit == card.idol.main_unit & is.4.0, 1.1, 1) + ifelse(skillType == "Scorer", contribution, 0),
                  coolPower = (cool * 1.03 + ifelse(card.attribute == "Cool", bond, 0)) * ifelse(card.attribute == "Cool", 1.1, 1) * ifelse(song.unit == card.idol.main_unit & is.4.0, 1.1, 1) + ifelse(skillType == "Scorer", contribution, 0),
                  coolHeart = (cool * 1.06 + ifelse(card.attribute == "Cool", bond, 0)) * ifelse(card.attribute == "Cool", 1.1, 1) * ifelse(song.unit == card.idol.main_unit & is.4.0, 1.1, 1) + ifelse(skillType == "Scorer", contribution, 0),
                  coolPrincess = (cool + smile * 0.12 + ifelse(card.attribute == "Cool", bond, 0)) * ifelse(card.attribute == "Cool", 1.1, 1) * ifelse(song.unit == card.idol.main_unit & is.4.0, 1.1, 1) + ifelse(skillType == "Scorer", contribution, 0),
                  coolAngel = (cool + pure * 0.12 + ifelse(card.attribute == "Cool", bond, 0)) * ifelse(card.attribute == "Cool", 1.1, 1) * ifelse(song.unit == card.idol.main_unit & is.4.0, 1.1, 1) + ifelse(skillType == "Scorer", contribution, 0),
                  coolEmpress = (cool * 1.09 + ifelse(card.attribute == "Cool", bond, 0)) * ifelse(card.attribute == "Cool", 1.1, 1) * ifelse(song.unit == card.idol.main_unit & is.4.0, 1.1, 1) + ifelse(skillType == "Scorer", contribution, 0))
  cards
}

CheckCards <- function(cards, song) {
  
  if ("points" %in% cards$activateType) {
    if (song$attribute == "Smile") {
      x <- sum(arrange(cards, desc(smilePrincess))[1:9, "smilePrincess"], na.rm = TRUE)
      y <- TRUE
    } else if (song$attribute == "Pure") {
      x <- sum(arrange(cards, desc(pureAngel))[1:9, "pureAngel"], na.rm = TRUE)
      y <- TRUE
    } else if (song$attribute == "Cool") {
      x <- sum(arrange(cards, desc(coolEmpress))[1:9, "coolEmpress"], na.rm = TRUE)
      y <- TRUE
    }
    
  } else {
    x <- 50000
    y <- FALSE
  }
  
  return(list(x, y))
}

FormTeam <- function(cards, song, teamType) {
  
  if(teamType == "Scorer") {
    
    if (song$attribute == "Smile") {
      
      want <- c("card.id", "idolized", "card.attribute", "card.center_skill",
                "card.round_card_image", "card.round_card_idolized_image",
                "smilePower", "smileHeart", "smilePrincess", "smileEmpress",
                "smileAngel")
      team.strength <- vector()
      
      if("Smile Power" %in% cards$card.center_skill) {
        
        smile.power <- arrange(cards, desc(smilePower))[1:9, want]
        if(!"Smile Power" %in% smile.power$card.center_skill) {
          smile.power[9, ] <- arrange(filter(cards, card.center_skill == "Smile Power"), desc(smilePower))[1, want]
        }
        team.strength <- c(team.strength, smilePower = sum(smile.power$smilePower))
      }
      
      if("Smile Heart" %in% cards$card.center_skill) {
        
        smile.heart <- arrange(cards, desc(smileHeart))[1:9, want]
        if(!"Smile Heart" %in% smile.heart$card.center_skill) {
          smile.heart[9, ] <- arrange(filter(cards, card.center_skill == "Smile Heart"), desc(smileHeart))[1, want]
        }
        team.strength <- c(team.strength, smileHeart = sum(smile.heart$smileHeart))
      }
      
      if("Smile Princess" %in% cards$card.center_skill) {
        
        smile.princess <- arrange(cards, desc(smilePrincess))[1:9, want]
        if(!"Smile Princess" %in% smile.princess$card.center_skill) {
          smile.princess[9, ] <- arrange(filter(cards, card.center_skill == "Smile Princess"), desc(smilePrincess))[1, want]
        }
        team.strength <- c(team.strength, smilePrincess = sum(smile.princess$smilePrincess))
      }
      
      if("Smile Angel" %in% cards$card.center_skill) {
        
        smile.angel <- arrange(cards, desc(smileAngel))[1:9, want]
        if(!"Smile Angel" %in% smile.angel$card.center_skill) {
          smile.angel[9, ] <- arrange(filter(cards, card.center_skill == "Smile Angel"), desc(smileAngel))[1, want]
        }
        team.strength <- c(team.strength, smileAngel = sum(smile.angel$smileAngel))
      }
      
      if("Smile Empress" %in% cards$card.center_skill) {
        
        smile.empress <- arrange(cards, desc(smileEmpress))[1:9, want]
        if(!"Smile Empress" %in% smile.empress$card.center_skill) {
          smile.empress[9, ] <- arrange(filter(cards, card.center_skill == "Smile Empress"), desc(smileEmpress))[1, want]
        }
        team.strength <- c(team.strength, smileEmpress = sum(smile.empress$smileEmpress))
      }
      
      # compares team strength
      team.best <- names(which.max(team.strength))
      
      team <- `if`(team.best == "smilePower", smile.power,
              `if`(team.best == "smileHeart", smile.heart,
              `if`(team.best == "smilePrincess", smile.princess,
              `if`(team.best == "smileAngel", smile.angel,
                                              smile.empress))))
      # settles center card
      if (team.best == "smilePower") {
        team <- smile.power
        temp <- team[5, ]
        main <- team[grep("Smile Power", team$card.center_skill)[1], ]
        team[grep("Smile Power", team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
      } else if(team.best == "smileHeart") {
        team <- smile.heart
        temp <- team[5, ]
        main <- team[grep("Smile Heart", team$card.center_skill)[1], ]
        team[grep("Smile Heart", team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
      } else if(team.best == "smilePrincess") {
        team <- smile.princess
        temp <- team[5, ]
        main <- team[grep("Smile Princess", team$card.center_skill)[1], ]
        team[grep("Smile Princess", team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
      } else if(team.best == "smileAngel") {
        team <- smile.angel
        temp <- team[5, ]
        main <- team[grep("Smile Angel", team$card.center_skill)[1], ]
        team[grep("Smile Angel", team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
      } else {
        team <- smile.empress
        temp <- team[5, ]
        main <- team[grep("Smile Empress", team$card.center_skill)[1], ]
        team[grep("Smile Empress", team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
      }
      
      # settle guest center
      guest.center <- c(sum(team[, "smilePrincess"], na.rm = TRUE),
                        sum(team[, "smileAngel"], na.rm = TRUE),
                        sum(team[, "smileEmpress"], na.rm = TRUE))
      names(guest.center) <- c("Smile Princess (Smile increases drastically)", 
                               "Smile Angel (Smile increases based on Pure)",
                               "Smile Empress (Smile increases based on Cool)")
      guest.center <- sort(guest.center, decreasing = TRUE)
      guest.center <- paste("Recommended guest center:", names(guest.center[1]))
      
    } else if (song$attribute == "Pure") {
      
      want <- c("card.id", "idolized", "card.attribute", "card.center_skill",
                "card.round_card_image", "card.round_card_idolized_image",
                "purePower", "pureHeart", "purePrincess", "pureEmpress",
                "pureAngel")
      team.strength <- vector()
      
      if("Pure Power" %in% cards$card.center_skill) {
        
        pure.power <- arrange(cards, desc(purePower))[1:9, want]
        if(!"Pure Power" %in% pure.power$card.center_skill) {
          pure.power[9, ] <- arrange(filter(cards, card.center_skill == "Pure Power"), desc(purePower))[1, want]
        }
        team.strength <- c(team.strength, purePower = sum(pure.power$purePower))
      }
      
      if("Pure Heart" %in% cards$card.center_skill) {
        
        pure.heart <- arrange(cards, desc(pureHeart))[1:9, want]
        if(!"Pure Heart" %in% pure.heart$card.center_skill) {
          pure.heart[9, ] <- arrange(filter(cards, card.center_skill == "Pure Heart"), desc(pureHeart))[1, want]
        }
        team.strength <- c(team.strength, pureHeart = sum(pure.heart$pureHeart))
      }
      
      if("Pure Princess" %in% cards$card.center_skill) {
        
        pure.princess <- arrange(cards, desc(purePrincess))[1:9, want]
        if(!"Pure Princess" %in% pure.princess$card.center_skill) {
          pure.princess[9, ] <- arrange(filter(cards, card.center_skill == "Pure Princess"), desc(purePrincess))[1, want]
        }
        team.strength <- c(team.strength, purePrincess = sum(pure.princess$purePrincess))
      }
      
      if("Pure Angel" %in% cards$card.center_skill) {
        
        pure.angel <- arrange(cards, desc(pureAngel))[1:9, want]
        if(!"Pure Angel" %in% pure.angel$card.center_skill) {
          pure.angel[9, ] <- arrange(filter(cards, card.center_skill == "Pure Angel"), desc(pureAngel))[1, want]
        }
        team.strength <- c(team.strength, pureAngel = sum(pure.angel$pureAngel))
      }
      
      if("Pure Empress" %in% cards$card.center_skill) {
        
        pure.empress <- arrange(cards, desc(pureEmpress))[1:9, want]
        if(!"Pure Empress" %in% pure.empress$card.center_skill) {
          pure.empress[9, ] <- arrange(filter(cards, card.center_skill == "Pure Empress"), desc(pureEmpress))[1, want]
        }
        team.strength <- c(team.strength, pureEmpress = sum(pure.empress$pureEmpress))
      }
      
      team.best <- names(which.max(team.strength))
      
      team <- `if`(team.best == "purePower", pure.power,
              `if`(team.best == "pureHeart", pure.heart,
              `if`(team.best == "purePrincess", pure.princess,
              `if`(team.best == "pureAngel", pure.angel,
                                             pure.empress))))
      
      if (team.best == "purePower") {
        team <- pure.power
        temp <- team[5, ]
        main <- team[grep("Pure Power", team$card.center_skill)[1], ]
        team[grep("Pure Power", team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
      } else if(team.best == "pureHeart") {
        team <- pure.heart
        temp <- team[5, ]
        main <- team[grep("Pure Heart", team$card.center_skill)[1], ]
        team[grep("Pure Heart", team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
      } else if(team.best == "purePrincess") {
        team <- pure.princess
        temp <- team[5, ]
        main <- team[grep("Pure Princess", team$card.center_skill)[1], ]
        team[grep("Pure Princess", team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
      } else if(team.best == "pureAngel") {
        team <- pure.angel
        temp <- team[5, ]
        main <- team[grep("Pure Angel", team$card.center_skill)[1], ]
        team[grep("Pure Angel", team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
      } else {
        team <- pure.empress
        temp <- team[5, ]
        main <- team[grep("Pure Empress", team$card.center_skill)[1], ]
        team[grep("Pure Empress", team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
      }
      
      guest.center <- c(sum(team[, "purePrincess"], na.rm = TRUE),
                        sum(team[, "pureAngel"], na.rm = TRUE),
                        sum(team[, "pureEmpress"], na.rm = TRUE))
      names(guest.center) <- c("Pure Princess (Pure increases based on Smile)", 
                               "Pure Angel (Pure increases drastically)",
                               "Pure Empress (Pure increases based on Cool)")
      guest.center <- sort(guest.center, decreasing = TRUE)
      guest.center <- paste("Recommended guest center:", names(guest.center[1]))
      
    } else {
      
      want <- c("card.id", "idolized", "card.attribute", "card.center_skill",
                "card.round_card_image", "card.round_card_idolized_image",
                "coolPower", "coolHeart", "coolPrincess", "coolEmpress",
                "coolAngel")
      team.strength <- vector()
      
      if("Cool Power" %in% cards$card.center_skill) {
        
        cool.power <- arrange(cards, desc(coolPower))[1:9, want]
        if(!"Cool Power" %in% cool.power$card.center_skill) {
          cool.power[9, ] <- arrange(filter(cards, card.center_skill == "Cool Power"), desc(coolPower))[1, want]
        }
        team.strength <- c(team.strength, coolPower = sum(cool.power$coolPower))
      }
      
      if("Cool Heart" %in% cards$card.center_skill) {
        
        cool.heart <- arrange(cards, desc(coolHeart))[1:9, want]
        if(!"Cool Heart" %in% cool.heart$card.center_skill) {
          cool.heart[9, ] <- arrange(filter(cards, card.center_skill == "Cool Heart"), desc(coolHeart))[1, want]
        }
        team.strength <- c(team.strength, coolHeart = sum(cool.heart$coolHeart))
      }
      
      if("Cool Princess" %in% cards$card.center_skill) {
        
        cool.princess <- arrange(cards, desc(coolPrincess))[1:9, want]
        if(!"Cool Princess" %in% cool.princess$card.center_skill) {
          cool.princess[9, ] <- arrange(filter(cards, card.center_skill == "Cool Princess"), desc(coolPrincess))[1, want]
        }
        team.strength <- c(team.strength, coolPrincess = sum(cool.princess$coolPrincess))
      }
      
      if("Cool Angel" %in% cards$card.center_skill) {
        
        cool.angel <- arrange(cards, desc(coolAngel))[1:9, want]
        if(!"Cool Angel" %in% cool.angel$card.center_skill) {
          cool.angel[9, ] <- arrange(filter(cards, card.center_skill == "Cool Angel"), desc(coolAngel))[1, want]
        }
        team.strength <- c(team.strength, coolAngel = sum(cool.angel$coolAngel))
      }
      
      if("Cool Empress" %in% cards$card.center_skill) {
        
        cool.empress <- arrange(cards, desc(coolEmpress))[1:9, want]
        if(!"Cool Empress" %in% cool.empress$card.center_skill) {
          cool.empress[9, ] <- arrange(filter(cards, card.center_skill == "Cool Empress"), desc(coolEmpress))[1, want]
        }
        team.strength <- c(team.strength, coolEmpress = sum(cool.empress$coolEmpress))
      }
      
      team.best <- names(which.max(team.strength))
      
      team <- `if`(team.best == "coolPower", cool.power,
              `if`(team.best == "coolHeart", cool.heart,
              `if`(team.best == "coolPrincess", cool.princess,
              `if`(team.best == "coolAngel", cool.angel,
                                             cool.empress))))

      if (team.best == "coolPower") {
        team <- cool.power
        temp <- team[5, ]
        main <- team[grep("Cool Power", team$card.center_skill)[1], ]
        team[grep("Cool Power", team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
      } else if(team.best == "coolHeart") {
        team <- cool.heart
        temp <- team[5, ]
        main <- team[grep("Cool Heart", team$card.center_skill)[1], ]
        team[grep("Cool Heart", team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
      } else if(team.best == "coolPrincess") {
        team <- cool.princess
        temp <- team[5, ]
        main <- team[grep("Cool Princess", team$card.center_skill)[1], ]
        team[grep("Cool Princess", team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
      } else if(team.best == "coolAngel") {
        team <- cool.angel
        temp <- team[5, ]
        main <- team[grep("Cool Angel", team$card.center_skill)[1], ]
        team[grep("Cool Angel", team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
      } else {
        team <- cool.empress
        temp <- team[5, ]
        main <- team[grep("Cool Empress", team$card.center_skill)[1], ]
        team[grep("Cool Empress", team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
      }
      
      guest.center <- c(sum(team[, "coolPrincess"], na.rm = TRUE),
                        sum(team[, "coolAngel"], na.rm = TRUE),
                        sum(team[, "coolEmpress"], na.rm = TRUE))
      names(guest.center) <- c("Cool Princess (Cool increases based on Smile)", 
                               "Cool Angel (Cool increases based on Pure)",
                               "Cool Empress (Cool increases drastically)")
      guest.center <- sort(guest.center, decreasing = TRUE)
      guest.center <- paste("Recommended guest center:", names(guest.center[1]))
      
    }
    
  } else if (teamType == "Perfect-Lock (Maximize PLock)") {
    
    cards.plock <- filter(cards, skillType == "Plock", card.rarity != "R", card.is_promo == "FALSE")
    
    if (nrow(cards.plock) < 9) {
      cards <- filter(cards, !(card.id %in% cards.plock$card.id))
      
      if (song$attribute == "Smile") {
        cards <- arrange(cards, desc(smilePrincess))
        cards.plock <- arrange(cards.plock, desc(contribution))
        cards.plock <- rbind(cards.plock, cards[1:(9 - nrow(cards.plock)), ])
        team <- cards.plock[, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image")]
      } else if (song$attribute == "Pure") {
        cards <- arrange(cards, desc(pureAngel))
        cards.plock <- arrange(cards.plock, desc(contribution))
        cards.plock <- rbind(cards.plock, cards[1:(9 - nrow(cards.plock)), ])
        team <- cards.plock[, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image")]
      } else {
        cards <- arrange(cards, desc(coolEmpress))
        cards.plock <- arrange(cards.plock, desc(contribution))
        cards.plock <- rbind(cards.plock, cards[1:(9 - nrow(cards.plock)), ])
        team <- cards.plock[, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image")]
      }
      
    } else {
      
      team <- arrange(cards.plock, desc(contribution))[1:9, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image")]
      
    }
    
    guest.center <- "This team consists of your best Perfect-Lock cards only, regardless of card attribute. Your song score may be severely affected."
    
  } else if (teamType == "Perfect-Lock (Maximize Score)") {
    
    if (song$attribute == "Smile") {
      cards.plock <- filter(cards, skillType == "Plock", card.attribute == "Smile", card.rarity != "R", card.is_promo == "FALSE")
      cards.plock <- arrange(cards.plock, desc(smilePrincess), desc(contribution))
      
      if (nrow(cards.plock) < 9) {
        cards <- filter(cards, !(card.id %in% cards.plock$card.id))
        cards <- arrange(cards, desc(smilePrincess))
        cards.plock <- rbind(cards.plock, cards[1:(9 - nrow(cards.plock)), ])
        team <- cards.plock[, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image", "smilePower", "smileHeart", "smilePrincess", "smileEmpress", "smileAngel")]
        
      } else {
        team <- cards.plock[1:9, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image", "smilePower", "smileHeart", "smilePrincess", "smileEmpress", "smileAngel")]
        
      }
      
      nine.smile <- c(sum(arrange(team, desc(smilePower))[, "smilePower"], na.rm = TRUE),
                      sum(arrange(team, desc(smileHeart))[, "smileHeart"], na.rm = TRUE),
                      sum(arrange(team, desc(smilePrincess))[, "smilePrincess"], na.rm = TRUE),
                      sum(arrange(team, desc(smileAngel))[, "smileAngel"], na.rm = TRUE),
                      sum(arrange(team, desc(smileEmpress))[, "smileEmpress"], na.rm = TRUE))
      names(nine.smile) <- c("Smile Power", "Smile Heart", "Smile Princess", "Smile Angel", "Smile Empress")
      nine.smile <- sort(nine.smile, decreasing = TRUE)
      
      if (names(nine.smile[1]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.smile[1]), team$card.center_skill)[1], ]
        team[grep(names(nine.smile[1]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.smile[2]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.smile[2]), team$card.center_skill)[1], ]
        team[grep(names(nine.smile[2]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.smile[3]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.smile[3]), team$card.center_skill)[1], ]
        team[grep(names(nine.smile[3]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.smile[4]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.smile[4]), team$card.center_skill)[1], ]
        team[grep(names(nine.smile[4]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else {
        temp <- team[5, ]
        main <- team[grep(names(nine.smile[5]), team$card.center_skill)[1], ]
        team[grep(names(nine.smile[5]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      }
      
      guest.center <- c(sum(team[, "smilePrincess"], na.rm = TRUE),
                        sum(team[, "smileAngel"], na.rm = TRUE),
                        sum(team[, "smileEmpress"], na.rm = TRUE))
      names(guest.center) <- c("Smile Princess (Smile increases drastically)", "Smile Angel (Smile increases based on Pure)", "Smile Empress (Smile increases based on Cool)")
      guest.center <- sort(guest.center, decreasing = TRUE)
      guest.center <- paste("Recommended guest center:", names(guest.center[1]))
      
      
    } else if (song$attribute == "Pure") {
      cards.plock <- filter(cards, skillType == "Plock", card.attribute == "Pure", card.rarity != "R", card.is_promo == "FALSE")
      cards.plock <- arrange(cards.plock, desc(pureAngel), desc(contribution))
      
      if (nrow(cards.plock) < 9) {
        cards <- filter(cards, !(card.id %in% cards.plock$card.id))
        cards <- arrange(cards, desc(pureAngel))
        cards.plock <- rbind(cards.plock, cards[1:(9 - nrow(cards.plock)), ])
        team <- cards.plock[, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image", "purePower", "pureHeart", "purePrincess", "pureEmpress", "pureAngel")]
        
      } else {
        team <- cards.plock[1:9, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image", "purePower", "pureHeart", "purePrincess", "pureEmpress", "pureAngel")]
        
      }
      
      nine.pure <- c(sum(arrange(team, desc(purePower))[, "purePower"], na.rm = TRUE),
                     sum(arrange(team, desc(pureHeart))[, "pureHeart"], na.rm = TRUE),
                     sum(arrange(team, desc(purePrincess))[, "purePrincess"], na.rm = TRUE),
                     sum(arrange(team, desc(pureAngel))[, "pureAngel"], na.rm = TRUE),
                     sum(arrange(team, desc(pureEmpress))[, "pureEmpress"], na.rm = TRUE))
      names(nine.pure) <- c("Pure Power", "Pure Heart", "Pure Princess", "Pure Angel", "Pure Empress")
      nine.pure <- sort(nine.pure, decreasing = TRUE)
      
      if (names(nine.pure[1]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.pure[1]), team$card.center_skill)[1], ]
        team[grep(names(nine.pure[1]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.pure[2]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.pure[2]), team$card.center_skill)[1], ]
        team[grep(names(nine.pure[2]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.pure[3]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.pure[3]), team$card.center_skill)[1], ]
        team[grep(names(nine.pure[3]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.pure[4]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.pure[4]), team$card.center_skill)[1], ]
        team[grep(names(nine.pure[4]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else {
        temp <- team[5, ]
        main <- team[grep(names(nine.pure[5]), team$card.center_skill)[1], ]
        team[grep(names(nine.pure[5]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      }
      
      guest.center <- c(sum(team[, "purePrincess"], na.rm = TRUE),
                        sum(team[, "pureAngel"], na.rm = TRUE),
                        sum(team[, "pureEmpress"], na.rm = TRUE))
      names(guest.center) <- c("Pure Princess (Pure increases based on Smile)", "Pure Angel (Pure increases drastically)", "Pure Empress (Pure increases based on Cool)")
      guest.center <- sort(guest.center, decreasing = TRUE)
      guest.center <- paste("Recommended guest center:", names(guest.center[1]))
      
    } else if (song$attribute == "Cool") {
      cards.plock <- filter(cards, skillType == "Plock", card.attribute == "Cool", card.rarity != "R", card.is_promo == "FALSE")
      cards.plock <- arrange(cards.plock, desc(coolEmpress), desc(contribution))
      
      if (nrow(cards.plock) < 9) {
        cards <- filter(cards, !(card.id %in% cards.plock$card.id))
        cards <- arrange(cards, desc(coolEmpress))
        cards.plock <- rbind(cards.plock, cards[1:(9 - nrow(cards.plock)), ])
        team <- cards.plock[, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image", "coolPower", "coolHeart", "coolPrincess", "coolEmpress", "coolAngel")]
        
      } else {
        team <- cards.plock[1:9, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image", "coolPower", "coolHeart", "coolPrincess", "coolEmpress", "coolAngel")]
        
      }
      
      nine.cool <- c(sum(arrange(team, desc(coolPower))[, "coolPower"], na.rm = TRUE),
                     sum(arrange(team, desc(coolHeart))[, "coolHeart"], na.rm = TRUE),
                     sum(arrange(team, desc(coolPrincess))[, "coolPrincess"], na.rm = TRUE),
                     sum(arrange(team, desc(coolAngel))[, "coolAngel"], na.rm = TRUE),
                     sum(arrange(team, desc(coolEmpress))[, "coolEmpress"], na.rm = TRUE))
      names(nine.cool) <- c("Cool Power", "Cool Heart", "Cool Princess", "Cool Angel", "Cool Empress")
      nine.cool <- sort(nine.cool, decreasing = TRUE)
      
      if (names(nine.cool[1]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.cool[1]), team$card.center_skill)[1], ]
        team[grep(names(nine.cool[1]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.cool[2]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.cool[2]), team$card.center_skill)[1], ]
        team[grep(names(nine.cool[2]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.cool[3]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.cool[3]), team$card.center_skill)[1], ]
        team[grep(names(nine.cool[3]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.cool[4]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.cool[4]), team$card.center_skill)[1], ]
        team[grep(names(nine.cool[4]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else {
        temp <- team[5, ]
        main <- team[grep(names(nine.cool[5]), team$card.center_skill)[1], ]
        team[grep(names(nine.cool[5]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      }
      
      guest.center <- c(sum(team[, "coolPrincess"], na.rm = TRUE),
                        sum(team[, "coolAngel"], na.rm = TRUE),
                        sum(team[, "coolEmpress"], na.rm = TRUE))
      names(guest.center) <- c("Cool Princess (Cool increases based on Smile)", "Cool Angel (Cool increases based on Pure)", "Cool Empress (Cool increases drastically)")
      guest.center <- sort(guest.center, decreasing = TRUE)
      guest.center <- paste("Recommended guest center:", names(guest.center[1]))
      
    }
    
    
  } else if (teamType == "Healer (Maximize Healing)") {
    
    cards.healer <- filter(cards, skillType == "Healer")
    
    if (nrow(cards.healer) < 9) {
      cards <- filter(cards, !(card.id %in% cards.healer$card.id))
      
      if (song$attribute == "Smile") {
        cards <- arrange(cards, desc(smilePrincess))
        cards.healer <- arrange(cards.healer, desc(contribution))
        cards.healer <- rbind(cards.healer, cards[1:(9 - nrow(cards.healer)), ])
        team <- cards.healer[, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image")]
      } else if (song$attribute == "Pure") {
        cards <- arrange(cards, desc(pureAngel))
        cards.healer <- arrange(cards.healer, desc(contribution))
        cards.healer <- rbind(cards.healer, cards[1:(9 - nrow(cards.healer)), ])
        team <- cards.healer[, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image")]
      } else {
        cards <- arrange(cards, desc(coolEmpress))
        cards.healer <- arrange(cards.healer, desc(contribution))
        cards.healer <- rbind(cards.healer, cards[1:(9 - nrow(cards.healer)), ])
        team <- cards.healer[, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image")]
      }
      
    } else {
      
      team <- arrange(cards.healer, desc(contribution))[1:9, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image")]
      
    }
    
    guest.center <- "This team consists of your best Healer cards only, regardless of card attribute. Your song score may be severely affected."
    
  } else if (teamType == "Healer (Maximize Score)") {
    
    if (song$attribute == "Smile") {
      cards.healer <- filter(cards, skillType == "Healer", card.attribute == "Smile", card.rarity != "R", card.is_promo == "FALSE")
      cards.healer <- arrange(cards.healer, desc(smilePrincess), desc(contribution))
      
      if (nrow(cards.healer) < 9) {
        cards <- filter(cards, !(card.id %in% cards.healer$card.id))
        cards <- arrange(cards, desc(smilePrincess))
        cards.healer <- rbind(cards.healer, cards[1:(9 - nrow(cards.healer)), ])
        team <- cards.healer[, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image", "smilePower", "smileHeart", "smilePrincess", "smileEmpress", "smileAngel")]
        
      } else {
        team <- cards.healer[1:9, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image", "smilePower", "smileHeart", "smilePrincess", "smileEmpress", "smileAngel")]
        
      }
      
      nine.smile <- c(sum(arrange(team, desc(smilePower))[, "smilePower"], na.rm = TRUE),
                      sum(arrange(team, desc(smileHeart))[, "smileHeart"], na.rm = TRUE),
                      sum(arrange(team, desc(smilePrincess))[, "smilePrincess"], na.rm = TRUE),
                      sum(arrange(team, desc(smileAngel))[, "smileAngel"], na.rm = TRUE),
                      sum(arrange(team, desc(smileEmpress))[, "smileEmpress"], na.rm = TRUE))
      names(nine.smile) <- c("Smile Power", "Smile Heart", "Smile Princess", "Smile Angel", "Smile Empress")
      nine.smile <- sort(nine.smile, decreasing = TRUE)
      
      if (names(nine.smile[1]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.smile[1]), team$card.center_skill)[1], ]
        team[grep(names(nine.smile[1]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.smile[2]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.smile[2]), team$card.center_skill)[1], ]
        team[grep(names(nine.smile[2]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.smile[3]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.smile[3]), team$card.center_skill)[1], ]
        team[grep(names(nine.smile[3]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.smile[4]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.smile[4]), team$card.center_skill)[1], ]
        team[grep(names(nine.smile[4]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else {
        temp <- team[5, ]
        main <- team[grep(names(nine.smile[5]), team$card.center_skill)[1], ]
        team[grep(names(nine.smile[5]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      }
      
      guest.center <- c(sum(team[, "smilePrincess"], na.rm = TRUE),
                        sum(team[, "smileAngel"], na.rm = TRUE),
                        sum(team[, "smileEmpress"], na.rm = TRUE))
      names(guest.center) <- c("Smile Princess (Smile increases drastically)", "Smile Angel (Smile increases based on Pure)", "Smile Empress (Smile increases based on Cool)")
      guest.center <- sort(guest.center, decreasing = TRUE)
      guest.center <- paste("Recommended guest center:", names(guest.center[1]))
      
      
    } else if (song$attribute == "Pure") {
      cards.healer <- filter(cards, skillType == "Healer", card.attribute == "Pure", card.rarity != "R", card.is_promo == "FALSE")
      cards.healer <- arrange(cards.healer, desc(pureAngel), desc(contribution))
      
      if (nrow(cards.healer) < 9) {
        cards <- filter(cards, !(card.id %in% cards.healer$card.id))
        cards <- arrange(cards, desc(pureAngel))
        cards.healer <- rbind(cards.healer, cards[1:(9 - nrow(cards.healer)), ])
        team <- cards.healer[, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image", "purePower", "pureHeart", "purePrincess", "pureEmpress", "pureAngel")]
        
      } else {
        team <- cards.healer[1:9, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image", "purePower", "pureHeart", "purePrincess", "pureEmpress", "pureAngel")]
        
      }
      
      nine.pure <- c(sum(arrange(team, desc(purePower))[, "purePower"], na.rm = TRUE),
                     sum(arrange(team, desc(pureHeart))[, "pureHeart"], na.rm = TRUE),
                     sum(arrange(team, desc(purePrincess))[, "purePrincess"], na.rm = TRUE),
                     sum(arrange(team, desc(pureAngel))[, "pureAngel"], na.rm = TRUE),
                     sum(arrange(team, desc(pureEmpress))[, "pureEmpress"], na.rm = TRUE))
      names(nine.pure) <- c("Pure Power", "Pure Heart", "Pure Princess", "Pure Angel", "Pure Empress")
      nine.pure <- sort(nine.pure, decreasing = TRUE)
      
      if (names(nine.pure[1]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.pure[1]), team$card.center_skill)[1], ]
        team[grep(names(nine.pure[1]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.pure[2]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.pure[2]), team$card.center_skill)[1], ]
        team[grep(names(nine.pure[2]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.pure[3]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.pure[3]), team$card.center_skill)[1], ]
        team[grep(names(nine.pure[3]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.pure[4]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.pure[4]), team$card.center_skill)[1], ]
        team[grep(names(nine.pure[4]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else {
        temp <- team[5, ]
        main <- team[grep(names(nine.pure[5]), team$card.center_skill)[1], ]
        team[grep(names(nine.pure[5]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      }
      
      guest.center <- c(sum(team[, "purePrincess"], na.rm = TRUE),
                        sum(team[, "pureAngel"], na.rm = TRUE),
                        sum(team[, "pureEmpress"], na.rm = TRUE))
      names(guest.center) <- c("Pure Princess (Pure increases based on Smile)", "Pure Angel (Pure increases drastically)", "Pure Empress (Pure increases based on Cool)")
      guest.center <- sort(guest.center, decreasing = TRUE)
      guest.center <- paste("Recommended guest center:", names(guest.center[1]))
      
    } else if (song$attribute == "Cool") {
      cards.healer <- filter(cards, skillType == "Healer", card.attribute == "Cool", card.rarity != "R", card.is_promo == "FALSE")
      cards.healer <- arrange(cards.healer, desc(coolEmpress), desc(contribution))
      
      if (nrow(cards.healer) < 9) {
        cards <- filter(cards, !(card.id %in% cards.healer$card.id))
        cards <- arrange(cards, desc(coolEmpress))
        cards.healer <- rbind(cards.healer, cards[1:(9 - nrow(cards.healer)), ])
        team <- cards.healer[, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image", "coolPower", "coolHeart", "coolPrincess", "coolEmpress", "coolAngel")]
        
      } else {
        team <- cards.healer[1:9, c("card.id", "idolized", "card.attribute", "card.center_skill", "card.round_card_image", "card.round_card_idolized_image", "coolPower", "coolHeart", "coolPrincess", "coolEmpress", "coolAngel")]
        
      }
      
      nine.cool <- c(sum(arrange(team, desc(coolPower))[, "coolPower"], na.rm = TRUE),
                     sum(arrange(team, desc(coolHeart))[, "coolHeart"], na.rm = TRUE),
                     sum(arrange(team, desc(coolPrincess))[, "coolPrincess"], na.rm = TRUE),
                     sum(arrange(team, desc(coolAngel))[, "coolAngel"], na.rm = TRUE),
                     sum(arrange(team, desc(coolEmpress))[, "coolEmpress"], na.rm = TRUE))
      names(nine.cool) <- c("Cool Power", "Cool Heart", "Cool Princess", "Cool Angel", "Cool Empress")
      nine.cool <- sort(nine.cool, decreasing = TRUE)
      
      if (names(nine.cool[1]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.cool[1]), team$card.center_skill)[1], ]
        team[grep(names(nine.cool[1]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.cool[2]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.cool[2]), team$card.center_skill)[1], ]
        team[grep(names(nine.cool[2]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.cool[3]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.cool[3]), team$card.center_skill)[1], ]
        team[grep(names(nine.cool[3]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else if (names(nine.cool[4]) %in% team$card.center_skill) {
        temp <- team[5, ]
        main <- team[grep(names(nine.cool[4]), team$card.center_skill)[1], ]
        team[grep(names(nine.cool[4]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      } else {
        temp <- team[5, ]
        main <- team[grep(names(nine.cool[5]), team$card.center_skill)[1], ]
        team[grep(names(nine.cool[5]), team$card.center_skill)[1], ] <- temp
        team[5, ] <- main
        
      }
      
      guest.center <- c(sum(team[, "coolPrincess"], na.rm = TRUE),
                        sum(team[, "coolAngel"], na.rm = TRUE),
                        sum(team[, "coolEmpress"], na.rm = TRUE))
      names(guest.center) <- c("Cool Princess (Cool increases based on Smile)", "Cool Angel (Cool increases based on Pure)", "Cool Empress (Cool increases drastically)")
      guest.center <- sort(guest.center, decreasing = TRUE)
      guest.center <- paste("Recommended guest center:", names(guest.center[1]))
      
    }
    
  }
  
  list(team, guest.center)
}

TeamImages <- function(team) {
  
  ifelse(team$idolized, 
         team$card.round_card_idolized_image,
         team$card.round_card_image)
  
}
