library(shiny)
library(jsonlite)
library(dplyr)
library(curl)
library(shinythemes)
source("functions.R")

shinyServer(function(input, output){
  
  all.songs <- eventReactive(input$fetchAccounts, {
    GetAllSongs()
  })
  
  fetchAccounts <- eventReactive(input$fetchAccounts, {
    GetAccount(input$username)
  })
  
  songlist <- reactive({
    if (input$songDifficulty == "Expert") {
      return(subset(all.songs(), all.songs()$attribute == input$songAttribute & 
                      !is.na(all.songs()$expert_notes)))
    } else if (input$songDifficulty == "Easy") {
      return(subset(all.songs(), all.songs()$attribute == input$songAttribute & 
                               !is.na(all.songs()$easy_notes)))
    } else if (input$songDifficulty == "Normal") {
      return(subset(all.songs(), all.songs()$attribute == input$songAttribute & 
                      !is.na(all.songs()$normal_notes)))
    } else if (input$songDifficulty == "Hard") {
      return(subset(all.songs(), all.songs()$attribute == input$songAttribute & 
                      !is.na(all.songs()$hard_notes)))
    } else {
      return(subset(all.songs(), all.songs()$attribute == input$songAttribute & 
                      !is.na(all.songs()$master_notes)))
    }
  })
  
  acntchoices <- reactive({
    return(fetchAccounts()$fullname)
  })
  
  output$accountid <- renderUI({
    selectInput("accountid", "Select SIF Account", acntchoices())
  })
  
  output$songlist <- renderUI({
    selectInput("song", "Select Song", songlist()$romaji_name)
  })
  
  cards <- eventReactive(input$createTeam, {
    GetAccountCards(input$accountid, fetchAccounts())
  })
  
  song <- eventReactive(input$createTeam, {
    return(input$song)
  })
  
  final <- eventReactive(input$createTeam, {
    
    # For songs whose star notes count deviate for the standard counts
    song.sp <- data.frame(c(11, 35, 65),
                          c(11, 34, 62),
                          c(11, 35, 65),
                          c(12, 40, 65))
    
    colnames(song.sp) <- c("Bokura no LIVE Kimi to no LIFE",
                           "Yuujou No Change",
                           "Snow halation",
                           "baby maybe Koi no Button")
    
    # Gets song data based on song choice input
    song <- songlist()[which(songlist()$romaji_name == input$song), ]
    song.notes <- if (input$songDifficulty == "Easy") {
      song$easy_notes
    } else if (input$songDifficulty == "Normal") {
      song$normal_notes
    } else if (input$songDifficulty == "Hard") {
      song$hard_notes
    } else if (input$songDifficulty == "Expert") {
      song$expert_notes
    } else {
      song$master_notes
    }
    song.length <- song$time
    
    # Assigns song's star notes count, taking into account special songs listed
    # in song.sp, and daily songs
    song.stars <- if (input$songDifficulty == "Easy") {
      0
    } else if (input$songDifficulty == "Normal") {
      if (input$song %in% colnames(song.sp)) {
        song.sp[1, grep(input$song, colnames(song.sp))]
      } else if (!is.na(song$daily_rotation)) {
        17
      } else {
        14
      }
    } else if (input$songDifficulty == "Hard") {
      
      if (input$song %in% colnames(song.sp)) {
        song.sp[2, grep(input$song, colnames(song.sp))]
      } else if (!is.na(song$daily_rotation)) {
        52
      } else {
        50
      }
    } else if (input$songDifficulty == "Expert") {
      if (input$song %in% colnames(song.sp)) {
        song.sp[3, grep(input$song, colnames(song.sp))]
      } else if (!is.na(song$daily_rotation)) {
        70
      } else {
        65
      }
    } else {
      70 #MASTER difficulty
    }
    
    song.unit <- song$main_unit
    
    approx.str <- 50000 # Approx estimation, needed for activateType score cards
    percentPerfect <- 0.01 * input$percentPerfect
    
    is.4.0 <- if(fetchAccounts()[which(fetchAccounts()$fullname == input$accountid), "language"] == "JP") {
      TRUE
    } else {
      FALSE
    }
    
    # Checks non-N-rarity cards for missing card skill details
    # Stops app and give error message if there are any
    # Continues rest of app execution if there are no missing data
    if (sum(is.na(cards()[which(cards()$card.rarity != "N"), "card.skill_details"])) > 0 | 
        nrow(cards()[which(cards()$card.rarity != "N" & cards()$card.skill_details == ""), ]) > 0) {
      
      stop("One or more cards in the account has missing skill details. ",
           "Please note that it takes a few days for the skill data of newly added JP cards to be entered into the schoolido.lu database. ",
           "The ids of the erroneous cards are: ",
           cards()[which(cards()$card.rarity != "N" & (is.na(cards()$card.skill_details) | cards()$card.skill_details == "")), c(3:5)])
      
    } else {
      
      cards <- CleanCards(cards(), percentPerfect, song.notes, song.length, song.stars, approx.str)
      cards <- CalculateFinalStats(cards, song.unit, is.4.0)
      cards.check <- CheckCards(cards, song)
      
      # Checks cards for activateType score cardsl; re-runs card stats
      # calculation with new total team strength data for increased accuracy
      if (cards.check[[2]]) {
        approx.str <- cards.check[[1]]
        cards <- CleanCards(cards(), percentPerfect, song.notes, song.length, song.stars, approx.str)
        cards <- CalculateFinalStats(cards, song.unit, is.4.0)
      }
      
      team <- FormTeam(cards, song, input$teamType)
      guest.center <- team[[2]]
      team <- team[[1]]
      final <- TeamImages(team)
      team <- team[, -(5:6)]
      
      # For HTML display of round card images
      card.image <- paste0("<img src = '", final, "' height = '86' width = '86'></img>")
      
      return(list(card.image, guest.center, team, cards))
      
    }
    
    
    
  })
  
  output$final <- renderUI({
    HTML(final()[[1]][1:9])
  })
  
  output$guestCenter <- renderText({
    final()[[2]]
  })
  
  output$rawdata <- renderDataTable({
    final()[[3]]
  })
  
  output$rawcards <- renderDataTable({
    final()[[4]]
  })
  
})