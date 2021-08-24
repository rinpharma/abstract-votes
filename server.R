library(readr)
library(lubridate)
library(googlesheets)
library(rdrop2)
library(tidyr)
library(dplyr)
library(shiny)
library(shinysense)

#source("google_api_info.R")

#token <- drop_auth()
#saveRDS(token, "papr-drop.rds")

# get responses
  # dat <- googlesheets4::read_sheet(
  #   "https://docs.google.com/spreadsheets/d/1yP8MbqSKku8nmEM1AxmEEBqBGTHPBjmeZwWb95UM-UQ/edit#gid=870824569",
  #   sheet = "TEST"
  #   ) %>%
  #   select(
  #     title = `What is the title of your talk?`,
  #     abstract = Abstract,
  #     submitted = `Submitted At`,
  #     types = `Which types of talk would you like to be considered for?`,
  #     affaliation = `{{field:47d88af0-489e-4c10-90a3-eea41eb6c246}}, what is your affiliation?`,
  #     speaker = Speaker,
  #     email = Email,
  #     topic = Topic
  #   ) %>%
  #   mutate(
  #     index = row_number(),
  #     submitted = as.Date(submitted),
  #     byline = glue::glue("{speaker} ({affaliation}) wishes this abstract to be considered for {types}")
  #   )
  #
  # saveRDS(dat,"data.rds")

## set some parameters
level_up <- 4 #Number of papers needed to review to level up.

shinyServer(function(input, output, session) {

  ## load data
  dat <- readRDS("data.rds") #R dataset of paper info
  load("./term_pca_df.Rda") #R dataset of paper PCA
  token <- readRDS("./papr-drop.rds")

  ## set up user data
  session_id <- as.numeric(Sys.time())

  ## variables that get updated throughout the session.
  ## need to be wrapped in reactiveValues to make sure their updates propigate
  rv <- reactiveValues(
    login = TRUE,
    person_id = 12345,
    counter = -1,
    name = NA,
    user_dat = data.frame(
      index   = NA,
      title   = NA,
      speaker    = NA,
      session = NA,
      result  = NA,
      person  = NA
    )
  )

  ## Login
  observeEvent(input$login_button, {
    rv$person_id <- digest::digest(input$name)
    warning(rv$person_id)
  })

  ## make a popup that alerts the user that we have super important data terms
  ## don't show the popup if the user is logged in though.

  make_popup <- callModule(shinypopup, "terms", accepted = FALSE)

  ## create temp csv that we use to track session
  file_path <- file.path(tempdir(), paste0(round(session_id), ".csv"))
  write_csv(isolate(rv$user_dat), file_path)

  ##########################################################################
  ## functions
  ##########################################################################

  ## function to rate a paper
  rate_paper <- function(choice, file_path, rv) {
    ## is this the first time the paper is being run?
    initializing <- choice == "initializing"

    ## are they deciding?
    deciding <- choice == "deciding"

    ## index of all papers in data
    vals <- dat %>%
      select(index, submitted)

    if (initializing) {
      ## grab our first paper!
      ## sample from all indeces, set probability to date
      ## so that it will favor newer ones
      new_ind <- sample(vals$index, 1, prob = vals$submitted)
    } else {
      ## randomly grab a new paper but ignore the ones we've read
      val <- vals[ - which(vals$index %in% isolate(rv$user_dat$index)), ]
      new_ind <- sample(val$index, 1, prob = val$submitted)
    }
    ## make a new row for our session data.
    new_row <- data.frame(
      index   = new_ind,
      title   = dat$title[dat$index == new_ind],
      speaker    = dat$speaker[dat$index == new_ind],
      session = session_id,
      result  = NA,

      person  = isolate(rv$person_id)

      # running
    )

    if (initializing) {
      ## if this is the first time we're running the function
      ## create the dataframe for session
      ## add new empty row the csv
      rv$user_dat <- new_row
    } else {
      ## if this is a normal rating after initialization append a
      ## new row to our session df
      ## put the last review into the review slot of their data.
      rv$user_dat[1, 5] <- choice
      ## add a new empty row to dataframe.
      rv$user_dat <- rbind(new_row, rv$user_dat)
    }

    write_csv(isolate(rv$user_dat), file_path) #write the csv
    drop_upload(file_path, "rinpharma/2021/call4papers/", dtoken = token) #upload to dropbox too.

    file_path2 <- file.path(tempdir(),
                            paste0("user_dat_",isolate(rv$person_id), ".csv")
    )
    # write_csv(data.frame(name = isolate(input$name),
    #                      twitter = isolate(input$twitter),
    #                      PC1 = isolate(rv$pc[1]),
    #                      PC2 = isolate(rv$pc[2]),
    #                      PC3 = isolate(rv$pc[3])),
    #           file_path2)
    #drop_upload(file_path2,"shiny/2016/papr/user_dat/", dtoken = token)

    return(new_ind)
  }

  level_func = function(x, level_up) {
    if (x < level_up) {
      return("Undergrad")
    }
    if (x == level_up) {
      return("Congrats grad!")
    }
    if (x < (2 * level_up)) {
      return("Grad Student")
    }
    if (x == (2 * level_up)) {
      return("Doctor?...Doctor.")
    }
    if (x < (3 * level_up)) {
      return("Postdoc")
    }
    if (x == (3 * level_up)) {
      return("Booyah tenure track!")
    }
    if (x < (4 * level_up)) {
      return("Assistant Prof")
    }
    if (x == (4 * level_up)) {
      return("Tenure baby!")
    }
    if (x < (5 * level_up)) {
      return("Associate Prof")
    }
    if (x == (5 * level_up)) {
      return("Top of the pile!")
    }
    if (x > (5 * level_up)) {
      return("Full Prof")
    }
  }

  icon_func = function(x, level_up) {
    if (x < level_up) {
      return(icon("user"))
    }
    if (x == level_up) {
      return(icon("graduation-cap"))
    }
    if (x < (2 * level_up)) {
      return(icon("graduation-cap"))
    }
    if (x == (2 * level_up)) {
      return(icon("coffee"))
    }
    if (x < (3 * level_up)) {
      return(icon("coffee"))
    }
    if (x == (3 * level_up)) {
      return(icon("briefcase", lib = "glyphicon"))
    }
    if (x < (4 * level_up)) {
      return(icon("briefcase", lib = "glyphicon"))
    }
    if (x == (4 * level_up)) {
      return(icon("university"))
    }
    if (x < (5 * level_up)) {
      return(icon("university"))
    }
    if (x == (5 * level_up)) {
      return(icon("tower", lib = "glyphicon"))
    }
    if (x > (5 * level_up)) {
      return(icon("tower", lib = "glyphicon"))
    }
  }

  ##########################################################################
  ##########################################################################

  ## start datastore and display user's Google display name after successful login
  output$display_username <- renderText({

    if(rv$login){
      paste("You're logged in with Google!") ## return name after validation
    } else {
      "Log in to keep track of rankings!"
    }
  })


  ## on the interaction with the swipe card do this stuff
  observeEvent(input$cardSwiped, {
    ## get swipe results from javascript
    swipeResults <- input$cardSwiped

    if (!(swipeResults %in% c("skipped", "deciding"))) {
      ## send this swipe result to the rating function to get a new index
      ## for a new paper
      ind <- rate_paper(swipeResults, file_path, rv)
      ## grab info on new paper
      selection <- dat[ind, ]
      ## send it over to javascript
      session$sendCustomMessage(type = "sendingpapers", selection)
      rv$counter = rv$counter + 1
    }
  })

  ## on each rating or skip send the counter sum to update level info.
  nextPaper <- reactive({
    rv$counter
  })
  output$level <- renderText(level_func(nextPaper(), level_up))
  output$icon  <- renderUI(icon_func(nextPaper(), level_up))

  # Let people download
  output$download_data <- downloadHandler(
    filename = "my_ratings.csv",
    content = function(file) {
      udat = rv$user_dat %>%
        filter(!is.na(result))
        # mutate(result = replace(result, result == "skipped", NA)) %>%
        # separate(result,
        #          into = c("exciting", "questionable"),
        #          sep = " and ") %>%
        # transmute(title, link, exciting, questionable, session) %>%
        # mutate(user_id = session) %>% select(-session)
      write.csv(udat, file)
    }
  )

})
