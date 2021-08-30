library(shiny)
library(markdown)
library(shinythemes)
library(plotly)
# devtools::install_github(‘nstrayer/shinysense’, ref=‘v1.00’)
library(shinysense)

#Load our terms content to put into the terms popup
source("terms_content.R")

navbarPage(
  title = div(img(src="images/logo.png", style="margin-top: -8px;", height = 40)),
  tabPanel("Rate",
           div(id = "rate_wrapper",
               sidebarLayout(
                 sidebarPanel(
                              width = 4, id = "rate_info",
                              tags$head(
                                tags$script(src = "touchSwipe.js"),
                                tags$script(src = "shinySwiper.js"),
                                tags$link(rel = "stylesheet", type = "text/css", href = "appStyle.css")
                              ),
                              h4("Swipe abstract to rate the submission"),
                              hr(),
                              textOutput("authenticated"),
                              hr(),
                              p("To vote, please swipe on touch screens,
                                or click and drag in the directions specified below."),
                              HTML(
                                "<table style='line-height:1.5em;'>
                                <tr>
                                <td style='font-weight:normal;'><img src = 'images/swipe_right.png', style='height: 40px'></td>
                                <td style='font-weight:normal;'> R/Pharma for sure!
                                </td>
                                </tr>
                                <tr>
                                <td><img src = 'images/swipe_up.png', style='height: 40px'></td>
                                <td> I'll sit on the fence
                                </td>
                                </tr>
                                <tr>
                                <td><img src = 'images/swipe_down.png', style='height: 40px'></td>
                                <td> I will skip this one
                                </td>
                                </tr>
                                <tr>
                                <td><img src = 'images/swipe_left.png', style='height: 40px'></td>
                                <td> Not an ideal talk for R/Pharma
                                </td>
                                </tr>
                                </table>"
                              ),
                              hr(),
                              h4("Abstracts reviewed"),
                              textOutput("togo"),

                              h4("Rate abstracts & level up:"),
                              uiOutput("icon"),
                              em(textOutput("level")),
                              h4("Download your ratings:"),
                              downloadButton("download_data", "Download")
                              ),
                 mainPanel(fluidPage(
                    shinypopupUI("terms",
                                 buttonText = "Log me in",
                                 popupDiv = terms_content_div,
                     div(id = "swipeCard", class = "card",
                         h3(id = "cardTitle", "Title"),
                         hr(),
                         p(id = "cardByline", "Author content"),
                         hr(),
                         p(id = "cardAbstract", "Abstract content")
                     )
                   )#end popup
                 ))
                 )
               )
          ),
  tabPanel("About",
           fluidPage(
             div(id = "about", class = "card",
                 includeMarkdown("./about.md")
             )
             )
           ),
  tabPanel("Help",
           fluidPage(
             div(class = "card",
              includeMarkdown("./help.md")
              )
             )
           ),
  collapsible = TRUE,
  windowTitle = "R/Pharma C4Paper review"
)
