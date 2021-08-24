library(shiny)
library(markdown)
library(shinythemes)
library(plotly)
# devtools::install_github(‘nstrayer/shinysense’, ref=‘v1.00’)
library(shinysense)

#Load our terms content to put into the terms popup
source("terms_content.R")

navbarPage(
  title = div(img(src="images/dark_logo_small.png", style="margin-top: -8px;", height = 40)),
  tabPanel("Rate",
           div(id = "rate_wrapper",
               sidebarLayout(
                 sidebarPanel(width = 4, id = "rate_info",
                              tags$head(
                                tags$script(src = "touchSwipe.js"),
                                tags$script(src = "shinySwiper.js"),
                                tags$link(rel = "stylesheet", type = "text/css", href = "appStyle.css")
                              ),
                              h4("Swipe abstract to rate the paper"),
                              hr(),
                              HTML(
                                "<table style='line-height:1.5em;'>
                                <tr>
                                <td style='font-weight:normal;'><img src = 'images/swipe_right.png', style='height: 40px'></td>
                                <td style='font-weight:normal;'> R/Pharma for sure!
                                </td>
                                </tr>
                                <tr>
                                <td><img src = 'images/swipe_up.png', style='height: 40px'></td>
                                <td> Maybe...
                                </td>
                                </tr>
                                <tr>
                                <td><img src = 'images/swipe_down.png', style='height: 40px'></td>
                                <td> Skip this one
                                </td>
                                </tr>
                                <tr>
                                <td><img src = 'images/swipe_left.png', style='height: 40px'></td>
                                <td> Do not recomend for R/Pharma
                                </td>
                                </tr>
                                </table>"
                              ),
                              hr(),
                              h4("Rate papers & level up:"),
                              uiOutput("icon"),
                              em(textOutput("level")),
                              h4("Download your ratings:"),
                              downloadButton("download_data", "Download"),
                              h4("Tell someone about papr:"),
                              a(href = "https://twitter.com/intent/tweet?text=Check%20out%20papr%20its%20like%20Tinder%20for%20preprints%20https://jhubiostatistics.shinyapps.io/papr", icon("twitter")),
                              a(href = "https://www.facebook.com/sharer/sharer.php?u=https%3A//jhubiostatistics.shinyapps.io/papr", icon("facebook"))
                              ),
                 mainPanel(fluidPage(
                   shinypopupUI("terms",
                                buttonText = "I understand, let's get swiping!",
                                popupDiv = terms_content_div,
                                 div(id = "swipeCard", class = "card",
                                     h3(id = "cardTitle", "Title"),
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
  windowTitle = "papr - peer review, but easier"
)
