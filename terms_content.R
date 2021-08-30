terms_content_div <- div(
  h1("Login"),
  hr(),
  textInput(
    inputId = "login_string",
    label = "Please provide a name or identifier to assign to your votes",
    value = NA),
  hr(),
  p("By default you will be asked to review all abstracts. Optionally you can remove topics you are not interested in reviewing from the list below before logging in"),
  p("Leave blank to test the app (blank entries will not be scored)."),
  textOutput("filtered_data_count_text"),
  selectInput(
    inputId = "topics_filter", label = "",
    choices = sort(unique(dat$topic)), selected = sort(unique(dat$topic)),
    multiple = TRUE
  )
)
