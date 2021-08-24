terms_content_div <- div(
  h1("Login"),
  hr(),
  textInput(
    inputId = "login_string",
    label = "Please provide the login string emailed to you",
    value = NA),
  p("Right now authentication is optional, and the app will record data to the back end regardless of what password you put in.")
)
