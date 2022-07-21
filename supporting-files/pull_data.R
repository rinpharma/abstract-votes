# get responses
library(dplyr)

dat <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1s-5VD_QfjA6r4r8ebvsLnNj8xuOGlTd3chbdKI0i2K8/edit#gid=0",
  sheet = "raw_call4papers"
  )

dat <- dat %>%
  select(
    title = `What is the title of your talk?`,
    abstract = `Please provide an abstract`,
    submitted = `Timestamp`,
    types = `Which types of talk would you like to be considered for?`,
    affaliation = `What company or institution are you affiliated with?`,
    speaker = `What is the speaker's name?`,
    email = `Email Address`,
    topic = Topic
  ) %>%
  mutate(
    index = row_number(),
    submitted = as.Date(submitted),
    byline = glue::glue("{speaker} ({affaliation}) wishes this abstract to be considered for {types}")
  )

saveRDS(dat,"data.rds")

