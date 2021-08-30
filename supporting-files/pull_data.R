# get responses
library(tidyverse)

dat <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1yP8MbqSKku8nmEM1AxmEEBqBGTHPBjmeZwWb95UM-UQ/edit#gid=870824569",
  sheet = "TEST"
  ) %>%
  select(
    title = `What is the title of your talk?`,
    abstract = Abstract,
    submitted = `Submitted At`,
    types = `Which types of talk would you like to be considered for?`,
    affaliation = `{{field:47d88af0-489e-4c10-90a3-eea41eb6c246}}, what is your affiliation?`,
    speaker = Speaker,
    email = Email,
    topic = Topic
  ) %>%
  mutate(
    index = row_number(),
    submitted = as.Date(submitted),
    byline = glue::glue("{speaker} ({affaliation}) wishes this abstract to be considered for {types}")
  )

saveRDS(dat,"data.rds")
