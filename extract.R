library(readr)
library(stringr)
library(stringi)
library(openxlsx)
library(dplyr)
library(rio)
library(utf8)
library(textfeatures)
library(quanteda)
library(qdap)
library(corpus)

time_morning <- function(string){
  time_email <- str_extract(string, "[0-9]{2}:[0-9]{2}:[0-9]{2}")
  hour <- as.numeric(str_split(time_email, ":")[[1]][1])
  if(hour <= 9){
    output = 1
  }
  else{
    output = 0
  }
  return(output)
}

time_working <- function(string) {
  time_email <- str_extract(string, "[0-9]{2}:[0-9]{2}:[0-9]{2}")
  hour <- as.numeric(str_split(time_email, ":")[[1]][1])
  if(hour > 9 & hour <= 18){
    output = 1
  }
  else{
    output = 0
  }
  return(output)
}

time_night <- function(string) {
  time_email <- str_extract(string, "[0-9]{2}:[0-9]{2}:[0-9]{2}")
  hour <- as.numeric(str_split(time_email, ":")[[1]][1])
  if(hour > 19){
    output = 1
  }
  else{
    output = 0
  }
  return(output)
}

extract_sent <- function(string) {
  str_count(string, ", ") + 1
}

extract_image <- function(string) {
  str_count(string, "IMAGE")
}


clean_body_pre <- function(message) {
  extract <- str_split(message, "-----Original Message-----")[[1]][1]
  extract <- tail(str_split(extract, "\n")[[1]], -16)
  extract <- do.call(paste, c(as.list(extract), sep = " "))
  if(length(extract) > 0) {
  extract <- str_split(extract, ".pst")[[1]][length(str_split(extract, ".pst")[[1]])]
  extract <- str_replace_all(extract, "=\r|\r", "")
  extract <- str_replace_all(extract, "X-FileName: JSKILLIN (Non-Privileged).pst", "")
  return(extract)
  }
  else {
    return(extract)
  }
}

clean_body_post <- function(message) {
  extract <- str_replace_all(message, "</O=ENRON/OU=NA/CN=RECIPIENTS/CN=[A-Za-z0-9]*>", "")
  extract <- str_replace_all(extract, "[[:punct:]]*[a-z|0-9|.|_]*@([A-Za-z]*.[a-z]+)+[[:punct:]]*","")
  extract <- str_replace_all(extract, "<<.*?>>", " ")
  extract <- str_replace_all(extract, "<.*?>", " ")
  extract <- str_replace_all(extract, "[[:space:]]+", " ")
  extract <- str_replace_all(extract, "[[:punct:]]|_|[$]", "")
  extract <- tolower(extract)
  extract <- rm_stopwords(extract, stopwords = tm::stopwords("english"), unlist = FALSE, separate = FALSE)
  stemmed <- text_tokens(extract, stemmer = "en")
  extract <- stri_join_list(stemmed, sep = " ", collapse = NULL)
  extract <- str_replace_all(extract, "http[A-Za-z0-9]*", "")
  extract <- str_replace_all(extract, "[0-9]+", "")
  extract <- str_replace_all(extract, "n9q|(nbsp)+|xfilenam|xfrom|xcc|xbcc|xfolder|[+=<>]", "")
  extract <- str_replace_all(extract, "\\b[a-z]\\b", "")
  extract <- str_replace_all(extract, "[[:space:]]+", " ")
  return(extract)
}

extract_subject <- function(message) {
  extract <- str_replace(message, "Subject: ", "")
  extract <- str_replace(extract, "R[Ee]: |\r|FW: |Fwd: ", "")
  extract <- str_replace_all(extract, "[[:punct:]]*[a-z|0-9|.|_]*@([A-Za-z]*.[a-z]+)+[[:punct:]]*","")
  extract <- str_replace_all(extract, "[[:space:]]+", " ")
  extract <- str_replace_all(extract, "[[:punct:]]|[$]|[<>]", "")
  extract <- str_replace_all(extract, "[0-9]+", "")
  stemmed <- text_tokens(extract, stemmer = "en")
  extract <- stri_join_list(stemmed, sep = " ", collapse = NULL)
  return(extract)
}

check_forward <- function(message) {
  ifelse(grepl( "Fwd|FW", message),1,0)
}

check_reply <- function(message) {
  ifelse(grepl( "R[Ee]:", message),1,0)
}



data <- data.frame(Person_Name=character(),
                            Type= character(),
                            Email=character(),
                            stringsAsFactors=FALSE)

setwd("C:/Users/aditw/OneDrive/Desktop/Adit - Backup/Adit/Ashoka/Semester-7/Unstructured Information Processing/Project/maildir")
files = list.files()
for (person in files) {
  inbox <- list.files(path = paste0(person, "/inbox"))
  len <- length(inbox)
  if (len > 300) {
    inbox <- inbox[1:300]
  }
  for (email in inbox) {
   text <- read_file(paste0(person, "/inbox/", email))
   data[nrow(data)+1,] <- c(person, 0, text)
  }
  sent <- list.files(path = paste0(person, "/sent"))
  len <- length(sent)
  if (len > 300) {
    sent <- sent[1:300]
  }
  for (email in sent) {
    text <- read_file(paste0(person, "/sent/", email))
    data[nrow(data)+1,] <- c(person, 1, text)
  }
  delete <- list.files(path = paste0(person, "/deleted_items"))
  len <- length(delete)
  if (len > 350) {
    delete <- delete[1:350]
  }
  for (email in delete) {
    text <- read_file(paste0(person, "/deleted_items/", email))
    data[nrow(data)+1,] <- c(person, 2, text)
  }
}

dataset <- filter(data, Email != "")

dataset$Time_Morning <- lapply(dataset$Email, function(x) {
  time_morning(str_split(x, "\n")[[1]][2])
})

dataset$Time_Working <- lapply(dataset$Email, function(x) {
  time_working(str_split(x, "\n")[[1]][2])
})


dataset$Time_Night <- lapply(dataset$Email, function(x) {
  time_night(str_split(x, "\n")[[1]][2])
})

dataset$Deleted_Email <- ifelse(dataset$Type == 2, 1, 0)

dataset$Number_Sent <- lapply(dataset$Email, function(x) {
  extract_sent(str_replace(str_split(x, "\n")[[1]][4], " \r", ""))
})

dataset$Subject <- lapply(dataset$Email, function(x) {
  str_split(x, "\n")[[1]][5]
})

dataset$Forward <- lapply(dataset$Subject, function(x) {
  check_forward(x)
})

dataset$Reply <- lapply(dataset$Subject, function(x) {
  check_reply(x)
})

dataset$Subject_Cleaned <- lapply(dataset$Subject, function(x) {
  extract_subject(x)
})


dataset$Message <- lapply(dataset$Email, function(x) {
  clean_body_pre(x)
})

dataset$Images <- lapply(dataset$Message, function(x) {
  extract_image(x)
})


dataset$Message<- lapply(dataset$Message, function(x){
  as_utf8(x)
})

dataset$Message<- as.character(dataset$Message)


features <- as.data.frame(textfeatures(as.character(unique$Message), normalize = FALSE))
features <- select(features, c(1:28))

dataset$Cleaned_Email <- lapply(dataset$Message, function(x) {
  clean_body_post(x)
})

dataset$Cleaned_Email <- as.character(dataset$Cleaned_Email)

dataset <- filter(dataset, Cleaned_Email != "")

unique <- dataset %>% group_by(Cleaned_Email) %>% slice(1)

unique$Cleaned_Email <- lapply(unique$Cleaned_Email, function(x) {
  str_replace_all(x, "[-~]", "" )
})

unique_final <- cbind(as.data.frame(unique), features)

write.xlsx(unique_final, "../Unique2.xlsx")
