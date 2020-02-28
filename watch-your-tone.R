library(tidyverse)
library(tuneR)

tp = read.csv("tone-perfect.csv")

test = function(n) {
  
  indices = sample(1:9840, n, replace = FALSE)
  selected = tp[indices,] %>% pull(full.name) %>% as.character()
  answers = tp[indices,] %>% pull(pinyin) %>% as.character()
  responses = character(n)
  
  for (i in 1:n) {
    f = readMP3(selected[i])
    r = "again"
    
    while (r == "again") {
      tuneR::play(f)
      r = readline(prompt = "What tone did you hear? Type 'again' if you need a repeat: ")
    }
    
    responses[i] = r
  }
  
  report = tibble(
    answers = answers,
    responses = responses,
    correct = answers == responses
  )
  
  print(paste0("You got ", 100 * round(mean(report$correct), 2), "% correct."))
  
  return(report)
}

combine_two_tones = function(first, second, v, p) {
  tone1 = readMP3(paste0(data_path, first, "_" , v, ".mp3"))
  tone2 = readMP3(paste0(data_path, second, "_" , v, ".mp3"))
  
  n_zeroes = tp.data %>% 
    filter(pinyin == second, voice == v) %>% 
    pull(first_zeroes)
  
  start = floor(n_zeroes * p)
  
  wave = Wave(
    left = c(tone1@left, tone2@left[start:length(tone2@left)]),
    right = c(tone1@right, tone2@right[start:length(tone2@right)]),
    samp.rate = 44100,
    bit = 16,
    pcm = TRUE
  )
  return(wave)
}

# Code for turning the files into a convenient tibble
# tp.data = tibble(
#   files = list.files("./tone_perfect_voice_files")
# ) %>% 
#   mutate(
#     root = str_replace(files, ".mp3", "")
#   ) %>% 
#   separate(., root, into = c("pinyin", "voice"), sep = "_") %>% 
#   mutate(
#     idx = 1:9840,
#     full.name = as.character(list.files("./tone_perfect_voice_files", full.names = TRUE)),
#     identifier = str_replace(files, ".mp3", ""),
#     first_zeroes = unlist(map2(.x = pinyin, .y = voice,
#                                .f = check_first_zeroes)),
#     last_zeroes = unlist(map2(.x = pinyin, .y = voice,
#                               .f = check_end_zeroes)),
#     xml_path = list.files("./tone_perfect_xml",
#                           pattern = "*CUSTOM.xml",
#                           full.names = TRUE),
#     xml_data = map(xml_path, xmlParse),
#     xml_list = map(xml_data, xmlToList),
#     is_lexical_gap = unlist(map(xml_list, function(x) { 
#       ifelse(x[["lexical_gap"]] == "No", FALSE, TRUE)
#     }))
#   ) %>% 
#   select(
#     idx, full.name, pinyin, identifier, first_zeroes, last_zeroes, is_lexical_gap
#   )
# 
# write.csv(tp.data, "tone-perfect.csv")
