require("stringr")
require("lubridate")
require("dplyr")
require("rvest")
require("pdftools")
require("tesseract")




# set var

url = "https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports/"
report_url = url %>% read_html %>% html_nodes(xpath = '//*[@id="PageContent_C006_Col01"]/div[1]') %>% 
  html_nodes("a") %>% html_attr("href") %>% unique() %>% sort()

start_date = "2020-01-20" %>% ymd() - 1

table = data.frame()




# get data

for(i in 1:length(report_url)){
  if(i >= 28){
    page_url = str_c("https://www.who.int", report_url[i])
    page_index = pdf_text(page_url) %>% str_which("SURVEILLANCE")
    page_text = pdf_text(page_url)[page_index] %>% str_replace_all("\\r\\n", "|") %>% 
      str_split("\\|") %>% unlist() %>% str_split("[:space:]{2}") %>% unlist() %>% stringi::stri_remove_empty()
    row_len = page_text[seq(page_text %>% str_which("Hubei") %>% head(1), page_text %>% str_which("Total") %>% tail(1))] %>% 
      str_remove_all("\\*") %>% 
      str_remove_all("[:digit:]+") %>% str_remove_all("\\-") %>% 
      str_remove_all("^ $") %>% stringi::stri_remove_empty() %>% length()
    col = 7
    
    table = table %>% 
      bind_rows(data.frame(date = rep(start_date + i + 1, row_len),
                           region = rep("Western Pacific", row_len),
                           nation = rep("China", row_len),
                           city = page_text[seq(page_text %>% str_which("Hubei") %>% head(1), page_text %>% str_which("Total") %>% tail(1))] %>% 
                             str_remove_all("\\*") %>% 
                             str_remove_all("[:digit:]+") %>% str_remove_all("\\-") %>% 
                             str_remove_all("^ $") %>% stringi::stri_remove_empty(),
                           confirmed_daily = page_text[seq(page_text %>% str_which("Hubei") %>% head(1) + 2, page_text %>% str_which("Total") %>% tail(1) + col - 1, col)],
                           suspected_daily = page_text[seq(page_text %>% str_which("Hubei") %>% head(1) + 3, page_text %>% str_which("Total") %>% tail(1) + col - 1, col)],
                           death_daily = page_text[seq(page_text %>% str_which("Hubei") %>% head(1) + 4, page_text %>% str_which("Total") %>% tail(1) + col - 1, col)],
                           confirmed_cumulative = page_text[seq(page_text %>% str_which("Hubei") %>% head(1) + 5, page_text %>% str_which("Total") %>% tail(1) + col - 1, col)],
                           death_cumulative = page_text[seq(page_text %>% str_which("Hubei") %>% head(1) + col - 1, page_text %>% str_which("Total") %>% tail(1) + col - 1, col)]
                          ) %>% 
                  mutate(region = as.character(region),
                         nation = as.character(nation),
                         city = as.character(city),
                         confirmed_daily = confirmed_daily %>% as.character() %>% as.numeric(),
                         suspected_daily = suspected_daily %>% as.character() %>% as.numeric(),
                         death_daily = death_daily %>% as.character() %>% as.numeric(),
                         confirmed_cumulative = confirmed_cumulative %>% as.character() %>% as.numeric(),
                         death_cumulative = death_cumulative %>% as.character() %>% as.numeric())
      )
  } # 26 ~ 27
  else if(26 <= i && i <= 27){
    page_url = str_c("https://www.who.int", report_url[i])
    page_index = pdf_text(page_url) %>% str_which("SURVEILLANCE")
    page_text = pdf_text(page_url)[page_index] %>% str_replace_all("\\r\\n", "  ") %>% 
      str_split("[:space:]{2,}") %>% unlist() %>% stringi::stri_remove_empty()
    page_text = ifelse((page_text %>% str_detect(" ") * page_text %>% str_detect("[:digit:]") == 1), str_split(page_text, " "), page_text) %>% unlist()
    col = 11
    row_len = 35
    
    table = table %>% 
      bind_rows(data.frame(date = rep(start_date + i + 1, row_len),
                           region = rep("Western Pacific", row_len),
                           nation = rep("China", row_len),
                           city = page_text[seq(page_text %>% str_which("Hubei"), page_text %>% str_which("Total") %>% tail(1))] %>% 
                             str_remove_all("[:digit:]+") %>% str_remove_all("\\-") %>% stringi::stri_remove_empty(),
                           confirmed_daily = page_text[seq(page_text %>% str_which("Hubei") + 4, page_text %>% str_which("Total") %>% tail(1) + col - 1, col)],
                           suspected_daily = page_text[seq(page_text %>% str_which("Hubei") + 5, page_text %>% str_which("Total") %>% tail(1) + col - 1, col)],
                           death_daily = page_text[seq(page_text %>% str_which("Hubei") + 6, page_text %>% str_which("Total") %>% tail(1) + col - 1, col)],
                           confirmed_cumulative = page_text[seq(page_text %>% str_which("Hubei") + 9, page_text %>% str_which("Total") %>% tail(1) + col - 1, col)],
                           death_cumulative = page_text[seq(page_text %>% str_which("Hubei") + 10, page_text %>% str_which("Total") %>% tail(1) + col - 1, col)]
                          ) %>% 
                  mutate(region = as.character(region),
                         nation = as.character(nation),
                         city = as.character(city),
                         confirmed_daily = confirmed_daily %>% as.character() %>% as.numeric(),
                         suspected_daily = suspected_daily %>% as.character() %>% as.numeric(),
                         death_daily = death_daily %>% as.character() %>% as.numeric(),
                         confirmed_cumulative = confirmed_cumulative %>% as.character() %>% as.numeric(),
                         death_cumulative = death_cumulative %>% as.character() %>% as.numeric())
               )
  } # 25
  else if(i == 25){
    page_url = str_c("https://www.who.int", report_url[i])
    page_index = pdf_text(page_url) %>% str_which("SURVEILLANCE")
    page_text = pdf_text(page_url)[page_index] %>% str_replace_all("\\r\\n", "  ") %>% 
      str_split("[:space:]{2,}") %>% unlist() %>% stringi::stri_remove_empty()
    page_text = ifelse((page_text %>% str_detect(" ") * page_text %>% str_detect("[:digit:]") == 1), str_replace_all(page_text, " ", ""), page_text)
    col = 11
    row_len = 35
    
    table = table %>% 
      bind_rows(data.frame(date = rep(start_date + i + 1, row_len),
                           region = rep("Western Pacific", row_len),
                           nation = rep("China", row_len),
                           city = page_text[seq(page_text %>% str_which("Hubei"), page_text %>% str_which("Total") %>% tail(1))] %>% 
                             str_remove_all("[:digit:]+") %>% str_remove_all("\\-") %>% stringi::stri_remove_empty(),
                           confirmed_daily = page_text[seq(page_text %>% str_which("Hubei") + 4, page_text %>% str_which("Total") %>% tail(1) + col - 1, col)],
                           suspected_daily = page_text[seq(page_text %>% str_which("Hubei") + 5, page_text %>% str_which("Total") %>% tail(1) + col - 1, col)],
                           death_daily = page_text[seq(page_text %>% str_which("Hubei") + 6, page_text %>% str_which("Total") %>% tail(1) + col - 1, col)],
                           confirmed_cumulative = page_text[seq(page_text %>% str_which("Hubei") + 9, page_text %>% str_which("Total") %>% tail(1) + col - 1, col)],
                           death_cumulative = page_text[seq(page_text %>% str_which("Hubei") + 10, page_text %>% str_which("Total") %>% tail(1) + col - 1, col)]
      ) %>% 
        mutate(region = as.character(region),
               nation = as.character(nation),
               city = as.character(city),
               confirmed_daily = confirmed_daily %>% as.character() %>% as.numeric(),
               suspected_daily = suspected_daily %>% as.character() %>% as.numeric(),
               death_daily = death_daily %>% as.character() %>% as.numeric(),
               confirmed_cumulative = confirmed_cumulative %>% as.character() %>% as.numeric(),
               death_cumulative = death_cumulative %>% as.character() %>% as.numeric())
      )
  }
  # 24 
  else if(i == 24){
    page_url = str_c("https://www.who.int", report_url[i])
    page_index = pdf_text(page_url) %>% str_which("SURVEILLANCE")
    page_text = pdf_text(page_url)[page_index] %>% str_replace_all("\\r\\n", "  ") %>% 
      str_split("[:space:]{2,}") %>% unlist() %>% stringi::stri_remove_empty()
    page_text = ifelse((page_text %>% str_detect(" ") * page_text %>% str_detect("[:digit:]") == 1), str_remove(page_text, " "), page_text)
    col = 3
    row_len = 35
    
    table = table %>% 
      bind_rows(data.frame(date = rep(start_date + i + 1, row_len),
                           region = rep("Western Pacific", row_len),
                           nation = rep("China", row_len),
                           city = page_text[seq(page_text %>% str_which("Hubei"), page_text %>% str_which("Total"), col)],
                           confirmed_daily = rep(NA, row_len),
                           suspected_daily = rep(NA, row_len),
                           death_daily = rep(NA, row_len),
                           confirmed_cumulative = page_text[seq(page_text %>% str_which("Hubei") + 2, page_text %>% str_which("Total") + col - 1, col)],
                           death_cumulative = rep(NA, row_len)
      ) %>% 
        mutate(region = as.character(region),
               nation = as.character(nation),
               city = as.character(city),
               confirmed_cumulative = confirmed_cumulative %>% as.character() %>% as.numeric())
      )
  } # 23
  else if(i == 23){
    page_url = str_c("https://www.who.int", report_url[i])
    page_index = pdf_text(page_url) %>% str_which("SURVEILLANCE")
    page_text = pdf_text(page_url)[page_index] %>% str_replace_all("\\r\\n", "  ") %>% 
      str_split("[:space:]{2,}") %>% unlist() %>% stringi::stri_remove_empty()
    page_text = ifelse((page_text %>% str_detect(" ") * page_text %>% str_detect("[:digit:]") == 1), str_remove(page_text, " "), page_text)
    col = 5
    row_len = 35
    
    table = table %>% 
      bind_rows(data.frame(date = rep(start_date + i + 1, row_len),
                           region = rep("Western Pacific", row_len),
                           nation = rep("China", row_len),
                           city = page_text[seq(page_text %>% str_which("Hubei"), (page_text %>% str_which("Total"))[2], col)],
                           confirmed_daily = rep(NA, row_len),
                           suspected_daily = rep(NA, row_len),
                           death_daily = rep(NA, row_len),
                           confirmed_cumulative = page_text[seq(page_text %>% str_which("Hubei") + 2, (page_text %>% str_which("Total"))[2] + col - 1, col)],
                           death_cumulative = page_text[seq(page_text %>% str_which("Hubei") + 4, (page_text %>% str_which("Total"))[2] + col - 1, col)]
                          ) %>% 
                  mutate(region = as.character(region),
                         nation = as.character(nation),
                         city = as.character(city),
                         confirmed_cumulative = confirmed_cumulative %>% as.character() %>% as.numeric(),
                         death_cumulative = death_cumulative %>% as.character() %>% as.numeric())
      )
  } # 12 ~ 22
  else if(12 <= i && i <= 22){
    page_url = str_c("https://www.who.int", report_url[i])
    page_index = pdf_text(page_url) %>% str_which("SURVEILLANCE")
    page_text = pdf_text(page_url)[page_index] %>% str_replace_all("\\r\\n", "  ") %>% 
      str_split("[:space:]{2,}") %>% unlist() %>% stringi::stri_remove_empty()
    page_text = ifelse((page_text %>% str_detect(" ") * page_text %>% str_detect("[:digit:]") == 1), str_remove(page_text, " "), page_text)
    col = 2
    row_len = 35
    
    table = table %>% 
      bind_rows(data.frame(date = rep(start_date + i + 1, row_len),
                           region = rep("Western Pacific", row_len),
                           nation = rep("China", row_len),
                           city = page_text[seq(page_text %>% str_which("Hubei"), page_text %>% str_which("Total") + col - 1)] %>% 
                             Filter(function(x){str_length(x) <= 20}, .) %>% str_remove("[:digit:]+") %>% stringi::stri_remove_empty(),
                           confirmed_daily = rep(NA, row_len),
                           suspected_daily = rep(NA, row_len),
                           death_daily = rep(NA, row_len),
                           confirmed_cumulative = page_text[seq(page_text %>% str_which("Hubei"), page_text %>% str_which("Total") + col - 1)] %>% 
                             str_replace_na("abcd") %>% str_extract_all("[:digit:]+") %>% unlist(),
                           death_cumulative = rep(NA, row_len)
                          ) %>% 
                  mutate(region = as.character(region),
                         nation = as.character(nation),
                         city = as.character(city),
                         confirmed_cumulative = confirmed_cumulative %>% as.character() %>% as.numeric())
               )
  } # 5 ~ 11
  else if(5 <= i && i <= 11){
    page_url = str_c("https://www.who.int", report_url[i])
    page_index = pdf_text(page_url) %>% str_which("SURVEILLANCE")
    page_text = pdf_text(page_url)[page_index] %>% str_replace_all("\\r\\n", "  ") %>% 
      str_split("[:space:]{2,}") %>% unlist() %>% stringi::stri_remove_empty()
    col = 2
    row_len = 4
    
    table = table %>% 
      bind_rows(data.frame(date = rep(start_date + i + 1, row_len),
                           region = rep("Western Pacific", row_len),
                           nation = rep("China", row_len),
                           city = c("China", "Hong Kong SAR", "Macau SAR", "Taipei and environ"),
                           confirmed_daily = rep(NA, row_len),
                           suspected_daily = rep(NA, row_len),
                           death_daily = rep(NA, row_len),
                           confirmed_cumulative = c(page_text[(page_text %>% str_which("China"))[1] + col - 1] %>% as.numeric() - 
                                                      sum(page_text %>% Filter(function(x){str_length(x) >= 20}, .) %>% 
                                                            tail(2) %>% str_extract_all("[0-9]+") %>% unlist() %>% as.numeric()),
                                                    page_text %>% Filter(function(x){str_length(x) >= 20}, .) %>% tail(2) %>% str_extract_all("[0-9]+") %>% unlist()),
                           death_cumulative = rep(NA, row_len)
                           ) %>% 
                  mutate(region = as.character(region),
                         nation = as.character(nation),
                         city = as.character(city),
                         confirmed_cumulative = confirmed_cumulative %>% as.character() %>% as.numeric())
               )
  } # 3 ~ 4
  else if(3 <= i && i <= 4){
    page_url = str_c("https://www.who.int", report_url[i])
    page_index = pdf_text(page_url) %>% str_which("SURVEILLANCE")
    page_text = pdf_text(page_url)[page_index] %>% str_replace_all("\\r\\n", "|") %>% 
      str_split("\\|") %>% unlist() %>% str_split("[:space:]{2}") %>% unlist() %>% 
      str_replace("Hong Kong Special Administrative", "Hong Kong SAR") %>% 
      str_replace("Macau Special Administrative Region", "Macau SAR") %>% 
      str_replace("Municipality", "") %>% str_replace("Province", "") %>% 
      str_replace("Taipep", "Taipei and environs") %>% 
      str_replace("\\*", "") %>% 
      str_remove("WHO Regional Office") %>% 
      str_remove("WHO WPRO Region") %>% str_remove("WHO SEARO Region") %>% 
      str_remove("WHO AMRO Region") %>% str_remove("Total Confirmed cases") %>%  
      str_remove("WHO EURO Region") %>% str_remove("WHO PAHO Region") %>% 
      str_remove("Nepal") %>% str_remove("Region") %>% 
      str_remove("Total Confirmed") %>% 
      str_replace("Federal Democratic Republic", "Nepal") %>% 
      str_remove("^ $") %>% 
      stringi::stri_remove_empty()
    col = 2
    row_len = (page_text %>% str_which("Japan") - (page_text %>% str_which("Total"))[1])/col
    
    table = table %>% 
      bind_rows(data.frame(date = rep(start_date + i, row_len),
                           region = rep("Western Pacific", row_len),
                           nation = rep("China", row_len),
                           city = page_text[seq((page_text %>% str_which("Total"))[1], page_text %>% str_which("Japan") - 1, col)],
                           confirmed_daily = rep(NA, row_len),
                           suspected_daily = rep(NA, row_len),
                           death_daily = rep(NA, row_len),
                           confirmed_cumulative = c(page_text[seq((page_text %>% str_which("Total"))[1] + col - 1,
                                                                  page_text %>% str_which("Japan") - 1,
                                                                  col)]),
                           death_cumulative = rep(NA, row_len)
                           ) %>% 
                  mutate(region = as.character(region),
                         nation = as.character(nation),
                         city = as.character(city),
                         confirmed_cumulative = confirmed_cumulative %>% as.character() %>% as.numeric())
      )
  } # ~ 2
  else{
    page_url = str_c("https://www.who.int", report_url[i])
    page_index = pdf_text(page_url) %>% str_which("SURVEILLANCE")
    page_text = pdf_text(page_url)[page_index] %>% str_replace_all("\\r\\n", "|") %>% 
      str_split("\\|") %>% unlist() %>% str_split("[:space:]{2}") %>% unlist() %>% 
      str_replace("Hong Kong Special Administrative", "Hong Kong SAR") %>% 
      str_replace("Macau Special Administrative Region", "Macau SAR") %>% 
      str_replace("Municipality", "") %>% str_replace("Province", "") %>% 
      str_replace("Taipep", "Taipei and environs") %>% 
      str_replace("\\*", "") %>% 
      str_replace("Taiwan, China", "Taipei and environ") %>% 
      str_remove("WHO Regional Office") %>% 
      str_remove("WHO WPRO Region") %>% str_remove("WHO SEARO Region") %>% 
      str_remove("WHO AMRO Region") %>% str_remove("Total Confirmed cases") %>%  
      str_remove("Region") %>% 
      stringi::stri_remove_empty()
    col = 2
    row_len = (page_text %>% str_which("Japan") - (page_text %>% str_which("Total"))[1])/col
  
    table = table %>% # 2
      bind_rows(data.frame(date = rep(start_date + 2, row_len),
                           region = rep("Western Pacific", row_len),
                           nation = rep("China", row_len),
                           city = page_text[seq((page_text %>% str_which("Total"))[1], page_text %>% str_which("Japan") - 1, col)],
                           confirmed_daily = rep(NA, row_len),
                           suspected_daily = rep(NA, row_len),
                           death_daily = rep(NA, row_len),
                           confirmed_cumulative = page_text[seq((page_text %>% str_which("Total"))[[1]] + 1, page_text %>% str_which("Japan") - 1, col)],
                           death_cumulative = rep(NA, row_len)
                          ) %>% 
                  mutate(region = as.character(region),
                         nation = as.character(nation),
                         city = as.character(city),
                         confirmed_cumulative = confirmed_cumulative %>% as.character() %>% as.numeric())
    ) %>% # 1
    bind_rows(data.frame(date = rep(start_date + 1, 5),
                         region = c(rep("Western Pacific", 5)),
                         nation = c(rep("China", 4), "Total"),
                         city = c("Hubei", "Guangdong", "Beijing", "Shanghai", "China"),
                         confirmed_daily = c(57, 13, 3, 1, 74),
                         suspected_daily = rep(NA, 5),
                         death_daily = c(3, 0, 0, 0, 3),
                         confirmed_cumulative = c(258, 14, 5, 1, 278),
                         death_cumulative = c(3, 0, 0, 0, 3)
                         ) %>% 
      mutate(region = as.character(region),
             nation = as.character(nation), 
             city = as.character(city),
             confirmed_cumulative = confirmed_cumulative %>% as.character() %>% as.numeric())
    ) %>% # 0
    bind_rows(data.frame(date = rep(start_date, 4),
                         region = c(rep("Western Pacific", 4)),
                         nation = c(rep("China", 2), rep("Total", 2)),
                         city = c("Guangdong", "Beijing", "China", NA),
                         confirmed_daily = c(1, 2, 3, 3),
                         suspected_daily = rep(NA, 4),
                         death_daily = rep(0, 4),
                         confirmed_cumulative = c(1, 2, 3, 3),
                         death_cumulative = rep(0, 4)
                         ) %>% 
                mutate(region = as.character(region),
                       nation = as.character(nation), 
                       city = as.character(city),
                       confirmed_cumulative = confirmed_cumulative %>% as.character() %>% as.numeric())
    )
  }
}