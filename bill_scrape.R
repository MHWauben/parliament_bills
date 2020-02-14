library(polite)
library(rvest)

parliament_bow <- polite::bow(url = "https://www.parliament.uk/business/bills-and-legislation/current-bills/previous-bills/", 
                              force = TRUE)

session_pages <- polite::scrape(parliament_bow) %>%
  rvest::html_node(xpath = '//*[@id="ctl00_ctl00_FormContent_SiteSpecificPlaceholder_PageContent_ctlMainBody_wrapperDiv"]/div') %>%
  rvest::html_nodes("a") %>%
  html_attr('href')
session_pages <- session_pages[grepl("html", session_pages)]

all_bills <- function(session_page){
  print(session_page)
  table_bow <- polite::bow(url = session_page)
  bill_table <- polite::scrape(table_bow) %>%
    rvest::html_node(xpath = '//*[@id="bill-summary"]/table') 
  
  bill_names <- bill_table %>%
    rvest::html_table() %>%
    dplyr::filter(nchar(`Bill title`) > 1)
  bill_houses <- bill_table %>%
    rvest::html_nodes("img") %>%
    rvest::html_attr('title')
  bill_links <- bill_table %>%
    rvest::html_nodes('td.bill-item-description') %>%
    rvest::html_nodes("a") %>%
    html_attr('href')
  bill_names$CurrentHouse <- bill_houses
  bill_names$link <- paste0("https://services.parliament.uk", bill_links)
  return(bill_names)
}

bill_names <- purrr::map_dfr(session_pages[1:3], all_bills)

text_link <- function(bill_name){
  print(bill_name)
  bill_bow <- polite::bow(bill_name)
  bill_text_link <- polite::scrape(bill_bow) %>%
    rvest::html_nodes(xpath = '//*[@id="bill-summary"]/table') %>%
    rvest::html_nodes('td.bill-item-description') %>%
    rvest::html_nodes("a") %>%
    html_attr('href')
  bill_text_link <- bill_text_link[grepl("htm", bill_text_link)]
  if (!identical(bill_text_link, character(0)) ) { return(bill_text_link)
  } else { return("")
      }
}

text_links <- purrr::map_chr(bill_names$link[1:5], text_link)

text_extract <- function(text_link){
  # Make sure the link is a real link
  if (text_link != "") {
    link_bow <- polite::bow(text_link)
    # Scraping for parliament publications
    if (grepl("publications\\.parliament", text_link)) {
      text_link_new <- polite::scrape(link_bow) %>%
        rvest::html_nodes(xpath = '//*[@id="ContentMain"]/div/div[1]/p[2]/a') %>%
        rvest::html_attr('href')
      bill_text_link_shorter <- gsub("/[^/]*htm$", "", text_link)
      text_link_full <- paste0(bill_text_link_shorter, "/", text_link_new)
      text_bow <- polite::bow(text_link_full)
      text_extract <- polite::scrape(text_bow) %>%
        rvest::html_node(xpath = '//*[@id="ContentMain"]/div/div[3]') %>%
        rvest::html_text()
    } else {
      text_extract <- ""
    }
    return(text_extract)
  } else {return("")}
}

text_extract <- purrr::map_chr(text_links, text_extract)
