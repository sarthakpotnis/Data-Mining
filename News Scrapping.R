require(rvest)
library(googleVis)
require(XML)

#################################################################################################################
#http://asia.nikkei.com/
#################################################################################################################
# Get link URLs
main.page <- read_html(x = "http://asia.nikkei.com/")


urls2 <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls3 <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


urls4 <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls5 <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h5 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


# Get link text
links2 <- main.page %>% # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_text() # extract the link text

links3 <- main.page %>% # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_text() # extract the link text

links4 <- main.page %>% # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_text() # extract the link text

links5 <- main.page %>% # feed `main.page` to the next step
  html_nodes("h5 a") %>% # get the CSS nodes
  html_text() # extract the link text


url2 <- paste0("<a href= http://asia.nikkei.com/",urls2,">",links2,"</a>" )
url3 <- paste0("<a href= http://asia.nikkei.com/",urls3,">",links3,"</a>" )
url4 <- paste0("<a href= http://asia.nikkei.com/",urls4,">",links4,"</a>" )
url5 <- paste0("<a href= http://asia.nikkei.com/",urls5,">",links5,"</a>" )

# Combine `links` and `urls` into a data.frame
sotu2<-data.frame(asia.nikkei.com = links2, urls = url2, stringsAsFactors = T)
sotu3<-data.frame(asia.nikkei.com = links3, urls = url3, stringsAsFactors = T)
sotu4<-data.frame(asia.nikkei.com = links4, urls = url4, stringsAsFactors = T)
sotu5<-data.frame(asia.nikkei.com = links5, urls = url5, stringsAsFactors = T)

total<- rbind(sotu2,sotu3,sotu4,sotu5)
foo<- gvisTable(total, options=list(allowHTML = T, page='enable',width="400px", height="600px"))
foo$html$header<-paste("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\ ......", "<h2>/asia.nikkei.com/</h2>", "</head>\n<body>\n")

#################################################################################################################
#http://asia.nikkei.com/business 
#################################################################################################################


# Get link URLs
main.page <- read_html(x = "http://asia.nikkei.com/Business")


urls2_b <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls3_b <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


urls4_b <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls5_b <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h5 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


# Get link text
links2_b <- main.page %>% # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_text() # extract the link text

links3_b <- main.page %>% # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_text() # extract the link text

links4_b <- main.page %>% # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_text() # extract the link text

links5_b <- main.page %>% # feed `main.page` to the next step
  html_nodes("h5 a") %>% # get the CSS nodes
  html_text() # extract the link text


url2_b <- paste0("<a href= http://asia.nikkei.com/",urls2_b,">",links2_b,"</a>" )
url3_b <- paste0("<a href= http://asia.nikkei.com/",urls3_b,">",links3_b,"</a>" )
url4_b <- paste0("<a href= http://asia.nikkei.com/",urls4_b,">",links4_b,"</a>" )
url5_b <- paste0("<a href= http://asia.nikkei.com/",urls5_b,">",links5_b,"</a>" )

# Combine `links` and `urls` into a data.frame
sotu2_b<-data.frame(asia.nikkei.com_business = links2_b, urls = url2_b, stringsAsFactors = T)
sotu3_b<-data.frame(asia.nikkei.com_business = links3_b, urls = url3_b, stringsAsFactors = T)
sotu4_b<-data.frame(asia.nikkei.com_business = links4_b, urls = url4_b, stringsAsFactors = T)
sotu5_b<-data.frame(asia.nikkei.com_business = links5_b, urls = url5_b, stringsAsFactors = T)

total_b<- rbind(sotu2_b,sotu3_b,sotu4_b,sotu5_b)
foo2<- gvisTable(total_b, options=list(allowHTML = T, page='enable',width="400px", height="600px"))
foo2$html$header<-paste("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\ ......", "<h2>/asia.nikkei.com/business</h2>", "</head>\n<body>\n")


#################################################################################################################
#http://asia.nikkei.com/company
#################################################################################################################


# Get link URLs
main.page <- read_html(x = "http://asia.nikkei.com/Business/Companies")


urls2_a <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls3_a <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


urls4_a <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls5_a <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h5 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


# Get link text
links2_a <- main.page %>% # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_text() # extract the link text

links3_a <- main.page %>% # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_text() # extract the link text

links4_a <- main.page %>% # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_text() # extract the link text

links5_a <- main.page %>% # feed `main.page` to the next step
  html_nodes("h5 a") %>% # get the CSS nodes
  html_text() # extract the link text


url2_a <- paste0("<a href= http://asia.nikkei.com/",urls2_a,">",links2_a,"</a>" )
url3_a <- paste0("<a href= http://asia.nikkei.com/",urls3_a,">",links3_a,"</a>" )
url4_a <- paste0("<a href= http://asia.nikkei.com/",urls4_a,">",links4_a,"</a>" )
url5_a <- paste0("<a href= http://asia.nikkei.com/",urls5_a,">",links5_a,"</a>" )

# Combine `links` and `urls` into a data.frame
sotu2_a<-data.frame(asia.nikkei.com_company = links2_a, urls = url2_a, stringsAsFactors = T)
sotu3_a<-data.frame(asia.nikkei.com_company = links3_a, urls = url3_a, stringsAsFactors = T)
sotu4_a<-data.frame(asia.nikkei.com_company = links4_a, urls = url4_a, stringsAsFactors = T)
sotu5_a<-data.frame(asia.nikkei.com_company = links5_a, urls = url5_a, stringsAsFactors = T)

total_a<- rbind(sotu2_a,sotu3_a,sotu5_a)
foo3<- gvisTable(total_a, options=list(allowHTML = T, page='enable',width="400px", height="600px"))
foo3$html$header<-paste("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\ ......", "<h2>#http://asia.nikkei.com/company</h2>", "</head>\n<body>\n")



################################################################################################################
#http://www.afr.com/
################################################################################################################


# Get link URLs
main.page <- read_html(x = "http://www.afr.com/")


urls2_f <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls3_f <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


urls4_f <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


# Get link text
links2_f <- main.page %>% # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_text() # extract the link text

links3_f <- main.page %>% # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_text() # extract the link text

links4_f <- main.page %>% # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_text() # extract the link text

url2_f <- paste0("<a href= http://www.afr.com/",urls2_f,">",links2_f,"</a>" )
url3_f <- paste0("<a href= http://www.afr.com/",urls3_f,">",links3_f,"</a>" )
url4_f <- paste0("<a href= http://www.afr.com/",urls4_f,">",links4_f,"</a>" )

# Combine `links` and `urls` into a data.frame
sotu2_f<-data.frame(www.afr.com = links2_f, urls = url2_f, stringsAsFactors = T)
sotu3_f<-data.frame(www.afr.com = links3_f, urls = url3_f, stringsAsFactors = T)
sotu4_f<-data.frame(www.afr.com = links4_f, urls = url4_f, stringsAsFactors = T)

total_f<- rbind(sotu2_f,sotu3_f,sotu4_f)

foo4<- gvisTable(total_f, options=list(allowHTML = T, page='enable',width="400px", height="600px"))
foo4$html$header<-paste("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\ ......", "<h2>/www.afr.com/</h2>", "</head>\n<body>\n")


################################################################################################################
#http://www.afr.com/markets
################################################################################################################


# Get link URLs
main.page <- read_html(x = "http://www.afr.com/markets")


urls2_fm <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls3_fm <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


urls4_fm <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


# Get link text
links2_fm <- main.page %>% # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_text() # extract the link text

links3_fm <- main.page %>% # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_text() # extract the link text

links4_fm <- main.page %>% # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_text() # extract the link text


url2_fm <- paste0("<a href= http://www.afr.com//markets",urls2_fm,">",links2_fm,"</a>" )
url3_fm <- paste0("<a href= http://www.afr.com//markets",urls3_fm,">",links3_fm,"</a>" )
url4_fm <- paste0("<a href= http://www.afr.com//markets",urls4_fm,">",links4_fm,"</a>" )

# Combine `links` and `urls` into a data.frame
sotu2_fm<-data.frame(www.afr.com_markets = links2_fm, urls = url2_fm, stringsAsFactors = T)
sotu3_fm<-data.frame(www.afr.com_markets = links3_fm, urls = url3_fm, stringsAsFactors = T)
sotu4_fm<-data.frame(www.afr.com_markets = links4_fm, urls = url4_fm, stringsAsFactors = T)

total_fm<- rbind(sotu2_fm,sotu3_fm,sotu4_fm)

foo5<- gvisTable(total_fm, options=list(allowHTML = T, page='enable',width="400px", height="600px"))
foo5$html$header<-paste("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\ ......", "<h2>/www.afr.com/markets</h2>", "</head>\n<body>\n")


################################################################################################################
#http://www.wsj.com/
################################################################################################################


# Get link URLs
main.page <- read_html(x = "http://www.wsj.com/")


urls2_wj <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls3_wj <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


urls4_wj <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


# Get link text
links2_wj <- main.page %>% # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_text() # extract the link text

links3_wj <- main.page %>% # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_text() # extract the link text

links4_wj <- main.page %>% # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_text() # extract the link text


url2_wj <- paste0("<a href= http://www.wsj.com/",urls2_wj,">",links2_wj,"</a>" )
url3_wj <- paste0("<a href= http://www.wsj.com/",urls3_wj,">",links3_wj,"</a>" )
url4_wj <- paste0("<a href= http://www.wsj.com/",urls4_wj,">",links4_wj,"</a>" )

# Combine `links` and `urls` into a data.frame
sotu2_wj<-data.frame(www.wsj.com = links2_wj, urls = url2_wj, stringsAsFactors = T)
sotu3_wj<-data.frame(www.wsj.com = links3_wj, urls = url3_wj, stringsAsFactors = T)
sotu4_wj<-data.frame(www.wsj.com = links4_wj, urls = url4_wj, stringsAsFactors = T)

total_wj<- rbind(sotu3_wj,sotu4_wj)

foo6<- gvisTable(total_wj, options=list(allowHTML = T, page='enable',width="400px", height="600px"))
foo6$html$header<-paste("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\ ......", "<h2>http://www.wsj.com/</h2>", "</head>\n<body>\n")


################################################################################################################
#http://www.wsj.com/news/world
################################################################################################################


# Get link URLs
main.page <- read_html(x = "http://www.wsj.com/news/world")


urls2_wjn <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls3_wjn <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


urls4_wjn <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


# Get link text
links2_wjn <- main.page %>% # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_text() # extract the link text

links3_wjn <- main.page %>% # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_text() # extract the link text

links4_wjn <- main.page %>% # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_text() # extract the link text


url2_wjn <- paste0("<a href= ",urls2_wjn,">",links2_wjn,"</a>" )
url3_wjn <- paste0("<a href= ",urls3_wjn,">",links3_wjn,"</a>" )
url4_wjn <- paste0("<a href= ",urls4_wjn,">",links4_wjn,"</a>" )

# Combine `links` and `urls` into a data.frame
sotu2_wjn<-data.frame(www.wsj.com_news_world = links2_wjn, urls = url2_wjn, stringsAsFactors = T)
sotu3_wjn<-data.frame(www.wsj.com_news_world = links3_wjn, urls = url3_wjn, stringsAsFactors = T)
sotu4_wjn<-data.frame(www.wsj.com_news_world = links4_wjn, urls = url4_wjn, stringsAsFactors = T)

total_wjn<- rbind(sotu2_wjn,sotu3_wjn,sotu4_wjn)

foo7<- gvisTable(total_wjn, options=list(allowHTML = T, page='enable',width="400px", height="600px"))
foo7$html$header<-paste("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\ ......", "<h2>http://www.wsj.com/news</h2>", "</head>\n<body>\n")


################################################################################################################
#http://www.wsj.com/news/economy
################################################################################################################


# Get link URLs
main.page <- read_html(x = "http://www.wsj.com/news/economy")


urls2_wjne <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls3_wjne <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


urls4_wjne <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


# Get link text
links2_wjne <- main.page %>% # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_text() # extract the link text

links3_wjne <- main.page %>% # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_text() # extract the link text

links4_wjne <- main.page %>% # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_text() # extract the link text


url2_wjne <- paste0("<a href= ",urls2_wjne,">",links2_wjne,"</a>" )
url3_wjne <- paste0("<a href= ",urls3_wjne,">",links3_wjne,"</a>" )
url4_wjne <- paste0("<a href= ",urls4_wjne,">",links4_wjne,"</a>" )

# Combine `links` and `urls` into a data.frame
sotu2_wjne<-data.frame(www.wsj.com_news_economy = links2_wjne, urls = url2_wjne, stringsAsFactors = T)
sotu3_wjne<-data.frame(www.wsj.com_news_economy = links3_wjne, urls = url3_wjne, stringsAsFactors = T)
sotu4_wjne<-data.frame(www.wsj.com_news_economy = links4_wjne, urls = url4_wjne, stringsAsFactors = T)

total_wjne<- rbind(sotu2_wjne,sotu3_wjne,sotu4_wjne)

foo8<- gvisTable(total_wjne, options=list(allowHTML = T, page='enable',width="400px", height="600px"))
foo8$html$header<-paste("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\ ......", "<h2>http://www.wsj.com/news/economy</h2>", "</head>\n<body>\n")
a<-gvisMerge(foo,foo2, horizontal = 'T')
b<- gvisMerge(foo3,foo4, horizontal = 'T')
c<- gvisMerge(a,b)
d<- gvisMerge(foo5,foo6, horizontal = 'T')
e<-gvisMerge(c,d)
f<-gvisMerge(foo7,foo8, horizontal = 'T')

plot(gvisMerge(e,f))


################################################################################################################
#http://www.reuters.com/finance
################################################################################################################


# Get link URLs
main.page <- read_html(x = "http://www.reuters.com/finance")


urls2_rs <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls3_rs <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


urls4_rs <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


# Get link text
links2_rs <- main.page %>% # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_text() # extract the link text

links3_rs <- main.page %>% # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_text() # extract the link text

links4_rs <- main.page %>% # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_text() # extract the link text


url2_rs <- paste0("<a href=",urls2_rs,">",links2_rs,"</a>" )
url3_rs <- paste0("<a href=",urls3_rs,">",links3_rs,"</a>" )
url4_rs <- paste0("<a href=",urls4_rs,">",links4_rs,"</a>" )

# Combine `links` and `urls` into a data.frame
sotu2_rs<-data.frame(links = links2_rs, urls = url2_rs, stringsAsFactors = T)
sotu3_rs<-data.frame(links = links3_rs, urls = url3_rs, stringsAsFactors = T)


total_rs<- rbind(sotu2_rs,sotu3_rs)

foo9<-gvisTable(total_rs, options=list(allowHTML = T, page='enable'))
foo9$html$header<-paste("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\ ......", "<h2>/reuters.com/</h2>", "</head>\n<body>\n")

a<-gvisMerge(foo,foo2, horizontal = 'T')
b<- gvisMerge(foo3,foo4, horizontal = 'T')
c<- gvisMerge(a,b)
d<- gvisMerge(foo5,foo6, horizontal = 'T')
e<-gvisMerge(c,d)
f<-gvisMerge(foo7,foo8, horizontal = 'T')
g<-gvisMerge(e,f)
h<- gvisMerge(g,foo9)

plot(foo9)

################################################################################################################
#http://www.barrons.com/
################################################################################################################


# Get link URLs
main.page <- read_html(x = "http://www.barrons.com/")


urls2_br <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls3_br <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


urls4_br <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


# Get link text
links2_br <- main.page %>% # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_text() # extract the link text

links3_br <- main.page %>% # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_text() # extract the link text

links4_br <- main.page %>% # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_text() # extract the link text


url2_br <- paste0("<a href=",urls2_br,">",links2_br,"</a>" )
url3_br <- paste0("<a href=",urls3_br,">",links3_br,"</a>" )
url4_br <- paste0("<a href=",urls4_br,">",links4_br,"</a>" )

# Combine `links` and `urls` into a data.frame
sotu2_br<-data.frame(links = links2_br, urls = url2_br, stringsAsFactors = T)
sotu3_br<-data.frame(links = links3_br, urls = url3_br, stringsAsFactors = T)


total_br<- rbind(sotu2_br,sotu3_br)

foo9<-gvisTable(total_br, options=list(allowHTML = T, page='enable'))
foo9$html$header<-paste("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\ ......", "<h2>hhtp:/barrons/</h2>", "</head>\n<body>\n")

a<-gvisMerge(foo,foo2, horizontal = 'T')
b<- gvisMerge(foo3,foo4, horizontal = 'T')
c<- gvisMerge(a,b)
d<- gvisMerge(foo5,foo6, horizontal = 'T')
e<-gvisMerge(c,d)
f<-gvisMerge(foo7,foo8, horizontal = 'T')
g<-gvisMerge(e,f)
h<- gvisMerge(g,foo9)


plot(g)
############################################################################################################
#http://www.ft.com/home/us
################################################################################################################


# Get link URLs
main.page <- read_html(x = "http://www.ft.com/home/us")


urls2_ft <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls3_ft <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


urls4_ft <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


# Get link text
links2_ft <- main.page %>% # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_text() # extract the link text

links3_ft <- main.page %>% # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_text() # extract the link text

links4_ft <- main.page %>% # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_text() # extract the link text


url2_ft <- paste0("<a href=http://www.ft.com/home/us ",urls2_ft,">",links2_ft,"</a>" )
url3_ft <- paste0("<a href= http://www.ft.com/home/us",urls3_ft,">",links3_ft,"</a>" )
url4_ft <- paste0("<a href=http://www.ft.com/home/us ",urls4_ft,">",links4_ft,"</a>" )

# Combine `links` and `urls` into a data.frame
sotu2_ft<-data.frame(links = links2_ft, urls = url2_ft, stringsAsFactors = T)
sotu3_ft<-data.frame(links = links3_ft, urls = url3_ft, stringsAsFactors = T)
sotu4_ft<-data.frame(links = links4_ft, urls = url4_ft, stringsAsFactors = T)

total_ft<- rbind(sotu3_ft,sotu4_ft)

foo9<-gvisTable(total_ft, options=list(allowHTML = T, page='enable'))
foo9$html$header<-paste("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\ ......", "<h2></h2>", "</head>\n<body>\n")

a<-gvisMerge(foo,foo2, horizontal = 'T')
b<- gvisMerge(foo3,foo4, horizontal = 'T')
c<- gvisMerge(a,b)
d<- gvisMerge(foo5,foo6, horizontal = 'T')
e<-gvisMerge(c,d)
f<-gvisMerge(foo7,foo8, horizontal = 'T')
g<-gvisMerge(e,f)


plot(foo9)



################################################################################################################
#http://gestion.pe/
################################################################################################################


# Get link URLs
main.page <- read_html(x = "http://gestion.pe/")


urls2 <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls3 <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


urls4 <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


# Get link text
links2 <- main.page %>% # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_text() # extract the link text

links3 <- main.page %>% # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_text() # extract the link text

links4 <- main.page %>% # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_text() # extract the link text


url2 <- paste0("<a href= http://gestion.pe/",urls2,">",links2,"</a>" )
url3 <- paste0("<a href= http://gestion.pe/",urls3,">",links3,"</a>" )
url4 <- paste0("<a href= http://gestion.pe/",urls4,">",links4,"</a>" )

# Combine `links` and `urls` into a data.frame
sotu2<-data.frame(links = links2, urls = url2, stringsAsFactors = T)
sotu3<-data.frame(links = links3, urls = url3, stringsAsFactors = T)
sotu4<-data.frame(links = links4, urls = url4, stringsAsFactors = T)

total<- rbind(sotu3,sotu4)
#sotu <- transform(sotu, url = )
foo<- gvisTable(total, options=list(allowHTML = T, page = 'enable'))
plot(foo)


################################################################################################################
#http://www.valor.com.br/
################################################################################################################


# Get link URLs
main.page <- read_html(x = "http://www.valor.com.br/")


urls2 <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls3 <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


# Get link text
links2 <- main.page %>% # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_text() # extract the link text

links3 <- main.page %>% # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_text() # extract the link text


url2 <- paste0("<a href= http://www.valor.com.br/",urls2,">",links2,"</a>" )
url3 <- paste0("<a href= http://www.valor.com.br/",urls3,">",links3,"</a>" )

# Combine `links` and `urls` into a data.frame
sotu2<-data.frame(links = links2, urls = url2, stringsAsFactors = T)
sotu3<-data.frame(links = links3, urls = url3, stringsAsFactors = T)



total<- rbind(sotu2)
#sotu <- transform(sotu, url = )
foo<- gvisTable(total,weight='value0', options=list(allowHTML = T, paging = 'enable'))
plot(foo)





############################################################
#http://www.business-standard.com/
############################################################ 





# Get link URLs
main.page2 <- read_html(x = "http://www.business-standard.com/")


urls2_bsd <- main.page2 %>%  # feed `main.page` to the next step
  html_nodes("h1 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls3_bsd <- main.page2 %>%  # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


urls4_bsd <- main.page2 %>%  # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


# Get link text
links2_bsd <- main.page2 %>% # feed `main.page` to the next step
  html_nodes("li a") %>% # get the CSS nodes
  html_text() # extract the link text

links3_bsd <- main.page2 %>% # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_text() # extract the link text


links4_bsd <- main.page2 %>% # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_text() # extract the link text


url2_bsd <- paste0("<a href= http://www.business-standard.com/",urls2_bsd,">",links2_bsd,"</a>" )
url3_bsd <- paste0("<a href= http://www.business-standard.com/",urls2_bsd,">",links2_bsd,"</a>" )
url4_bsd <- paste0("<a href= http://www.business-standard.com/",urls4_bsd,">",links4_bsd,"</a>" )

# Combine `links` and `urls` into a data.frame
sotu2<-data.frame(links = links2_bsd, urls = url2_bsd, stringsAsFactors = T)
sotu3<-data.frame(links = links2_bsd, urls = url2_bsd, stringsAsFactors = T)


total<- rbind(sotu2,sotu3)
#sotu <- transform(sotu, url = )
foo<- gvisTable(total, options=list(allowHTML = T, paging = 'enable'))
plot(foo)





################################################################################################################
#http://www.scmp.com/frontpage/international
################################################################################################################



# Get link URLs
main.page <- read_html(x = "http://www.scmp.com/frontpage/international")


urls2 <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls3 <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


urls4 <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

urls5 <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h5 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs


urls6 <- main.page %>%  # feed `main.page` to the next step
  html_nodes("h6 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

# Get link text
links2 <- main.page %>% # feed `main.page` to the next step
  html_nodes("h2 a") %>% # get the CSS nodes
  html_text() # extract the link text

links3 <- main.page %>% # feed `main.page` to the next step
  html_nodes("h3 a") %>% # get the CSS nodes
  html_text() # extract the link text

links4 <- main.page %>% # feed `main.page` to the next step
  html_nodes("h4 a") %>% # get the CSS nodes
  html_text() # extract the link text

links5 <- main.page %>% # feed `main.page` to the next step
  html_nodes("h5 a") %>% # get the CSS nodes
  html_text() # extract the link text


links6 <- main.page %>% # feed `main.page` to the next step
  html_nodes("h6 a") %>% # get the CSS nodes
  html_text() # extract the link text

url2 <- paste0("<a href= http://asia.nikkei.com/",urls2,">",links2,"</a>" )
url3 <- paste0("<a href= http://asia.nikkei.com/",urls3,">",links3,"</a>" )
url4 <- paste0("<a href= http://asia.nikkei.com/",urls4,">",links4,"</a>" )
url5 <- paste0("<a href= http://asia.nikkei.com/",urls5,">",links5,"</a>" )
url6 <- paste0("<a href= http://asia.nikkei.com/",urls6,">",links6,"</a>" )

# Combine `links` and `urls` into a data.frame
sotu2<-data.frame(links = links2, urls = url2, stringsAsFactors = T)
sotu3<-data.frame(links = links3, urls = url3, stringsAsFactors = T)
sotu4<-data.frame(links = links4, urls = url4, stringsAsFactors = T)
sotu5<-data.frame(links = links5, urls = url5, stringsAsFactors = T)
sotu6<-data.frame(links = links6, urls = url6, stringsAsFactors = T)









total<- rbind(sotu2,sotu3)
#sotu <- transform(sotu, url = )
foo<- gvisTable(total, options=list(allowHTML = T))
plot(foo)



