#2¹ø ¹®Á¦

#ÆÄÀÏ ½ÇÇàµÇ´Â °æ·Î È®ÀÎ
getwd()
#ÅØ½ºÆ® ÆÄÀÏ ÀĞ¾î¿È
raw_bom <- readLines("bom.txt", encoding = "UTF-8")
#ÅØ½ºÆ® ÆÄÀÏ À§¿¡¼­ ºÎÅÍ Ãâ·Â
head(raw_bom)#ÅØ½ºÆ® ÆÄÀÏ À§¿¡¼­ ºÎÅÍ Ãâ·Â


install.packages("stringr")
library(stringr)


#ÇÑ±ÛÀÌ ¾Æ´Ñ ¸ğµç ¹®ÀÚ Á¦°Å
new = raw_bom %>% str_replace_all("[^°¡-ÆR]", replacement = " ")
#À§¿¡¼­ ºÎÅÍ Ãâ·Â
head(new)

#tibble±¸Á¶·Î º¯°æ(µ¥ÀÌÅÍ ±¸Á¶ ÆÄ¾Ç, ÅÃ½ºÆ® °ü·Ã ÇÔ¼ö Àû¿ë ½¬¿ò)
new = new %>% as_tibble()
new

library(dplyr)


install.packages("tidytext")
library(tidytext)

#´Ü¾î ºóµµ¼ö ºĞ¼® À§ÇØ¼­ token= "words"¿É¼Ç »ç¿ë
word_space <- new %>% unnest_tokens(input = value, output = word, token = "words")
word_space
#´Ü¾î ºóµµ°¡ ³ôÀº¼ø Á¤·Ä
word_space <- word_space %>% count(word, sort = T)
word_space

#ºóµµ¼ö ³ôÀº ¼ø¼­·Î top20º¸¿©ÁÜ
top20 <- word_space %>% head(20)
top20


install.packages("ggplot2")
library(ggplot2)

#ggplot¸¦ »ç¿ëÇá¼­ ´Ü¾î ºóµµ¼ö top20 ±×·¡ÇÁ·Î º¸¿©ÁÜ
ggplot(top20, aes(x = reorder(word, n), y = n)) + geom_col() + coord_flip() + geom_text(aes(label = n), hjust = -0.3)+ labs(title = "¿µÈ­ ´Ü¾î ºóµµ", x = NULL, y = NULL) + theme(title = element_text(size=20))


#3¹ø ¹®Á¦TF-IDF »ó´ëÀûÀ¸·Î Áß¿äÇÑ ´Ü¾î ºñ±³
install.packages("readr")
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidytext)
#csvÆÄÀÏÀ» ÀĞ¾î¿Â´Ù
raw_msheet <- read.csv("bom2.csv")
#csvÆÄÀÏ Ãâ·Â
raw_msheet

#githubÆÄÀÏÀ» ¹Ş¾Æ KONLP ¼³Ä¡
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP)
#ºÒÇÊ¿äÇÑ°Å Á¦°Å À§ÇØ ÀüÃ³¸® ÇØÁØ´Ù
msheet <- raw_msheet %>% mutate(value = str_replace_all(value, "[^°¡-ÆR]"," "), value = str_squish(value))
#µ¥ÀÌÅÍ¸¦ ÅäÅ«È­ ÇÑ´Ù
msheet <- msheet %>% unnest_tokens(input = value, output = word, token = extractNoun)
#´Ü¾îºóµµ ±¸ÇÏ±â
freq <- msheet %>% count(movie, word) %>% filter(str_count(word)>1)
freq

#freqÀÇ TF-IDF¸¦ ±¸ÇÑ´Ù(³»¸²Â÷¼ø Á¤·Ä·Î ½ÇÇà)
freq <- freq %>% bind_tf_idf(term= word, document = movie, n=n)%>% arrange(-tf_idf)
#tf-IDf °ªÀÌ°¡ ³ª¿Â´Ù
freq



library(dplyr)
library(readr)
#freq¿¡¼­ ¿µÈ­ ÀÌ¸§ "º½³¯"À» ±âÁØÀ¸·Î ³ª´«´Ù
freq %>% filter(movie == "º½³¯")
#freq¿¡¼­ ¿µÈ­ ÀÌ¸§ "¼­¿ï±«´ã"À» ±âÁØÀ¸·Î ³ª´«´Ù
freq %>% filter(movie == "¼­¿ï±«´ã")

#freqÀÇ top10ÀÎ ´Ü¾îµé¸¦ ¿µÈ­ Á¦¸ñÀ» ±âÁØÀ¸·Î ¹­¾îÁØ´Ù.
top10 <- freq %>% group_by(movie) %>% slice_max(tf_idf, n=10, with_ties = F)
#top10ÀÇ ¿µÈ­ ºÎºĞÀ» factorÇü µ¥ÀÌÅÍ·Î °¡°øÇÑ´Ù
top10$movie <- factor(top10$movie, levels = c("º½³¯","¼­¿ï±«´ã"))
#º½³¯°ú ¼­¿ï±«´ã ´Ü¾î ºóµµ¼ö top10 ±×·¡ÇÁ¸¦ ¸¸µç´Ù
ggplot(top10,aes(x=reorder_within(word,tf_idf,movie), y = tf_idf, fill = movie)) + geom_col(show.legend = F) + coord_flip() + facet_wrap(~movie, scales = "free", ncol = 2) + scale_x_reordered() + labs(x=NULL)










#4¹ø °¨Á¤»çÀü
library(stringr)
library(dplyr)
library(readr)
library(textclean)
#°¨Á¤ »çÀüÀ» ÀĞ¾î¿Â´Ù
dic <- read_csv("knu_sentiment_lexicon.csv")

#csvÆÄÀÏÀ» ÀĞ¾î¿Â´Ù
movie_new_comment <- read.csv("bom2.csv")
#csvÆÄÀÏ¿¡¼­ °íÀ¯ ¹øÈ£¸¦ ¸¸µé¾îÁÖ°í, htmlÆ¯¼ö¹®ÀÚ¸¦ Á¦°ÅÇÑ´Ù
new_comment <- movie_new_comment %>% mutate(id=row_number(), value = str_squish(replace_html(value)))
#µ¥ÀÌÅÍ ±¸Á¶ È®ÀÎ
glimpse(new_comment)

library(tidytext)

#´Ü¾î¸¦ ±âÁØÀ¸·Î ÅäÅ«È­ ÇØÁİ´Ï´Ù.
word_comment <- new_comment %>% unnest_tokens(input = value, output = word, token = "words", drop = F)
#word_comment¿¡¼­ word¿Í valueºÎºĞÀ» °í¸¥´Ù
word_comment %>% select(word,value)

#°¨Á¤»çÀü ÀĞ±â
dic <- read_csv("knu_sentiment_lexicon.csv")
#´Ü¾î¿¡ °¨Á¤Á¡¼ö ºÎ¿©
word_comment <- word_comment %>% left_join(dic, by ="word")%>% mutate(polarity = ifelse(is.na(polarity), 0, polarity))
#°¨Á¤Á¡¼ö º¸±â
word_comment %>% select(word, polarity)

#°¨Á¤ÀÌ µé¾î³­ ´Ü¾î ºĞ·ù
word_comment <- word_comment %>% mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))
#´Ü¾î¼ö º¸±â
word_comment %>% count(sentiment)

#±àÁ¤ »óÀ§ 10°³, ºÎÁ¤ »óÀ§10°³·Î ÃßÃâ
top10_sentiment <- word_comment %>% filter(sentiment != "neu")%>% count(sentiment, word) %>% group_by(sentiment)%>% slice_max(n, n=10)
top10_sentiment

library(ggplot2)
#±àÁ¤ , ºÎÁ¤ »óÀ§ 10À§±×·¡ÇÁ¸¦ º¸¿©ÁØ´Ù
ggplot(top10_sentiment, aes(x=reorder(word, n), y = n, fill = sentiment)) + geom_col() + coord_flip() + geom_text(aes(label = n), hjust = - 0.3) + facet_wrap(~sentiment, scales = "free") + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) + labs(x= NULL)


#³»¿ëÀÌ ¶È°°Àº ´ñ±ÛÀ» ¼­·Î ´Ù¸¥ ´ñ±Û·Î Ãë±ŞÇÏ¿©polarity ÇÕ»êÇÏ±âÀ§ÇØ ¾´´Ù.
score_comment <- word_comment %>% group_by(id, value) %>% summarise(score = sum(polarity)) %>% ungroup()

#´ñ±Ûº° °¨Á¤Á¡¼ö °è»ê
score_comment %>% select(score,value)

#°¨Á¤Á¡¼ö ³ôÀº ´ñ±Û Ã£±â(±àÁ¤)
score_comment %>% select(score, value)%>% arrange(-score)
#°¨Á¤Á¡¼ö ³ôÀº ´ñ±Û Ã£±â(ºÎÁ¤)
score_comment %>% select(score, value)%>% arrange(score)


#°¨Á¤Á¡¼ö¸¦ ±âÁØÀ¸·Î pos,neg,neu·Î ³ª´«´Ù.
score_comment <- score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))
#ºñÀ² °è»ê
freq_score <- score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
#°è»ê °á°ú º¸¿©ÁÜ
freq_score
#°¨Á¤ ºĞ·ùÈù°¥ ¸·´ë±×·¡ÇÁ·Î º¸¿©ÁÜ
ggplot(freq_score, aes(x= sentiment, y = n, fill = sentiment)) + geom_col() + geom_text(aes(label = n), vjust = -0.3)+ scale_x_discrete(limits = c("pos","neu","neg"))



#5¹ø

#KNU ÇÑ±¹¾î °¨Á¤»çÀü ¿øº»¿¡¼­ "¹«¼­¿ò"ÀÇ polarity¸¦ +2·Î ¼öÁ¤
new_dic <- dic %>% mutate(polarity = ifelse(word %in% c("¹«¼­¿ò"), 2, polarity))
#¹«¼­¿ò Á¡¼ö ¼öÁ¤µÈ°Å º¸¿©ÁÜ
new_dic %>% filter(word %in% c("¹«¼­¿ò"))

#¼öÁ¤ÇÑ »çÀüÀ¸·Î °¨Á¤Á¡¼ö ºÎ¿©
new_word_comment <- word_comment %>% select(-polarity)%>% left_join(new_dic, by = "word")%>% mutate(polarity = ifelse(is.na(polarity),0,polarity))
#³»¿ëÀÌ ¶È°°Àº ´ñ±ÛÀ» ¼­·Î ´Ù¸¥ ´ñ±Û·Î Ãë±ŞÇÏ¿©polarity ÇÕ»êÇÏ±âÀ§ÇØ ¾´´Ù.
new_score_comment <- new_word_comment %>% group_by(id, value)%>% summarise(score = sum(polarity))%>% ungroup()
#°¨Á¤Á¡¼ö ³ôÀº ¼øÀ¸·Î Ãâ·Â(³»¸²Â÷¼ø Á¤·Ä·Î ½ÇÇà)
new_score_comment %>% select(score , value)%>% arrange(-score)
#ÀÌÀü °¨Á¤Á¡¼ö º¸¿©ÁÜ
score_comment %>% select(score, value)%>% arrange(-score)

#°¨Á¤ºĞ·ù : score ±âÁØ 1ÀÌ»ópos, -1ÀÌÇÏ neg, ±× ¿Ü neu
new_score_comment <- new_score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))
#ºñÀ²°è»ê
new_freq_score <- new_score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
#ÇöÀç ºñÀ²
new_freq_score
#ÀÌÀüºñÀ²
freq_score



library(tidytext)
#»õ·Î¿î ´Ü¾î »ı¼º
newword <- tibble(word = c("±â´ëº¸´Ù","º¸Áö¸¶¼À"), polarity = c(2,-2))
#»õ·Î¿î ´Ü¾î Ãß°¡
newword_dic <- bind_rows(dic,newword)
#µÚ¿¡ Ãß°¡µÈ Á¡¼ö ¸ñ·Ï º¸±â
tail(newword_dic)


#¼öÁ¤ÇÑ »çÀüÀ¸·Î °¨Á¤Á¡¼ö ºÎ¿©
new_word_comment <- word_comment %>% select(-polarity)%>% left_join(newword_dic, by = "word")%>% mutate(polarity = ifelse(is.na(polarity),0,polarity))
#³»¿ëÀÌ ¶È°°Àº ´ñ±ÛÀ» ¼­·Î ´Ù¸¥ ´ñ±Û·Î Ãë±ŞÇÏ¿©polarity ÇÕ»êÇÏ±âÀ§ÇØ ¾´´Ù.
new_score_comment <- new_word_comment %>% group_by(id, value)%>% summarise(score = sum(polarity))%>% ungroup()
#°¨Á¤Á¡¼ö ³ôÀº ¼øÀ¸·Î Ãâ·Â(³»¸²Â÷¼ø Á¤·Ä·Î ½ÇÇà)
new_score_comment %>% select(score , value)%>% arrange(-score)
#ÀÌÀü °¨Á¤Á¡¼ö º¸¿©ÁÜ
score_comment %>% select(score, value)%>% arrange(-score)


#»õ·Î¿î °¨Á¤Á¡¼ö¸¦ ±âÁØÀ¸·Î pos,neg,neu·Î ³ª´«´Ù.
new_score_comment <- new_score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))
#ºñÀ² °è»ê
new_freq_score <- new_score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
#°è»ê °á°ú º¸¿©ÁÜ
new_freq_score
#»õ·Ó°Ô °¨Á¤ ºĞ·ùÈù°É ¸·´ë±×·¡ÇÁ·Î º¸¿©ÁÜ
ggplot(new_freq_score, aes(x= sentiment, y = n, fill = sentiment)) + geom_col() + geom_text(aes(label = n), vjust = -0.3)+ scale_x_discrete(limits = c("pos","neu","neg"))

#ÀÌÀü °¨Á¤ ºĞ·ùÈù°¥ ¸·´ë±×·¡ÇÁ·Î º¸¿©ÁÜ
ggplot(freq_score, aes(x= sentiment, y = n, fill = sentiment)) + geom_col() + geom_text(aes(label = n), vjust = -0.3)+ scale_x_discrete(limits = c("pos","neu","neg"))



