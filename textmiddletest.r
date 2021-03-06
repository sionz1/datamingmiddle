#2번 문제

#파일 실행되는 경로 확인
getwd()
#텍스트 파일 읽어옴
raw_bom <- readLines("bom.txt", encoding = "UTF-8")
#텍스트 파일 위에서 부터 출력
head(raw_bom)#텍스트 파일 위에서 부터 출력


install.packages("stringr")
library(stringr)


#한글이 아닌 모든 문자 제거
new = raw_bom %>% str_replace_all("[^가-힣]", replacement = " ")
#위에서 부터 출력
head(new)

#tibble구조로 변경(데이터 구조 파악, 택스트 관련 함수 적용 쉬움)
new = new %>% as_tibble()
new

library(dplyr)


install.packages("tidytext")
library(tidytext)

#단어 빈도수 분석 위해서 token= "words"옵션 사용
word_space <- new %>% unnest_tokens(input = value, output = word, token = "words")
word_space
#단어 빈도가 높은순 정렬
word_space <- word_space %>% count(word, sort = T)
word_space

#빈도수 높은 순서로 top20보여줌
top20 <- word_space %>% head(20)
top20


install.packages("ggplot2")
library(ggplot2)

#ggplot를 사용햐서 단어 빈도수 top20 그래프로 보여줌
ggplot(top20, aes(x = reorder(word, n), y = n)) + geom_col() + coord_flip() + geom_text(aes(label = n), hjust = -0.3)+ labs(title = "영화 단어 빈도", x = NULL, y = NULL) + theme(title = element_text(size=20))


#3번 문제TF-IDF 상대적으로 중요한 단어 비교
install.packages("readr")
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidytext)
#csv파일을 읽어온다
raw_msheet <- read.csv("bom2.csv")
#csv파일 출력
raw_msheet

#github파일을 받아 KONLP 설치
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP)
#불필요한거 제거 위해 전처리 해준다
msheet <- raw_msheet %>% mutate(value = str_replace_all(value, "[^가-힣]"," "), value = str_squish(value))
#데이터를 토큰화 한다
msheet <- msheet %>% unnest_tokens(input = value, output = word, token = extractNoun)
#단어빈도 구하기
freq <- msheet %>% count(movie, word) %>% filter(str_count(word)>1)
freq

#freq의 TF-IDF를 구한다(내림차순 정렬로 실행)
freq <- freq %>% bind_tf_idf(term= word, document = movie, n=n)%>% arrange(-tf_idf)
#tf-IDf 값이가 나온다
freq



library(dplyr)
library(readr)
#freq에서 영화 이름 "봄날"을 기준으로 나눈다
freq %>% filter(movie == "봄날")
#freq에서 영화 이름 "서울괴담"을 기준으로 나눈다
freq %>% filter(movie == "서울괴담")

#freq의 top10인 단어들를 영화 제목을 기준으로 묶어준다.
top10 <- freq %>% group_by(movie) %>% slice_max(tf_idf, n=10, with_ties = F)
#top10의 영화 부분을 factor형 데이터로 가공한다
top10$movie <- factor(top10$movie, levels = c("봄날","서울괴담"))
#봄날과 서울괴담 단어 빈도수 top10 그래프를 만든다
ggplot(top10,aes(x=reorder_within(word,tf_idf,movie), y = tf_idf, fill = movie)) + geom_col(show.legend = F) + coord_flip() + facet_wrap(~movie, scales = "free", ncol = 2) + scale_x_reordered() + labs(x=NULL)










#4번 감정사전
library(stringr)
library(dplyr)
library(readr)
library(textclean)
#감정 사전을 읽어온다
dic <- read_csv("knu_sentiment_lexicon.csv")

#csv파일을 읽어온다
movie_new_comment <- read.csv("bom2.csv")
#csv파일에서 고유 번호를 만들어주고, html특수문자를 제거한다
new_comment <- movie_new_comment %>% mutate(id=row_number(), value = str_squish(replace_html(value)))
#데이터 구조 확인
glimpse(new_comment)

library(tidytext)

#단어를 기준으로 토큰화 해줍니다.
word_comment <- new_comment %>% unnest_tokens(input = value, output = word, token = "words", drop = F)
#word_comment에서 word와 value부분을 고른다
word_comment %>% select(word,value)

#감정사전 읽기
dic <- read_csv("knu_sentiment_lexicon.csv")
#단어에 감정점수 부여
word_comment <- word_comment %>% left_join(dic, by ="word")%>% mutate(polarity = ifelse(is.na(polarity), 0, polarity))
#감정점수 보기
word_comment %>% select(word, polarity)

#감정이 들어난 단어 분류
word_comment <- word_comment %>% mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))
#단어수 보기
word_comment %>% count(sentiment)

#긍정 상위 10개, 부정 상위10개로 추출
top10_sentiment <- word_comment %>% filter(sentiment != "neu")%>% count(sentiment, word) %>% group_by(sentiment)%>% slice_max(n, n=10)
top10_sentiment

library(ggplot2)
#긍정 , 부정 상위 10위그래프를 보여준다
ggplot(top10_sentiment, aes(x=reorder(word, n), y = n, fill = sentiment)) + geom_col() + coord_flip() + geom_text(aes(label = n), hjust = - 0.3) + facet_wrap(~sentiment, scales = "free") + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) + labs(x= NULL)


#내용이 똑같은 댓글을 서로 다른 댓글로 취급하여polarity 합산하기위해 쓴다.
score_comment <- word_comment %>% group_by(id, value) %>% summarise(score = sum(polarity)) %>% ungroup()

#댓글별 감정점수 계산
score_comment %>% select(score,value)

#감정점수 높은 댓글 찾기(긍정)
score_comment %>% select(score, value)%>% arrange(-score)
#감정점수 높은 댓글 찾기(부정)
score_comment %>% select(score, value)%>% arrange(score)


#감정점수를 기준으로 pos,neg,neu로 나눈다.
score_comment <- score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))
#비율 계산
freq_score <- score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
#계산 결과 보여줌
freq_score
#감정 분류힌갈 막대그래프로 보여줌
ggplot(freq_score, aes(x= sentiment, y = n, fill = sentiment)) + geom_col() + geom_text(aes(label = n), vjust = -0.3)+ scale_x_discrete(limits = c("pos","neu","neg"))



#5번

#KNU 한국어 감정사전 원본에서 "무서움"의 polarity를 +2로 수정
new_dic <- dic %>% mutate(polarity = ifelse(word %in% c("무서움"), 2, polarity))
#무서움 점수 수정된거 보여줌
new_dic %>% filter(word %in% c("무서움"))

#수정한 사전으로 감정점수 부여
new_word_comment <- word_comment %>% select(-polarity)%>% left_join(new_dic, by = "word")%>% mutate(polarity = ifelse(is.na(polarity),0,polarity))
#내용이 똑같은 댓글을 서로 다른 댓글로 취급하여polarity 합산하기위해 쓴다.
new_score_comment <- new_word_comment %>% group_by(id, value)%>% summarise(score = sum(polarity))%>% ungroup()
#감정점수 높은 순으로 출력(내림차순 정렬로 실행)
new_score_comment %>% select(score , value)%>% arrange(-score)
#이전 감정점수 보여줌
score_comment %>% select(score, value)%>% arrange(-score)

#감정분류 : score 기준 1이상pos, -1이하 neg, 그 외 neu
new_score_comment <- new_score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))
#비율계산
new_freq_score <- new_score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
#현재 비율
new_freq_score
#이전비율
freq_score



library(tidytext)
#새로운 단어 생성
newword <- tibble(word = c("기대보다","보지마셈"), polarity = c(2,-2))
#새로운 단어 추가
newword_dic <- bind_rows(dic,newword)
#뒤에 추가된 점수 목록 보기
tail(newword_dic)


#수정한 사전으로 감정점수 부여
new_word_comment <- word_comment %>% select(-polarity)%>% left_join(newword_dic, by = "word")%>% mutate(polarity = ifelse(is.na(polarity),0,polarity))
#내용이 똑같은 댓글을 서로 다른 댓글로 취급하여polarity 합산하기위해 쓴다.
new_score_comment <- new_word_comment %>% group_by(id, value)%>% summarise(score = sum(polarity))%>% ungroup()
#감정점수 높은 순으로 출력(내림차순 정렬로 실행)
new_score_comment %>% select(score , value)%>% arrange(-score)
#이전 감정점수 보여줌
score_comment %>% select(score, value)%>% arrange(-score)


#새로운 감정점수를 기준으로 pos,neg,neu로 나눈다.
new_score_comment <- new_score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))
#비율 계산
new_freq_score <- new_score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
#계산 결과 보여줌
new_freq_score
#새롭게 감정 분류힌걸 막대그래프로 보여줌
ggplot(new_freq_score, aes(x= sentiment, y = n, fill = sentiment)) + geom_col() + geom_text(aes(label = n), vjust = -0.3)+ scale_x_discrete(limits = c("pos","neu","neg"))

#이전 감정 분류힌갈 막대그래프로 보여줌
ggplot(freq_score, aes(x= sentiment, y = n, fill = sentiment)) + geom_col() + geom_text(aes(label = n), vjust = -0.3)+ scale_x_discrete(limits = c("pos","neu","neg"))



