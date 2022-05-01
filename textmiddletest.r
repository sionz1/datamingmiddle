#2�� ����

#���� ����Ǵ� ��� Ȯ��
getwd()
#�ؽ�Ʈ ���� �о��
raw_bom <- readLines("bom.txt", encoding = "UTF-8")
#�ؽ�Ʈ ���� ������ ���� ���
head(raw_bom)#�ؽ�Ʈ ���� ������ ���� ���


install.packages("stringr")
library(stringr)


#�ѱ��� �ƴ� ��� ���� ����
new = raw_bom %>% str_replace_all("[^��-�R]", replacement = " ")
#������ ���� ���
head(new)

#tibble������ ����(������ ���� �ľ�, �ý�Ʈ ���� �Լ� ���� ����)
new = new %>% as_tibble()
new

library(dplyr)


install.packages("tidytext")
library(tidytext)

#�ܾ� �󵵼� �м� ���ؼ� token= "words"�ɼ� ���
word_space <- new %>% unnest_tokens(input = value, output = word, token = "words")
word_space
#�ܾ� �󵵰� ������ ����
word_space <- word_space %>% count(word, sort = T)
word_space

#�󵵼� ���� ������ top20������
top20 <- word_space %>% head(20)
top20


install.packages("ggplot2")
library(ggplot2)

#ggplot�� ����Ἥ �ܾ� �󵵼� top20 �׷����� ������
ggplot(top20, aes(x = reorder(word, n), y = n)) + geom_col() + coord_flip() + geom_text(aes(label = n), hjust = -0.3)+ labs(title = "��ȭ �ܾ� ��", x = NULL, y = NULL) + theme(title = element_text(size=20))


#3�� ����TF-IDF ��������� �߿��� �ܾ� ��
install.packages("readr")
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidytext)
#csv������ �о�´�
raw_msheet <- read.csv("bom2.csv")
#csv���� ���
raw_msheet

#github������ �޾� KONLP ��ġ
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP)
#���ʿ��Ѱ� ���� ���� ��ó�� ���ش�
msheet <- raw_msheet %>% mutate(value = str_replace_all(value, "[^��-�R]"," "), value = str_squish(value))
#�����͸� ��ūȭ �Ѵ�
msheet <- msheet %>% unnest_tokens(input = value, output = word, token = extractNoun)
#�ܾ�� ���ϱ�
freq <- msheet %>% count(movie, word) %>% filter(str_count(word)>1)
freq

#freq�� TF-IDF�� ���Ѵ�(�������� ���ķ� ����)
freq <- freq %>% bind_tf_idf(term= word, document = movie, n=n)%>% arrange(-tf_idf)
#tf-IDf ���̰� ���´�
freq



library(dplyr)
library(readr)
#freq���� ��ȭ �̸� "����"�� �������� ������
freq %>% filter(movie == "����")
#freq���� ��ȭ �̸� "���ﱫ��"�� �������� ������
freq %>% filter(movie == "���ﱫ��")

#freq�� top10�� �ܾ�鸦 ��ȭ ������ �������� �����ش�.
top10 <- freq %>% group_by(movie) %>% slice_max(tf_idf, n=10, with_ties = F)
#top10�� ��ȭ �κ��� factor�� �����ͷ� �����Ѵ�
top10$movie <- factor(top10$movie, levels = c("����","���ﱫ��"))
#������ ���ﱫ�� �ܾ� �󵵼� top10 �׷����� �����
ggplot(top10,aes(x=reorder_within(word,tf_idf,movie), y = tf_idf, fill = movie)) + geom_col(show.legend = F) + coord_flip() + facet_wrap(~movie, scales = "free", ncol = 2) + scale_x_reordered() + labs(x=NULL)










#4�� ��������
library(stringr)
library(dplyr)
library(readr)
library(textclean)
#���� ������ �о�´�
dic <- read_csv("knu_sentiment_lexicon.csv")

#csv������ �о�´�
movie_new_comment <- read.csv("bom2.csv")
#csv���Ͽ��� ���� ��ȣ�� ������ְ�, htmlƯ�����ڸ� �����Ѵ�
new_comment <- movie_new_comment %>% mutate(id=row_number(), value = str_squish(replace_html(value)))
#������ ���� Ȯ��
glimpse(new_comment)

library(tidytext)

#�ܾ �������� ��ūȭ ���ݴϴ�.
word_comment <- new_comment %>% unnest_tokens(input = value, output = word, token = "words", drop = F)
#word_comment���� word�� value�κ��� ����
word_comment %>% select(word,value)

#�������� �б�
dic <- read_csv("knu_sentiment_lexicon.csv")
#�ܾ �������� �ο�
word_comment <- word_comment %>% left_join(dic, by ="word")%>% mutate(polarity = ifelse(is.na(polarity), 0, polarity))
#�������� ����
word_comment %>% select(word, polarity)

#������ �� �ܾ� �з�
word_comment <- word_comment %>% mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))
#�ܾ�� ����
word_comment %>% count(sentiment)

#���� ���� 10��, ���� ����10���� ����
top10_sentiment <- word_comment %>% filter(sentiment != "neu")%>% count(sentiment, word) %>% group_by(sentiment)%>% slice_max(n, n=10)
top10_sentiment

library(ggplot2)
#���� , ���� ���� 10���׷����� �����ش�
ggplot(top10_sentiment, aes(x=reorder(word, n), y = n, fill = sentiment)) + geom_col() + coord_flip() + geom_text(aes(label = n), hjust = - 0.3) + facet_wrap(~sentiment, scales = "free") + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) + labs(x= NULL)


#������ �Ȱ��� ����� ���� �ٸ� ��۷� ����Ͽ�polarity �ջ��ϱ����� ����.
score_comment <- word_comment %>% group_by(id, value) %>% summarise(score = sum(polarity)) %>% ungroup()

#��ۺ� �������� ���
score_comment %>% select(score,value)

#�������� ���� ��� ã��(����)
score_comment %>% select(score, value)%>% arrange(-score)
#�������� ���� ��� ã��(����)
score_comment %>% select(score, value)%>% arrange(score)


#���������� �������� pos,neg,neu�� ������.
score_comment <- score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))
#���� ���
freq_score <- score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
#��� ��� ������
freq_score
#���� �з����� ����׷����� ������
ggplot(freq_score, aes(x= sentiment, y = n, fill = sentiment)) + geom_col() + geom_text(aes(label = n), vjust = -0.3)+ scale_x_discrete(limits = c("pos","neu","neg"))



#5��

#KNU �ѱ��� �������� �������� "������"�� polarity�� +2�� ����
new_dic <- dic %>% mutate(polarity = ifelse(word %in% c("������"), 2, polarity))
#������ ���� �����Ȱ� ������
new_dic %>% filter(word %in% c("������"))

#������ �������� �������� �ο�
new_word_comment <- word_comment %>% select(-polarity)%>% left_join(new_dic, by = "word")%>% mutate(polarity = ifelse(is.na(polarity),0,polarity))
#������ �Ȱ��� ����� ���� �ٸ� ��۷� ����Ͽ�polarity �ջ��ϱ����� ����.
new_score_comment <- new_word_comment %>% group_by(id, value)%>% summarise(score = sum(polarity))%>% ungroup()
#�������� ���� ������ ���(�������� ���ķ� ����)
new_score_comment %>% select(score , value)%>% arrange(-score)
#���� �������� ������
score_comment %>% select(score, value)%>% arrange(-score)

#�����з� : score ���� 1�̻�pos, -1���� neg, �� �� neu
new_score_comment <- new_score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))
#�������
new_freq_score <- new_score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
#���� ����
new_freq_score
#��������
freq_score



library(tidytext)
#���ο� �ܾ� ����
newword <- tibble(word = c("��뺸��","��������"), polarity = c(2,-2))
#���ο� �ܾ� �߰�
newword_dic <- bind_rows(dic,newword)
#�ڿ� �߰��� ���� ��� ����
tail(newword_dic)


#������ �������� �������� �ο�
new_word_comment <- word_comment %>% select(-polarity)%>% left_join(newword_dic, by = "word")%>% mutate(polarity = ifelse(is.na(polarity),0,polarity))
#������ �Ȱ��� ����� ���� �ٸ� ��۷� ����Ͽ�polarity �ջ��ϱ����� ����.
new_score_comment <- new_word_comment %>% group_by(id, value)%>% summarise(score = sum(polarity))%>% ungroup()
#�������� ���� ������ ���(�������� ���ķ� ����)
new_score_comment %>% select(score , value)%>% arrange(-score)
#���� �������� ������
score_comment %>% select(score, value)%>% arrange(-score)


#���ο� ���������� �������� pos,neg,neu�� ������.
new_score_comment <- new_score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))
#���� ���
new_freq_score <- new_score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
#��� ��� ������
new_freq_score
#���Ӱ� ���� �з����� ����׷����� ������
ggplot(new_freq_score, aes(x= sentiment, y = n, fill = sentiment)) + geom_col() + geom_text(aes(label = n), vjust = -0.3)+ scale_x_discrete(limits = c("pos","neu","neg"))

#���� ���� �з����� ����׷����� ������
ggplot(freq_score, aes(x= sentiment, y = n, fill = sentiment)) + geom_col() + geom_text(aes(label = n), vjust = -0.3)+ scale_x_discrete(limits = c("pos","neu","neg"))



