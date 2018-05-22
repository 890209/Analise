library(Rfacebook) # usado para extrair dados do facebook
library(tidyverse) # pq nao da pra viver sem
library(ggExtra)
library(magrittr) # ???
library(lubridate)
library(stringr) # essencial para trabalhar com textos
library(tidytext) # um dos melhores pacotes para text mining
library(lexiconPT)
install.packages("tidyverse")
install.packages("tidytext")
install.packages("lexiconPT")
install.packages("stringr")

cand2 <- read.csv("/Users/Ariel/Desktop/TG/Lula/searchLula.csv", header = TRUE, encoding = "UCS-2LE")
cand2$text %>% iconv(to = "ASCII//TRANSLIT")
View(cand2)
cand2$text %>% iconv(sub="", 'UTF-8', 'ASCII')

glimpse(tweets_df)

data("oplexicon_v3.0")
data("sentiLex_lem_PT02")

op30 <- oplexicon_v3.0
sent <- sentiLex_lem_PT02

glimpse(op30)
glimpse(sent)

cand2["comment_id"] = seq.int(nrow(cand2));

cand2 %<>% mutate(comment_id = row_number())
cand2 %>% mutate(comment_id = row_number())

glimpse(cand2)
df_comments_unnested <- cand2 %>% unnest_tokens(term, text)

df_comments_unnested %>%
  select(comment_id, term) %>%
  head(20)

View(df_comments_unnested)
df_comments_unnested %>% 
  left_join(op30, by = "term") %>% 
  left_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>% 
  select(comment_id, term, polarity, lex_polarity) %>% 
  head(10)


df_comments_unnested <- df_comments_unnested %>% 
  inner_join(op30, by = "term") %>% 
  inner_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>% 
  group_by(comment_id) %>% 
  summarise(
    comment_sentiment_op = sum(polarity),
    comment_sentiment_lex = sum(lex_polarity),
    n_words = n()
  ) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    most_neg = min(comment_sentiment_lex, comment_sentiment_op),
    most_pos = max(comment_sentiment_lex, comment_sentiment_op)
  )

head(df_comments_unnested)
View(df_comments_unnested)

p <- df_comments_unnested %>% 
  ggplot(aes(x = comment_sentiment_op, y = comment_sentiment_lex)) +
  geom_point(aes(color = n_words)) + 
  scale_color_continuous(low = "green", high = "red") +
  labs(x = "Polaridade no OpLexicon", y = "Polaridade no SentiLex") +
  #geom_smooth(method = "lm") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed")

p


df_comments_unnested %>% filter(between(comment_sentiment_op, -4, 3))

most_pos <- which.max(df_comments_unnested$most_pos)
most_neg <- which.min(df_comments_unnested$most_neg)
cat(cand2$text[cand2$comment_id == df_comments_unnested$comment_id[most_pos]])

cat(cand2$text[cand2$comment_id == df_comments_unnested$comment_id[most_neg]])

cand2<-cand2 %>% inner_join(
  df_comments_unnested %>% select(comment_id, sentiment = comment_sentiment_op),
  by = "comment_id"
)
glimpse(cand15)
# criar coluna de data (variavel da classe Date)
cand2$data <- as.Date(cand2$created)

View(cand2)

##
df_comments_wide <- cand2 %>% 
  # filtrar fora palavras neutras
  filter(sentiment != 0) %>% 
  # converter numerico para categorico
  mutate(sentiment = ifelse(sentiment < 0, "negativo", "positivo")) %>% 
  # agrupar os dados
  count(data, text, sentiment)%>%
  ##count( text, sentiment) %>% 
  # converter para formato wide
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentimento = positivo - negativo) %>% 
  ungroup() %>% 
  arrange(data)

head(df_comments_wide) %>% knitr::kable()

View(df_comments_wide)


df_comments_wide %>% 
  arrange(sentimento) %>% 
  filter(row_number() == 1 | row_number() == nrow(df_comments_wide)) %>% 
  knitr::kable()


cand2 %>% 
  mutate(
    temer = str_detect(str_to_lower(text), "temer"),
    lula = str_detect(str_to_lower(text), "lula"),
    pmdb = str_detect(str_to_lower(text), "pmdb"),
    psdb = str_detect(str_to_lower(text), "psdb"),
    pt = str_detect(str_to_lower(text), "pt"),
    dilma = str_detect(str_to_lower(text), "dilma"),
    doria = str_detect(str_to_lower(text), "doria"),
    governo = str_detect(str_to_lower(text), "governo"),
    bolsonaro = str_detect(str_to_lower(text), "bolsonaro")
  ) %>% 
  gather(termo, eh_presente, temer:bolsonaro) %>% 
  filter(eh_presente) %>% 
  group_by(termo) %>% 
  summarise(sentiment = mean(sentiment)) %>% 
  ggplot(aes(x = termo, y = sentiment)) + 
  geom_col(fill = "#C10534")

View(df_comments_wide)
write.csv(df_comments_wide, file ="/Users/Ariel/Desktop/TG/Lula/AnaliseLula.csv",row.names=FALSE,
          col.names=T,sep=",")