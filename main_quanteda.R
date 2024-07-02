# Statement ---
# 分析人们对食用鹿肉的看法，及其影响因素。

# Preparation ----
pacman::p_load(
  openxlsx, dplyr, stopwords, topicmodels, ggplot2, ggsci, DescTools, 
  tidytext, quanteda, quanteda.textstats, LSX, showtext
)
showtext_auto()

# 问卷调查吃鹿肉态度得分、性别、对狩猎的态度分数等。
survey <- 
  read.xlsx("data_raw/kyoto_venison_raw.xlsx", sheet = "Num") %>% 
  tibble() %>% 
  rename_with(~ tolower(.x)) %>% 
  rename(id = "no.") %>% 
  mutate(
    id = as.character(id), 
    ven = as.character(q10), 
    hunting = as.integer(q11b)
  ) %>% 
  # select(id, gender, age, education, ven, hunting) %>% 
  # 加入Q10日语回答信息。
  left_join(
    read.xlsx("data_raw/kyoto_venison_raw.xlsx", sheet = "Text") %>%
      select(id = "No.", ven_reason = "Q10") %>% 
      mutate(id = as.character(id)), 
    by = "id"
  ) %>% 
  # 漏洞：该数据筛选操作是否合理？
  mutate(gender = case_when(
    gender == 2 ~ NA, gender == 0 | gender == 1 ~ gender, is.na(gender) ~ NA
  )) %>% 
  filter(!is.na(ven_reason)) %>% 
  # 漏洞：是否需要执行更加严格的数据筛选操作？
  # filter(
  #   !is.na(ven_reason), !is.na(ven), !is.na(hunting), 
  #   !is.na(gender), gender != 2, !is.na(age)
  # ) %>% 
  # 删除不符合要求的回答。383号受访者的回答是“わからない”。725号的回答是“Don't have any information about it .so,I can't say”。删除454号受访者的吃肉态度和回答，因为它们相互冲突：他的吃肉态度为-2，但是回答是“美味しい”。
  filter(id != "383", id != "725", id != "454") %>% 
  # 将部分非日文文本翻译成日文。用ChatGPT 3.5进行翻译，指令为“Translate the text to Japanese: [text]”。
  mutate(ven_reason = case_when(
    ven_reason == "没有这个习惯…像是要吃狗肉一样不舒服" ~ 
      "この習慣がないから…犬の肉を食べるみたいに気持ち悪い", 
    ven_reason == "鹿鹿那么可爱怎么可以吃鹿鹿~" ~ 
      "鹿はこんなに可愛いのに、どうして鹿を食べられるの？", 
    TRUE ~ ven_reason
  )) %>% 
  # 将一些假名换成同义汉字。
  mutate(ven_reason = gsub("シカ", "鹿", ven_reason)) %>% 
  mutate(ven_reason = gsub("よい", "良い", ven_reason)) %>% 
  mutate(ven_reason = gsub("おいしい", "美味しい", ven_reason)) %>% 
  mutate(ven_reason = gsub("おいしく", "美味しく", ven_reason)) %>% 
  mutate(ven_reason = gsub("美味い", "美味しい", ven_reason)) %>% 
  cbind(
    ., 
    sapply(head(letters, 8), function(letter) grepl(letter, .$q7)) %>% 
      as.data.frame() %>% 
      rename_with(~ paste0("q7_", .))
  )

# Statistic ----
# Function to do Kruskal test for subset of survey data. 
get_kruskal <- function(x_name, g_name) {
  survey_sub <- survey %>% 
    filter(!is.na({x_name}), !is.na({g_name}))
  kruskal.test(
    as.numeric(survey_sub[[x_name]]) ~ as.character(survey_sub[[g_name]])
  )
}

## Venison score ~ attributes ----
# 吃鹿肉态度～性别：朱珠已进行了卡方分析，此处基于每个受访者数据进行组间对比。
by(as.numeric(survey$ven), survey$gender, shapiro.test)
# 男女组均不符合正态分布，因此用非参数方法进行组间对比。
get_kruskal("ven", "gender")
# 结论：不同性别之间吃鹿肉态度有差异。
by(as.numeric(survey$ven), survey$gender, function(x) mean(x, na.rm = TRUE))
by(as.numeric(survey$ven), survey$gender, function(x) median(x, na.rm = TRUE))

# 吃鹿肉态度～年龄组：大部分年龄组不符合正态分布，因此用非参数方法。
by(as.numeric(survey$ven), survey$age, shapiro.test)
get_kruskal("ven", "age")

# 吃鹿肉态度～教育水平。
lapply(
  as.character(1:4), 
  function(x) {
    filter(survey, education == x)$ven %>% 
      as.numeric() %>% 
      shapiro.test()
  }
)
get_kruskal("ven", "education")

## Hunting score ~ attributes ----
# 狩猎态度～性别。
by(as.numeric(survey$hunting), survey$gender, shapiro.test)
get_kruskal("hunting", "gender")
# 卡方检验。
gender_hunting_chisq <- table(survey$gender, survey$hunting)
gender_hunting_chisq
chisq.test(gender_hunting_chisq)

# 狩猎态度～年龄组。
by(as.numeric(survey$hunting), survey$age, shapiro.test)
get_kruskal("hunting", "age")

# 狩猎态度～教育水平。
lapply(
  as.character(1:4), 
  function(x) {
    filter(survey, education == x)$hunting %>% 
      as.numeric() %>% 
      shapiro.test()
  }
)
get_kruskal("hunting", "education")

## Venison score ~ hunting score ----
cor.test(as.numeric(survey$ven), as.numeric(survey$hunting))

## Venison score ~ have land ----
# 吃肉态度和是否有农地林地的关系。
by(as.numeric(survey$ven), survey$q1, shapiro.test)
get_kruskal("ven", "q1")

## Venison score ~ encounter deer ----
# 吃肉态度和是否碰到鹿的关系。
by(as.numeric(survey$ven), survey$q2, shapiro.test)
get_kruskal("ven", "q2")

## Venison score ~ deer impression ----
# 吃肉态度和对鹿的印象的关系。
lapply(
  paste0("q7_", head(letters, 8)), 
  function(x) {
    print(x)
    get_kruskal("ven", x) %>% print()
  }
)

# 如果有显著差异，则进一步分析。
table(survey$q7_b)
survey %>% 
  filter(!is.na(ven)) %>% 
  group_by(q7_b, ven) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  ggplot() + 
  geom_col(aes(q7_b, n, fill = ven), position = "fill")
by(as.numeric(survey$ven), survey$q7_b, function(x) median(x, na.rm = T))
by(as.numeric(survey$ven), survey$q7_b, function(x) mean(x, na.rm = T))

table(survey$q7_e)
survey %>% 
  filter(!is.na(ven)) %>% 
  group_by(q7_e, ven) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  ggplot() + 
  geom_col(aes(q7_e, n, fill = ven), position = "fill")
by(as.numeric(survey$ven), survey$q7_e, function(x) median(x, na.rm = T))
by(as.numeric(survey$ven), survey$q7_e, function(x) mean(x, na.rm = T))

table(survey$q7_f)
survey %>% 
  filter(!is.na(ven)) %>% 
  group_by(q7_f, ven) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  ggplot() + 
  geom_col(aes(q7_f, n, fill = ven), position = "fill")
by(as.numeric(survey$ven), survey$q7_f, function(x) median(x, na.rm = T))
by(as.numeric(survey$ven), survey$q7_f, function(x) mean(x, na.rm = T))

# Text mining data ----
# 停止词：在分词之后去除的不重要的日语词汇。
jp_stop_word <- tibble(
  word = c(
    stopwords("ja", source = "marimo"), 
    # Hiragana in Japanese: define Unicode code points for hiragana characters and convert code points to UTF-8 characters. 
    strsplit(intToUtf8(c(12353:12435)), "")[[1]], 
    "amp", "ます", "です", "こと", "って", "てい", "という", "んで", "ので", 
    "なく", "など", "なる", "せん", "しま", "とか", "しょう", "ろう", "けど", 
    "さん", "あっ", "られる", "ぜひ", "てる", "なら", "思い", "思う", "れる", 
    "たく", "なので", "ただ", "ほうが", "もの", "かも", "たら", "そう", " ",
    "いと", "とも", "どちら", "にし", "しく", "しか", "しな", "すぎ", "ほしい"
  )
)

# Stopwords for sentiment analysis, inherit from last part. . 
quan_jp_stop_word <- jp_stop_word$word

# Corpus. 
quan_corp <- corpus(survey, text_field = "ven_reason")

# 自定义词典。
quan_dict <- 
  dictionary(list(
    not_good = "よく ない", 
    not_delicious = "美味 しく ない"
  ))

# Tokenization. 
quan_tok <- 
  tokens(
    quan_corp, 
    remove_symbols = TRUE, 
    remove_numbers = TRUE, 
    remove_url = TRUE, 
    remove_separators = TRUE, 
    remove_punct = TRUE
  ) %>% 
  # Bug: Should delete text with just a few words? 
  tokens_compound(pattern = quan_dict, concatenator = "") %>% 
  # Keep tokens in Japanese. 
  # Bug: Need to keep the words in other language? 
  tokens_select(
    pattern = "^[ぁ-んァ-ヶー一-龠]+$", valuetype = "regex", padding = TRUE
  ) %>% 
  tokens_remove(pattern = quan_jp_stop_word)

# Bug: 根据词语连接的频率找出词组：语料库比较小，未必要做。
# quan_tstat_col <- 
#   tok_pre %>% 
#   tokens_select("^[ァ-ヶー一-龠]+$", valuetype = "regex", padding = TRUE) %>%  
#   textstat_collocations() %>% 
#   # Remove some compounds. 
#   filter(
#     !grepl("生物多様性 小泉|小泉 生物多様性|小泉 生態系|氏 生態系",
#            collocation)
#   )
# quan_tok <- 
#   tokens_compound(
#     quan_tok_pre, quan_tstat_col[quan_tstat_col$z > 3,], concatenator = ""
#   ) %>% 
#   tokens_keep(min_nchar = 2) %>% 
#   tokens_remove(pattern = stopword)

# Text ID. 
text_id <- tibble(text = names(quan_tok), id = docvars(quan_tok)$id)

# Topic analysis ----
# 生成term-document矩阵。
quan_dtm <- dfm(quan_tok)
quan_dtm_tm <- convert(quan_dtm, to = "tm")

# 要选取几个主题呢？当话题数量太少时，大部分人的回答属于各个主题的概率差不多，这显然不合理；主题数很多时，各个回答属于各个话题的概率会更加有区分度，但是主题也不应太多。
# 函数：基于自定义主题数量，获得各个文档属于不同主题的概率。
quan_test_k_topic <- function(k_x) {
  # 生成LDA数据。
  lda <- LDA(quan_dtm_tm, k_x, control = list(seed = 1234))
  
  # 各篇文章属于各个主题的概率。
  id_topic_res <- tidy(lda, matrix = "gamma") %>% 
    mutate(k = k_x)
  return(id_topic_res)
}
# 测试主题数量思路：如果区分度越高的话，一个文档被划分到各个主题下的概率就越离散，基尼系数就越高。所以，可以给定一定范围的自定义主题数量，计算不同主题数量下，各个文档被划分到各个主题中的概率。看在那个自定义主题数量下，平均基尼系数最高，或者看看基尼系数在什么时候突变。
# 要测试的自定义主题数量范围。
range_k <- 2:15
# 存储测试结果。
quan_id_topic_test <- 
  lapply(range_k, quan_test_k_topic) %>% 
  setNames(as.character(range_k)) %>% 
  bind_rows() %>% 
  left_join(text_id, by = c("document" = "text"))
# 计算基尼系数之前，先直观观察不同主题下文档主题划分的区分度。如果区分度越高，格子之间的颜色差异就越明显。
quan_id_topic_test %>% 
  bind_rows() %>% 
  ggplot() + 
  geom_tile(aes(id, as.integer(topic), fill = gamma)) + 
  scale_fill_gradient(high = "red", low = "green") + 
  theme(axis.text.x = element_blank()) + 
  facet_wrap(.~ k, scales = "free_y")
# 计算基尼系数并比较不同自定义主题数量下基尼系数的差异。
quan_id_topic_test %>% 
  group_by(k, id) %>% 
  summarise(gini = Gini(gamma), .groups = "drop") %>% 
  mutate(k = factor(k, levels = as.character(range_k))) %>% 
  ggplot() + 
  geom_boxplot(aes(k, gini), alpha = 0.5) + 
  theme_bw() + 
  labs(x = "Topic number", y = "Gini")

# 正式进行主题模型分析。
# 获得测试范围内的最佳自定义主题数量：基尼系数最大，区分度最高。
# 计算突变点：平均基尼系数突然增加的点对应的主题数，就是目标主题数。
quan_gini_chg_rate <- quan_id_topic_test %>% 
  group_by(k, document) %>% 
  summarise(gini = Gini(gamma), .groups = "drop") %>% 
  group_by(k) %>% 
  summarise(gini = mean(gini), .groups = "drop") %>% 
  mutate(
    gini_lag = lag(gini), gini_mean_chg_rate = (gini - gini_lag) / gini_lag
  )
ggplot(quan_gini_chg_rate) + 
  geom_point(aes(k, gini_mean_chg_rate))
(
  quan_tar_k <- quan_gini_chg_rate %>% 
    filter(gini_mean_chg_rate == max(gini_mean_chg_rate, na.rm = TRUE)) %>% 
    pull(k)
)

# 构建LDA数据。
quan_lda <- LDA(quan_dtm_tm, k = quan_tar_k, control = list(seed = 1234))

# 评估区分度。
# 每个回答属于各个主题的概率。
quan_id_topic <- tidy(quan_lda, matrix = "gamma") %>% 
  # 计算基尼系数。
  rename(text = document) %>% 
  group_by(text) %>% 
  mutate(gini = Gini(gamma)) %>% 
  ungroup() %>% 
  # 区分回答：有偏向型的还是均衡型的。基尼系数越高，回答越偏向某个主题，系数越低，回答和多个主题相关的可能性越大。
  mutate(gini_cls = case_when(
    gini <= quantile(gini, 1/3) ~ "1", 
    gini <= quantile(gini, 2/3) ~ "2", 
    gini <= quantile(gini, 3/3) ~ "3"
  )) %>% 
  left_join(text_id, by = "text")
# 基尼系数越高的组，概率越离散。
quan_id_topic %>% 
  ggplot() + 
  geom_tile(aes(id, topic, fill = gamma)) + 
  facet_wrap(.~ gini_cls, scales = "free_x", ncol = 1) + 
  theme(axis.text.x = element_blank()) + 
  scale_fill_gradient(high = "red", low = "green")

# 转化成可阅读的主题数据，并取每个主题的前几位关键词。
# 漏洞：需要再增加停止词，并且统一日语词汇如“鹿”和“しか”。
(
  topic_word <- tidy(quan_lda, matrix = "beta") %>% 
    # Bug: Why there are empty term?
    filter(term != "") %>% 
    group_by(topic) %>% 
    slice_max(beta, n = 15) %>% 
    mutate(term = reorder_within(term, beta, topic)) %>% 
    summarise(term = list(term), .groups = "drop") %>% 
    mutate(
      term = unlist(lapply(term, function(x) paste0(x, collapse = ", ")))
    ) %>% 
    mutate(term = gsub("_", "", term), term = gsub("[0-9]", "", term))
)
# 导出结果。
# write.xlsx(topic_word, "data_proc/topic_word.xlsx")

# 抽出和各个主题匹配度最高的前10条回答，解读各个主题的含义。
(
  topic_text <- quan_id_topic %>% 
    group_by(topic) %>% 
    arrange(topic, -gamma) %>% 
    # slice_head(n = 10) %>% 
    ungroup() %>% 
    # 漏洞：需要提前更改id的类型。
    left_join(mutate(survey, id = as.character(id)), by = "id") %>% 
    select(id, topic, gamma, ven_reason) 
)
# 导出结果。
# write.xlsx(topic_text, "data_proc/topic_text.xlsx")

## Score ~ topic ----
# 对狩猎的态度和对食用鹿肉的态度之间的关系。
survey %>% 
  group_by(ven, hunting) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  ggplot() + 
  geom_col(aes(hunting, n, fill = ven), position = "fill")
# 漏洞：注意：极度反感狩猎和极度喜欢狩猎的人都比较支持食用鹿肉，为什么呢？

# 食用鹿肉打分和主题的关系。
# 从两个角度看主题和得分的关系。
quan_id_topic %>% 
  left_join(select(survey, id, ven), by = "id") %>% 
  # 漏洞：应该早点把383号删除。
  filter(id != "383") %>% 
  group_by(ven, topic) %>% 
  summarise(gamma = sum(gamma), .groups = "drop") %>% 
  ggplot() + 
  geom_col(aes(ven, gamma, fill = as.character(topic)), position = "fill") + 
  theme_bw() + 
  scale_fill_d3() + 
  labs(x = "Ven", y = "Proportion", fill = "Topic") +
  theme(axis.text.x = element_text(angle = 90))

quan_id_topic %>% 
  left_join(select(survey, id, ven), by = "id") %>% 
  # 漏洞：应该早点把383号删除。
  filter(id != "383") %>% 
  group_by(ven, topic) %>% 
  summarise(gamma = sum(gamma), .groups = "drop") %>% 
  ggplot() + 
  geom_col(aes(topic, gamma, fill = as.character(ven)), position = "fill") + 
  theme_bw() + 
  scale_fill_d3() + 
  labs(x = "Topic", y = "Proportion", fill = "Ven") +
  theme(axis.text.x = element_text(angle = 90))

# 对狩猎的态度和主题的关系。
quan_id_topic %>% 
  left_join(select(survey, id, hunting), by = "id") %>% 
  # 漏洞：应该早点把383号删除。
  filter(id != "383") %>% 
  group_by(hunting, topic) %>% 
  summarise(gamma = sum(gamma), .groups = "drop") %>% 
  ggplot() + 
  geom_col(
    aes(hunting, gamma, fill = as.character(topic)), position = "fill"
  ) + 
  theme_bw() + 
  scale_fill_d3() + 
  labs(x = "Hunting", y = "Proportion", fill = "Topic") +
  theme(axis.text.x = element_text(angle = 90))

quan_id_topic %>% 
  left_join(select(survey, id, hunting), by = "id") %>% 
  # 漏洞：应该早点把383号删除。
  filter(id != "383") %>% 
  group_by(hunting, topic) %>% 
  summarise(gamma = sum(gamma), .groups = "drop") %>% 
  ggplot() + 
  geom_col(
    aes(topic, gamma, fill = as.character(hunting)), position = "fill"
  ) + 
  theme_bw() + 
  scale_fill_d3() + 
  labs(x = "Topic", y = "Proportion", fill = "Hunting") +
  theme(axis.text.x = element_text(angle = 90))

# Sentiment analysis ----
# Seed words for sentiment analysis. 
seed_word <- c(rep(1, 7), rep(-1, 6)) %>% 
  setNames(c(
    c("恵み", "絶賛", "良い", "美味", "活用", "美食", "美味しい"), 
    c("破壊", "危険", "心配", "残酷", "怖い", "冒涜")
  ))

# Bug: How to define context word? 
# quan_context_word <- char_context(quan_tok, "鹿", p = 0.05)

# LSS model. 
lss <- 
  textmodel_lss(
    quan_dtm, 
    seeds = seed_word, 
    # Bug: How to define context word? 
    # terms = quan_context_word, 
    k = 300, 
    include_data = TRUE, 
    group_data = TRUE
  )

# Top positive and negative words. 
head(coef(lss), 20)
tail(coef(lss), 20)

# Sentiment score. 
lss_score <-  
  docvars(quan_dtm) %>% 
  # Bug: Specify the "by" argument. 
  left_join(survey) %>% 
  mutate(fit = predict(lss, newdata = quan_dtm)) %>% 
  mutate(ven = factor(ven, levels = c(-2:2)))

textplot_terms(lss, highlighted = names(topfeatures(quan_dtm, n = 15)))
lss_score %>% 
  filter(!is.na(ven)) %>% 
  ggplot(aes(ven, fit)) + 
  geom_boxplot() + 
  # geom_jitter(alpha = 0.2) + 
  theme_bw() + 
  labs(x = "Urban residents‘ attitude towards venison ", y = "LSS score")
# 漏洞：需要提取出和支持与否有关的种子词。

by(lss_score$fit, lss_score$ven, function(x) mean(x, na.rm = T))
by(lss_score$fit, lss_score$ven, function(x) median(x, na.rm = T))
lss_score %>% 
  filter(!is.na(ven)) %>% 
  group_by(ven) %>% 
  summarise(
    lss_mean = mean(fit, na.rm = T), 
    lss_max = max(fit, na.rm = T), 
    lss_min = min(fit, na.rm = T), 
    lss_sd = sd(fit, na.rm = T), 
    .groups = "drop"
  )

# 挑出吃肉态度较低（ven = -1）但是LSS得分高的回答；以及吃肉态度较高，但是LSS得分较低低回答。
(
  lss_mean_ven0 <- lss_score %>% 
    filter(ven == "0") %>% 
    pull(fit) %>% 
    mean()
)

lss_score %>% 
  filter(ven == "-2") %>% 
  arrange(-fit) %>% 
  filter(fit > lss_mean_ven0) %>% 
  select(id, ven, ven_reason, fit) %>% 
  View()

lss_score %>% 
  filter(ven == "2") %>% 
  arrange(fit) %>% 
  filter(fit < lss_mean_ven0) %>% 
  select(ven, ven_reason, fit) %>% 
  head(10) %>% 
  View()
