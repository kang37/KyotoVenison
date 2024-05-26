library(quanteda)
library(quanteda.textstats)
library(LSX)

# Text mining data ----
# Stopwords for sentiment analysis, inherit from last part. . 
quan_jp_stop_word <- jp_stop_word$word

# Corpus. 
quan_corp <- corpus(survey, text_field = "ven_reason")

# 自定义词典。
quan_dict <- 
  dictionary(list(
    biodiversity = "生物 多様 性", 
    diversity = "多様 性", 
    global_warming = "地球 温暖 化", 
    warming = "温暖 化", 
    climate_change = "気候 変動", 
    ll = "地産 地 消",
    oa = "有機 農業"
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

# Topic analysis ----
# 生成term-document矩阵。
quan_dtm <- dfm(quan_tok) %>% 
  convert(., to = "tm")

# 要选取几个主题呢？当话题数量太少时，大部分人的回答属于各个主题的概率差不多，这显然不合理；主题数很多时，各个回答属于各个话题的概率会更加有区分度，但是主题也不应太多。
# 函数：基于自定义主题数量，获得各个文档属于不同主题的概率。
quan_test_k_topic <- function(k_x) {
  # 生成LDA数据。
  lda <- LDA(quan_dtm, k_x, control = list(seed = 1234))
  
  # 各篇文章属于各个主题的概率。
  id_topic_res <- tidy(lda, matrix = "gamma") %>% 
    mutate(k = k_x)
  return(id_topic_res)
}
# 测试主题数量思路：如果区分度越高的话，一个文档被划分到各个主题下的概率就越离散，基尼系数就越高。所以，可以给定一定范围的自定义主题数量，计算不同主题数量下，各个文档被划分到各个主题中的概率。看在那个自定义主题数量下，平均基尼系数最高，或者看看基尼系数在什么时候突变。
# 要测试的自定义主题数量范围。
range_k <- 2:20
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
  geom_boxplot(aes(k, gini)) + 
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
quan_lda <- LDA(quan_dtm, k = quan_tar_k, control = list(seed = 1234))

# 评估区分度。
# Text ID. 
text_id <- tibble(text = names(quan_tok), id = docvars(quan_tok)$id)

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
tidy(quan_lda, matrix = "beta") %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 15) %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  summarise(term = list(term), .groups = "drop") %>% 
  mutate(
    term = unlist(lapply(term, function(x) paste0(x, collapse = ", ")))
  ) %>% 
  mutate(term = gsub("_", "", term), term = gsub("[0-9]", "", term))

# 抽出和各个主题匹配度最高的前10条回答，解读各个主题的含义。
quan_id_topic %>% 
  group_by(topic) %>% 
  arrange(topic, -gamma) %>% 
  slice_head(n = 10) %>% 
  ungroup() %>% 
  # 漏洞：需要提前更改id的类型。
  left_join(mutate(survey, id = as.character(id)), by = "id") %>% 
  select(id, topic, ven_reason) 

# Sentiment analysis ----
# Seed words for sentiment analysis. 
seed_word <- c(rep(1, 12), rep(-1, 11)) %>% 
  setNames(c(
    c("恵み", "絶賛", "創出", "改善", "配慮", 
      "対話", "効果的", "支持", "機会", "成功",  
      "効果", "成功例"), 
    c("破壊", "危険", "壊滅", "被害", "貧相", 
      "酷評", "悪徳", 
      "危険性", "壊滅的", "農業被害", "豪雨被害")
  ))

# Bug: How to define context word? 
# quan_context_word <- 
# char_context(tok, "食用", p = 0.05)

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
  mutate(fit = predict(lss, newdata = quan_dtm))

textplot_terms(lss, highlighted = names(topfeatures(quan_dtm)))

ggplot(lss_score) + 
  geom_boxplot(aes(ven, fit))
ggplot(lss_score, aes(ven, fit)) + 
  geom_point() + 
  geom_smooth(method = "loess")
# 漏洞：需要提取出和支持与否有关的种子词。


