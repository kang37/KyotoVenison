# Statement ---
# 分析人们对食用鹿肉的看法，及其影响因素。
# 主要包含3部分分析：属性对态度的影响，鹿印象对态度的影响，文本分析。

# Preparation ----
pacman::p_load(
  openxlsx, dplyr, stopwords, topicmodels, dunn.test, ggplot2, ggsci, ggrepel, 
  DescTools, tidyr, tidytext, quanteda, quanteda.textstats, LSX, showtext
)
showtext_auto()

# 问卷调查吃鹿肉态度得分、性别、对狩猎的态度分数等。
survey <- 
  read.xlsx("data_raw/kyoto_venison_raw.xlsx", sheet = "Num") %>% 
  tibble() %>% 
  rename_with(~ tolower(.x)) %>% 
  rename("id" = "no.", "land" = "q1", "encounter_deer" = "q2") %>% 
  mutate(
    id = as.character(id), 
    ven = as.character(q10), 
    hunting = as.integer(q11b)
  ) %>% 
  # 加入Q10日语回答信息。
  left_join(
    read.xlsx("data_raw/kyoto_venison_raw.xlsx", sheet = "Text") %>%
      select(id = "No.", ven_reason = "Q10") %>% 
      mutate(id = as.character(id)), 
    by = "id"
  ) %>% 
  # 漏洞：该数据筛选操作是否合理？
  mutate(
    gender = case_when(
      gender == 2 ~ NA, gender == 0 | gender == 1 ~ gender, is.na(gender) ~ NA
    ), 
    # Education有空值。
    education = case_when(education == " " ~ NA, TRUE ~ education)
  ) %>% 
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
  mutate(ven_reason = gsub("おいしかった", "美味しい", ven_reason)) %>% 
  # 将一些过去时换成现在时。
  mutate(ven_reason = gsub("食べた", "食べる", ven_reason)) %>% 
  # 拆分Q7:对鹿的印象。
  cbind(
    ., 
    sapply(head(letters, 8), function(letter) grepl(letter, .$q7)) %>% 
      as.data.frame() %>% 
      rename_with(~ paste0("q7_", .))
  ) %>% 
  tibble() %>% 
  # 更改变量名称。
  mutate(
    gender = case_when(gender == 0 ~ "male", gender == 1 ~ "female"), 
    age = case_when(
      age == "1" ~ "10-19", age == "2" ~ "20-29", age == "3" ~ "30-39", 
      age == "4" ~ "40-49", age == "5" ~ "50-59", age == "6" ~ "60-69", 
      age == "7" ~ "70-79", age == "8" ~ ">80"
    ), 
    education = case_when(
      education == 1 ~ "high school or below", education == 2 ~ "bachelor",
      education == 3 ~ "master", education == 4 ~ "doctor"
    ),
    encounter_deer = case_when(
      encounter_deer == 1 ~ "yes", encounter_deer != 1 ~ "no"
    ),
    ven = as.numeric(ven),
    land = case_when(
      land == 1 ~ "farmland", land == 2 ~ "forest",
      land == 3 ~ "farmland_forest", land == 4 ~ "none"
    )
  ) %>% 
  # 是否可以用于第一部分分析：各属性对态度的影响。
  mutate(
    ana_sub_1 = case_when(
      if_all(
        c(gender, age, education, land, encounter_deer, ven), ~ !is.na(.)
      ) ~ 1,
      TRUE ~ 0
    ), 
    ana_sub_2 = case_when(
      if_all(
        c(all_of(paste0("q7_", head(letters, 8))), ven), ~ !is.na(.)
      ) ~ 1,
      TRUE ~ 0
    )
  )

# Statistic analysis ----
# Function to identify if data is normally distributed. 
id_normal_distribution <- function(grp_x) {
  by(
    as.numeric(survey$ven), survey[[grp_x]], 
    function(x) {
      shapiro_res <- shapiro.test(x)
      tibble(p = shapiro_res$p.value)
    }
  ) %>% 
    unlist()
}

# Function to do Kruskal test for subset of survey data. 
grp_comp <- function(x_name, g_name, sub_dt) {
  survey_sub <- survey %>% filter(get(sub_dt) == 1)
  smp_size <- nrow(survey_sub)
  if(length(unique(survey_sub[[g_name]])) <= 2) {
    stat_label <- "w"
    stat_res <- wilcox.test(
      as.numeric(survey_sub[[x_name]]) ~ as.character(survey_sub[[g_name]])
    )
  } else {
    stat_label <- "h"
    stat_res <- kruskal.test(
      as.numeric(survey_sub[[x_name]]) ~ as.character(survey_sub[[g_name]])
    )
  }
  # 统计量。
  stat_val <- stat_res$statistic %>% 
    round(digits = 2)
  # 返回结果。
  return(
    c(g_name, smp_size, stat_label, stat_val, stat_res$p.value) %>% 
      setNames(c("g_name", "smp_size", "stat_label", "stat_val", "p"))
  )
}

## Venison score ~ attributes ----
# 基本属性、有何土地（q1）、是否遇见鹿（q2）对吃肉态度的影响。
# 正态检验。
id_normal_distribution("gender")
id_normal_distribution("age")
lapply(
  as.character(1:4), 
  function(x) {
    filter(survey, education == x)$ven %>% 
      as.numeric() %>% 
      shapiro.test()
  }
)
by(as.numeric(survey$ven), survey$land, shapiro.test)
by(as.numeric(survey$ven), survey$encounter_deer, shapiro.test)

# 组间对比。
(
  ana_1_res <- 
    lapply(
      c("gender", "age", "education", "land", "encounter_deer"), 
      grp_comp, x_name = "ven", sub_dt = "ana_sub_1"
    ) %>% 
    bind_rows() %>% 
    mutate(
      p = round(as.numeric(p), digits = 4), 
      p_label = case_when(
        p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.1 ~ "*", p >= 0.05 ~ ""
      )
    )
)

## Venison score ~ deer impression ----
# 吃肉态度和对鹿的印象的关系。
lapply(
  paste0("q7_", head(letters, 8)), 
  grp_comp, x_name = "ven", sub_dt = "ana_sub_2"
) %>% 
  bind_rows() %>% 
  mutate(
    p = round(as.numeric(p), digits = 4), 
    p_mark = case_when(
      p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p >= 0.05 ~ ""
    )
  ) %>% 
  # Add question ID. 
  mutate(adj_word = paste0("q7_", head(letters, 8)), .before = 1)

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

## Hunting score ~ attributes ----
# 狩猎态度～性别，年龄，教育水平。
# 正态分布检验。
by(as.numeric(survey$hunting), survey$gender, shapiro.test)

# 组间对比。
lapply(
  c("gender", "age", "education"), 
  grp_comp, x_name = "hunting", sub_dt = "ana_sub_1"
) %>% 
  bind_rows()
grp_comp("hunting", "gender", "ana_sub_1")
# 卡方检验。
gender_hunting_chisq <- table(survey$gender, survey$hunting)
gender_hunting_chisq
chisq.test(gender_hunting_chisq)

# 狩猎态度～年龄组。
by(as.numeric(survey$hunting), survey$age, shapiro.test)
grp_comp("hunting", "age", "ana_sub_1")

# 狩猎态度～教育水平。
lapply(
  as.character(1:4), 
  function(x) {
    filter(survey, education == x)$hunting %>% 
      as.numeric() %>% 
      shapiro.test()
  }
)
grp_comp("hunting", "education", "ana_sub_1")

## Venison score ~ hunting score ----
cor.test(as.numeric(survey$ven), as.numeric(survey$hunting))

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
    "いと", "とも", "どちら", "にし", "しく", "しか", "しな", "すぎ", "ほしい", 
    "おい", "なか"
    # Bug: Stop words from text plot or topic analysis key words. 
    # "思", "聞", "等"
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
write.xlsx(
  topic_word, 
  paste0("data_proc/topic_word_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
)

# 抽出和各个主题匹配度最高的前10条回答，解读各个主题的含义。
(
  topic_text <- quan_id_topic %>% 
    group_by(topic) %>% 
    arrange(topic, -gamma) %>% 
    slice_head(n = 10) %>% 
    ungroup() %>% 
    # 漏洞：需要提前更改id的类型。
    left_join(mutate(survey, id = as.character(id)), by = "id") %>% 
    select(id, topic, gamma, ven_reason) 
)
# 导出结果。
write.xlsx(
  topic_text, 
  paste0("data_proc/topic_text_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
)

## Hunting & ven core ~ topic ----
# 对狩猎的态度和对食用鹿肉的态度之间的关系。
png(
  paste0("data_proc/column_hunt_ven_", format(Sys.Date(), "%Y%m%d"), ".png"), 
  width = 1200, height = 800, res = 300
)
survey %>% 
  filter(!is.na(ven), !is.na(hunting)) %>% 
  group_by(ven, hunting) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  mutate(ven = factor(ven, levels = -2:2)) %>% 
  ggplot() + 
  geom_col(aes(hunting, n, fill = ven), position = "fill") + 
  scale_fill_manual(
    breaks = -2:2, 
    values = c("#e2e1ec", "#ced7e5", "#d7d9c9", "#f9ebd3", "#f5cbcc")
  ) + 
  theme_bw() + 
  labs(
    x = "Attitudes toward hunting", y = "Percentage", 
    fill = "Attitudes\ntoward\nvenison"
  ) 
dev.off()

# 食用鹿肉打分和主题的关系。
# 分颜色柱状图。
png(
  paste0(
    "data_proc/column_ven_topic_num_black", format(Sys.Date(), "%Y%m%d"), ".png"
  ), 
  width = 1200, height = 800, res = 300
)
quan_id_topic %>% 
  left_join(select(survey, id, ven), by = "id") %>% 
  group_by(ven, topic) %>% 
  summarise(gamma = sum(gamma), .groups = "drop") %>% 
  filter(!is.na(ven)) %>% 
  mutate(ven = factor(ven, levels = c("-2", "-1", "0", "1", "2"))) %>% 
  # 计算每个得分内各个话题的占比。
  group_by(ven) %>% 
  arrange(ven, -topic) %>% 
  mutate(
    gamma_prop = gamma / sum(gamma), 
    # 作图时标签的高度。
    label_y_max = cumsum(gamma_prop), 
    label_y_min = lag(label_y_max, default = 0), 
    label_y = (label_y_max + label_y_min) / 2
  ) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_col(aes(ven, gamma_prop, fill = as.character(topic))) + 
  geom_text(
    aes(ven, label_y, label = sprintf("%.0f%%", gamma_prop * 100)), 
    col = "black", size = 3
  ) + 
  theme_bw() + 
  scale_fill_manual(
    breaks = 1:6, 
    values = c("#d6cbda", "#c9e0e5", "#bed2c6", "#ffd19d", "#feb29b", "#ed8687")
  ) + 
  labs(
    x = "Public attitudes toward venison", y = "Percentage", 
    fill = "Topics\nabout\nvenison"
  ) +
  theme(axis.text.x = element_text(angle = 90))
dev.off()

# 分成子图。
png(
  paste0(
    "data_proc/column_ven_topic_facet", format(Sys.Date(), "%Y%m%d"), ".png"
  ), 
  width = 1400, height = 1000, res = 300
)
quan_id_topic %>% 
  left_join(select(survey, id, ven), by = "id") %>% 
  group_by(ven, topic) %>% 
  summarise(gamma = sum(gamma), .groups = "drop") %>% 
  filter(!is.na(ven)) %>% 
  mutate(ven = factor(ven, levels = c("-2", "-1", "0", "1", "2"))) %>% 
  # 计算每个得分内各个话题的占比。
  group_by(ven) %>% 
  arrange(ven, -topic) %>% 
  mutate(gamma_prop = gamma / sum(gamma)) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_col(aes(ven, gamma_prop, fill = as.character(topic))) + 
  theme_bw() + 
  scale_fill_manual(
    breaks = 1:6, 
    values = c("#d6cbda", "#c9e0e5", "#bed2c6", "#ffd19d", "#feb29b", "#ed8687")
  ) + 
  labs(
    x = "Public attitudes toward venison", y = "Percentage", 
    fill = "Topics\nabout\nvenison"
  ) +
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~ topic)
dev.off()

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
  scale_fill_manual(
    breaks = 1:6, 
    values = c("#d6cbda", "#c9e0e5", "#bed2c6", "#ffd19d", "#feb29b", "#ed8687")
  ) + 
  theme_bw() + 
  labs(
    x = "Public attitudes towards hunting", y = "Percentage", fill = "Topic"
  ) +
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

# Polarity score of terms.
# 将LSS模型结果中的日语词汇翻译成英语。
# 对LSS模型结果的整理代码如下：
# full_join(
#   data.frame(lss$beta) %>% 
#     rename_with(~ "beta") %>% 
#     mutate(term = rownames(.), .before = 1), 
#   data.frame(lss$frequency) %>% 
#     rename_with(~ "freq") %>% 
#     mutate(term = rownames(.), .before = 1)
# ) %>% 
#   tibble() %>% 
#   mutate(
#     top_beta = beta %in% sort(beta, decreasing = TRUE)[1:3], 
#     tail_beta = beta %in% sort(beta, decreasing = FALSE)[1:2], 
#     top_freq = freq %in% sort(freq, decreasing = TRUE)[1:15], 
#     stand_out_term = top_beta | tail_beta | top_freq
#   )
# 翻译后导出新的数据文件并作图。
lss_beta_freq <- read.xlsx("data_raw/lss_term_beta_freq_trans.xlsx") %>% 
  tibble() %>% 
  select(term, term_en, beta, freq, top_freq) %>% 
  # 将大写字母都替换成小写，并去除括号。
  mutate(term_en = tolower(term_en), term_en = gsub("[()]", "", term_en))
png(
  paste0("data_proc/term_polarity_", format(Sys.Date(), "%Y%m%d"), ".png"), 
  width = 1500, height = 1200, res = 300
)
ggplot() + 
  # geom_text(
  #   data = lss_beta_freq %>% filter(top_freq == FALSE), 
  #   aes(beta, log(freq), label = term_en), col = "grey"
  # ) + 
  geom_point(
    data = lss_beta_freq %>% filter(top_freq == FALSE), 
    aes(beta, log(freq)), col = "black", alpha = 0.3
  ) + 
  geom_point(
    data = lss_beta_freq %>% filter(top_freq), 
    aes(beta, log(freq)), col = "black", alpha = 0.3
  ) + 
  geom_text_repel(
    data = lss_beta_freq %>% filter(top_freq), 
    aes(beta, log(freq), label = term_en), box.padding = 0.3, col = "black"
  ) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  labs(x = "Polarity", y = "Frequency (log scale)")
# textplot_terms(lss, highlighted = names(topfeatures(quan_dtm, n = 15)))
dev.off()

# 导出词语极性结果。
full_join(
  data.frame(lss$beta) %>% 
    rename_with(~ c("beta")) %>% 
    mutate(term = rownames(.), .before = 1), 
  data.frame(lss$frequency) %>% 
    rename_with(~ c("frequency")) %>% 
    mutate(term = rownames(.), .before = 1), 
  by = "term"
) %>% 
  tibble() %>% 
  mutate(freq_log = log(frequency)) %>% 
  write.xlsx(paste0("data_proc/term_polarity_", Sys.Date(), ".xlsx"))

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
ggplot(lss_score %>% filter(!is.na(ven))) + 
  geom_boxplot(aes(ven, fit)) + 
  theme_bw() 

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

# Result export ----
# 输出分析1，即各属性及分组吃肉态度相关数据。
survey %>% 
  filter(ana_sub_1 == 1) %>% 
  select(gender, age, education, ven, land, encounter_deer) %>% 
  pivot_longer(
    cols = c(gender, age, education, land, encounter_deer), 
    names_to = "attr", values_to = "attr_val"
  ) %>% 
  # Turn attributes into factors. 
  mutate(
    attr = factor(attr, levels = c(
      "gender", "age", "education", "land", "encounter_deer"
    )), 
    attr_val = factor(attr_val, levels = c(
      "female", "male", 
      "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", ">80", 
      "high school or below", "bachelor", "master", "doctor", 
      "farmland", "forest", "farmland_forest", "none", 
      "yes", "no"
    ))
  ) %>%
  # Summarize data. 
  group_by(attr, attr_val) %>% 
  summarise(
    sample_size = n(), avg = mean(ven), sd_val = sd(ven), .groups = "drop"
  ) %>% 
  # Total sample size. 
  group_by(attr) %>% 
  mutate(
    tot_sample_size = sum(sample_size), 
    prop = sample_size / tot_sample_size * 100
  ) %>% 
  ungroup() %>% 
  # Sort. 
  arrange(attr, attr_val) %>% 
  # Only keep first row values of attr and attr_val. 
  group_by(attr) %>% 
  mutate(
    grp_row_id = row_number(), 
    attr = case_when(grp_row_id == 1 ~ attr, TRUE ~ ""), 
    tot_sample_size = case_when(grp_row_id == 1 ~ tot_sample_size, TRUE ~ NA)
  ) %>% 
  ungroup() %>% 
  select(attr, tot_sample_size, attr_val, sample_size, prop, avg, sd_val) %>% 
  mutate(across(c(prop, avg, sd_val), ~ round(.x, digits = 2))) %>% 
  write.xlsx(paste0("data_proc/ana_1_general_descript_", Sys.Date(), ".xlsx"))

# 输出分析1结果。
write.xlsx(
  ana_1_res, 
  paste0("data_proc/ana_1_grp_comparison_", Sys.Date(), ".xlsx")
)

# 输出分析2（即印象对态度影响）部分的基本描述信息。
survey %>% 
  filter(ana_sub_2 == 1) %>% 
  mutate(across(paste0("q7_", head(letters, 8)), as.character)) %>% 
  select(paste0("q7_", head(letters, 8)), ven) %>% 
  pivot_longer(
    cols = c(paste0("q7_", head(letters, 8))), 
    names_to = "attr", values_to = "attr_val"
  ) %>% 
  # Change names of Q7-questions. 
  mutate(attr = case_when(
    attr == "q7_a" ~ "virus_carry", 
    attr == "q7_b" ~ "holy", 
    attr == "q7_c" ~ "destructive", 
    attr == "q7_d" ~ "national_symbol", 
    attr == "q7_e" ~ "cruel", 
    attr == "q7_f" ~ "cute", 
    attr == "q7_g" ~ "rude", 
    attr == "q7_h" ~ "docile"
  )) %>% 
  mutate(
    attr = factor(attr, levels = c(
      "virus_carry", "holy", "destructive", "national_symbol", 
      "cruel", "cute", "rude", "docile"
    )), 
    attr_val = factor(attr_val, levels = c(TRUE, FALSE))
  ) %>% 
  # Summarize data. 
  group_by(attr, attr_val) %>% 
  summarise(
    sample_size = n(), avg = mean(ven), sd_val = sd(ven), .groups = "drop"
  ) %>% 
  # Total sample size. 
  group_by(attr) %>% 
  mutate(
    tot_sample_size = sum(sample_size), 
    prop = sample_size / tot_sample_size * 100
  ) %>% 
  ungroup() %>% 
  # Sort. 
  arrange(attr, attr_val) %>% 
  # Only keep first row values of attr and attr_val. 
  group_by(attr) %>% 
  mutate(
    grp_row_id = row_number(), 
    attr = case_when(grp_row_id == 1 ~ attr, TRUE ~ ""), 
    tot_sample_size = case_when(grp_row_id == 1 ~ tot_sample_size, TRUE ~ NA)
  ) %>% 
  ungroup() %>% 
  select(attr, tot_sample_size, attr_val, sample_size, prop, avg, sd_val) %>% 
  mutate(across(c(prop, avg, sd_val), ~ round(.x, digits = 2))) %>% 
  write.xlsx(paste0("data_proc/ana_2_general_descript_", Sys.Date(), ".xlsx"))
