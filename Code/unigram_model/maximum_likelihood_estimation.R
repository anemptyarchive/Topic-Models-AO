
# ch2.3 最尤推定 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)


# 単語数(サイコロを振る回数)を指定
N_d <- 10

# 語彙数(サイコロの目の数)を指定
V = 6

# 真の単語分布(カテゴリ分布のパラメータ)を指定
phi_turth <- rep(1, V) / V
sum(phi_turth)

# 文書を生成(サイコロを振る)
w_dn <- sample(x = 1:V, size = N_d, replace = TRUE, prob = phi_turth)
w_dn

# 各語彙の出現回数を集計
doc_df1 <- tibble(v = w_dn) %>% 
  group_by(v) %>% # 出目でグループ化
  summarise(N_v = n()) # 出目ごとにカウント

# 出ない目があったとき用の対策
doc_df2 <- tibble(v = 1:V) %>% # 1からVまでの受け皿(行)を作成
  left_join(doc_df1, by = "v") %>% # 結合
  mutate(N_v = replace_na(N_v, 0)) # NAを0に置換

# 最尤推定
ml_df <- doc_df2 %>% 
  mutate(prob = N_v / sum(N_v)) # 式(4.2)

# 作図
ggplot(ml_df, aes(x = v, y = prob)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
  scale_x_continuous(breaks = 1:V, labels = 1:V) + # 軸目盛
  labs(title = "Unigram Model", 
       subtitle = "Maximum Likelihood Estimation", 
       y = "prob") # ラベル


