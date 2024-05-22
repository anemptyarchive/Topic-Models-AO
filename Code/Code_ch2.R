
# ch2.5 ベイズ推定 -----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)


# 単語数(サイコロを振る回数)を指定
N_d <- 64

# 語彙数(サイコロの目の数):(V=3)
V = 3

# 真の単語分布(カテゴリ分布のパラメータ)を指定
phi_turth <- rep(1, V) / V
sum(phi_turth)

# 単語分布のパラメータを指定
beta <- 2

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


# 値をランダムに生成
phi_mat <- seq(0.001, 1, 0.001) %>% 
  sample(size = 120000, replace = TRUE) %>% 
  matrix(ncol = 3)

# 正規化
phi_mat <- phi_mat / rowSums(phi_mat)

# ベイズ推定
bayes_df <- tibble(
  x = phi_mat[, 2] + (phi_mat[, 3] / 2), # 三角座標に変換
  y = sqrt(3) * (phi_mat[, 3] / 2), # 三角座標に変換
  nmlz_term = lgamma(sum(doc_df2[["N_v"]]) + beta * V) - sum(lgamma(doc_df2[["N_v"]] + beta)), # 
  density = exp(nmlz_term + colSums((doc_df2[["N_v"]] + beta - 1) * log(t(phi_mat)))) # 正規化項
)

# 作図
ggplot(bayes_df, aes(x = x, y = y, color = density)) + # データ
  geom_point() + # 散布図
  scale_color_gradientn(colors = c("blue", "green", "yellow", "red")) +  # 色
  scale_x_continuous(breaks = c(0, 1), labels = c("(1, 0, 0)", "(0, 1, 0)")) + # x軸目盛
  scale_y_continuous(breaks = c(0, 0.87), labels = c("(1, 0, 0)", "(0, 0, 1)")) + # y軸目盛
  coord_fixed(ratio = 1) + # 縦横の比率
  labs(title = "Unigram Model:Bayesian Estimation", 
       subtitle = paste0("N=", N_d), 
       x = expression(paste(phi[1], ", ", phi[2], sep = "")), 
       y = expression(paste(phi[1], ", ", phi[3], sep = ""))) # ラベル


# ch2.6 ベイズ予測分布 --------------------------------------------------------------

# 利用パッケージ
library(tidyverse)


# 単語数(サイコロを振る回数)を指定
N_d <- 10

# 語彙数(サイコロの目の数)
V = 6

# 真の単語分布(カテゴリ分布のパラメータ)を指定
phi_turth <- rep(1, V) / V
sum(phi_turth)

# 単語分布のパラメータを指定
beta <- 2

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

# ベイズ予測分布を計算
predict_df <- doc_df2 %>% 
  mutate(prob = (N_v + beta) / sum(N_v + beta)) # 確率

# 作図
ggplot(predict_df, aes(x = v, y = prob)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
  scale_x_continuous(breaks = 1:V, labels = 1:V) + # 軸目盛
  labs(title = "Unigram Model:Bayesian Predictive Distribution", 
       subtitle = paste0("N=", N_d)) # ラベル


