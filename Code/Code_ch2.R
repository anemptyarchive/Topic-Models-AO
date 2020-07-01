
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


# ch2.4 MAP推定 --------------------------------------------------------------

# 利用パッケージ
library(tidyverse)


# 単語数(サイコロを振る回数)を指定
N_d <- 10

# 語彙数(サイコロの目の数)を指定
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


# MAP推定
map_df <- doc_df2 %>% 
  mutate(prob = (N_v + beta - 1) / (sum(N_v) + (beta - 1) * V))

# 作図
ggplot(map_df, aes(x = v, y = prob)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
  scale_x_continuous(breaks = 1:V, labels = 1:V) + # 軸目盛
  labs(title = "Unigram Model:Maximum A Posteriori Estimation", 
       subtitle = paste0("N=", N_d)) # ラベル


# 最尤推定とMAP推定の比較 -----------------------------------------------------------

# 単語数を指定
N_d_vec <- c(1, 10, 100)

# パラメータ生成
res_df <- tibble()
for(i in seq_along(N_d_vec)){
  
  # 文書を生成
  doc_df <- tibble(
    v = sample(x = 1:V, size = N_d_vec[i], replace = TRUE, prob = phi_turth) # 単語を生成
  ) %>% 
    group_by(v) %>% # 語彙ごとにグループ化
    summarise(N_v = n()) %>% # 各語彙の出現回数を集計
    right_join(
      tibble(
        v = 1:V, 
        N = as.factor(N_d_vec[i]) # 作図用のラベル
      ), 
      by = "v"
    ) %>% # 結合
    mutate(N_v = replace_na(N_v, 0)) # NAを0に置換
  
  # 推定
  tmp_df <- doc_df %>% 
    mutate(ml = N_v / sum(N_v)) %>% # 最尤推定:式(2.3)
    mutate(map = (N_v + beta - 1) / (sum(N_v) + (beta - 1) * V)) # MAP推定
  
  # 推定結果を結合
  res_df <- rbind(res_df, tmp_df)
}

# wide型データフレームをlong型に変換
res_df_long <- pivot_longer(
  res_df, 
  cols = c(ml, map), # 変換する列を指定
  names_to = "method", # 現列名を値として格納する列名
  names_ptypes = list(method = factor()), # 現列名を値とする際の型を指定
  values_to = "prob" # 現セルの値を格納する列前
)

# 作図
ggplot(res_df_long, aes(x = v, y = prob, fill = method)) + # データの設定
  geom_bar(stat = "identity") + # 棒グラフ
  facet_wrap(method ~ N, nrow = 2, labeller = label_both) + # グラフの分割
  #facet_wrap(N ~ method, ncol = 2, labeller = label_both) + # グラフの分割
  scale_x_continuous(breaks = 1:V, labels = 1:V) + # 軸目盛
  scale_fill_manual(values = c("#00A968", "Orange")) + # 塗りつぶし色
  labs(title = "Unigram Model") # ラベル
  

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


# ch2.7 ハイパーパラメータ推定 --------------------------------------------------------------

# 不動点反復法 ------------------------------------------------------------------

# ディガンマ関数 -----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)


# 作図範囲を指定
x_plot <- seq(0.01, 10, 0.01)


# ディガンマ関数
digamma_df <- tibble(
  x = x_plot, 
  y = digamma(x)
)

# 作図
ggplot(digamma_df, aes(x = x, y = y)) + 
  geom_line() + 
  labs(title = "Digamma Function", 
       y = expression(paste(psi, "(x)", sep = ""))) # ラベル


# ディガンマ関数の性質 --------------------------------------------------------------

# 対数をとったガンマ関数の曲線
log_Gamma_df <- tibble(
  x = x_plot, # 作図用のxの値
  y = lgamma(x) # 対数をとったガンマ関数の値
)

# 曲線上の2点を指定
point_df <- tibble(
  x = c(2.5, 5), # 2点のx軸の値を指定
  y = lgamma(x)
)

# 曲線上の2点を結ぶ直線
a_line <- (lgamma(point_df[["x"]][2]) - lgamma(point_df[["x"]][1])) / (point_df[["x"]][2] - point_df[["x"]][1]) # 傾き
b_line <- point_df[["y"]][1] - a_line * point_df[["x"]][1] # 切片
straight_line_df <- tibble(
  x = x_plot, 
  y = a_line * x + b_line
)

# 接線
b_tangent <- point_df[["y"]][1] - digamma(point_df[["x"]][1]) * point_df[["x"]][1] # 切片
tangent_df <- tibble(
  x = x_plot, 
  y = digamma(point_df[["x"]][1]) * x + b_tangent
)


# 作図
ggplot() + 
  geom_line(data = log_Gamma_df, mapping = aes(x = x, y = y)) + # 曲線
  geom_line(data = straight_line_df, mapping = aes(x = x, y = y), color = "#00A968") + # 直線
  geom_line(data = tangent_df, mapping = aes(x = x, y = y), color = "Orange") + # 接線
  geom_point(data = point_df, mapping = aes(x = x, y = y)) +  # 曲線上の2点
  labs(title = "log Gamma Function", 
       y = expression(paste(log, Gamma, "(x)", sep = ""))) # ラベル

