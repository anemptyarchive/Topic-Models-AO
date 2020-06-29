
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
  mutate(phi_v = N_v / sum(N_v)) # 式(4.2)

# 作図
ggplot(ml_df, aes(x = v, y = phi_v)) + # データ
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
  mutate(phi_v = (N_v + beta - 1) / (sum(N_v) + (beta - 1) * V))

# 作図
ggplot(map_df, aes(x = v, y = phi_v)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
  scale_x_continuous(breaks = 1:V, labels = 1:V) + # 軸目盛
  labs(title = "Unigram Model", 
       subtitle = "Maximum A Posteriori Estimation", 
       y = "prob") # ラベル


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
  facet_wrap(method ~ N, nrow = 2) + # グラフの分割
  #facet_wrap(N ~ method, ncol = 2) + # グラフの分割
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
  labs(title = "Unigram Model", 
       subtitle = "Bayesian Estimation", 
       x = expression(paste(phi[1], ", ", phi[2], sep = "")), 
       y = expression(paste(phi[1], ", ", phi[3], sep = ""))) # ラベル


# chapter2.6 --------------------------------------------------------------

# 利用パッケージ
library(tidyverse)

# パラメータの指定
beta  <- 2 # 任意の値を指定する
shake <- 1 # 任意の試行回数の指定

# 語彙インデックス：v
v <- 1:6

# サイコロを振る
shake_result1 <- data.frame(w_dn = sample(x = v, size = shake, replace = TRUE)) %>% 
                 group_by(w_dn) %>%   # 出目でグループ化
                 summarise(N_v = n()) # 出目ごとにカウント

# 出ない目があったとき用の対策
shake_result2 <- left_join(data.frame(w_dn = v), shake_result1, by = "w_dn")
shake_result2$N_v[is.na(shake_result2$N_v)] <- 0 # NAを0に置換

# 総単語数：N
N <- sum(shake_result2$N_v)

# 総語彙数：V
V <- length(v)

# ベイズ予測
p_w <- (shake_result2$N_v + beta) / (N + beta * V)

# 作図用のdfを作成
bpd_result <- data.frame(v = v, 
                         likelihood = shake_result2$N_v / N, 
                         predicted = p_w)
bpd_long <- gather(bpd_result, key = "tag", value = "prob", -v)

# 描画
ggplot(data = bpd_long, mapping = aes(x = tag, y = prob, fill = tag)) + # データの指定
  geom_bar(stat = "identity", position = "dodge") +    # 棒グラフ
  ylim(c(0, 1)) +                                      # y軸の最小値・最大値
  scale_fill_manual(values = c("Orange", "#00A968")) + # 色
  facet_wrap( ~ v, labeller = label_both)              # グラフの分割



# chapter2.7 --------------------------------------------------------------

#### 不動点反復法####

# 利用パッケージ
library(tidyverse)


# 対数をとったガンマ関数の曲線
log_Gamma <- tibble(x_logGamma = seq(0.1, 10, 0.1),  # 作図用連続値：x
                    y_logGamma = lgamma(x_logGamma))

# 曲線上の2点
x_points <- c(2.5, 5)        # 曲線上の2点を指定する：x
y_points <- lgamma(x_points) # 曲線上の2点を指定する：y
points <- data.frame(x = x_points, y = y_points)

# 曲線上の2点を結ぶ直線
a_line <- (lgamma(x_points[2]) - lgamma(x_points[1])) / (x_points[2] - x_points[1]) # 傾き：a
b_line <- y_points[1] - a_line * x_points[1]                                        # 切片：b
straight_line <- tibble(x_line = seq(0.1, 10, 0.1),        # 作図用連続値：x
                        y_line = a_line * x_line + b_line)

# 曲線上の点1を通る接線
b_tangent <- y_points[1] - digamma(x_points[1]) * x_points[1] # 切片：b
tangent <- tibble(x_tangent = seq(0.1, 10, 0.1),              # 作図用連続値：x
                  y_tangent = digamma(x_points[1]) * x_tangent + b_tangent)

# 描画
ggplot() + 
  geom_line(data = log_Gamma, mapping = aes(x = x_logGamma, 
                                            y = y_logGamma)) + # 曲線
  geom_line(data = straight_line, mapping = aes(x = x_line, 
                                                y = y_line), color = "#00A968") + # 直線
  geom_line(data = tangent, mapping = aes(x = x_tangent, 
                                          y = y_tangent), color = "Orange") + # 接線
  geom_point(data = points, mapping = aes(x = x, y = y)) + 
  labs(x = "x", y = "y") # ラベル

