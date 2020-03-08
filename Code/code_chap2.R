
# chapter2.3 ----------------------------------------------------------------


# 利用パッケージ
library(tidyverse)

# パラメーターの設定
beta  <- 2  # 任意の値を指定する
shake <- 10 # 任意の試行回数を指定する

# サイコロを振る
shake_result1 <- data.frame(w_dn = sample(x = 1:6, size = shake, replace = TRUE)) %>% # サイコロを振る
                 group_by(w_dn) %>%   # 出目でグループ化
                 summarise(N_v = n()) # 出目ごとにカウント

# 出ない目があったとき用の対策
shake_result2 <- left_join(data.frame(w_dn = 1:6), shake_result1, by = "w_dn")
shake_result2$N_v[is.na(shake_result2$N_v)] <- 0 # NAを0に置換

# 最尤推定
mle_result <- shake_result2 %>% 
              mutate(phi_v = N_v / sum(N_v))

# 描画
ggplot(mle_result, aes(x = w_dn, y = phi_v)) +        # データの設定
  geom_bar(stat = "identity", fill = "#00A968") +     # 棒グラフ
  scale_x_continuous(breaks = 1:6, labels = 1:6) +    # 軸目盛
  labs(title = "Maximum Likelihood Estimation",       # 図のタイトル
       x = expression(w[dn]), y = expression(phi[v])) # 軸ラベル


# chapter2.4 --------------------------------------------------------------

# 利用パッケージ
library(tidyverse)

# パラメーターの設定
beta  <- 2  # 任意の値を指定する
shake <- 10 # 任意の試行回数を指定する

# サイコロを振る
shake_result1 <- data.frame(w_dn = sample(x = 1:6, size = shake, replace = TRUE)) %>% # サイコロを振る
                 group_by(w_dn) %>%   # 出目でグループ化
                 summarise(N_v = n()) # 出目ごとにカウント

# 出ない目があったとき用の対策
shake_result2 <- left_join(data.frame(w_dn = 1:6), shake_result1, by = "w_dn")
shake_result2$N_v[is.na(shake_result2$N_v)] <- 0 # NAを0に置換

# 最大事後確率推定
map_result <- shake_result2 %>% 
              mutate(phi_v = (N_v + beta - 1) / (sum(N_v) + (beta - 1) * length(w_dn)))

# 描画
ggplot(map_result, aes(x = w_dn, y = phi_v)) +        # データの設定
  geom_bar(stat = "identity", fill = "#00A968") +     # 棒グラフ
  scale_x_continuous(breaks = 1:6, labels = 1:6) +    # 軸目盛
  labs(title = "Maximum A Posteriori Estimation",       # 図のタイトル
       x = expression(w[dn]), y = expression(phi[v])) # 軸ラベル



# パラメーターの指定
beta  <- 2
shake <- c(1, 10, 100)

# パラメータ生成
df <- data.frame()
for(i in seq_along(shake)){
  # サイコロサンプル
  dice <- data.frame(w_dn = sample(1:6, shake[i], replace = TRUE)) %>% 
    group_by(w_dn) %>% 
    summarise(N_v = n())
  tmp_df1 <- left_join(data.frame(w_dn = 1:6), dice, by = "w_dn")
  tmp_df1$N_v[is.na(tmp_df1$N_v)] <- 0
  
  # パラメータ生成：MLe
  tmp_ml <- tibble(w_dn = tmp_df1$w_dn, 
                   N_v = tmp_df1$N_v, 
                   phi_v = N_v / sum(N_v), 
                   method = "ML", 
                   shake = shake[i])
  
  # パラメータ生成：MAPe
  tmp_map <- tibble(w_dn = tmp_df1$w_dn, 
                    N_v = tmp_df1$N_v, 
                    phi_v = (N_v + beta - 1) / (sum(N_v) + (beta - 1) * length(w_dn)), 
                    method = "MAP", 
                    shake = shake[i])
  
  tmp_df2 <- rbind(tmp_map, tmp_ml)
  df <- rbind(df, tmp_df2)
}

# 描画
ggplot(df, aes(x = w_dn, y = phi_v, fill = method)) +  # データの設定
  geom_bar(stat = "identity") +                        # 棒グラフ
  facet_wrap(method ~ shake) +                         # グラフの分割
  scale_x_continuous(breaks = 1:6, labels = 1:6) +     # 軸目盛
  scale_fill_manual(values = c("#00A968", "Orange")) + # 色
  labs(x = expression(w[dn]), y = expression(phi[v]))  # 軸ラベル
  


# chapter2.5 -----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)


# パラメーターの設定
beta  <- 2 # 任意の値を指定する
shake <- 64 # 任意の試行回数を指定する


# サイコロを振る
shake_result1 <- data.frame(w_dn = sample(x = 1:3, size = shake, replace = TRUE)) %>% # サイコロを振る
                 group_by(w_dn) %>%   # 出目でグループ化
                 summarise(N_v = n()) # 出目ごとにカウント

# 出ない目があったとき用の対策
shake_result2 <- left_join(data.frame(w_dn = 1:3), shake_result1, by = "w_dn")
shake_result2$N_v[is.na(shake_result2$N_v)] <- 0 # NAを0に置換


# 散布図用のランダムな値を用意
n_1 <- sample(seq(0, 1, 0.001), 50000, replace = TRUE)
n_2 <- sample(seq(0, 1, 0.001), 50000, replace = TRUE)
n_3 <- sample(seq(0, 1, 0.001), 50000, replace = TRUE)
n <- n_1 + n_2 + n_3 # 和が1となるための処理

# パラメータ：Φ
phi_1 <- n_1 / n
phi_2 <- n_2 / n
phi_3 <- n_3 / n

## ベイズ推定
# Gamma(N + beta * V)
numerator <- lgamma(sum(shake_result2$N_v) + beta * nrow(shake_result2))

# prod_{V=1}^V Gamma(N_v + beta)
denominator <- sum(lgamma(shake_result2$N_v + beta))

# N_v + beta - 1
exponent <- shake_result2$N_v + beta - 1

# phi_v^{`exponent`}
phi_v <- exponent[1] * log(phi_1) + exponent[2] * log(phi_2) + exponent[3] * log(phi_3)

# 作図用のdfを作成
bayesian_result <- data.frame(x = phi_2 + (phi_3 / 2),   # 三角座標への変換処理
                              y = sqrt(3) * (phi_3 / 2), # 三角座標への変換処理
                              z = exp(numerator - denominator + phi_v)) # 推定値

# 描画
ggplot(data = bayesian_result, mapping = aes(x = x, y = y, color = z)) + # データの指定
  geom_point() +                                                         # 散布図
  scale_color_gradientn(colors = c("blue", "green", "yellow", "red")) +  # 色
  scale_x_continuous(breaks = c(0, 1), labels = c("(1, 0, 0)", "(0, 1, 0)")) +    # x軸目盛
  scale_y_continuous(breaks = c(0, 0.87), labels = c("(1, 0, 0)", "(0, 0, 1)")) + # y軸目盛
  coord_fixed(ratio = 1) + # 縦横の比率
  labs(title = "Bayesian Estimation",                         # タイトル
       x = expression(paste(phi[1], ", ", phi[2], sep = "")), # x軸ラベル
       y = expression(paste(phi[1], ", ", phi[3], sep = ""))) # y軸ラベル



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

