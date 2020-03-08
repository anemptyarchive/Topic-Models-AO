
# Chapter1.2 確率分布 ---------------------------------------------------------


# 1.2.1 二項分布 -------------------------------------------------------------------

### 二項分布

# 利用パッケージ
library(tidyverse)

# パラメーターを指定
N   <- 10     # 任意の値を指定する
phi <- 0.33   # 任意の値を指定する

# 作図
tibble(n = 0:N,                       # 表の回数
       prob = dbinom(n, N, phi)) %>%  # 確率の算出
  ggplot(mapping = aes(x = n, y = prob)) +          # データ
    geom_bar(stat = "identity", position = "dodge", fill = "#00A968") + # 棒グラフ
    labs(title = "Binomial Distribution",           # タイトル
         subtitle = paste0("N=", N, ", phi=", phi)) # サブタイトル


### 二項分布(複数)


# パラメーターを指定
N  <- 100   # 任意の値を指定する
phi <- c(0.1, 0.33, 0.5, 0.8, 0.9) # 任意の値を指定する

# 作図用のデータフレームを作成
res_df <- data.frame()
for(i in seq_along(phi)) {
  # 計算結果のデータフレームを作成
  tmp_df <- tibble(n = 0:N,                            # 表の回数
                   prob = dbinom(n, N, phi[i]),        # 確率の算出
                   parameter = paste0("phi=", phi[i])) # 作図用のラベル
  
  # データフレームを結合
  res_df <- rbind(res_df, tmp_df)
}

# 描画
ggplot(data = res_df, mapping = aes(x = n, y = prob, 
                                    fill = parameter, color = parameter)) + # データ
  geom_bar(stat = "identity", position = "dodge") +  # 棒グラフ
  labs(title = "Binomial Distribution")              # タイトル



# 1.2.3 ベータ分布 -------------------------------------------------------------------

### ベータ分布

# 利用パッケージ
library(tidyverse)


# パラメーターを指定
val_alpha <- 0.5   # 任意の値を指定する
val_beta  <- 0.5   # 任意の値を指定する

# 作図
tibble(phi = seq(0, 1, 0.01),  # phi(x軸)の値の設定
       y = (1 / beta(val_alpha, val_beta)) * phi ^ (val_alpha - 1) * (1 - phi) ^ (val_beta - 1)) %>%  # y軸の値を算出
  ggplot(mapping = aes(x = phi, y = y)) +  # データ
    geom_line(color = "#00A968") +         # 折れ線グラフ
    coord_cartesian(ylim = c(0, 3)) +      # y軸の範囲
    labs(title = "Beta Distribution:",     # タイトル
         subtitle = paste0("alpha=", val_alpha, ", beta=", val_beta),  # サブタイトル
         x = expression(phi), y = "")      # 軸ラベル



### ベータ分布(複数)

# パラメーターの指定
val_alpha <- c(1, 2, 2, 0.9, 0.8) # 任意の値を指定する
val_beta  <- c(1, 2, 4, 0.7, 1.2) # 任意の値を指定する

# 数値計算
beta_df <- data.frame()
for(i in seq_along(val_alpha)) {
  # 数値計算
  tmp_df <- tibble(phi = seq(0, 1, 0.01),  # phi(x軸)の値の設定
                   y = (1 / beta(val_alpha[i], val_beta[i])) * phi ^ (val_alpha[i] - 1) * (1 - phi) ^ (val_beta[i] - 1),  # y軸の値を算出
                   parameter = paste0("alpha=", val_alpha[i], ", beta=", val_beta[i]))
  
  # 計算結果を結合
  beta_df <- rbind(beta_df, tmp_df)
}

# 描画
ggplot(data = beta_df, mapping = aes(x = phi, y = y, color = parameter)) + # データの指定
  geom_line() +                      # 折れ線グラフ
  scale_color_manual(values = c("#FFC0CB", "#FF0000", "#FFFF00", "#EE82EE", "#7FFF00")) + 
  theme_dark() +                     # 背景色
  ylim(c(0, 3)) +                    # y軸の範囲
  labs(title = "Beta Distribution",  # タイトル
       x = expression(phi), y = "")  # 軸ラベル




# 1.2.4 ディリクレ分布 -------------------------------------------------------------------

### ディリクレ分布

# 利用パッケージ
library(tidyverse)

# パラメーターを指定
beta_v <- c(4, 2, 3) # 任意の値を指定する

# ランダムな値を生成
phi <- matrix(sample(seq(0, 1, 0.001), 120000, replace = TRUE), nrow = 3)

# 正規化
for(i in 1:ncol(phi)) {
  phi[, i] <- phi[, i] / sum(phi[, i])
}

# 正規化項を計算(対数)
tmp_beta <- lgamma(sum(beta_v)) - sum(lgamma(beta_v))

# 分布(対数)
tmp_phi  <- apply((beta_v - 1) * log(phi), 2, sum)

# 計算結果のデータフレームを作成
resu_df <- data.frame(x = phi[2, ] + (phi[3, ] / 2),  # 三角座標への変換
                      y = sqrt(3) * (phi[3, ] / 2),   # 三角座標への変換
                      z = exp(tmp_beta + tmp_phi))    # phiを算出

# 描画
ggplot(data = resu_df, mapping = aes(x = x, y = y, color = z)) +        # データ
  geom_point() +                                                        # 散布図
  scale_color_gradientn(colors = c("blue", "green", "yellow", "red")) + # プロットの色
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c("(1, 0, 0)", "(0, 1, 0)")) +            # x軸の範囲
  scale_y_continuous(breaks = c(0, 0.87), 
                     labels = c("(1, 0, 0)", "(0, 1, 0)")) +            # y軸の範囲
  coord_fixed(ratio = 1) +                                              # グラフの縦横比
  labs(title = "Dirichlet Distribution",                                # タイトル
       subtitle = paste0("beta=(", beta_v[1], ", ", beta_v[2], ", ", beta_v[3], ")"), 
       x = expression(paste(phi[1], ", ", phi[2], sep = "")),           # x軸ラベル
       y = expression(paste(phi[1], ", ", phi[3], sep = "")),           # y軸ラベル
       color = "")


### ディリクレ分布(複数)

# パラメーターを指定
beta_df <- data.frame(beta1 = c(1, 1, 1), 
                      beta2 = c(0.9, 0.9, 0.9), 
                      beta3 = c(3, 3, 3), 
                      beta4 = c(10, 10, 10), 
                      beta5 = c(4, 2, 3), 
                      beta6 = c(3, 0.9, 2)) # 任意の値を指定する

# ランダムな値を生成
phi <- matrix(sample(seq(0, 1, 0.001), 30000, replace = TRUE), nrow = 3)

# 正規化
for(i in 1:ncol(phi)) {
  phi[, i] <- phi[, i] / sum(phi[, i])
}

# 数値計算
resu_df <- data.frame()
for(i in 1:ncol(beta_df)){
  # 正規化項を計算(対数)
  tmp_beta <- lgamma(sum(beta_df[, i])) - sum(lgamma(beta_df[, i]))
  
  # 分布(対数)
  tmp_phi  <- apply((beta_df[, i] - 1) * log(phi), 2, sum)
  
  # 計算結果のデータフレームを作成
  tmp_df <- tibble(x = phi[2, ] + (phi[3, ] / 2),  # 三角座標への変換
                   y = sqrt(3) * (phi[3, ] / 2),   # 三角座標への変換
                   z = exp(tmp_beta + tmp_phi),    # phiを算出
                   tag = paste0("beta=(", beta_df[1, i], ", ", beta_df[2, i], ", ", beta_df[3, i], ")"))
  
  # 計算結果を結合
  resu_df <- rbind(resu_df, tmp_df)
}

# 描画
ggplot(data = resu_df, mapping = aes(x = x, y = y, color = z)) +        # データ
  geom_point() +                                                        # 散布図
  scale_color_gradientn(colors = c("blue", "green", "yellow", "red")) + # プロットの色
  scale_x_continuous(breaks = 1, labels = "(0, 1, 0)") +                # x軸の範囲
  scale_y_continuous(breaks = c(0, 0.87), 
                     labels = c("(1, 0, 0)", "(0, 1, 0)")) +            # y軸の範囲
  coord_fixed(ratio = 1) +                                              # グラフの縦横比
  facet_wrap(~ tag, ncol = 3) +                                         # グラフの分割
  labs(title = "Dirichlet distribution",                                # タイトル
       x = expression(paste(phi[1], ", ", phi[2], sep = "")),           # x軸ラベル
       y = expression(paste(phi[1], ", ", phi[3], sep = "")),           # y軸ラベル
       color = "")

