
# ch1.2 確率分布 ---------------------------------------------------------

# ch1.2.1 ベルヌーイ分布 -------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)


# 表となる確率を指定
phi = 0.6

# 作図
tibble(
  x = 0:1, # 表の回数
  prob = c(1 - phi, phi) # 確率
) %>% 
  ggplot(aes(x = x, y = prob)) + # データ
    geom_bar(stat = "identity", position = "dodge", fill = "#00A968") + # 棒グラフ
    labs(title = "Bernoulli Distribution", 
         subtitle = paste0("phi=", phi)) # ラベル


# ch1.2.1 二項分布 -------------------------------------------------------------------

# 通常 ----------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)


# 試行回数を指定
N <- 10

# 表となる確率(コインの歪み具合)を指定
phi <- 0.33

# 作図
tibble(
  x = 0:N, # 表の回数
  prob = dbinom(x = x, size = N, prob = phi) # 二項分布に従う確率
) %>% 
  ggplot(mapping = aes(x = x, y = prob)) + # データ
    geom_bar(stat = "identity", position = "dodge", fill = "#00A968") + # 棒グラフ
    labs(title = "Binomial Distribution", # タイトル
         subtitle = paste0("N=", N, ", phi=", phi)) # サブタイトル


# 複数 ----------------------------------------------------------------------

# 試行回数を指定
N <- 100

# 表となる確率を指定
phi <- c(0.1, 0.33, 0.5, 0.8, 0.9)

# 作図用のデータフレームを作成
res_df <- tibble()
for(i in seq_along(phi)) {
  
  # 計算結果のデータフレームを作成
  tmp_df <- tibble(
    x = 0:N, # 表の回数
    prob = dbinom(x = x, size = N, prob = phi[i]), # 二項分布に従う確率
    phi = as.factor(phi[i]) # 作図用のラベル
)
  
  # データフレームを結合
  res_df <- rbind(res_df, tmp_df)
}

# 作図
ggplot(data = res_df, mapping = aes(x = x, y = prob, fill = phi, color = phi)) + # データ
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  labs(title = "Binomial Distribution") # タイトル


# パラメータの変化による分布の変化：gif画像 ---------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)


# 試行回数を指定
N <- 100

# 表となる確率
phi <- seq(0.01, 0.99, by = 0.01)

# 作図用のデータフレームを作成
res_df <- tibble()
for(i in seq_along(phi)) {
  
  # 計算結果のデータフレームを作成
  tmp_df <- tibble(
    x = 1:N, # 表の回数
    prob = dbinom(x = x, size = N, prob = phi[i]), # 確率
    phi = as.factor(phi[i]) # 作図用のラベル
  )
  
  # データフレームを結合
  res_df <- rbind(res_df, tmp_df)
}

# 作図
graph_binom <- ggplot(data = res_df, mapping = aes(x = x, y = prob)) + # データ
  geom_bar(stat = "identity", position = "dodge", fill = "#00A968", color = "#00A968") + # 棒グラフ
  transition_manual(phi) + # フレーム
  labs(title = "Binomial Distribution", 
       subtitle = "phi={current_frame}") # ラベル

# gif画像を作成
animate(graph_binom, nframes = length(phi), fps = 10)


# 試行回数の変化によるグラフの変化：gif画像 --------------------------------------------------

# 試行回数を指定
N <- 200

# 表となる確率を指定
phi <- 0.5

# 作図用のデータフレームを作成
res_df <- tibble()
for(i in 1:N) {
  
  # 計算結果のデータフレームを作成
  tmp_df <- tibble(
    x = 1:i, # 表の回数
    prob = dbinom(x = x, size = i, prob = phi), # 確率
    N = as.factor(i) # 作図用のラベル
  )
  
  # データフレームを結合
  res_df <- rbind(res_df, tmp_df)
}

# 作図
graph_binom <- ggplot(data = res_df, mapping = aes(x = x, y = prob)) + # データ
  geom_bar(stat = "identity", position = "dodge", fill = "#00A968", color = "#00A968") + # 棒グラフ
  transition_manual(N) + # フレーム
  labs(title = "Binomial Distribution", 
       subtitle = "N={current_frame}") # ラベル

# gif画像を作成
animate(graph_binom, nframes = N, fps = 10)


# ch1.2.2 カテゴリ分布 ----------------------------------------------------------

# 利用パッケージ
library(tidyverse)


# パラメータを指定
phi <- c(0.2, 0.4, 0.1, 0.3)

# 作図
tibble(
  x = 1:length(phi), # 出目
  prob = phi # 確率
) %>% 
  ggplot(aes(x = x, y = prob)) + # データ
    geom_bar(stat = "identity", position = "dodge", fill = "#00A968") + # 棒グラフ
    labs(title = "Categorical Distribution") # ラベル


# ch1.2.2 多項分布 --------------------------------------------------------------------

# try ---------------------------------------------------------------------


# 利用パッケージ
library(tidyverse)


# 試行回数
N = 10

# 各目となる確率を指定
phi_v <- c(0.2, 0.4, 0.1)

# 出目
x <- 1:N

# 出目の種類
V <- length(phi_v)

# 
x_df = tibble(
  x_1 = rep(rep(0:N, times = 2), times = 2), 
  x_2 = rep(rep(0:N, each = 2), times = 2), 
  x_3 = rep(0:N, each = 4)
) %>% 
  mutate(x_sum = apply(., 1, sum))

# 
prob = rep(0, nrow(x_df))
for(i in 1:nrow(x_df)) {
 tmp_p <- dmultinom(x_df[i, 1:3], prob = phi)
 prob[i] <- tmp_p
}

x_df[["x_3"]]
# 計算結果のデータフレームを作成
res_df <- tibble(
  x = x_df[["x_2"]] / x_df[["x_sum"]] + (x_df[["x_3"]] / x_df[["x_sum"]] / 2),  # 三角座標への変換
  y = sqrt(3) * (x_df[["x_3"]] / x_df[["x_sum"]] / 2),   # 三角座標への変換
  z = prob
)    # phiを算出


ggplot(res_df, aes(x = x, y = y, color = z)) + 
  geom_point()

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


# ch1.2.3 ベータ分布 -------------------------------------------------------------------

# 通常 ----------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)


# パラメーターを指定
alpha <- 0.5
beta  <- 0.5

# 作図
tibble(
  phi = seq(0.01, 0.99, 0.01), # phiの値
  B = gamma(alpha + beta) / gamma(alpha) / gamma(beta), # 正規化項
  density = B * phi^(alpha - 1) * (1 - phi)^(beta - 1) # 確率密度
) %>% 
  ggplot(mapping = aes(x = phi, y = density)) + # データ
    geom_line(color = "#00A968") + # 折れ線グラフ
    labs(title = "Beta Distribution:", 
         subtitle = paste0("alpha=", alpha, ", beta=", beta), 
         x = expression(phi)) # ラベル


# 複数 ----------------------------------------------------------------------

# パラメーターの指定
alpha_vec <- c(1, 2, 2, 0.9, 0.8)
beta_vec  <- c(1, 2, 4, 0.7, 1.2)

# 作図用のデータフレームを作成
res_df <- tibble()
for(i in seq_along(alpha_vec)) {
  
  # 分布を計算
  tmp_df <- tibble(
    phi = seq(0.01, 0.99, 0.01), # phiの値
    B = 1 / beta(alpha_vec[i], beta_vec[i]), # 正規化項
    density = B * phi^(alpha_vec[i] - 1) * (1 - phi)^(beta_vec[i] - 1), # 確率密度
    parameter = paste0("alpha=", alpha_vec[i], ", beta=", beta_vec[i]) # 作図用のラベル
  )
  
  # 計算結果を結合
  res_df <- rbind(res_df, tmp_df)
}

# 作図
ggplot(data = res_df, mapping = aes(x = phi, y = density, color = parameter)) + # データの指定
  geom_line() + # 折れ線グラフ
  scale_color_manual(values = c("#FFC0CB", "#FF0000", "#FFFF00", "#EE82EE", "#7FFF00")) + # 線の色(不必要)
  theme_dark() + # 背景色(不必要)
  labs(title = "Beta Distribution", 
       x = expression(phi)) # ラベル


# パラメータの変化によるグラフの変化：gif画像 --------------------------------------------------

# 利用するライブラリ
library(tidyverse)
library(gganimate)


# パラメータの組み合わせ
parameter_df = tibble(
  alpha = rep(seq(0, 10, by = 0.25), times = 41), 
  beta = rep(seq(0, 10, by = 0.25), each = 41)
)

# 作図用のデータフレームを作成
res_df <- tibble()
for(i in 1:nrow(parameter_df)) {
  
  # 分布を計算
  tmp_df <- tibble(
    phi = seq(0.01, 0.99, 0.01), # phiの値
    density = dbeta(
      x = phi, shape1 = parameter_df[["alpha"]][i], shape2 = parameter_df[["beta"]][i]
    ), # 確率密度
    parameter = paste0(
      "alpha=", parameter_df[["alpha"]][i], ", beta=", parameter_df[["beta"]][i]
    ) %>% 
      as.factor() # 作図用のラベル
  )
  
  # 計算結果を結合
  res_df <- rbind(res_df, tmp_df)
}

# 作図
graph_data <- ggplot(data = res_df, mapping = aes(x = phi, y = density)) + # データの指定
  geom_line(color = "#00A968") + # 折れ線グラフ
  transition_manual(parameter) + # フレーム
  labs(title = "Beta Distribution", 
       subtitle = "{current_frame}", 
       x = expression(phi)) # ラベル

# gif画像を作成
animate(graph_data, nframes = nrow(parameter_df), fps = 25)


# ch1.2.4 ディリクレ分布 -------------------------------------------------------------------

# 通常 ----------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)


# パラメーターを指定(V=3)
beta_v <- c(4, 2, 3)

# 値をランダムに生成
phi <- seq(0.001, 1, 0.001) %>% 
  sample(120000, replace = TRUE) %>% 
  matrix(ncol = 3)

# 値を満遍なく生成
phi <- tibble(
  V1 = rep(rep(seq(0.02, 1, by = 0.02), times = 50), times = 50), 
  V2 = rep(rep(seq(0.02, 1, by = 0.02), each = 50), times = 50), 
  V3 = rep(seq(0.02, 1, by = 0.02), each = 50^2)
) %>% 
  as.matrix()

# 正規化
phi <- phi / rowSums(phi)

# 正規化項を計算(対数)
tmp_beta <- lgamma(sum(beta_v)) - sum(lgamma(beta_v))

# 分布を計算(対数)
tmp_phi  <- colSums((beta_v - 1) * log(t(phi)))

# 計算結果のデータフレームを作成
res_df <- tibble(
  x = phi[, 2] + (phi[, 3] / 2), # 三角座標への変換
  y = sqrt(3) * (phi[, 3] / 2), # 三角座標への変換
  density = exp(tmp_beta + tmp_phi) # 確率密度
)

# 描画
ggplot(data = res_df, mapping = aes(x = x, y = y, color = density)) + # データ
  geom_point() + # 散布図
  scale_color_gradientn(colors = c("blue", "green", "yellow", "red")) + # プロットの色
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c("(1, 0, 0)", "(0, 1, 0)")) + # x軸の範囲
  scale_y_continuous(breaks = c(0, 0.87), 
                     labels = c("(1, 0, 0)", "(0, 1, 0)")) + # y軸の範囲
  coord_fixed(ratio = 1) + # グラフの縦横比
  labs(title = "Dirichlet Distribution", 
       subtitle = paste0("beta=(", beta_v[1], ", ", beta_v[2], ", ", beta_v[3], ")"), 
       x = expression(paste(phi[1], ", ", phi[2], sep = "")), 
       y = expression(paste(phi[1], ", ", phi[3], sep = ""))) # ラベル


# 複数 ----------------------------------------------------------------------

# パラメーターを指定
beta <- tibble(
  beta1 = c(1, 1, 1), 
  beta2 = c(0.9, 0.9, 0.9), 
  beta3 = c(3, 3, 3), 
  beta4 = c(10, 10, 10), 
  beta5 = c(4, 2, 3), 
  beta6 = c(3, 0.9, 2)
) %>% 
  t()

# 値をランダムに生成
phi <- seq(0.001, 1, 0.001) %>% 
  sample(45000, replace = TRUE) %>% 
  matrix(ncol = 3)

# 正規化
phi <- phi / rowSums(phi)

# 作図用のデータフレームを作成
res_df <- tibble()
for(i in 1:nrow(beta)){
  
  # 正規化項を計算(対数)
  tmp_beta <- lgamma(sum(beta[i, ])) - sum(lgamma(beta[i, ]))
  
  # 分布を計算(対数)
  tmp_phi <- colSums((beta[i, ] - 1) * log(t(phi)))
  
  # 分布を計算
  tmp_df <- tibble(
    x = phi[, 2] + (phi[, 3] / 2), # 三角座標への変換
    y = sqrt(3) * (phi[, 3] / 2), # 三角座標への変換
    density = exp(tmp_beta + tmp_phi), # 確率密度
    parameter = paste0(
      "beta=(", beta[i, 1], ", ", beta[i, 2], ", ", beta[i, 3], ")"
    ) # 作図用のラベル
  )
  
  # 計算結果を結合
  res_df <- rbind(res_df, tmp_df)
}

# 描画
ggplot(data = res_df, mapping = aes(x = x, y = y, color = density)) + # データ
  geom_point() + # 散布図
  scale_color_gradientn(colors = c("blue", "green", "yellow", "red")) + # プロットの色
  scale_x_continuous(breaks = 1, labels = "(0, 1, 0)") + # x軸の範囲
  scale_y_continuous(breaks = c(0, 0.87), 
                     labels = c("(1, 0, 0)", "(0, 1, 0)")) + # y軸の範囲
  coord_fixed(ratio = 1) + # グラフの縦横比
  facet_wrap(~ parameter, ncol = 3) + # グラフの分割
  labs(title = "Dirichlet distribution", 
       x = expression(paste(phi[1], ", ", phi[2], sep = "")), 
       y = expression(paste(phi[1], ", ", phi[3], sep = ""))) # ラベル

