
# 1.1.8 KL情報量 -------------------------------------------------------------------

# 情報量 ---------------------------------------------------------------------

# 確率
p <- c(0.2, 0.6)

# 情報量
- log2(p)


## 確率と情報量の関係

# 利用パッケージ
library(tidyverse)

# 作図
tibble(
  p = seq(0.01, 0.99, 0.01), # 確率
  I = - log2(p) # 情報量
) %>% 
  ggplot(mapping = aes(x = p, y = I)) + # データ
    geom_line(color = "#00A968") + # 折れ線グラフ
    labs(title = "Information Content(bit)") # タイトル


# 平均情報量 -------------------------------------------------------------------

## 二項分布のパラメータと平均情報量(エントロピー)の関係

# 試行回数を指定
N <- 100

# 表の回数
x <- 0:N

# 二項分布のパラメータ(コインの歪み具合)
phi <- seq(0.01, 0.99, 0.01)

# phiごとに平均情報量を計算
H <- NULL # 平均情報量を初期化
for(i in seq_along(phi)) {
  p <- dbinom(x, N, phi[i]) # 表の回数ごとの確率
  I <- - log2(p) # 情報量
  H <- c(H, sum(p * I)) # 平均情報量を追加
}

# 作図
tibble(
  phi = phi, # 二項分布のパラメータ
  H = H # 平均情報量
) %>% 
  ggplot(mapping = aes(x = phi, y = H)) + # データ
    geom_line(color = "#00A968") + # 折れ線グラフ
    labs(title = "Entropy:Binomial Distribution") # タイトル


# 1.1.11 ラグランジュの未定乗数法 ------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)


# 面積のデータフレームを作成
f_xy <- tibble(
  x = rep(seq(0, 10, 0.1), 101), 
  y = rep(seq(0, 10, 0.1), each = 101), 
  z = x * y
)

# 辺と面積の関係を可視化(ヒートマップ)
ggplot(data = f_xy, mapping = aes(x = x, y = y, fill = z)) + # データ
  geom_tile() + # ヒートマップ
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "orange")) + # タイルの色
  coord_fixed(ratio = 1) # 縦横比


# 辺と面積の関係を可視化(等高線)
ggplot(data = f_xy, mapping = aes(x = x, y = y, z = z)) + # データ
  geom_tile(aes(fill = z), alpha = 0.5) + # ヒートマップ
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "orange")) + # タイルの色
  geom_contour(aes(color = stat(level)), binwidth = 5, size = 1) + # 等高線
  scale_color_gradientn(colors = c("blue", "green", "yellow", "orange")) + # 線の色
  guides(color = FALSE) + # 凡例
  coord_fixed(ratio = 1) # 縦横比


# 制約のデータフレームを作成
g_xy <- tibble(
  x = seq(0, 10, 0.1), 
  y = 10 - x
)

# 面積と制約の関係を可視化
ggplot() + 
  geom_tile(data = f_xy, mapping = aes(x = x, y = y, fill = z), alpha = 0.5) + # ヒートマップ
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "orange")) + # タイルの色
  geom_contour(data = f_xy, aes(x = x, y = y, z = z, color = stat(level)), 
               binwidth = 5, size = 1) + # 等高線
  scale_color_gradientn(colors = c("blue", "green", "yellow", "orange")) + # 線の色
  geom_line(data = g_xy, mapping = aes(x = x, y = y), size = 1) + # 折れ線グラフ
  guides(color = FALSE) + # 凡例
  coord_fixed(ratio = 1) # 縦横比


