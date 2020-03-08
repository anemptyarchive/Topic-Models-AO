
# 1.1.8 -------------------------------------------------------------------

# 情報量
- log2(0.025)



# 利用パッケージ
library(tidyverse)

### 確率と情報量の関係

# 作図
tibble(p = seq(0.01, 0.99, 0.01),  # 確率
       I = - log2(p)) %>%          # 情報量
  ggplot(mapping = aes(x = p, y = I)) +   # データ
  geom_line(color = "#00A968") +          # 折れ線グラフ
  labs(title = "Information Content:bit") # タイトル



### 二項分布のパラメータと平均情報量(エントロピー)の関係

# パラメータを指定
N <- 100   # 試行回数
x <- 0:N   # 表の回数
phi <- seq(0, 1, 0.01) # パラメータ(コインの歪み具合)

# 平均情報量を算出
H <- NULL
for(i in seq_along(phi)) {
  p <- dbinom(x, N, phi[i]) # 表の回数ごとの確率
  I <- - log2(p)            # 情報量
  H <- c(H, sum(p * I))     # 平均情報量
}

# 作図用のデータフレームを作成
entropy_df <- data.frame(phi = phi, 
                         H = H)

# 描画
ggplot(data = entropy_df, mapping = aes(x = phi, y = H)) +  # データ
  geom_line(color = "#00A968") +                # 折れ線グラフ
  labs(title = "Entropy:Binomial Distribution") # タイトル


# 1.1.11 ------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)


# 面積のデータフレームを作成
f_xy <- tibble(x = rep(seq(0, 10, 0.1), 101), 
               y = rep(seq(0, 10, 0.1), each = 101), 
               z = x * y)

# 辺と面積の関係を描画
ggplot(data = f_xy, mapping = aes(x = x, y = y, z = z, fill = z)) +       # データ
  geom_tile() +                                                           # ヒートマップ
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "orange")) + # タイルの色
  coord_fixed(ratio = 1) # 縦横比


# 辺と面積(等高線)の関係を描画
ggplot(data = f_xy, mapping = aes(x = x, y = y, z = z)) +  # データ
  geom_tile(aes(fill = z), alpha = 0.5) +                                  # ヒートマップ
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "orange")) +  # タイルの色
  geom_contour(aes(color = stat(level)), binwidth = 5, size = 1) +         # 等高線
  scale_color_gradientn(colors = c("blue", "green", "yellow", "orange")) + # 線の色
  guides(color = FALSE) +  # 凡例
  coord_fixed(ratio = 1)   # 縦横比


# 制約のデータフレームを作成
g_xy <- tibble(x = seq(0, 10, 0.1), 
               y = 10 - x)

# 辺・面積(等高線)と制約の関係を描画
ggplot() + 
  geom_tile(data = f_xy, mapping = aes(x = x, y = y, fill = z), alpha = 0.5) +  # ヒートマップ
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "orange")) +       # タイルの色
  geom_contour(data = f_xy, aes(x = x, y = y, z = z, color = stat(level)), 
               binwidth = 5, size = 1) +                                   # 等高線
  scale_color_gradientn(colors = c("blue", "green", "yellow", "orange")) + # 線の色
  geom_line(data = g_xy, mapping = aes(x = x, y = y), size = 1) +  # 折れ線グラフ
  guides(color = FALSE) +  # 凡例
  coord_fixed(ratio = 1)   # 縦横比



# try ---------------------------------------------------------------------

# 辺と面積(等高線)の関係を描画
ggplot(data = f_xy, mapping = aes(x = x, y = y, z = z)) +  # データ
  geom_point(aes(fill = z, color = z), alpha = 0.5, size = 2) +                                  # ヒートマップ
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "orange")) +  # タイルの色
  geom_contour(aes(color = stat(level)), binwidth = 5, size = 1) +         # 等高線
  scale_color_gradientn(colors = c("blue", "green", "yellow", "orange")) + # 線の色
  guides(color = FALSE) +  # 凡例
  coord_fixed(ratio = 1)   # 縦横比


z = volcano
v = cbind(expand.grid(y = nrow(z):1, x = 1:ncol(z)), z = c(z))
ggplot(v, aes(x, y, z = z, fill = z)) +
  geom_tile(alpha = .8) +
  geom_contour(aes(color = stat(level)), breaks = seq(100, 190, by = 10), show.legend = FALSE) +
  scale_color_gradient(limits = range(z))
