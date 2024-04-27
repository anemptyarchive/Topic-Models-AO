
# chapter 2.4
# ユニグラムモデル
# 一様パラメータの場合

# 最大事後確率推定(MAP推定) ---------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(ggpattern)
library(gganimate)

# パッケージ名の省略用
library(ggplot2)


# 文書データの簡易生成 --------------------------------------------------------------

### ・真のパラメータの設定 -----

# 文書数を指定
D <- 10

# 語彙数を指定
V <- 6

# 単語分布のハイパーパラメータを指定
true_beta   <- 1
true_beta_v <- rep(true_beta, times = V)

# 単語分布のパラメータを生成
#true_phi_v <- rep(1/V, times = V) # 一様パラメータの場合
true_phi_v <- MCMCpack::rdirichlet(n = 1, alpha = true_beta_v) |> # 多様パラメータの場合
  as.vector()


### ・文書データの生成 -----

# 各文書の単語数を生成
N_d <- sample(x = 10:20, size = D) # 下限・上限を指定

# 文書ごとの各語彙の出現回数を生成
N_dv <- matrix(NA, nrow = D, ncol = V)
for(d in 1:D) { 
  
  # 各語彙の出現回数を生成
  N_dv[d, ] <- rmultinom(n = 1, size = N_d[d], prob = true_phi_v) |> # (多項乱数)
    as.vector()
}


# パラメータの推定 ----------------------------------------------------------------

### ・データの集計 -----

# 各文書の単語数を取得
N_d <- rowSums(N_dv)

# 各語彙の出現回数を取得
N_v <- colSums(N_dv)

# 全文書の単語数を取得
N <- sum(N_dv)

# 文書数を取得
D <- nrow(N_dv)

# 語彙数を取得
V <- ncol(N_dv)


### ・MAP推定 -----

# 単語分布のハイパーパラメータを指定
beta <- 2

# 単語分布のパラメータを計算
phi_v <- (N_v + beta - 1) / (N + V * (beta - 1))


### 結果の可視化 -----

# 真の単語分布を格納
true_phi_df <- tibble::tibble(
  v    = 1:V, 
  prob = true_phi_v
)

# 推定した単語分布を格納
phi_df <- tibble::tibble(
  v    = 1:V, 
  prob = phi_v
)

# ハイパーパラメータによる基準値を格納
beta_df <- tibble::tibble(
  v     = 1:V, 
  value = (beta - 1) / (N + V * (beta - 1))
)

# ラベル用の文字列を作成
vocab_label <- paste0(
  "list(", 
  "beta == ", beta, ", ", 
  "V == ", V, ", ", 
  "N == ", N, 
  ")"
)

# 凡例用の設定を格納
linetype_lt <- list(
  fill      = c("#F8766D", NA, NA), 
  color     = c(NA, "red", "navyblue"), 
  linewidth = 0.5, 
  pattern   = c("none", "none", "crosshatch")
)

# 単語分布を作図
ggplot() + 
  geom_bar(data = phi_df, 
           mapping = aes(x = v, y = prob, fill = factor(v), linetype = "estimated"), 
           stat = "identity", show.legend = FALSE) + # 推定した分布
  ggpattern::geom_bar_pattern(
    data = beta_df, 
    mapping = aes(x = v, y = value, linetype = "hyparam"), stat = "identity", 
    fill = NA, color = "navyblue", pattern_fill = "navyblue", pattern_color = NA, 
    pattern = "crosshatch", pattern_density  = 0.05, pattern_spacing  = 0.08
  ) + # ハイパラによる基準値
  geom_bar(data = true_phi_df, 
           mapping = aes(x = v, y = prob, linetype = "true"), 
           stat = "identity", show.legend = FALSE, 
           fill = NA, color = "red", linewidth = 1) + # 真の分布
  scale_linetype_manual(breaks = c("estimated", "true", "hyparam"), 
                        values = c("solid", "dashed", "dotted"), 
                        labels = c("estimated", "true", "hyperparameter"), 
                        name = "distribution") + # 凡例の表示用
  guides(fill = "none", 
         linetype = guide_legend(override.aes = linetype_lt)) + # 凡例の体裁
  labs(title = "word distribution (maximum a posteriori estimation)", 
       subtitle = parse(text = vocab_label), 
       x = expression(vocabulary ~ (v)), 
       y = expression(probability ~ (phi[v])))


# データ数と推定値の関係 -------------------------------------------------------------------

# 単語分布のハイパーパラメータを指定
beta <- 2

# データ数(フレーム数)を指定
N <- 100

# 語彙を生成
w_i <- sample(x = 1:V, size = N, prob = true_phi_v, replace = TRUE) # 語彙番号

# サンプルを格納
anim_w_df <- tibble::tibble(
  i = 0:N, # データ番号
  v = c(NA, w_i), # 語彙番号
  label = paste0(
    "list(beta == ", beta, ", N == ", i, ")"
  )
)

# 頻度を集計
anim_phi_df <- tidyr::expand_grid(
  i = 0:N, # データ番号
  v = 1:V  # 語彙番号
) |> # 全ての組み合わせを作成
  dplyr::left_join(
    anim_w_df |> 
      dplyr::select(i, v) |> 
      tibble::add_column(freq = 1), # カウント用の値
    by = c("i", "v")
  ) |> # サンプルを結合
  dplyr::mutate(
    freq = freq |> 
      tidyr::replace_na(replace = 0) |> # 未観測をカウント値0に変換
      cumsum(), # 頻度を集計
    .by = v
  ) |> 
  dplyr::mutate(
    const = i + V * (beta - 1), # 正規化項
    prob  = (freq + beta - 1) / const, # 確率
    value = (beta - 1) / const # ハイパラによる基準値
  )


# 単語分布のアニメーションを作図
anim <- ggplot() + 
  geom_bar(data = anim_phi_df, 
           mapping = aes(x = v, y = prob, fill = factor(v), linetype = "estimated"), 
           stat = "identity", show.legend = FALSE) + # 推定した分布
  ggpattern::geom_bar_pattern(
    data = anim_phi_df, 
    mapping = aes(x = v, y = value, linetype = "hyparam"), stat = "identity", 
    fill = NA, color = "navyblue", pattern_fill = "navyblue", pattern_color = NA, 
    pattern = "crosshatch", pattern_density  = 0.05, pattern_spacing  = 0.08
  ) + # ハイパラによる基準値
  geom_bar(data = true_phi_df, 
           mapping = aes(x = v, y = prob, linetype = "true"), 
           stat = "identity", show.legend = FALSE, 
           fill = NA, color = "red", linewidth = 1) + # 真の分布
  geom_point(data = anim_w_df, 
             mapping = aes(x = v, y = 0), na.rm = TRUE, 
             color = "orange", size = 5) + # 観測した語彙
  geom_text(data = anim_w_df, 
            mapping = aes(x = -Inf, y = Inf, label = label), 
            parse = TRUE, hjust = 0, vjust = -0.5) + # データ数ラベル:(subtitleの代用)
  gganimate::transition_manual(frames = i) + # フレーム制御
  scale_linetype_manual(breaks = c("estimated", "true", "hyparam"), 
                        values = c("solid", "dashed", "dotted"), 
                        labels = c("estimated", "true", "hyperparameter"), 
                        name = "distribution") + # 凡例の表示用
  coord_cartesian(clip = "off") + # データ数ラベルの表示用
  guides(fill = "none", 
         linetype = guide_legend(override.aes = linetype_lt)) + # 凡例の体裁
  labs(title = "word distribution (maximum a posteriori estimation)", 
       subtitle = "", # (ラベル表示用の空行)
       x = expression(vocabulary ~ (v)), 
       y = expression(probability ~ (phi[v])))

# 動画を作成
gganimate::animate(
  plot = anim, nframes = N+1, fps = 10, width = 8, height = 6, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/ch2/ch2_4_word_dist.mp4")
)


