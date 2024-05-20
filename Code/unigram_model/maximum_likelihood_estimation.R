
# chapter 2.3
# ユニグラムモデル

# 最尤推定 --------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
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

# 文書データを生成
N_dv <- matrix(NA, nrow = D, ncol = V)
for(d in 1:D) { 
  
  # 各語彙の出現回数を生成
  N_dv[d, ] <- rmultinom(n = 1, size = N_d[d], prob = true_phi_v) |> # (多項乱数)
    as.vector()
  
  # 途中経過を表示
  print(paste0("document: ", d, ", words: ", N_d[d]))
}


# パラメータの推定 ----------------------------------------------------------------

### ・データの集計 -----

# 文書数を取得
D <- nrow(N_dv)

# 語彙数を取得
V <- ncol(N_dv)

# 全文書の単語数を取得
N <- sum(N_dv)

# 各文書の単語数を取得
N_d <- rowSums(N_dv)

# 各語彙の出現回数を取得
N_v <- colSums(N_dv)


### ・最尤推定 -----

# 単語分布のパラメータを計算:式(2.4)
phi_v <- N_v / N


# 推定結果の可視化 ----------------------------------------------------------------

### ・分布の作図 -----

# 推定した単語分布を格納
phi_df <- tibble::tibble(
  v    = 1:V, 
  prob = phi_v
)

# 真の単語分布を格納
true_phi_df <- tibble::tibble(
  v    = 1:V, 
  prob = true_phi_v
)

# ラベル用の文字列を作成
param_label <- paste0(
  "N == ", N
)

# 凡例用の設定を格納
linetype_lt <- list(
  fill  = c("gray", NA), 
  color = c(NA, "red")
)

# 単語分布を作図
ggplot() + 
  geom_bar(data = phi_df, 
           mapping = aes(x = v, y = prob, fill = factor(v), linetype = "estimated"), 
           stat = "identity") + # 推定した分布
  geom_bar(data = true_phi_df, 
           mapping = aes(x = v, y = prob, linetype = "true"), 
           stat = "identity", show.legend = FALSE, 
           fill = NA, color = "red", linewidth = 1) + # 真の分布
  scale_linetype_manual(breaks = c("estimated", "true"), 
                        values = c("solid", "dashed"), 
                        labels = c("estimated", "true"), 
                        name = "distribution") + # 凡例の表示用
  guides(fill = "none", 
         linetype = guide_legend(override.aes = linetype_lt)) + # 凡例の体裁
  labs(title = "word distribution (maximum likelihood estimation)", 
       subtitle = parse(text = param_label), 
       x = expression(vocabulary ~ (v)), 
       y = expression(probability ~ (phi[v])))


# データ数と推定値の関係 -------------------------------------------------------------------

# 文書ごとに単語集合を作成
w_lt <- list() # リストを初期化
for(d in 1:D) {
  
  # 語彙の出現フラグを作成
  flag_vec <- N_dv[d, ] > 0
  
  # 語彙番号を作成
  v_vec <- mapply(
    FUN = \(v, n) {rep(v, times = n)}, 
    (1:V)[flag_vec], 
    N_dv[d, flag_vec]
  ) |> 
    unlist()
  
  # 語彙番号を割当
  w_n <- sample(x = v_vec, size = N_d[d], replace = FALSE) # 単語集合
  
  # 単語集合を格納
  w_lt[[d]] <- w_n
}


# 語彙番号を取得
w_i <- w_lt |> 
  unlist()

# 語彙頻度を集計
anim_phi_df <- tidyr::expand_grid(
  i = 0:N, # データ番号
  v = 1:V  # 語彙番号
) |> # 全ての組み合わせを作成
  dplyr::left_join(
    tibble::tibble(
      i = 0:N,        # データ番号
      v = c(NA, w_i), # 語彙番号
      freq = 1 # 語彙頻度の集計用
    ), 
    by = c("i", "v")
  ) |> # サンプルを結合
  dplyr::mutate(
    freq = freq |> 
      tidyr::replace_na(replace = 0) |> # 未観測を頻度0に置換
      cumsum(), # 語彙頻度を集計
    .by = v
  ) |> 
  dplyr::mutate(
    prob  = freq / i # 確率
  )

# サンプルを格納
anim_w_df <- tibble::tibble(
  i = 0:N,        # データ番号
  v = c(NA, w_i), # 語彙番号
  KL = anim_phi_df |> 
    dplyr::reframe(
      KL = sum(true_phi_v * (log(true_phi_v) - log(prob))),  .by = i # 真のパラメータとのKL情報量を計算
    ) |> 
    dplyr::pull(KL), 
  label = paste0(
    "list(", 
    "N == ", i, ", ", 
    "KL(list(phi[truth], phi[ML])) == ", round(KL, digits = 5), 
    ")"
  )
)


# 単語分布の推移のアニメーションを作図
anim <- ggplot() + 
  geom_bar(data = anim_phi_df, 
           mapping = aes(x = v, y = prob, fill = factor(v), linetype = "estimated"), 
           stat = "identity") + # 推定した分布
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
  scale_linetype_manual(breaks = c("estimated", "true"), 
                        values = c("solid", "dashed"), 
                        labels = c("estimated", "true"), 
                        name = "distribution") + # 凡例の表示用
  coord_cartesian(clip = "off") + # (ラベル表示用の設定)
  guides(fill = "none", 
         linetype = guide_legend(override.aes = linetype_lt)) + 
  labs(title = "word distribution (maximum likelihood estimation)", 
       subtitle = "", # (ラベル表示用の空行)
       x = expression(vocabulary ~ (v)), 
       y = expression(probability ~ (phi[v])))

# 動画を作成
gganimate::animate(
  plot = anim, nframes = N+1, fps = 10, width = 8, height = 6, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/ch2/ch2_3_word_dist.mp4")
)


# KL情報量の推移の作図
ggplot() + 
  geom_line(data = anim_w_df, 
            mapping = aes(x = i, y = KL), na.rm = TRUE) + # KL情報量
  labs(title = "KL divergence (maximum likelihood estimation)", 
       x = expression(number~of~data ~ (N)), 
       y = expression(value ~ (KL(list(phi[truth], phi[ML])))))


