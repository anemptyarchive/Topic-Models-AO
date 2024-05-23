
# chapter 3.1
# 混合ユニグラムモデル

# 生成モデル -------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(MCMCpack)

# チェック用
library(ggplot2)


# 真のパラメータの設定 --------------------------------------------------------------

# 文書数を指定
D <- 20

# 語彙数を指定
V <- 26

# トピック数を指定
K <- 4


# トピック分布のハイパーパラメータを指定
true_alpha   <- 1
true_alpha_k <- rep(true_alpha, times = K) # 一様な値の場合
#true_alpha_k <- runif(n = K, min = 1, max = 2) # 多様な値の場合

# トピック分布のパラメータを生成
#true_theta_k <- rep(1/K, times = K) # 一様な値の場合
true_theta_k <- MCMCpack::rdirichlet(n = 1, alpha = true_alpha_k) |> # 多様な値の場合
  as.vector()


# 単語分布のハイパーパラメータを指定
true_beta   <- 1
true_beta_v <- rep(true_beta, times = V) # 一様な値の場合
#true_beta_v <- runif(n = V, min = 1, max = 2) # 多様な値の場合

# 単語分布のパラメータを生成
#true_phi_kv <- matrix(1/V, nrow = K, ncol = V) # 一様な値の場合
true_phi_kv <- MCMCpack::rdirichlet(n = K, alpha = true_beta_v) # 多様な値の場合


# 文書データの簡易生成 --------------------------------------------------------------

### ・語順を記録する場合 -----

# 文書集合を初期化
w_lt <- list()

# 各文書のトピックを初期化
true_z_d <- rep(NA, times = D)

# 各文書の単語数を初期化
N_d <- rep(NA, times = D)

# 文書ごとの各語彙の単語数を初期化
N_dv <- matrix(0, nrow = D, ncol = V)

# 文書データを生成
for(d in 1:D) {
  
  # トピックを生成
  onehot_k <- rmultinom(n = 1, size = 1, prob = true_theta_k) |> # (カテゴリ乱数)
    as.vector() # one-hot符号
  k <- which(onehot_k == 1) # トピック番号
  
  # トピックを割当
  true_z_d[d] <- k
  
  # 単語数を生成
  N_d[d] <- sample(x = 10:20, size = 1) # 下限:上限を指定
  
  # 各単語の語彙を初期化
  w_n <- rep(NA, times = N_d[d]) # 単語集合
  
  for(n in 1:N_d[d]) { # 単語ごと
    
    # 語彙を生成
    onehot_v <- rmultinom(n = 1, size = 1, prob = true_phi_kv[k, ]) |> # (カテゴリ乱数)
      as.vector() # one-hot符号
    v <- which(onehot_v == 1) # 語彙番号
    
    # 語彙を割当
    w_n[n] <- v
    
    # 頻度をカウント
    N_dv[d, v] <- N_dv[d, v] + 1
  }
  
  # 単語集合を格納
  w_lt[[d]] <- w_n
  
  # 途中経過を表示
  print(paste0("document: ", d, ", topic: ", k, ", words: ", N_d[d]))
}


### ・語順を記録しない場合 -----

# 各文書のトピックを割当
true_z_d <- sample(x = 1:K, size = D, replace = TRUE)

# 各文書の単語数を生成
N_d <- sample(x = 10:20, size = D, replace = TRUE) # 下限:上限を指定

# 文書ごとの各語彙の単語数を初期化
N_dv <- matrix(0, nrow = D, ncol = V)

# 文書データを生成
for(d in 1:D) { 
  
  # 各語彙の単語数を生成
  N_dv[d, ] <- rmultinom(n = 1, size = N_d[d], prob = true_phi_kv[true_z_d[d], ]) |> # (多項乱数)
    as.vector()
  
  # 途中経過を表示
  print(paste0("document: ", d, ", topic: ", true_z_d[d], ", words: ", N_d[d]))
}


# 文書データの集計 ----------------------------------------------------------------

### ・語数から語順の作成 -----

# 文書集合を初期化
w_lt <- list()

# 文書ごとに単語集合を作成
for(d in 1:D) {
  
  # 語彙番号を作成
  v_vec <- mapply(
    FUN = \(v, n) {rep(v, times = n)}, # 語彙番号を複製
    1:V,      # 語彙番号
    N_dv[d, ] # 出現回数
  ) |> 
    unlist()
  
  # 語彙を割当
  w_n <- sample(x = v_vec, size = N_d[d], replace = FALSE) # 単語集合
  
  # 単語集合を格納
  w_lt[[d]] <- w_n
}


### ・観測データの集計 -----

# 文書数を取得
D <- nrow(N_dv)

# 語彙数を取得
V <- ncol(N_dv)

# 全文書の単語数を取得
N <- sum(N_dv)

# 各文書の単語数を取得
N_d <- rowSums(N_dv)


# 真の分布の可視化 ---------------------------------------------------------------

###・トピック分布の作図 -----

# トピック分布を格納
true_theta_df <- tibble::tibble(
  k    = 1:K,         # トピック番号
  prob = true_theta_k # 生成確率
)

# ラベル用の文字列を作成
param_label <- paste0(
  "alpha == ", true_alpha
) # 一様な値の場合
param_label <- paste0(
  "alpha == (list(", paste0(true_alpha_k, collapse = ", "), "))"
) # 多様な値の場合

# トピック分布を作図
ggplot() + 
  geom_bar(data = true_theta_df, 
           mapping = aes(x = k, y = prob, fill = factor(k)), 
           stat = "identity", show.legend = FALSE) + # 確率
  labs(title = "topic distribution", 
       subtitle = parse(text = param_label), 
       x = expression(topic ~ (k)), 
       y = expression(probability ~ (theta[k])))


###・単語分布の作図 -----

# 単語分布を格納
true_phi_df <- true_phi_kv |> 
  tibble::as_tibble(.name_repair = ~paste0("V", 1:V)) |> # 語彙番号の列名を設定
  tibble::add_column(
    k = 1:K # トピック番号
  ) |> 
  tidyr::pivot_longer(
    cols      = !k, 
    names_to  = "v", # 語彙番号
    names_prefix = "V", 
    names_transform = list(v = as.numeric), 
    values_to = "prob" # 生成確率
  )

# ラベル用の文字列を作成
param_label <- paste0(
  "beta == ", true_beta
) # 一様な値の場合
param_label <- paste0(
  "beta == (list(", paste0(true_beta_v, collapse = ", "), "))"
) # 多様な値の場合

# 単語分布を作図
ggplot() + 
  geom_bar(data = true_phi_df, 
           mapping = aes(x = v, y = prob, fill = factor(k)), 
           stat = "identity", show.legend = FALSE) + # 確率
  facet_wrap(k ~ ., labeller = label_bquote(topic ~ (k): .(k)), scales = "free_x") + # トピックごとに分割
  labs(title = "word distribution", 
       subtitle = parse(text = param_label), 
       x = expression(vocabulary ~ (v)), 
       y = expression(probability ~ (phi[kv])))


# 文書データの可視化 ---------------------------------------------------------------

### ・各文書の語彙頻度の作図 -----

# 文書ごと語彙頻度を格納
Ndv_df <- N_dv |> 
  tibble::as_tibble(.name_repair = ~paste0("V", 1:V)) |> # 語彙番号の列名を設定
  tibble::add_column(
    d = 1:D,      # 文書番号
    k = true_z_d, # トピック番号
  ) |> 
  tidyr::pivot_longer(
    cols         = !c(d, k), 
    names_to     = "v", # 列名を語彙番号に変換
    names_prefix = "V", 
    names_transform = list(v = as.numeric), 
    values_to    = "freq" # 語彙頻度を1列に結合
  )

# 各文書の単語数を格納
Nd_df <- tibble::tibble(
  d    = 1:D,      # 文書番号
  k    = true_z_d, # トピック番号
  freq = N_d,      # 単語数
  label = paste0(
    "list(", 
    "z[d] ==", k, ", ", 
    "N[d] ==", freq, 
    ")"
  )
)

# ラベル用の文字列を作成
doc_label <- paste0(
  "list(", 
  "D == ", D, ", ", 
  "V == ", V, ", ", 
  "K == ", K, ", ", 
  "N == ", N, 
  ")"
)

# 文書ごとの語彙頻度を作図
ggplot() + 
  geom_bar(data = Ndv_df, 
           mapping = aes(x = v, y = freq, fill = factor(k)), 
           stat = "identity") + # 頻度
  geom_label(data = Nd_df, 
             mapping = aes(x = -Inf, y = Inf, label = label), 
             parse = TRUE, 
             hjust = 0, vjust = 1, alpha = 0.5) + # 単語数
  facet_wrap(d ~ ., labeller = label_bquote(document ~ (d): .(d))) + # 文書ごとに分割
  guides(fill = "none") + # 凡例の体裁
  labs(title = "document data", 
       subtitle = parse(text = doc_label), 
       x = expression(vocabulary ~ (v)), 
       y = expression(frequency ~ (N[dv])))


