
# chapter 2.1
# ユニグラムモデル

# 生成モデル -------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)

# パッケージ名の省略用
library(ggplot2)


# 文書データの簡易生成 --------------------------------------------------------------

### ・パラメータの設定 -----

# 文書数を指定
D <- 10

# 語彙数を指定
V <- 6

# 単語分布のハイパーパラメータを指定
true_beta   <- 1
true_beta_v <- rep(true_beta, times = V) # 一様なパラメータの場合
#true_beta_v <- runif(n = V, min = 1, max = 2) # 多様なパラメータの場合

# 単語分布のパラメータを生成
#true_phi_v <- rep(1/V, times = V) # 一様なパラメータの場合
true_phi_v <- MCMCpack::rdirichlet(n = 1, alpha = true_beta_v) |> # 多様なパラメータの場合
  as.vector()


### ・文書データの生成 -----

# 文書集合を初期化
w_lt <- list()

# 各文書の単語数を初期化
N_d <- rep(NA, times = D)

# 文書ごとの各語彙の単語数を初期化
N_dv <- matrix(0, nrow = D, ncol = V)

# 文書データを生成
for(d in 1:D) {
  
  # 単語数を生成
  N_d[d] <- sample(x = 10:20, size = 1) # 下限・上限を指定
  
  # 各単語の語彙を初期化
  w_n <- rep(NA, times = N_d[d])
  
  for(n in 1:N_d[d]) { # 単語ごと
    
    # 語彙を生成
    onehot_v <- rmultinom(n = 1, size = 1, prob = true_phi_v) |> # one-hot符号
      as.vector()
    v <- which(onehot_v == 1) # 語彙番号
    
    # 語彙を割当
    w_n[n] <- v
    
    # 頻度をカウント
    N_dv[d, v] <- N_dv[d, v] + 1
  }
  
  # データを記録
  w_lt[[d]] <- w_n
  
  # 途中経過を表示
  print(paste0("document: ", d, ", words: ", N_d[d]))
}


### ・データの生成：簡易版 -----

# 各文書の単語数を生成
N_d <- sample(x = 10:20, size = D) # 下限・上限を指定

# 文書ごとの各語彙の出現回数を生成
N_dv <- matrix(NA, nrow = D, ncol = V)
for(d in 1:D) { 
  
  # 各語彙の出現回数を生成
  N_dv[d, ] <- rmultinom(n = 1, size = N_d[d], prob = true_phi_v) |> # (多項乱数)
    as.vector()
  
  # 途中経過を表示
  print(paste0("document: ", d, ", words: ", N_d[d]))
}


### ・データの集計 -----

# 文書ごとの各語彙の出現回数を集計
N_dv <- w_lt |> 
  lapply(
    FUN = \(w_n) {
      tibble::tibble(
        v    = w_n, # 語彙番号
        freq = 1    # 語彙頻度の集計用
      ) |> 
        dplyr::bind_rows(tibble::tibble(v = 1:V, freq = 0)) |> # 未出現の語彙の補完用
        dplyr::reframe(
          freq = sum(freq), .by = v # 語彙頻度を集計
        ) |> 
        dplyr::arrange(v) |> # 頻度列の結合用
        dplyr::select(freq)
    }
  ) |> 
  dplyr::bind_cols(.name_repair = ~paste0("V", 1:D)) |> # 頻度列を結合
  t() # 行と列を入替・マトリクスに変換
names(N_dv) <- NULL

# 各文書の単語数を取得
N_d <- rowSums(N_dv)

# 全文書の単語数を取得
N <- sum(N_dv)

# 文書数を取得
D <- nrow(N_dv)

# 語彙数を取得
V <- ncol(N_dv)


### ・データの可視化

# 文書と語彙ごとの頻度データを格納
Ndv_df <- N_dv |> 
  tibble::as_tibble(.name_repair = ~paste0("V", 1:V)) |> # 語彙番号の列名を設定
  tibble::add_column(
    d = 1:D # 文書番号
  ) |> 
  tidyr::pivot_longer(
    cols         = !d, 
    names_to     = "v", # 語彙番号
    names_prefix = "V", 
    names_transform = list(v = as.numeric), 
    values_to    = "freq" # 語彙頻度
  )

# ラベル用の文字列を作成
doc_label_df <- tibble::tibble(
  d     = 1:D, # 文書番号
  max_n = N_d  # 単語数
)
set_label <- paste0(
  "list(", 
  "D == ", D, ", ", 
  "V == ", V, ", ", 
  "N == ", N, 
  ")"
)

# 文書ごとの語彙頻度を作図
ggplot() + 
  geom_bar(data = Ndv_df, 
           mapping = aes(x = v, y = freq, fill = factor(v)), 
           stat = "identity") + # 文書ごとの各語彙の出現回数
  geom_label(data = doc_label_df, 
             mapping = aes(x = -Inf, y = Inf, label = paste("N[d] ==", max_n)), 
             parse = TRUE, hjust = 0, vjust = 1, alpha = 0.5) + # 各文書の単語数
  facet_wrap(d ~ ., labeller = label_bquote(document ~ (d): .(d))) + # 文書ごとに分割
  guides(fill = "none") + # 凡例の体裁
  labs(title = "document data", 
       subtitle = parse(text = set_label), 
       x = expression(vocabulary ~ (v)), 
       y = expression(frequency ~ (N[dv])))


# 語彙ごとの語彙頻度データを格納
Nv_df <- Ndv_df |> 
  dplyr::reframe(
    freq = sum(freq), .by = v, # 各語彙の単語数を集計
  )

# 全文書の頻度を作図
ggplot() + 
  geom_bar(data = Nv_df, 
           mapping = aes(x = v, y = freq, fill = factor(v)), 
           stat = "identity") + # 全文書の各語彙の出現回数
  guides(fill = "none") + # 凡例の体裁
  labs(title = "document data", 
       subtitle = parse(text = set_label), 
       x = expression(vocabulary ~ (v)), 
       y = expression(frequency ~ (N[v])))


# 単語分布を格納
true_phi_df <- tibble::tibble(
  v    = 1:V, # 語彙番号
  prob = true_phi_v # 生成確率
)

# ラベル用の文字列を作成
vocab_label <- paste0(
  "V == ", V
)

# 単語分布を作図
ggplot() + 
  geom_bar(data = true_phi_df, 
           mapping = aes(x = v, y = prob, fill = factor(v)), 
           stat = "identity") + # 生成確率
  guides(fill = "none") + # 凡例の体裁
  labs(title = "word distribution", 
       subtitle = parse(text = vocab_label), 
       x = expression(vocabulary ~ (v)), 
       y = expression(probability ~ (phi[v])))


