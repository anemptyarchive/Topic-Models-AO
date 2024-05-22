
# ch3.4 混合ユニグラムモデル：生成モデル ----------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(gtools)

# チェック用
library(ggplot2)


# 文書データの簡易生成 --------------------------------------------------------------

### ・真の分布の設定 -----

# 文書数を指定
D <- 20

# 語彙数を指定
V <- 26

# トピック数を指定
K <- 4


# 真のトピック分布を生成
theta_true_k <- MCMCpack::rdirichlet(n = 1, alpha = rep(1, times = K)) |> 
  as.vector()

# 作図用のデータフレームを作成
true_topic_df <- tibble::tibble(
  topic = factor(1:K), # トピック番号
  probability = theta_true_k # 割り当て確率
)

# 真のトピック分布を作図
ggplot() + 
  geom_bar(data = true_topic_df, mapping = aes(x = topic, y = probability, fill = topic), 
           stat = "identity", show.legend = FALSE) + # トピック分布
  labs(title = "Topic Distribution", 
       subtitle = "truth", 
       x = "k", y = expression(theta[k]))


# 真の単語分布を生成
phi_true_kv <- MCMCpack::rdirichlet(n = K, alpha = rep(1, times = V))

# 作図用のデータフレームを作成
true_word_df <- phi_true_kv |> 
  tibble::as_tibble(.name_repair = ~as.character(1:V)) |> 
  tibble::add_column(
    topic = factor(1:K) # トピック番号
  ) |> 
  tidyr::pivot_longer(
    cols = !topic, 
    names_to = "vocabulary", # 列名を語彙番号に変換
    names_ptypes = list(vocabulary = factor()), 
    values_to = "probability" # 出現確率列をまとめる
  )

# 真の単語分布を作図
ggplot() + 
  geom_bar(data = true_word_df, mapping = aes(x = vocabulary, y = probability, fill = vocabulary), 
           stat = "identity", show.legend = FALSE) + # 単語分布
  facet_wrap(topic ~ ., labeller = label_bquote(k==.(topic)), scales = "free_x") + # 分割
  labs(title = "Word Distribution", 
       subtitle = "truth", 
       x = "v", y = expression(phi[kv]))


### ・文書データの生成 -----

# 文書を生成
z_d  <- rep(NA, times = D)
N_d  <- rep(NA, times = D)
N_dv <- matrix(NA, nrow = D, ncol = V)
for(d in 1:D) {
  
  # 文書dのトピックを生成
  one_hot_k <- rmultinom(n = 1, size = 1, prob = theta_true_k) |> 
    as.vector()
  k <- which(one_hot_k == 1) # トピック番号を抽出
  z_d[d] <- k
  
  # 単語数を決定
  N_d[d] <- sample(100:200, size = 1) # 範囲を指定
  
  # トピックkに従い単語を生成
  N_dv[d, ] <- rmultinom(n = 1, size = N_d[d], prob = phi_true_kv[k, ]) |> 
    as.vector()
}

# 割り当てトピック数を確認
table(z_d)

# 作図用のデータフレームを作成
freq_df <- N_dv |> 
  tibble::as_tibble(.name_repair = ~as.character(1:V)) |> 
  tibble::add_column(
    document = factor(1:D), # 文書番号
    word_count = N_d, # 単語数
    topic = factor(z_d) # 割り当てトピック
  ) |> 
  tidyr::pivot_longer(
    cols = !c(document, word_count, topic), 
    names_to = "vocabulary", # 列名を語彙番号に変換
    names_ptypes = list(vocabulary = factor()), 
    values_to = "frequency" # 出現度数列をまとめる
  ) |> 
  dplyr::mutate(
    relative_frequency = frequency / N_d[d] # 相対度数
  )

# 度数を作図
ggplot() + 
  geom_bar(data = freq_df, mapping = aes(x = vocabulary, y = frequency, fill = vocabulary), 
           stat = "identity", show.legend = FALSE) + # 出現度数
  facet_wrap(document ~ ., labeller = label_bquote(list(d==.(document), N[d]==.(N_d[document]))), scales = "free_x") + # 分割
  labs(title = "Word Frequency", 
       x = "v", y = expression(N[dv]))

# 真のトピック分布を複製
tmp_word_df <- tidyr::expand_grid(
  document = factor(1:D), # 文書番号
  true_word_df
) |> # 文書数分にトピック分布を複製
  dplyr::filter(topic == z_d[document]) # 割り当てられたトピックを抽出
tmp_word_df

# 相対度数と真の単語分布を比較
ggplot() + 
  geom_bar(data = freq_df, mapping = aes(x = vocabulary, y = relative_frequency, fill = vocabulary, linetype = "sample"), 
           stat = "identity", alpha = 0.5) + # 出現度数
  geom_bar(data = tmp_word_df, mapping = aes(x = vocabulary, y = probability, color = vocabulary, linetype = "truth"), 
           stat = "identity", alpha = 0) + # 単語分布
  facet_wrap(document ~ ., labeller = label_bquote(list(d==.(document), z[d]==.(z_d[document]))), scales = "free_x") + # 分割
  scale_x_discrete(breaks = 1:V, labels = LETTERS[1:V]) + # x軸目盛:(雰囲気用の演出)
  scale_linetype_manual(breaks = c("sample", "truth"), values = c("solid", "dashed"), name = "distribution") + # 線の種類:(凡例表示用)
  guides(color = "none", fill = "none", 
         linetype = guide_legend(override.aes = list(color = c("black", "black"), fill = c("white", "white")))) + # 凡例の体裁:(凡例表示用)
  labs(title = "Word Frequency", 
       x = "v", y = expression(frac(N[dv], N[d])))


