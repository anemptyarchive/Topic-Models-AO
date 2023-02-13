
# ch3.4 混合ユニグラムモデル：変分ベイズ推定 ----------------------------------------------------------------------

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

# 度数と単語分布を比較
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


# 変分ベイズ推定 -----------------------------------------------------------------

# トピック数を指定
K <- 4

# 文書数と語彙数を取得
D <- nrow(N_dv)
V <- ncol(N_dv)


# 負担率qの初期化
q_dk <- matrix(0, nrow = D, ncol = K)

# トピック分布θの事前分布のパラメータαを指定
alpha <- 1

# 単語分布Φの事前分布のパラメータβを指定
beta  <- 1

# トピック分布θの変分事後分布のパラメータαの初期化
alpha_k <- runif(n = K, min = 0.01, max = 2) # 範囲を指定

# 単語分布Φの変分事後分布のパラメータβの初期化
beta_kv <- runif(n = K*V, min = 0.01, max = 2) |> # 範囲を指定
  matrix(nrow = K, ncol = V)


# 試行回数を指定
MaxIter <- 20

# 推移の確認用の受け皿を作成
trace_q_dki    <- array(NA, dim = c(D, K, MaxIter+1))
trace_alpha_ki <- matrix(NA, nrow = K, ncol = MaxIter+1)
trace_beta_kvi <- array(NA, dim = c(K, V, MaxIter+1))

# 初期値を保存
trace_q_dki[, , 1]    <- q_dk
trace_alpha_ki[, 1]   <- alpha_k
trace_beta_kvi[, , 1] <- beta_kv

# 変分ベイズ推定
for(i in 1:MaxIter) {
  
  # 次ステップのハイパーパラメータの初期化
  new_alpha_k <- rep(alpha, times = K)
  new_beta_kv <- matrix(beta, nrow = K, ncol = V)
  
  for(d in 1:D){ ## (各文書)
    for(k in 1:K){ ## (各トピック)
      
      # 負担率qの計算:式(3.22)
      term_alpha <- digamma(alpha_k[k]) - digamma(sum(alpha_k))
      term_beta  <- sum(N_dv[d, ] * digamma(beta_kv[k, ])) - N_d[d] * digamma(sum(beta_kv[k, ]))
      q_dk[d, k] <- exp(term_alpha + term_beta)
      
    } ## (各トピック:続く)
    
    # 負担率qの正規化
    if(all(q_dk[d, ] == 0)) {
      # 全ての要素が0の場合は等確率に設定
      q_dk[d, ] <- 1 / K
    } else {
      q_dk[d, ] <- q_dk[d, ] / sum(q_dk[d, ])
    }
    
    for(k in 1:K) { ## (各トピック:続き)
      
      # トピック分布θの変分事後分布のパラメータαの計算:式(3.19')
      new_alpha_k[k] <- new_alpha_k[k] + q_dk[d, k]
      
      for(v in 1:V){ ## (各語彙)
        
        # 単語分布Φの変分事後分布のパラメータβの計算:式(3.20')
        new_beta_kv[k, v] <- new_beta_kv[k, v] + q_dk[d, k] * N_dv[d, v]
        
      } ## (各語彙)
    } ## (各トピック)
  } ## (各文書)
  
  # ハイパーパラメータの更新
  alpha_k <- new_alpha_k
  beta_kv <- new_beta_kv
  
  # i回目の更新値を保存
  trace_q_dki[, , i+1]    <- q_dk
  trace_alpha_ki[, i+1]   <- alpha_k
  trace_beta_kvi[, , i+1] <- beta_kv
  
  # 動作確認
  message("\r", i, "/", MaxIter, appendLF = FALSE)
}


# 推定結果の可視化 ---------------------------------------------------------------

### ・分布の確認 -----

# トピック分布の期待値を計算:式(1.15)
E_theta_k <- alpha_k / sum(alpha_k)

# 作図用のデータフレームを作成
E_topic_df <- tibble::tibble(
  topic = factor(1:K), # トピック番号
  probability = E_theta_k # 割り当て確率
)

# 推定トピック分布を作図
ggplot() + 
  geom_bar(data = E_topic_df, mapping = aes(x = topic, y = probability, fill = topic), 
           stat = "identity", show.legend = FALSE) + # トピック分布の期待値
  labs(title = "Topic Distribution", 
       subtitle = "Variational Bayesian Estimation", 
       x = "k", y = expression(E(theta[k])))


# 単語分布の期待値を計算:式(1.15)
E_phi_kv <- beta_kv / rowSums(beta_kv)

# 作図用のデータフレームを作成
E_word_df <- E_phi_kv |> 
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

# 推定単語分布を作図
ggplot() + 
  geom_bar(data = E_word_df, mapping = aes(x = vocabulary, y = probability, fill = vocabulary), 
           stat = "identity", show.legend = FALSE) + # 単語分布の期待値
  facet_wrap(topic ~ ., labeller = label_bquote(k==.(topic)), scales = "free_x") + # 分割
  labs(title = "Word Distribution", 
       subtitle = "Variational Bayesian Estimation", 
       x = "v", y = expression(E(phi[kv])))


### ・更新推移の確認 -----

# 作図用のデータフレームを作成
trace_topic_df <- trace_alpha_ki |> 
  tibble::as_tibble(.name_repair = ~as.character(0:MaxIter)) |> 
  tibble::add_column(
    topic = factor(1:K) # トピック番号
  ) |> 
  tidyr::pivot_longer(
    cols = !topic, 
    names_to = "iteration", # 列名を試行番号に変換
    names_transform = list(iteration = as.numeric), 
    values_to = "hyperparameter" # ハイパーパラメータ列をまとめる
  ) |> 
  dplyr::group_by(iteration) |> # トピック分布の期待値計算用
  dplyr::mutate(
    probability = hyperparameter / sum(hyperparameter) # 割り当て確率
  ) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(iteration, topic)

# トピック分布のハイパーパラメータの推移を作図
ggplot() + 
  geom_line(data = trace_topic_df, mapping = aes(x = iteration, y = hyperparameter, color = topic), 
            alpha = 0.5, size = 1) + # 更新推移
  labs(title = "Hyperparameter of the Topic Distribution", 
       subtitle = "Variational Bayesian Estimation", 
       x = "iteration", y = expression(alpha[k]))

# トピック分布の期待値の推移を作図
ggplot() + 
  geom_line(data = trace_topic_df, mapping = aes(x = iteration, y = probability, color = topic), 
            alpha = 0.5, size = 1) + # 更新推移
  labs(title = "Topic Distribution", 
       subtitle = "Variational Bayesian Estimation", 
       x = "iteration", y = expression(E(theta[k])))

# 推定トピック分布のアニメーションを作図
anime_topic_graph <- ggplot() + 
  geom_bar(data = trace_topic_df, mapping = aes(x = topic, y = probability, fill = topic), 
           stat = "identity", show.legend = FALSE) + # トピック分布の期待値
  gganimate::transition_manual(frames = iteration) + # フレーム
  labs(title = "Topic Distribution", 
       subtitle = "iteration = {current_frame}", 
       x = "k", y = expression(E(theta[k])))

# gif画像を作成
gganimate::animate(plot = anime_topic_graph, nframes = MaxIter+1, fps = 5, width = 600, height = 450)


# 作図用のデータフレームを作成
trace_word_df <- trace_beta_kvi |> 
  tibble::as_tibble(.name_repair = ~as.character(1:(V*(MaxIter+1)))) |> 
  tibble::add_column(
    topic = factor(1:K) # トピック番号
  ) |> 
  tidyr::pivot_longer(
    cols = !topic, 
    names_to = "vi", # 列名を語彙番号×試行番号に変換
    names_transform = list(vi = as.numeric), 
    values_to = "hyperparameter" # ハイパーパラメータ列をまとめる
  ) |> 
  dplyr::arrange(vi, topic) |> # 番号列の追加用
  dplyr::bind_cols(
    tidyr::expand_grid(
      iteration = 0:MaxIter, # 試行番号
      vocabulary = factor(1:V), # 語彙番号
      tmp_topic = 1:K
    ) # 試行ごとに語彙番号を複製
  )|> 
  dplyr::select(!c(vi, tmp_topic)) |> # 不要な列を除去
  dplyr::group_by(iteration, topic) |> # 単語分布の期待値計算用
  dplyr::mutate(
    probability = hyperparameter / sum(hyperparameter) # 出現確率
  ) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(iteration, topic, vocabulary)

# 単語分布のハイパーパラメータの推移を作図
ggplot() + 
  geom_line(data = trace_word_df, mapping = aes(x = iteration, y = hyperparameter, color = vocabulary), 
            alpha = 0.5, size = 1) + # 更新推移
  facet_wrap(topic ~ ., labeller = label_bquote(k==.(topic)), scales = "free_y") + # 分割
  labs(title = "Hyperparameter of the Word Distribution", 
       subtitle = "Variational Bayesian Estimation", 
       color = "v", 
       x = "iteration", y = expression(beta[kv]))

# 単語分布の期待値の推移を作図
ggplot() + 
  geom_line(data = trace_word_df, mapping = aes(x = iteration, y = probability, color = vocabulary), 
            alpha = 0.5, size = 1) + # 更新推移
  facet_wrap(topic ~ ., labeller = label_bquote(k==.(topic))) + # 分割
  labs(title = "Word Distribution", 
       subtitle = "Variational Bayesian Estimation", 
       color = "v", 
       x = "iteration", y = expression(E(phi[kv])))

# 推定単語分布のアニメーションを作図
anime_word_graph <- ggplot() + 
  geom_bar(data = trace_word_df, mapping = aes(x = vocabulary, y = probability, fill = vocabulary), 
           stat = "identity", show.legend = FALSE) + # 単語分布の期待値
  gganimate::transition_manual(frames = iteration) + # フレーム
  facet_wrap(topic ~ ., labeller = label_bquote(k==.(topic)), scales = "free_x") + # 分割
  labs(title = "Word Distribution", 
       subtitle = "iteration = {current_frame}", 
       x = "v", y = expression(E(phi[kv])))

# gif画像を作成
gganimate::animate(plot = anime_word_graph, nframes = MaxIter+1, fps = 5, width = 800, height = 600)


# 作図用のデータフレームを作成
trace_response_df <- trace_q_dki |> 
  tibble::as_tibble(.name_repair = ~as.character(1:(K*(MaxIter+1)))) |> 
  tibble::add_column(
    document = factor(1:D) # 文書番号
  ) |> 
  tidyr::pivot_longer(
    cols = !document, 
    names_to = "ki", # 列名を語彙番号×試行番号に変換
    names_transform = list(ki = as.numeric), 
    values_to = "responsibility" # 負担率列をまとめる
  ) |> 
  dplyr::arrange(ki, document) |> # 番号列の追加用
  dplyr::bind_cols(
    tidyr::expand_grid(
      iteration = 0:MaxIter, # 試行番号
      topic = factor(1:K), # トピック番号
      tmp_document = 1:D
    ) # 試行ごとにトピック番号を複製
  )|> 
  dplyr::select(!c(ki, tmp_document)) |> # 不要な列を除去
  dplyr::arrange(iteration, document, topic)

# 負担率の推移を作図
ggplot() + 
  geom_line(data = trace_response_df, mapping = aes(x = iteration, y = responsibility, color = topic), 
            alpha = 0.5, size = 1) + # 更新推移
  facet_wrap(document ~ ., labeller = label_bquote(d==.(document))) + # 分割
  labs(title = "responsibility", 
       subtitle = "Variational Bayesian Estimation", 
       color = "k", 
       x = "iteration", y = expression(q[dk]))


### ・真の分布との比較 -----

# インデックスの組み合わせを作成してKL情報量を計算
kl_df <- tidyr::expand_grid(
  group = 1:gamma(K+1), # 組み合わせ番号
  k = factor(1:K) # 真の分布・並べ替え後のトピック番号
) |> # トピック番号を複製
  tibble::add_column(
    j = gtools::permutations(n = K, r = K, v = 1:K) |> # トピック番号の順列を作成
      t() |> 
      as.vector() |> 
      factor() # 元のトピック番号・並べ替え用のインデックス
  ) |> 
  dplyr::group_by(group) |> # 組み合わせごとの計算用
  dplyr::mutate(
    # KL情報量を計算
    kl_topic = sum(theta_true_k * (log(theta_true_k) - log(E_theta_k[j]))), # トピック分布のKL情報量
    kl_word = sum(phi_true_kv * (log(phi_true_kv) - log(E_phi_kv[j, ]))), # 単語分布のKL情報量
    kl_sum = kl_topic + kl_word/V
  ) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(kl_sum, group, k) # 当て嵌まりが良い順に並べ替え

# KL情報量が最小となるトピック番号を抽出
adapt_idx <- kl_df |> 
  dplyr::slice_head(n = K) |> # 最小の組み合わせを抽出
  dplyr::pull(j) |> # 列をベクトルに変換
  as.numeric() # 数値に変換


# 作図用のデータフレームを作成
E_topic_df <- tibble::tibble(
  k = factor(adapt_idx), # 推定時のトピック番号
  topic = factor(1:K), # 真の分布に対応したトピック番号
  probability = E_theta_k[adapt_idx] # 割り当て確率
)

# トピック分布を作図
ggplot() + 
  geom_bar(data = E_topic_df, mapping = aes(x = topic, y = probability, fill = k, linetype = "result"), 
           stat = "identity", alpha = 0.5) + # 推定トピック分布
  geom_bar(data = true_topic_df, mapping = aes(x = topic, y = probability, color = topic, linetype = "truth"), 
           stat = "identity", alpha = 0) + # 真のトピック分布
  scale_linetype_manual(breaks = c("result", "truth"), values = c("solid", "dashed"), name = "distribution") + # 線の種類:(凡例表示用)
  guides(color = "none", fill = "none", 
         linetype = guide_legend(override.aes = list(color = c("black", "black"), fill = c("white", "white")))) + # 凡例の体裁:(凡例表示用)
  labs(title = "Topic Distribution", 
       subtitle = "EM Algorithm", 
       x = "k", y = expression(theta[k]))


# 作図用のデータフレームを作成
E_word_df <- E_phi_kv[adapt_idx, ] |> 
  tibble::as_tibble(.name_repair = ~as.character(1:V)) |> 
  tibble::add_column(
    k = factor(adapt_idx), # 推定時のトピック番号
    topic = factor(1:K) # 真の分布に対応したトピック番号
  ) |> 
  tidyr::pivot_longer(
    cols = !c(k, topic), 
    names_to = "vocabulary", # 列名を語彙番号に変換
    names_ptypes = list(vocabulary = factor()), 
    values_to = "probability" # 出現確率列をまとめる
  )

# 単語分布を作図
ggplot() + 
  geom_bar(data = E_word_df, mapping = aes(x = vocabulary, y = probability, fill = vocabulary, linetype = "result"), 
           stat = "identity", alpha = 0.5) + # 推定単語分布
  geom_bar(data = true_word_df, mapping = aes(x = vocabulary, y = probability, color = vocabulary, linetype = "truth"), 
           stat = "identity", alpha = 0) + # 真の単語分布
  facet_wrap(topic ~ ., labeller = label_bquote(k==.(topic)), scales = "free_x") + # 分割
  scale_linetype_manual(breaks = c("result", "truth"), values = c("solid", "dashed"), name = "distribution") + # 線の種類:(凡例表示用)
  guides(color = "none", fill = "none", 
         linetype = guide_legend(override.aes = list(color = c("black", "black"), fill = c("white", "white")))) + # 凡例の体裁:(凡例表示用)
  labs(title = "Word Distribution", 
       subtitle = "EM Algorithm", 
       x = "v", y = expression(phi[kv]))


# 作図用のデータフレームを作成
trace_topic_df <- trace_alpha_ki[adapt_idx, ] |> 
  tibble::as_tibble(.name_repair = ~as.character(0:MaxIter)) |> 
  tibble::add_column(
    k = factor(adapt_idx), # 推定時のトピック番号
    topic = factor(1:K) # トピック番号
  ) |> 
  tidyr::pivot_longer(
    cols = !c(k, topic), 
    names_to = "iteration", # 列名を試行番号に変換
    names_transform = list(iteration = as.numeric), 
    values_to = "hyperparameter" # ハイパーパラメータ列をまとめる
  ) |> 
  dplyr::group_by(iteration) |> # トピック分布の期待値計算用
  dplyr::mutate(
    probability = hyperparameter / sum(hyperparameter) # 割り当て確率
  ) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(iteration, topic)

# 真のトピック分布を複製
anime_true_topic_df <- tidyr::expand_grid(
  iteration = 0:MaxIter, # 試行番号
  true_topic_df
)

# トピック分布のアニメーションを作図
anime_topic_graph <- ggplot() + 
  geom_bar(data = trace_topic_df, mapping = aes(x = topic, y = probability, fill = k, linetype = "result"), 
           stat = "identity", alpha = 0.5) + # 推定トピック分布
  geom_bar(data = anime_true_topic_df, mapping = aes(x = topic, y = probability, color = topic, linetype = "truth"), 
           stat = "identity", alpha = 0) + # 真のトピック分布
  gganimate::transition_manual(frames = iteration) + # フレーム
  scale_linetype_manual(breaks = c("result", "truth"), values = c("solid", "dashed"), name = "distribution") + # 線の種類:(凡例表示用)
  guides(color = "none", fill = "none", 
         linetype = guide_legend(override.aes = list(color = c("black", "black"), fill = c("white", "white")))) + # 凡例の体裁:(凡例表示用)
  labs(title = "Topic Distribution", 
       subtitle = "iteration = {current_frame}", 
       x = "k", y = expression(theta[k]))

# gif画像を作成
gganimate::animate(plot = anime_topic_graph, nframes = MaxIter+1, fps = 5, width = 700, height = 450)


# 作図用のデータフレームを作成
trace_word_df <- trace_beta_kvi[adapt_idx, , ] |> 
  tibble::as_tibble(.name_repair = ~as.character(1:(V*(MaxIter+1)))) |> 
  tibble::add_column(
    k = factor(adapt_idx), # 推定時のトピック番号
    topic = factor(1:K) # トピック番号
  ) |> 
  tidyr::pivot_longer(
    cols = !c(k, topic), 
    names_to = "vi", # 列名を語彙番号×試行番号に変換
    names_transform = list(vi = as.numeric), 
    values_to = "hyperparameter" # ハイパーパラメータ列をまとめる
  ) |> 
  dplyr::arrange(vi, topic) |> # 番号列の追加用
  dplyr::bind_cols(
    tidyr::expand_grid(
      iteration = 0:MaxIter, # 試行番号
      vocabulary = factor(1:V), # 語彙番号
      tmp_topic = 1:K
    ) # 試行ごとに語彙番号を複製
  )|> 
  dplyr::select(!c(vi, tmp_topic)) |> # 不要な列を除去
  dplyr::group_by(iteration, topic) |> # 単語分布の期待値計算用
  dplyr::mutate(
    probability = hyperparameter / sum(hyperparameter) # 出現確率
  ) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(iteration, topic, vocabulary)

# 真の単語分布を複製
anime_true_word_df <- tidyr::expand_grid(
  iteration = 0:MaxIter, # 試行番号
  true_word_df
)

# 単語分布のアニメーションを作図
anime_word_graph <- ggplot() + 
  geom_bar(data = trace_word_df, mapping = aes(x = vocabulary, y = probability, fill = vocabulary, linetype = "result"), 
           stat = "identity", alpha = 0.5) + # 推定単語分布
  geom_bar(data = anime_true_word_df, mapping = aes(x = vocabulary, y = probability, color = vocabulary, linetype = "truth"), 
           stat = "identity", alpha = 0) + # 真の単語分布
  gganimate::transition_manual(frames = iteration) + # フレーム
  facet_wrap(topic ~ ., labeller = label_bquote(k==.(topic)), scales = "free_x") + # 分割
  scale_linetype_manual(breaks = c("result", "truth"), values = c("solid", "dashed"), name = "distribution") + # 線の種類:(凡例表示用)
  guides(color = "none", fill = "none", 
         linetype = guide_legend(override.aes = list(color = c("black", "black"), fill = c("white", "white")))) + # 凡例の体裁:(凡例表示用)
  labs(title = "Word Distribution", 
       subtitle = "iteration = {current_frame}", 
       x = "v", y = expression(phi[kv]))

# gif画像を作成
gganimate::animate(plot = anime_word_graph, nframes = MaxIter+1, fps = 5, width = 900, height = 600)


