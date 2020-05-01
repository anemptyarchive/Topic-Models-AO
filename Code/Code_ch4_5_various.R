
# 4.5 LDA：ギブスサンプリング(多様) ------------------------------------------------------------

# 利用パッケージ
library(tidyverse)

## 簡易文書データ
# 文書数
D <- 10
# 語彙数
V <- 20
# 各文書における各語彙数
N_dv <- matrix(sample(0:10, D * V, replace = TRUE), D, V)


# パラメータの初期設定 ----------------------------------------------------------------------

# トピック数(K)を指定
K <- 4

# ハイパーパラメータ(alpha, beta)を指定
alpha_k <- rep(2, K)
beta_v  <- rep(2, V)

# 文書dの語彙vに割り当てられたトピック：z_dnの初期化
z_dn <- array(
  0, dim = c(D, V, max(N_dv)), 
  dimnames = list(paste0("d=", 1:D), 
                  paste0("v=", 1:V), 
                  paste0("N_dv=", 1:max(N_dv)))
)

# 文書dにおいてトピックkが割り当てられた単語数：N_dkの初期化
N_dk <- matrix(
  0, nrow = D, ncol = K, 
  dimnames = list(paste0("d=", 1:D), # 行名
                  paste0("k=", 1:K)) # 列名
)

# トピックkが割り当てられた語彙vの出現回数：N_kvの初期化
N_kv <- matrix(
  0, nrow = K, ncol = V, 
  dimnames = list(paste0("k=", 1:K), # 行名
                  paste0("v=", 1:V)) # 列名
)

# 全文書でトピックkが割り当てられた単語数：N_kの初期化
N_k <- rep(0, K)


# ギブスサンプリング ---------------------------------------------------------------

# 試行回数(サンプリング数)
Iter <- 1000

# 受け皿の用意
p_z_k <- NULL

# 結果の確認用
trace_alpha <- matrix(0, nrow = K, ncol = Iter + 1)
trace_beta  <- matrix(0, nrow = V, ncol = Iter + 1)
# 初期値を代入
trace_alpha[, 1] <- alpha_k
trace_beta[, 1]  <- beta_v

for(i in 1:Iter) {
  
  # 動作確認用
  start_time <- Sys.time()
  
  ## 新たに割当られたトピックに関するカウントを初期化
  new_N_dk <- matrix(rep(0, D * K), nrow = D, ncol = K, 
                     dimnames = list(paste0("d=", 1:D), paste0("k=", 1:K)))
  new_N_kv <- matrix(rep(0, K * V), nrow = K, ncol = V, 
                     dimnames = list(paste0("k=", 1:K), paste0("v=", 1:V)))
  new_N_k  <- rep(0, K)
  
  for(d in 1:D) { ## (各文書)
    
    for(v in 1:V) { ## (各語彙)
      if(N_dv[d, v] > 0) {
        for(ndv in 1:N_dv[d, v]) { ## (各語彙の出現回数)
          
          # 現ステップの計算のためにカウントを移す
          tmp_N_dk <- N_dk
          tmp_N_kv <- N_kv
          tmp_N_k  <- N_k
          
          if(z_dn[d, v, ndv] > 0) { # 初回を飛ばす処理
            
            # 前ステップで文書dの語彙vに与えられたトピックを`k`に代入
            k <- z_dn[d, v, ndv]
            
            # 文書dの語彙vの分のカウントを引く
            tmp_N_dk[d, k] <- N_dk[d, k] - 1
            tmp_N_kv[k, v] <- N_kv[k, v] - 1
            tmp_N_k[k]     <- N_k[k] - 1
          }
          
          for(k in 1:K) { ## 各トピック
            
            # サンプリング確率の計算
            tmp_p_alpha      <- tmp_N_dk[d, k] + alpha_k[k]
            tmp_p_beta_numer <- tmp_N_kv[k, v] + beta_v[v]
            tmp_p_beta_denom <- tmp_N_k[k] + sum(beta_v)
            p_z_k[k] <- tmp_p_alpha * tmp_p_beta_numer / tmp_p_beta_denom
            
          }
          
          # サンプリング
          tmp_z_dn <- rmultinom(n = 1, size = 1:K, prob = p_z_k)
          z_dn[d, v, ndv] <- which(tmp_z_dn == 1)
          
          # 新たに与えられたトピックを`k`に代入
          k <- z_dn[d, v, ndv]
          
          # 文書dの語彙vの分のカウントを加える
          new_N_dk[d, k] <- new_N_dk[d, k] + 1
          new_N_kv[k, v] <- new_N_kv[k, v] + 1
          new_N_k[k]     <- new_N_k[k] + 1
          
        } ## (/各語彙の出現回数)
      }
    } ## (/各語彙)
  } ## (/各文書)
  
  # トピック集合とカウントを更新
  N_dk <- new_N_dk
  N_kv <- new_N_kv
  N_k  <- new_N_k
  
  # ハイパーパラメータαの更新
  tmp_alpha_numer1 <- apply(digamma(t(N_dk) + alpha_k), 1, sum)
  tmp_alpha_numer2 <- D * digamma(alpha_k)
  tmp_alpha_denom1 <- sum(digamma(N_d + sum(alpha_k)))
  tmp_alpha_denom2 <- D * digamma(sum(alpha_k))
  alpha_k <- alpha_k * (tmp_alpha_numer1 - tmp_alpha_numer2) / (tmp_alpha_denom1 - tmp_alpha_denom2)
  
  # ハイパーパラメータβの更新
  tmp_beta_numer1 <- apply(digamma(t(N_kv) + beta_v), 1, sum)
  tmp_beta_numer2 <- K * digamma(beta_v)
  tmp_beta_denom1 <- sum(digamma(N_k + sum(beta_v)))
  tmp_beta_denom2 <- K * digamma(sum(beta_v))
  beta_v <- beta_v * (tmp_beta_numer1 - tmp_beta_numer2) / (tmp_beta_denom1 - tmp_beta_denom2)
  
  # 結果の確認用
  trace_alpha[, i + 1] <- alpha_k
  trace_beta[, i + 1]  <- beta_v
  
  # 動作確認
  print(paste0(i, "th try...", round(Sys.time() - start_time, 2)))
}


# パラメータの推定結果(平均値)の確認 ------------------------------------------------------------

## トピック分布(平均値)
# パラメータの平均値を計算:式(1.15)
theta_k <- alpha_k / sum(alpha_k)

# 作図用のデータフレームを作成
theta_df <- data.frame(
  topic = as.factor(1:K), 
  prob = theta_k
)

# 作図
ggplot(theta_df, aes(x = topic, y = prob, fill = topic)) + 
  geom_bar(stat = "identity", position = "dodge") +  # 棒グラフ
  labs(title = "LDA:Gibbs Sampling", 
       subtitle = expression(theta)) # ラベル


## 単語分布の推定結果(平均値)の確認
# パラメータの平均値を計算:式(1.15)
phi_v <- beta_v / sum(beta_v)

# 作図用のデータフレームを作成
phi_df <- data.frame(
  word = as.factor(1:V), 
  prob = phi_v
)

# 作図
ggplot(phi_df, aes(x = word, y = prob, fill = word, color = word)) + 
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  scale_x_discrete(breaks = seq(0, V, by = 10)) + # x軸目盛
  theme(legend.position = "none") + # 凡例
  labs(title = "LDA:Gibbs Sampling", 
       subtitle = expression(phi)) # ラベル


# 推定結果の確認 -----------------------------------------------------------------

## トピック分布のパラメータ(alpha)
# 作図用のデータフレームを作成
alpha_df <- data.frame(
  topic = as.factor(1:K), 
  value = alpha_k
)

# 作図
ggplot(alpha_df, aes(x = topic, y = value, fill = topic)) + 
  geom_bar(stat = "identity", position = "dodge") +  # 棒グラフ
  labs(title = "LDA:Gibbs Sampling", 
       subtitle = expression(alpha)) # ラベル


## 単語分布のパラメータ(beta)
# 作図用のデータフレームを作成
beta_df <- data.frame(
  word = as.factor(1:V), 
  value = beta_v
)

# 描画
ggplot(beta_df, aes(x = word, y = value, fill = word, color = word)) + 
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  scale_x_discrete(breaks = seq(0, V, by = 10)) + # x軸目盛
  theme(legend.position = "none") + # 凡例
  labs(title = "LDA:Gibbs Sampling", 
       subtitle = expression(beta)) # ラベル


# 推定結果の推移の確認 --------------------------------------------------------------

## トピック分布の推移の確認
# 作図用のデータフレームを作成
trace_alpha_df_wide <- cbind(
  as.data.frame(trace_alpha), 
  topic = as.factor(1:K)
)

# データフレームをlong型に変換
trace_alpha_df_long <- pivot_longer(
  trace_alpha_df_wide, 
  cols = -topic, # 変換せずにそのまま残す現列名
  names_to = "iteration", # 現列名を格納する新しい列の名前
  names_prefix = "V", # 現列名から取り除く文字列
  names_ptypes = list(iteration = numeric()), # 現列名を要素とする際の型
  values_to = "value" # 現要素を格納する新しい列の名前
)

# 作図
ggplot(trace_alpha_df_long, aes(x = iteration, y = value, color = topic)) + 
  geom_line() + # 折れ線グラフ
  labs(title = "LDA:Gibbs Sampling", 
       subtitle = expression(alpha)) # ラベル


## 単語分布のパラメータの推移の確認
# 作図用のデータフレームを作成
trace_beta_df_wide <- cbind(
  as.data.frame(trace_beta), 
  word = as.factor(1:V)
)

# データフレームをlong型に変換
trace_beta_df_long <- pivot_longer(
  trace_beta_df_wide, 
  cols = -word, # 変換せずにそのまま残す現列名
  names_to = "iteration", # 現列名を格納する新しい列の名前
  names_prefix = "V", # 現列名から取り除く文字列
  names_ptypes = list(iteration = numeric()), # 現列名を要素とする際の型
  values_to = "value" # 現要素を格納する新しい列の名前
)

# 作図
ggplot(trace_beta_df_long, aes(x = iteration, y = value, color = word)) + 
  geom_line(alpha = 0.5) + # 折れ線グラフ
  theme(legend.position = "none") + # 凡例
  labs(title = "LDA:Gibbs Sampling", 
       subtitle = expression(beta)) # ラベル


# 推定値の推移を確認：gif画像 ---------------------------------------------------------

# 利用パッケージ
library(gganimate)


## トピック分布のパラメータ(alpha)
# 作図
graph_alpha <- ggplot(trace_alpha_df_long, aes(x = topic, y = value, fill = topic)) + 
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  transition_manual(iteration) + # フレーム
  labs(title = "LDA:Gibbs Sampling", 
       subtitle = "i={current_frame}") # ラベル

# gif画像を作成
animate(graph_alpha, nframes = Iter + 1, fps = 10)


## 単語分布のパラメータ(beta)
# 作図
graph_beta <- ggplot(trace_beta_df_long, aes(x = word, y = value, fill = word, color = word)) + 
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  scale_x_discrete(breaks = seq(0, V, by = 10)) + # x軸目盛
  theme(legend.position = "none") + # 凡例
  transition_manual(iteration) + # フレーム
  labs(title = "LDA:Gibbs Sampling", 
       subtitle = "i={current_frame}") # ラベル

# gif画像を作成
animate(graph_beta, nframes = Iter + 1, fps = 10)


## トピック分布(平均値)
# 作図用のデータフレームを作成
trace_theta_df_wide <- cbind(
  as.data.frame(t(trace_alpha) / apply(t(trace_alpha), 1, sum)), # 平均値を計算
  iteration = 0:Iter
)

# データフレームをlong型に変換
trace_theta_df_long <- pivot_longer(
  trace_theta_df_wide, 
  cols = -iteration, # 変換せずにそのまま残す現列名
  names_to = "topic", # 現列名を格納する新しい列の名前
  names_prefix = "k=", # 現列名から取り除く文字列
  names_ptypes = list(topic = factor()), # 現列名を要素とする際の型
  values_to = "prob" # 現要素を格納する新しい列の名前
)

# 作図
graph_theta <- ggplot(trace_theta_df_long, aes(x = topic, y = prob, fill = topic)) + 
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  transition_manual(iteration) + # フレーム
  labs(title = "LDA:Gibbs Sampling", 
       subtitle = "i={current_frame}") # ラベル

# gif画像を作成
animate(graph_theta, nframes = Iter + 1, fps = 10)


## 単語分布(平均値)
# 作図用のデータフレームを作成
trace_phi_df_wide <- cbind(
  as.data.frame(t(trace_beta) / apply(t(trace_beta), 1, sum)), # 平均値を計算
  iteration = 0:Iter
)

# データフレームをlong型に変換
trace_phi_df_long <- pivot_longer(
  trace_phi_df_wide, 
  cols = -iteration, # 変換せずにそのまま残す現列名
  names_to = "word", # 現列名を格納する新しい列の名前
  names_prefix = "v=", # 現列名から取り除く文字列
  names_ptypes = list(word = factor()), # 現列名を要素とする際の型
  values_to = "prob" # 現要素を格納する新しい列の名前
)

# 作図
graph_beta <- ggplot(trace_phi_df_long, aes(x = word, y = prob, fill = word, color = word)) + 
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  scale_x_discrete(breaks = seq(0, V, by = 10)) + # x軸目盛
  theme(legend.position = "none") + # 凡例
  transition_manual(iteration) + # フレーム
  labs(title = "LDA:Gibbs Sampling", 
       subtitle = "i={current_frame}") # ラベル

# gif画像を作成
animate(graph_beta, nframes = Iter + 1, fps = 10)


