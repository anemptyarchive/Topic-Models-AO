
# ch3.4 混合ユニグラムモデル：変分ベイズ推定 ----------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)


## 簡易文書データ
# 文書数
D <- 10
# 語彙数
V <- 20
# 各文書における各語彙数
N_dv <- matrix(sample(1:10, D * V, replace = TRUE), D, V)


# パラメータの初期設定 --------------------------------------------------------------

# トピック数(K)を指定
K <- 4

# 負担率(q_dk)の受け皿
q_dk <- matrix(
  0, nrow = D, ncol = K, 
  dimnames = list(paste0("d=", 1:D), # 確認用の行名
                  paste0("k=", 1:K)) # 確認用の列名
)

# 事前分布のパラメータ(α, β)を指定
alpha <- 2
beta  <- 2

# 変分事後分布のパラメータ(α_k)
alpha_k <- seq(1, 3, 0.1) %>% # 範囲を指定
  sample(size = K, replace = TRUE)
# 確認用の要素名
names(alpha_k) <- paste0("k=", 1:K)

# 変分事後分布のパラメータ(β_kv)
beta_kv <- seq(1, 3, 0.1) %>% # 範囲を指定
  sample(size = K * V, replace = TRUE) %>% 
  matrix(
    nrow = K, ncol = V, 
    dimnames = list(paste0("k=", 1:K), # 行名
                    paste0("v=", 1:V)) # 列名
  )


# 変分ベイズ推定 -----------------------------------------------------------------

# 試行回数を指定
Iter <- 50

# 推移の確認用
trace_alpha <- matrix(
  0, nrow = K, ncol = Iter + 1, 
  dimnames = list(paste0("k=", 1:K), paste0("i=", 0:Iter))
)
trace_beta  <- array(
  0, dim = c(K, V, Iter + 1), 
  dimnames = list(paste0("k=", 1:K), paste0("v=", 1:V), paste0("i=", 0:Iter))
)
# 初期値を代入
trace_alpha[, 1]  <- alpha_k
trace_beta[, , 1] <- beta_kv

for(i in 1:Iter) {
  
  # 次ステップのハイパーパラメータ(alpha, beta)を初期化
  new_alpha_k <- rep(alpha, K)
  new_beta_kv <- matrix(
    beta, nrow = K, ncol = V, 
    dimnames = list(paste0("k=", 1:K), paste0("v=", 1:V))
  )
  
  for(d in 1:D){ ## (各文書：1,...,D)
    
    for(k in 1:K){ ## (各トピック：1,...,K)
      
      # 負担率(q_dk)を計算
      tmp_q_alpha <- digamma(alpha_k[k]) - digamma(sum(alpha_k))
      tmp_q_beta  <- sum(N_dv[d, ] * digamma(beta_kv[k, ])) - N_d[d] * digamma(sum(beta_kv[k, ]))
      q_dk[d, k]  <- exp(tmp_q_alpha + tmp_q_beta)
    }
    
    # 負担率の正規化：sum_{k=1}^K q_dk = 1
    if(sum(q_dk[d, ]) > 0) { ## (全ての値が0の場合は回避)
      q_dk[d, ] <- q_dk[d, ] / sum(q_dk[d, ])
    } else if(sum(q_dk[d, ]) == 0) {
      q_dk[d, ] <- 1 / K
    }
    
    for(k in 1:K) { ## (続き)
      
      # ハイパーパラメータ(alpha_k)を更新
      new_alpha_k[k] <- new_alpha_k[k] + q_dk[d, k]
      
      for(v in 1:V){ ## (各語彙：1,...,V)
        
        # ハイパーパラメータ(beta_kv)を更新
        new_beta_kv[k, v] <- new_beta_kv[k, v] + q_dk[d, k] * N_dv[d, v]
        
      } ## (/各語彙)
    } ## (/各トピック)
  } ## (/各文書)
  
  # ハイパーパラメータの(alpha, beta)を更新
  alpha_k <- new_alpha_k
  beta_kv <- new_beta_kv
  
  # 推移の確認用に推定値を保存
  trace_alpha[, i + 1]  <- alpha_k
  trace_beta[, , i + 1] <- beta_kv
}


# ハイパーパラメータの推定結果の確認 -----------------------------------------------------------------

## トピック分布パラメータ(α)
# 作図用のデータフレームを作成
alpha_df <- data.frame(
  alpha = alpha_k, 
  topic = as.factor(1:K)
)

# 作図
ggplot(alpha_df, aes(x = topic, y = alpha, fill = topic)) + 
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  labs(title = "Mixture of Unigram Models", 
       subtitle = "Variational Bayesian Estimation") # ラベル


## 単語分布パラメータ(β)
# 作図用のデータフレームを作成
beta_df_wide <- cbind(
  as.data.frame(beta_kv), 
  topic = as.factor(1:K)
)

# データフレームをlong型に変換
beta_df_long <- pivot_longer(
  beta_df_wide, 
  cols = -topic, # 変換せずにそのまま残す現列名
  names_to = "word", # 現列名を格納する新しい列の名前
  names_prefix = "v=", # 現列名から取り除く文字列
  names_ptypes = list(word = factor()), # 現列名を要素とする際の型
  values_to = "value" # 現要素を格納する新しい列の名前
)

# 作図
ggplot(beta_df_long, aes(x = word, y = value, fill = word, color = word)) + 
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  facet_wrap(~ topic, labeller = label_both) + # グラフの分割
  scale_x_discrete(breaks = seq(0, V, by = 10)) + # x軸目盛
  theme(legend.position = "none") + # 凡例
  labs(title = "Mixture of Unigram Models", 
       subtitle = "Variational Bayesian Estimation") # ラベル


# 推定結果の確認 -----------------------------------------------------------

## トピック分布(平均値)
# ディリクレ分布の平均値を計算:式(1.15)
theta_k  <- alpha_k / sum(alpha_k)

# 作図用のデータフレームを作成
theta_df <- data.frame(
  prob = theta_k, 
  topic = as.factor(1:K)
)

# 作図
ggplot(theta_df, aes(x = topic, y = prob, fill = topic)) + 
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  labs(title = "Mixture of Unigram Models", 
       subtitle = "Variational Bayesian Estimation") # ラベル


## 単語分布(平均値)
# ディリクレ分布の平均値を計算:式(1.15)
phi_kv <- beta_kv / apply(beta_kv, 1, sum)

# 作図用のデータフレームを作成
phi_df_wide <- cbind(
  as.data.frame(phi_kv), 
  topic = as.factor(1:K)
)

# データフレームをlong型に変換
phi_df_long <- pivot_longer(
  phi_df_wide, 
  cols = -topic, # 変換せずにそのまま残す現列名
  names_to = "word", # 現列名を格納する新しい列の名前
  names_prefix = "v=", # 現列名から取り除く文字列
  names_ptypes = list(word = factor()), # 現列名を要素とする際の型
  values_to = "prob" # 現要素を格納する新しい列の名前
)

# 作図
ggplot(phi_df_long, aes(x = word,  y = prob, fill = word, color = word)) + 
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  facet_wrap(~ topic, labeller = label_both) + # グラフの分割
  scale_x_discrete(breaks = seq(0, V, by = 10)) + # x軸目盛
  theme(legend.position = "none") + # 凡例
  labs(title = "Mixture of Unigram Models", 
       subtitle = "Variational Bayesian Estimation") # ラベル


# 更新値の推移の確認 -------------------------------------------------------------------

## トピック分布のパラメータ
# データフレームを作成
trace_alpha_df_wide <- cbind(
  as.data.frame(trace_alpha), 
  topic = as.factor(1:K)
)

# データフレームをlong型に変換
trace_alpha_df_long <- pivot_longer(
  trace_alpha_df_wide, 
  cols = -topic, # 変換せずにそのまま残す現列名
  names_to = "Iteration", # 現列名を格納する新しい列の名前
  names_prefix = "i=", # 現列名から取り除く文字列
  names_ptypes = list(Iteration = numeric()), # 現列名を要素とする際の型
  values_to = "value" # 現要素を格納する新しい列の名前
)

# 作図
ggplot(trace_alpha_df_long, aes(x = Iteration, y = value, color = topic)) + 
  geom_line() + # 棒グラフ
  labs(title = "Mixture of Unigram Models", 
       subtitle = "Variational Bayesian Estimation") # ラベル


## 単語分布
# 確認するトピック番号を指定
TopicNum <- 1

# 作図用のデータフレームを作成
trace_beta_df_wide <- cbind(
  as.data.frame(trace_beta[TopicNum, , ]), 
  word = as.factor(1:V)
)

# データフレームをlong型に変換
trace_beta_df_long <- pivot_longer(
  trace_beta_df_wide, 
  cols = -word, # 変換せずにそのまま残す現列名
  names_to = "Iteration", # 現列名を格納する新しい列の名前
  names_prefix = "i=", # 現列名から取り除く文字列
  names_ptypes = list(Iteration = numeric()), # 現列名を要素とする際の型
  values_to = "value" # 現要素を格納する新しい列の名前
)

# 作図
ggplot(trace_beta_df_long, aes(x = Iteration, y = value, color = word)) + 
  geom_line(alpha = 0.5) + # 折れ線グラフ
  theme(legend.position = "none") + # 凡例
  labs(title = "Mixture of Unigram Models", 
       subtitle = "Variational Bayesian Estimation") # ラベル



# 更新値の推移を確認：gif画像 ---------------------------------------------------------

## 単語分布のパラメータ
# 作図用のデータフレームを作成
trace_beta_df_wide <- data.frame()
for(i in 1:(Iter + 1)) {
  # データフレームに変換
  tmp_beta_df <- cbind(
    as.data.frame(trace_beta[, , i]), 
    topic = as.factor(1:K), # トピック番号
    Iteration = i - 1 # 試行回数
  )
  # データフレームを結合
  trace_beta_df_wide <- rbind(trace_beta_df_wide, tmp_beta_df)
}

# データフレームをlong型に変換
trace_beta_df_long <- pivot_longer(
  trace_beta_df_wide, 
  cols = -c(topic, Iteration), # 変換せずにそのまま残す現列名
  names_to = "word", # 現列名を格納する新しい列の名前
  names_prefix = "v=", # 現列名から取り除く文字列
  names_ptypes = list(word = factor()), # 現列名を要素とする際の型
  values_to = "value" # 現要素を格納する新しい列の名前
)

# 作図
graph_beta <- ggplot(trace_beta_df_long, aes(x = word, y = value, fill = word, color = word)) + 
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  facet_wrap( ~ topic, labeller = label_both) + # グラフの分割
  scale_x_discrete(breaks = seq(0, V, by = 10)) + # x軸目盛
  theme(legend.position = "none") + # 凡例
  transition_manual(Iteration) + # フレーム
  labs(title = "Mixture of Unigram Models:VBE", 
       subtitle = "i={current_frame}") # ラベル

# gif画像を作成
animate(graph_beta, nframes = Iter + 1, fps = 5)


