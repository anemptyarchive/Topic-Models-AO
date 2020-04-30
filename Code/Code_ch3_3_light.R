
# Chapter3.3 混合ユニグラムモデル：EMアルゴリズム(最尤推定) ----------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)


## 簡易文書データ
# 文書数
D <- 10
# 語彙数
V <- 20
# 各文書における各語彙数
N_dv <- matrix(sample(0:10, D * V, replace = TRUE), D, V)


# パラメータの初期設定 --------------------------------------------------------------

# トピック数(K)を指定
K <- 5

# 負担率(q_dk)の受け皿
q_dk <- matrix(
  0, nrow = D, ncol = K, 
  dimnames = list(paste0("d=", 1:D), # 行名
                  paste0("k=", 1:K)) # 列名
)

## トピック分布(theta)の初期値
# ランダムに値を設定
tmp_theta_k <- seq(0, 1, by = 0.01) %>% 
  sample(size = K, replace = TRUE)

# 正規化
theta_k <- tmp_theta_k / sum(tmp_theta_k)

# 確認用の要素名
names(theta_k) <- paste0("k=", 1:K)

## 単語分布(phi)の初期値
# ランダムに値を設定
tmp_phi_kv <- seq(0, 1, by = 0.01) %>% 
  sample(size = K * V, replace = TRUE) %>% 
  matrix(
    nrow = K, ncol = V, 
    dimnames = list(paste0("k=", 1:K), # 確認用の行名
                    paste0("v=", 1:V)) # 確認用の列名
  )

# 正規化
phi_kv <- tmp_phi_kv / apply(tmp_phi_kv, 1, sum)


# EMアルゴリズム(最尤推定) --------------------------------------------------------------------

# 試行回数を指定
Iter <- 50

# 推移の確認用
trace_theta <- matrix(
  0, nrow = K, ncol = Iter + 1, 
  dimnames = list(paste0("k=", 1:K), paste0("i=", 0:Iter))
)
trace_phi <- array(
  0, dim = c(K, V, Iter + 1), 
  dimnames = list(paste0("k=", 1:K), paste0("v=", 1:V), paste0("i=", 0:Iter))
)
# 初期値を代入
trace_theta[, 1] <- theta_k
trace_phi[, , 1] <- phi_kv

for(i in 1:Iter){
  
  # 次ステップのパラメータを初期化
  new_theta_k <- rep(0, K)
  names(new_theta_k) <- paste0("k=", 1:K)
  new_phi_kv <- matrix(
    0, nrow = K, ncol = V, 
    dimnames = list(paste0("k=", 1:K), paste0("v=", 1:V))
  )
  
  for(d in 1:D){ ## (各文書)
    
    # 負担率の計算
    term_k <- apply(t(phi_kv)^N_dv[d, ], 2, prod)
    q_dk[d, ] <- theta_k * term_k / sum(theta_k * term_k)
    
    # トピック分布(theta)を更新
    new_theta_k <- new_theta_k + q_dk[d, ]
    
    for(k in 1:K){ ## (各トピック)
      
      # 単語分布(phi)を更新
      new_phi_kv[k, ] <- new_phi_kv[k, ] + q_dk[d, k] * N_dv[d, ]
      
    } ## (/各トピック)
  } ## (/各文書)
  
  # パラメータの更新
  theta_k <- new_theta_k
  phi_kv  <- new_phi_kv
  
  # パラメータの正規化
  theta_k <- theta_k / sum(theta_k)
  phi_kv  <- phi_kv / apply(phi_kv, 1, sum)
  
  # 推移の確認用
  trace_theta[, i + 1] <- theta_k
  trace_phi[, , i + 1] <- phi_kv
  
  # 動作確認
  print(paste0(i, "th try..."))
}


# 推定結果の確認 -----------------------------------------------------------------

## トピック分布
# 作図用のデータフレームを作成
theta_df <- data.frame(
  topic = as.factor(1:K), 
  prob = theta_k
)

# 作図
ggplot(theta_df, aes(x = topic, y = prob, fill = topic)) + 
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  labs(title = "Mixture of Unigram Models", 
       subtitle = "EM Algorithm") # ラベル


## 単語分布
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
       subtitle = "EM Algorithm") # ラベル



# 更新値の推移の確認 -------------------------------------------------------------------

## トピック分布
# 作図用のデータフレームを作成
trace_theta_df_wide <- cbind(
  as.data.frame(trace_theta), 
  topic = as.factor(1:K)
)

# データフレームをlong型に変換
trace_theta_df_long <- pivot_longer(
  trace_theta_df_wide, 
  cols = -topic, # 変換せずにそのまま残す現列名
  names_to = "Iteration", # 現列名を格納する新しい列の名前
  names_prefix = "i=", # 現列名から取り除く文字列
  names_ptypes = list(Iteration = numeric()), # 現列名を要素とする際の型
  values_to = "prob" # 現要素を格納する新しい列の名前
)

# 作図
ggplot(trace_theta_df_long, aes(x = Iteration, y = prob, color = topic)) + 
  geom_line() + # 折れ線グラフ
  labs(title = "Mixture of Unigram Models", 
       subtitle = "EM Algorithm") # ラベル


## 単語分布
# 確認するトピック番号を指定
TopicNum <- 1

# 作図用のデータフレームを作成
trace_phi_df_wide <- cbind(
  as.data.frame(trace_phi[TopicNum, , ]), 
  word = as.factor(1:V)
)

# データフレームをlong型に変換
trace_phi_df_long <- pivot_longer(
  trace_phi_df_wide, 
  cols = -word, # 変換せずにそのまま残す現列名
  names_to = "Iteration", # 現列名を格納する新しい列の名前
  names_prefix = "i=", # 現列名から取り除く文字列
  names_ptypes = list(Iteration = numeric()), # 現列名を要素とする際の型
  values_to = "prob" # 現要素を格納する新しい列の名前
)

# 作図
ggplot(trace_phi_df_long, aes(x = Iteration, y = prob, color = word)) + 
  geom_line(alpha = 0.5) + # 折れ線グラフ
  theme(legend.position = "none") + # 凡例
  labs(title = "Mixture of Unigram Models", 
       subtitle = "EM Algorithm") # ラベル


# 推移の確認用gif ---------------------------------------------------------------------

# 利用パッケージ
library(gganimate)


## トピック分布
# 作図
graph_theta <- ggplot(trace_theta_df_long, aes(x = topic, y = prob, fill = topic)) + 
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  transition_manual(Iteration) + 
  labs(title = "Mixture of Unigram Models:EM Algorithm", 
       subtitle = "i={current_frame}") # ラベル

# gif画像を作成
animate(graph_theta, nframes = Iter + 1, fps = 5)


## 単語分布
# 作図用のデータフレームを作成
trace_phi_WideDF <- data.frame()
for(i in 1:(Iter + 1)) {
  # データフレームに変換
  tmp_trace_phi <- cbind(
    as.data.frame(trace_phi[, , i]), 
    topic = as.factor(1:K), # トピック番号
    Iteration = i - 1 # 試行回数
  )
  # データフレームを結合
  trace_phi_WideDF <- rbind(trace_phi_WideDF, tmp_trace_phi)
}

# データフレームをlong型に変換
trace_phi_LongDF <- pivot_longer(
  trace_phi_WideDF, 
  cols = -c(topic, Iteration), # 変換せずにそのまま残す現列名
  names_to = "word", # 現列名を格納する新しい列の名前
  names_prefix = "v=", # 現列名から取り除く文字列
  names_ptypes = list(word = factor()), # 現列名を要素とする際の型
  values_to = "prob" # 現要素を格納する新しい列の名前
)

# 作図
graph_phi <- ggplot(trace_phi_LongDF, aes(x = word, y = prob, fill = word, color = word)) + 
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  facet_wrap( ~ topic, labeller = label_both) + # グラフの分割
  scale_x_discrete(breaks = seq(0, V, by = 10)) + # x軸目盛
  theme(legend.position = "none") + # 凡例
  transition_manual(Iteration) + # フレーム
  labs(title = "Mixture of Unigram Models:EM Algorithm", 
       subtitle = "i={current_frame}") # ラベル

# gif画像を作成
animate(graph_phi, nframes = Iter + 1, fps = 5)


