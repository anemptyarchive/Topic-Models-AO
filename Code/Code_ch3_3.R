
# Chapter3.3 混合ユニグラムモデル：最尤推定 ----------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)


# パラメータの初期設定 --------------------------------------------------------------

# トピック数(K)を指定
K <- 5

# 負担率(q_dk)の受け皿
q_dk <- matrix(0, nrow = D, ncol = K, 
               dimnames = list(paste0("d=", 1:D),  # 行名
                               paste0("k=", 1:K))) # 列名
q_dk <- matrix(
  0, nrow = D, ncol = K, 
  dimnames = list(
    paste0("d=", 1:D), # 行名
    paste0("k=", 1:K)  # 列名
  )
)


## トピック分布(theta)
# 初期値をランダムに設定
tmp_theta <- seq(0, 1, by = 0.01) %>% 
  sample(size = K, replace = TRUE)

# 正規化
theta_k <- tmp_theta / sum(tmp_theta)

# 確認用の要素名
names(theta_k) <- paste0("k=", 1:K)


## 単語分布(phi)
# 初期値をランダムに設定
phi_kv <- seq(0, 1, by = 0.01) %>% 
  sample(size = K * V, replace = TRUE) %>% 
  matrix(nrow = K, ncol = V, 
         dimnames = list(paste0("k=", 1:K),  # 行名
                         paste0("v=", 1:V))) # 列名
phi_kv <- seq(0, 1, by = 0.01) %>% 
  sample(size = K * V, replace = TRUE) %>% 
  matrix(
    nrow = K, ncol = V, 
    dimnames = list(
      paste0("k=", 1:K), # 行名
      paste0("v=", 1:V)  # 列名
    )
  )

# 正規化
phi_kv <- phi_kv / apply(phi_kv, 1, sum)
for(k in 1:K) {
  phi_kv[k, ] <- phi_kv[k, ] / sum(phi_kv[k, ])
}


# 最尤推定 --------------------------------------------------------------------

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
  
  new_phi_kv <- matrix(0, nrow = K, ncol = V, 
                       dimnames = list(paste0("k=", 1:K), 
                                       paste0("v=", 1:V)))
  
  for(d in 1:D){ ## (各文書：1,...,D)
    
    for(k in 1:K){ ## (各トピック：1,...,K)
      
      # 負担率の計算
      tmp_phi_k <- apply(t(phi_kv)^N_dv[d, ], 2, prod)
      q_dk[d, k] <- theta_k[k] * tmp_phi_k[k] / sum(theta_k * tmp_phi_k)
      
      # トピック分布(theta)を更新
      new_theta_k[k] <- new_theta_k[k] + q_dk[d, k]
      
      for(v in 1:V){ ## (各語彙：1,...,V)
        
        # 単語分布(phi)を更新
        new_phi_kv[k, v] <- new_phi_kv[k, v] + q_dk[d, k] * N_dv[d, v]
        
      } ## (/各語彙)
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
  
  print(paste0(i, "th try..."))
}


# 推定結果の確認 -----------------------------------------------------------------

## トピック分布の推定結果を確認
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


## 単語分布の推定結果を確認
# 作図用のデータフレームを作成
phi_df_wide <- cbind(
  as.data.frame(phi_kv), 
  topic = as.factor(1:K)
)

# データフレームをlong型に変換
phi_df_long <- pivot_longer(
  phi_df_wide, 
  cols = -topic, 
  names_to = "word", 
  names_prefix = "v=", 
  names_ptypes = list(word = factor()), 
  values_to = "prob"
)

# 作図
ggplot(phi_df_long, aes(x = word,  y = prob, fill = word, color = word)) + 
  geom_bar(stat = "identity", position = "dodge") +  # 棒グラフ
  facet_wrap(~ topic, labeller = label_both) +       # グラフの分割
  theme(legend.position = "none") +                  # 凡例
  scale_x_discrete(breaks = seq(1, V, by = 10)) + # x軸目盛
  labs(title = "Mixture of Unigram Models", 
       subtitle = "EM Algorithm") # ラベル



# 推移の確認 -------------------------------------------------------------------

## トピック分布の推定値の推移を確認
# 作図用のデータフレームを作成
trace_theta_df_wide <- cbind(
  as.data.frame(trace_theta), 
  topic = as.factor(1:K)
)

# データフレームをlong型に変換
trace_theta_df_long <- pivot_longer(
  trace_theta_df_wide, 
  cols = -topic, 
  names_to = "Iteration", 
  names_prefix = "i=", 
  names_ptypes = list(Iteration = numeric()), 
  values_to = "prob"
)

# 作図
ggplot(trace_theta_df_long, aes(x = Iteration, y = prob, color = topic)) + 
  geom_line() + # 折れ線グラフ
  labs(title = "Mixture of Unigram Models", 
       subtitle = "EM Algorithm") # ラベル


## 単語分布の推定値の推移を確認
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
  cols = -word, 
  names_to = "Iteration", 
  names_prefix = "i=", 
  names_ptypes = list(Iteration = numeric()), 
  values_to = "prob"
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
  geom_bar(stat = "identity", position = "dodge") +  # 棒グラフ
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
  names_to = "word",   # 現列名を格納する新しい列の名前
  names_prefix = "v=",  # 現列名から取り除く文字列
  names_ptypes = list(word = factor()),  # 現列名を要素とする際の型
  values_to = "prob"   # 現要素を格納する新しい列の名前
)

# 作図
graph_phi <- ggplot(trace_phi_LongDF, aes(x = word, y = prob, fill = word, color = word)) + 
  geom_bar(stat = "identity", position = "dodge") +  # 棒グラフ
  facet_wrap( ~ topic, labeller = label_both) +      # グラフの分割
  scale_x_discrete(breaks = seq(1, V, by = 10)) +    # x軸目盛
  theme(legend.position = "none") +                  # 凡例
  transition_manual(Iteration) + 
  labs(title = "Mixture of Unigram Models:EM Algorithm", 
       subtitle = "i={current_frame}") # ラベル

# 描画
animate(graph_phi, nframes = Iter + 1, fps = 5)


# 各トピックの出現確率の上位語 ---------------------------------------------------------------------


# 文書インデックス(d)
d_index <- list.files(file_path) %>% 
  str_remove_all(".txt")


## 語彙インデックス(v)
# 指定した出現回数以上の単語の行番号
num <- mecab_df %>% 
  select(-c(TERM, POS1, POS2)) %>% 
  apply(1, sum) >= 5 # 抽出する総出現回数を指定する

v_index <- mecab_df[num, ] %>% 
  filter(grepl("名詞|形容詞", POS1)) %>%  # 抽出する品詞を指定する|^動詞
  filter(grepl("一般|^自立", POS2)) %>% 
  filter(!grepl(stop_words, TERM)) %>% 
  .[, 1]  # 単語の列を抽出する

# データフレームを作成
phi_df_wide2 <- cbind(as.data.frame(t(phi_kv)), 
                      v_index) %>% 
  as.data.frame()
colnames(phi_df_wide2) <- c(paste0("topic", 1:K), "word") # key用の行名を付与

# データフレームの整形
phi_df_long2 <- data.frame()
for(i in 1:K) {
  tmp_df <- phi_df_wide2 %>% 
    select(paste0("topic", i), word) %>% 
    arrange(-.[, 1]) %>%  # 降順に並べ替え
    head(20) %>%          # 任意で指定した上位n語を抽出
    gather(key = "topic", value = "prob", -word) # long型に変換
  
  phi_df_long2 <- rbind(phi_df_long2, tmp_df)
}


# 描画
ggplot(data = phi_df_long2, 
       mapping = aes(x = reorder(x = word, X = prob), y = prob, fill = word)) +  # データ
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  coord_flip() +                                    # グラフの向き
  facet_wrap( ~ topic, scales = "free") +           # グラフの分割
  theme(legend.position = "none") +                 # 凡例
  labs(title = "Mixture of Unigram Models:VBE")     # タイトル


