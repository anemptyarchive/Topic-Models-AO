
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
  
  # 初期値を代入
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


# 推移の確認 -------------------------------------------------------------------


## トピック分布のパラメータ
# データフレームを作成
trace_alpha_df_wide <- cbind(t(trace_alpha), 
                             1:ncol(trace_alpha)) %>% 
                       as.data.frame()

# データフレームをlong型に変換
colnames(trace_alpha_df_wide) <- c(1:K, "n") # key用の行名を付与
trace_alpha_df_long <- gather(trace_alpha_df_wide, key = "topic", value = "alpha", -n) # 変換

# 描画
ggplot(data = trace_alpha_df_long, mapping = aes(x = n, y = alpha, color = topic)) + 
  geom_line() +                                       # 棒グラフ
labs(title = "Mixture of Unigram Models:VBE:(alpha)") # タイトル


## 単語分布のパラメータ
# データフレームを作成
trace_beta_df_wide <- cbind(t(trace_beta), 
                             1:ncol(trace_beta)) %>% 
                      as.data.frame()

# データフレームをlong型に変換
colnames(trace_beta_df_wide) <- c(1:V, "n") # key用の行名を付与
trace_beta_df_long <- gather(trace_beta_df_wide, key = "word", value = "beta", -n) # 変換
trace_beta_df_long$word <- trace_beta_df_long$word %>%  # 文字列になるため因子に変換
                           as.numeric() %>% 
                           as.factor()

# 描画
ggplot(data = trace_beta_df_long, mapping = aes(x = n, y = beta, color = word)) + 
  geom_line() +                                        # 棒グラフ
  theme(legend.position = "none") +                    # 凡例
  labs(title = "Mixture of Unigram Models:VBE:(beta)") # タイトル


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
    select(paste0("topic", i), word) %>%  # 
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



