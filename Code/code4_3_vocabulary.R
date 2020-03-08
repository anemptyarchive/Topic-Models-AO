
# Chapter4.3 トピックモデルの最尤推定：PLSA ------------------------------------------------------------


# 利用パッケージ
library(RMeCab)
library(tidyverse)


# テキスト処理 ---------------------------------------------------------------------


# 抽出しない単語を指定
stop_words <- "[a-z]"


# 形態素解析
mecab_df <- docDF("text_data/lyrics/juicejuice", type = 1) # テキストファイルの保存先を指定する


# 文書dの語彙vの出現回数(N_dv)の集合
N_dv <- mecab_df %>% 
        filter(grepl("名詞|形容詞", POS1)) %>% # 抽出する品詞(大分類)を指定する|^動詞
        filter(grepl("一般|^自立", POS2)) %>%        # 抽出する品詞(細分類)を指定する
        filter(!grepl(stop_words, TERM)) %>%         # ストップワードを除く
        select(-c(TERM, POS1, POS2)) %>%             # 数値列のみを残す
        filter(apply(., 1, sum) >= 5) %>%            # 抽出する総出現回数を指定する
        t()                                          # 転置

# 確認用の行列名
dimnames(N_dv) <- list(paste0("d=", 1:nrow(N_dv)), # 行名
                       paste0("v=", 1:ncol(N_dv))) # 列名

# 文書dの単語数(N_d)のベクトル
N_d <- apply(N_dv, 1, sum) # 行方向に和をとる

# 文書数(D)
D <- nrow(N_dv)

# 総語彙数(V)
V <- ncol(N_dv)


# パラメータの初期設定 ----------------------------------------------------------------------


# トピック数(K)
K <- 10   # 任意の値を指定する


# 負担率(q_dvk)の集合
q_dvk <- array(0, dim = c(D, V, K), 
               dimnames = list(paste0("d=", 1:D),  # 確認用の次元名
                               paste0("v=", 1:V), 
                               paste0("k=", 1:K)))


## トピック分布(theta_dk)の集合
# 値をランダムに付ける
theta_dk <- sample(seq(0.1, 1, 0.01), D * K, replace = TRUE) %>%  # ランダムな値を生成
            matrix(nrow = D, ncol = K, 
                   dimnames = list(paste0("d=", 1:D),  # 行名
                                   paste0("k=", 1:K))) # 列名

# 初期値の正規化
for(d in 1:D) {
  theta_dk[d, ] <- theta_dk[d, ] / sum(theta_dk[d, ])
}


## 単語分布(phi_kv)の集合
# 値をランダムに付ける
phi_kv <- sample(seq(0, 1, 0.01), K * V, replace = TRUE) %>%  # ランダムな値を生成
          matrix(nrow = K, ncol = V, 
                 dimnames = list(paste0("k=", 1:K),  # 行名
                                 paste0("V=", 1:V))) # 列名

# 初期値の正規化
for(k in 1:K) {
  phi_kv[k, ] <- phi_kv[k, ] / sum(phi_kv[k, ])
}


# 最尤推定 -----------------------------------------------------------------------


# 推移の確認用
trace_theta <- theta_dk[1, ]
trace_phi   <- phi_kv[1, ]

for(i in 1:5) { # 任意の回数を指定する
  
  # パラメータを初期化
  next_theta_dk <- matrix(0, nrow = D, ncol = K, 
                          dimnames = list(paste0("d=", 1:D),  # 行名
                                          paste0("k=", 1:K))) # 列名
  
  next_phi_kv <- matrix(0, nrow = K, ncol = V, 
                        dimnames = list(paste0("k=", 1:K),  # 行名
                                        paste0("V=", 1:V))) # 列名
  
  for(d in 1:D) { ## (各文書)
    
    for(v in 1:V) { ## (各語彙1)
      
      for(k in 1:K) { ## (各トピック)
        
        # 負担率を計算
        tmp_q_numer <- theta_dk[d, k] * (phi_kv[k, v] ^ N_dv[d, v])    # 分子
        tmp_q_denom <- sum(theta_dk[d, ] * (phi_kv[, v] ^ N_dv[d, v])) # 分母
        q_dvk[d, v, k] <- tmp_q_numer / tmp_q_denom
        
        # theta_dkを更新
        next_theta_dk[d, k] <- next_theta_dk[d, k] + q_dvk[d, v, k] * N_dv[d, v]
        
        for(v2 in 1:V) { ## (各語彙2)
          
          # phi_kvを更新
          next_phi_kv[k, v2] <- next_phi_kv[k, v2] + q_dvk[d, v2, k] * N_dv[d, v2]
          
        } ## (/各語彙2)
      } ## (/各トピック)
    } ## (/各語彙1)
  } ## (/各文書)
  
  # パラメータを更新
  theta_dk <- next_theta_dk
  phi_kv   <- next_phi_kv
  
  
  # パラメータを正規化
  for(d in 1:D) {
    theta_dk[d, ] <- theta_dk[d, ] / sum(theta_dk[d, ])
  }
  
  for(k in 1:K) {
    phi_kv[k, ] <- phi_kv[k, ] / sum(phi_kv[k, ])
  }
  
  # 推移の確認用
  trace_theta <- rbind(trace_theta, theta_dk[1, ])
  trace_phi   <- rbind(trace_phi, phi_kv[1, ])
  
  # 動作確認用
  print(paste0(i, "th try..."))
}




# 推定結果の確認 -----------------------------------------------------------------------


## トピック分布
# データフレームを作成
theta_df_wide <- cbind(as.data.frame(theta_dk), 
                       as.factor(1:D)) # 文書番号

# データフレームをlong型に変換
colnames(theta_df_wide) <- c(1:K, "doc") # key用の行名を付与
theta_df_long <- gather(theta_df_wide, key = "topic", value = "prob", -doc) # 変換

# 描画
ggplot(data = theta_df_long, mapping = aes(x = topic, y = prob, fill = topic)) + # データ
  geom_bar(stat = "identity", position = "dodge") +      # 棒グラフ
  facet_wrap( ~ doc, labeller = label_both) +            # グラフの分割
  labs(title = "Probabilistic Latent Semantic Analysis") # タイトル


## 単語分布
# データフレームを作成
phi_df_wide <- cbind(as.data.frame(t(phi_kv)), 
                     as.factor(1:V)) # 語彙番号

# データフレームをlong型に変換
colnames(phi_df_wide) <- c(1:K, "word") # key用の行名を付与
phi_df_long <- gather(phi_df_wide, key = "topic", value = "prob", -word) # 変換

# 描画
ggplot(data = phi_df_long, mapping = aes(x = word, y = prob, fill = word)) + # データ
  geom_bar(stat = "identity", position = "dodge") +      # 棒グラフ
  facet_wrap( ~ topic, labeller = label_both) +          # グラフの分割
  theme(legend.position = "none") +                      # 凡例
  labs(title = "Probabilistic Latent Semantic Alanysis") # タイトル


## 単語分布
# データフレームをlong型に変換
phi_df_wide <- cbind(as.data.frame(t(phi_kv)), 
                     1:V)
colnames(phi_df_wide) <- c(1:K, "word")
phi_df_long <- gather(phi_df_wide, key = topic, value = prob, -word)

# 描画
phi_df_long %>% 
  as.numeric()
  filter(word >= 1 & word <= 100) %>% 
  ggplot(mapping = aes(x = topic, y = prob, fill = topic)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    facet_wrap( ~ word, labeller = label_both) + 
    theme(legend.position = "none") + 
    labs(title = "Probabilistic Latent Semantic Alanysis")


# 推移の確認 -------------------------------------------------------------------


## トピック分布
# データフレームを作成
theta_df_wide <- cbind(as.data.frame(trace_theta), 
                       1:nrow(trace_theta)) # 試行回数

# データフレームをlong型に変換
colnames(theta_df_wide) <- c(1:K, "n") # key用の行名を付与
theta_df_long <- gather(theta_df_wide, key = "topic", value = "prob", -n) # 変換

# 描画
ggplot(data = theta_df_long, mapping = aes(x = n, y = prob, color = topic)) + # データ
  geom_line() +              # 折れ線グラフ
  labs(title = "theta:PLSA") # タイトル


## 単語分布
# データフレームを作成
phi_df_wide <- cbind(as.data.frame(trace_phi), 
                     1:nrow(trace_phi)) # 試行回数

# データフレームをlong型に変換
colnames(phi_df_wide) <- c(1:V, "n") # key用の行名を付与
phi_df_long <- gather(phi_df_wide, key = "word", value = "prob", -n) # 変換

# 描画
ggplot(data = phi_df_long, mapping = aes(x = n, y = prob, color = word)) + # データ
  geom_line(alpha = 0.5) +          # 折れ線グラフ
  theme(legend.position = "none") + # 凡例
  labs(title = "phi:PLSA")          # タイトル


# try ---------------------------------------------------------------------


## 語彙インデックス:v_index
# 指定した出現回数以上の単語の行番号
num <- mecab_df %>% 
  select(-c(TERM, POS1, POS2)) %>% 
  apply(1, sum) >= 10 # 抽出する総出現回数を指定する

v_index <- mecab_df[num, ] %>% 
  filter(grepl("名詞|^動詞|形容詞", POS1)) %>%  # 抽出する品詞を指定する
  filter(grepl("一般|^自立", POS2)) %>% 
  .[, 1]  # 単語の列を抽出する

phi_df_wide2 <- phi_df_wide
phi_df_wide2$word <- v_index
colnames(phi_df_wide2) <- c(paste0("topic", 1:K), "word")


phi_df_long2 <- data.frame()
for(i in 1:K) {
  tmp_df <- phi_df_wide2 %>% 
    select(paste0("topic", i), word) %>% 
    arrange(-.[, 1]) %>% 
    head(15) %>% 
    gather(key = "topic", value = "prob", -word)
  
  phi_df_long2 <- rbind(phi_df_long2, tmp_df)
}


# 描画
ggplot(phi_df_long2, mapping = aes(x = reorder(x = word, X = prob), y = prob, fill = word)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  facet_wrap( ~ topic, scales = "free") + 
  theme(legend.position = "none")



# 文書dの語彙vの出現回数：N_dv
num <- mecab_df %>% 
  select(-c(TERM, POS1, POS2)) %>% 
  apply(., 1, sum) >= 10



q_df_wide <- cbind(as.data.frame(q_dvk[1, 1:50, ]), 
                   as.factor(1:50))
colnames(q_df_wide) <- c(1:K, "word")
q_df_long <- gather(q_df_wide, key = "topic", value = "value", -word)

ggplot(data = q_df_long, mapping = aes(x = topic, y = value, fill = topic)) + 
  geom_bar(stat = "identity") + 
  facet_wrap( ~ word) + 
  theme(legend.position = "none") + 
  labs(title = "PLSA")
