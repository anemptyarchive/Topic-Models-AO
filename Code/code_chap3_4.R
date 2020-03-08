
# 混合ユニグラムモデル：変分ベイズ推定 ----------------------------------------------------------------------


# 利用パッケージ
library(RMeCab)
library(tidyverse)


# テキスト処理 ------------------------------------------------------------------


# テキストファイルの保存先を指定する
file_path <- "text_data/lyrics/juicejuice"


# 抽出しない単語を指定
stop_words <- "[a-z]"


# 形態素解析
mecab_df <- docDF(file_path, type = 1)


# 文書dの語彙vの出現回数(N_dv)の集合
N_dv <- mecab_df %>% 
        filter(grepl("名詞|形容詞", POS1)) %>% # 抽出する品詞(大分類)を指定する|^動詞
        filter(grepl("一般|^自立", POS2)) %>%  # 抽出する品詞(細分類)を指定する
        filter(!grepl(stop_words, TERM)) %>%   # ストップワードを除く
        select(-c(TERM, POS1, POS2)) %>%       # 数値列のみを残す
        filter(apply(., 1, sum) >= 5) %>%      # 抽出する総出現回数を指定する
        t()                                          # 転置

# 確認用の行列名
dimnames(N_dv) <- list(paste0("d=", 1:nrow(N_dv)), # 行名
                       paste0("v=", 1:ncol(N_dv))) # 列名

# 文書dの単語数(N_d)のベクトル
N_d <- apply(N_dv, 1, sum)

# 文書数(D)
D <- nrow(N_dv)

# 総語彙数(V)
V <- ncol(N_dv)


# パラメータの初期設定 --------------------------------------------------------------


# トピック数(K)
K <- 4 # 任意の値を指定する


# 負担率(q_dk)の集合
q_dk <- matrix(0, nrow = D, ncol = K, 
               dimnames = list(paste0("d=", 1:D),  # 行名
                               paste0("k=", 1:K))) # 列名


# 事前分布のパラメータ(α, β)
alpha <- 2 # 任意の値を指定する
beta  <- 2 # 任意の値を指定する


# 変分事後分布のパラメータ(α_k)のベクトル
alpha_k <- seq(1, 3, 0.1) %>%               # 任意の範囲を指定する
           sample(size = K, replace = TRUE) # 値をランダムに割り振る
names(alpha_k) <- paste0("k=", 1:K)         # 確認用の要素名

# 変分事後分布のパラメータ(β_kv)の集合
beta_kv <- seq(1, 3, 0.1) %>%                        # 任意の範囲を指定する
           sample(size = K * V, replace = TRUE) %>%  # 値をランダムに割り振る
           matrix(nrow = K, ncol = V, 
                  dimnames = list(paste0("k=", 1:K),  # 行名
                                  paste0("v=", 1:V))) # 列名


# 変分ベイズ推定 -----------------------------------------------------------------


# 推移の確認用
trace_alpha <- as.matrix(alpha_k)
trace_beta  <- beta_kv[1, ]

for(i in 1:50) { # 任意の回数を指定する
  
  # 次ステップのハイパーパラメータ(alpha, beta)を初期化
  new_alpha_k <- rep(alpha, K)
  new_beta_kv <- matrix(beta, nrow = K, ncol = V, 
                        dimnames = list(paste0("k=", 1:K), 
                                        paste0("v=", 1:V)))
  
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
        
      } ## (/各語彙：1,...,V)
    } ## (/各トピック：1,...,K)
  } ## (/各文書：1,...,D)
  
  # ハイパーパラメータの(alpha, beta)を更新
  alpha_k <- new_alpha_k
  beta_kv <- new_beta_kv
  
  # 推移の確認用
  trace_alpha <- cbind(trace_alpha, alpha_k)
  trace_beta  <- cbind(trace_beta, beta_kv[1, ])
}


# ハイパーパラメータの推定結果の確認 -----------------------------------------------------------------

## ハイパーパラメータ(α)の推定結果を確認
# データフレームを作成
alpha_df <- data.frame(topic = as.factor(1:K), 
                       alpha = alpha_k)

# 作図
ggplot(data = alpha_df, mapping = aes(x = topic, y = alpha, fill = topic)) + # データ
  geom_bar(stat = "identity", position = "dodge") +  # 棒グラフ
  labs(title = "Mixture of Unigram Models:VBE")      # タイトル


## ハイパーパラメータ(β)の推定結果を確認
# データフレームを作成
beta_df_wide <- cbind(as.data.frame(beta_kv), as.factor(1:K))
colnames(beta_df_wide) <- c(1:V, "topic")
beta_df_long <- gather(beta_df_wide, key = "term", value = "beta", -topic)
beta_df_long$term <- as.factor(as.numeric(beta_df_long$term))

# 作図
ggplot(data = beta_df_long, mapping = aes(x = term, y = beta, fill = term)) + # データ
  geom_bar(stat = "identity", position = "dodge") +  # 棒グラフ
  facet_wrap(~topic, labeller = label_both) +        # グラフの分割
  theme(legend.position = "none") +                  # 凡例
  labs(title = "Mixture of Unigram Models:VBE")      # タイトル


# パラメータの推定結果の確認 -----------------------------------------------------------

## トピック分布の推定結果(平均値)を確認
# パラメータの平均値を算出
theta_k  <- alpha_k / sum(alpha_k) # 平均値を計算

# データフレームを作成
theta_df <- data.frame(topic = as.factor(1:K), 
                       prob = theta_k)

# 描画
ggplot(data = theta_df, mapping = aes(x = topic, y = prob, fill = topic)) + # データ
  geom_bar(stat = "identity", position = "dodge") +  # 棒グラフ
  labs(title = "Mixture of Unigram Models:VBE")      # タイトル


## 単語分布の推定結果(平均値)を確認
# パラメータの平均値を算出
phi_kv <- matrix(nrow = K, ncol = V)
for(k in 1:K) {
  phi_kv[k, ] <- beta_kv[k, ] / sum(beta_kv[k, ])
}

# データフレームを作成
phi_df_wide <- cbind(as.data.frame(phi_kv), 
                     as.factor(1:K))

# データフレームをlong型に変換
colnames(phi_df_wide) <- c(1:V, "topic") # key用の行名を付与
phi_df_long <- gather(phi_df_wide, key = "word", value = "prob", -topic) # 変換
phi_df_long$term <- phi_df_long$word %>% 
                    as.numeric() %>% 
                    as.factor()

# 描画
ggplot(data = phi_df_long, mapping = aes(x = word,  y = prob, fill = word)) + # データ
  geom_bar(stat = "identity", position = "dodge") +  # 棒グラフ
  facet_wrap(~ topic, labeller = label_both) +       # グラフの分割
  theme(legend.position = "none") +                  # 凡例
  labs(title = "Mixture of Unigram Models:VBE")      # タイトル


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



