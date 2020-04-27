
# Chapter3.3 混合ユニグラムモデル：最尤推定 ----------------------------------------------------------------------


# 利用パッケージ
library(RMeCab)
library(tidyverse)


# テキスト処理 ------------------------------------------------------------------


# テキストファイルの保存先を指定する
file_path <- "text"

# 抽出しない単語を指定
#stop_words <- "[a-z]"


# 形態素解析
mecab_df <- docDF(file_path, type = 1)


# 文書dの語彙vの出現回数(N_dv)の集合
N_dv <- mecab_df %>% 
  filter(grepl("名詞|形容詞|形容動詞|^動詞|副詞|連体詞", POS1)) %>% # 抽出する品詞(大分類)を指定する
  filter(grepl("一般|^自立", POS2)) %>%        # 抽出する品詞(細分類)を指定する
#        filter(!grepl(stop_words, TERM)) %>%         # ストップワードを除く
        select(-c(TERM, POS1, POS2)) %>%             # 数値列のみを残す
        filter(apply(., 1, sum) >= 10) %>%            # 抽出する総出現回数を指定する
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


# パラメータの初期設定 --------------------------------------------------------------


# トピック数(K)
K <- 10   # 任意の値を指定する


# 負担率(q_dk)の集合の受け皿
q_dk <- matrix(0, nrow = D, ncol = K, 
               dimnames = list(paste0("d=", 1:D),  # 行名
                               paste0("k=", 1:K))) # 列名


## トピック分布(theta)
# 初期値をランダムに設定
theta_k <- sample(seq(0.1, 10, 0.1), size = K, replace = TRUE)

# 正規化：sum_{k=1}^K theta_k = 1
theta_k <- theta_k / sum(theta_k)
names(theta_k) <- paste0("k=", 1:K) # 確認用の要素名


## 単語分布(phi)
# 初期値をランダムに設定
phi_kv <- sample(seq(0.1, 10, 0.1), size = K * V, replace = TRUE) %>% 
          matrix(nrow = K, ncol = V, 
                 dimnames = list(paste0("k=", 1:K),  # 行名
                                 paste0("v=", 1:V))) # 列名

# 正規化：sum_{v=1}^V phi_{kv} = 1
for(k in 1:K) {
  phi_kv[k, ] <- phi_kv[k, ] / sum(phi_kv[k, ])
}


# 最尤推定 --------------------------------------------------------------------


# 推移の確認用
trace_theta <- as.matrix(theta_k)
trace_phi   <- as.matrix(phi_kv[1, ])

for(i in 1:50){
  
  # 次ステップのパラメータを初期化
  new_theta_k <- rep(0, K)
  names(new_theta_k) <- paste0("k=", 1:K)
  
  new_phi_kv <- matrix(0, nrow = K, ncol = V, 
                       dimnames = list(paste0("k=", 1:K), 
                                       paste0("v=", 1:V)))
  
  for(d in 1:D){ ## (各文書：1,...,D)
    
    for(k in 1:K){ ## (各トピック：1,...,K)
      
      # 負担率の計算
      tmp_phi_k <- t(phi_kv) ^ N_dv[d, ] %>% 
                   apply(2, prod)
      q_dk[d, k] <- theta_k[k] * tmp_phi_k[k] / sum(theta_k * tmp_phi_k)
      
      # トピック分布(theta_k)を更新
      new_theta_k[k] <- new_theta_k[k] + q_dk[d, k]
      
      for(v in 1:V){ ## (各語彙：1,...,V)
        
        # 単語分布(phi_kv)を更新
        new_phi_kv[k, v] <- new_phi_kv[k, v] + q_dk[d, k] * N_dv[d, v]
        
      } ## (/各語彙：1,...,V)
    } ## (/各トピック：1,...,K)
  } ## (/各文書：1,...,D)
  
  # パラメータの更新
  theta_k <- new_theta_k
  phi_kv  <- new_phi_kv
  
  # パラメータの正規化
  theta_k <- theta_k / sum(theta_k)
  for(k2 in 1:K) {
    phi_kv[k2, ] <- phi_kv[k2, ] / sum(phi_kv[k2, ])
  }
  
  # 推移の確認用
  trace_theta <- cbind(trace_theta, as.matrix(theta_k))
  trace_phi   <- cbind(trace_phi, as.matrix(phi_kv[1, ]))
  
  print(paste0(i, "th try..."))
}


# 推定結果の確認 -----------------------------------------------------------------


## トピック分布の推定結果を確認
# データフレームを作成
theta_df <- data.frame(topic = as.factor(1:K), 
                       prob = theta_k)

# 描画
ggplot(data = theta_df, mapping = aes(x = topic, y = prob, fill = topic)) + # データ
  geom_bar(stat = "identity", position = "dodge") +  # 棒グラフ
  labs(title = "Mixture of Unigram Models:MLE")      # タイトル


## 単語分布の推定結果を確認
# データフレームを作成
phi_df_wide <- cbind(as.data.frame(phi_kv), 
                     as.factor(1:K))

# データフレームをlong型に変換
colnames(phi_df_wide) <- c(1:V, "topic") # key用の行名を付与
phi_df_long <- gather(phi_df_wide, key = "word", value = "prob", -topic) # 変換
phi_df_long$word <- phi_df_long$word %>%  # 文字列になるため因子に変換
                    as.numeric() %>% 
                    as.factor()

# 描画
ggplot(data = phi_df_long, mapping = aes(x = word,  y = prob, fill = word)) + # データ
  geom_bar(stat = "identity", position = "dodge") +  # 棒グラフ
  facet_wrap(~ topic, labeller = label_both) +       # グラフの分割
  theme(legend.position = "none") +                  # 凡例
  labs(title = "Mixture of Unigram Models:MLE")      # タイトル



# 推移の確認 -------------------------------------------------------------------

## トピック分布の推定値の推移を確認
# データフレームを作成
trace_theta_df_wide <- cbind(t(trace_theta), 
                             1:ncol(trace_theta)) %>% # 推定回数
                       as.data.frame()

# データフレームをlong型に変換
colnames(trace_theta_df_wide) <- c(1:K, "n") # key用の行名を付与
trace_theta_df_long <- gather(trace_theta_df_wide, key = "topic", value = "prob", -n) # 変換

# 描画
ggplot(data = trace_theta_df_long, mapping = aes(x = n, y = prob, color = topic)) + 
  geom_line() +                                         # 折れ線グラフ
  labs(title = "Mixture of Unigram Models:MLE:(theta)") # タイトル


## 単語分布の推定値の推移を確認
# データフレームを作成
trace_phi_df_wide <- cbind(t(trace_phi), 
                           1:ncol(trace_phi)) %>% 
                     as.data.frame()

# データフレームをlong型に変換
colnames(trace_phi_df_wide) <- c(1:V, "n")            # key用の行名を付与
trace_phi_df_long <- gather(trace_phi_df_wide, key = "word", value = "prob", -n) # 変換
trace_phi_df_long$word <- trace_phi_df_long$word %>%  # 文字列になるため因子に変換
  as.numeric() %>% 
  as.factor()

# 描画
ggplot(data = trace_phi_df_long, mapping = aes(x = n, y = prob, color = word)) + 
  geom_line() +                                       # 折れ線グラフ
  theme(legend.position = "none") +                   # 凡例
  labs(title = "Mixture of Unigram Models:MLE:(phi)") # タイトル


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


