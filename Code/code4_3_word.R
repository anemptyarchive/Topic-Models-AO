
# Chapter4.3 トピックモデルの最尤推定：PLSA：単語 ------------------------------------------------------------


# 利用パッケージ
library(RMeCab)
library(tidyverse)


# テキスト処理 ---------------------------------------------------------------------


# 抽出しない単語を指定
stop_words <- "[a-z]"


# 形態素解析
mecab_df <- docDF("text_data/sanninsen", type = 1) # テキストファイルの保存先を指定する


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
K <- 4   # 任意の値を指定する


# 負担率(q_dnk)の集合
q_dnk <- array(0, dim = c(D, V, max(N_dv), K), 
               dimnames = list(paste0("d=", 1:D),  # 確認用の次元名
                               paste0("v=", 1:V), 
                               paste0("N_dv=", 1:max(N_dv)), 
                               paste0("k=", 1:K)))


## トピック分布(theta_dk)の集合
# 値をランダムに付ける
theta_dk <- sample(seq(0.1, 1, 0.01), D * K, replace = TRUE) %>%  # ランダムな値を生成
            matrix(nrow = D, ncol = K, 
                   dimnames = list(paste0("d=", 1:D),  # 行名
                                   paste0("k=", 1:K))) # 列名

# 初期値の正規化理
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

for(i in 1:30) { # 任意の回数を指定する
  
  # パラメータを初期化
  next_theta_dk <- matrix(0, nrow = D, ncol = K, 
                          dimnames = list(paste0("d=", 1:D),  # 行名
                                          paste0("k=", 1:K))) # 列名
  
  next_phi_kv <- matrix(0, nrow = K, ncol = V, 
                        dimnames = list(paste0("k=", 1:K),  # 行名
                                        paste0("V=", 1:V))) # 列名
  
  for(d in 1:D) { ## (各文書)
    
    for(v in 1:V) { ## (各語彙)
      
      if(N_dv[d, v] > 0) {
        
        for(Ndv in 1:N_dv[d, v]) { ## (語彙の出現回数)
          
          for(k in 1:K) { ## (各トピック)
            
            # 負担率を計算
            tmp_q_numer <- theta_dk[d, k] * (phi_kv[k, v] ^ N_dv[d, v])    # 分子
            tmp_q_denom <- sum(theta_dk[d, ] * (phi_kv[, v] ^ N_dv[d, v])) # 分母
            q_dnk[d, v, Ndv, k] <- tmp_q_numer / tmp_q_denom
            
            # theta_dkを更新
            next_theta_dk[d, k] <- next_theta_dk[d, k] + q_dnk[d, v, Ndv, k]
            
            for(v2 in 1:V) {
              
              if(N_dv[d, v2] > 0) {
                
                for(Ndv2 in 1:N_dv[d, v2]) {
                  
                  # phi_kvを更新
                  next_phi_kv[k, v2] <- next_phi_kv[k, v2] + q_dnk[d, v2, Ndv2, k]
                }
              }
            }
          } ## (/各トピック)
        } ## (/語彙の出現回数)
      }
    } ## (/各語彙)
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



q_dnk

# 推定結果の確認 -----------------------------------------------------------------------

## トピック分布集合：Θ
# dfの整形
theta_df_wide <- cbind(as.data.frame(theta_dk), as.factor(1:D))
colnames(theta_df_wide) <- c(1:K, "doc")
theta_df_long <- gather(theta_df_wide, key = topic, value = p, -doc)

# 描画
ggplot(theta_df_long) + 
  geom_bar(aes(topic, p, fill = topic), 
           stat = "identity", position = "dodge") + 
  facet_wrap( ~ doc, labeller = label_both) + 
  labs(title = "Probabilistic Latent Semantic Analysis")


## 単語分布集合：Φ
# dfの整形
phi_df_wide <- cbind(as.data.frame(t(phi_kv)), as.factor(1:V))
colnames(phi_df_wide) <- c(1:K, "word")
phi_df_long <- gather(phi_df_wide, key = topic, value = p, -word)

# 描画
ggplot(data = phi_df_long, mapping = aes(word, p, fill = word)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap( ~ topic, labeller = label_both) + 
  theme(legend.position = "none") + 
  labs(title = element_text("Probabilistic Latent Semantic Alanysis"))



# 推移の確認 -------------------------------------------------------------------


## トピック分布
# データフレームを作成
theta_df_wide <- cbind(as.data.frame(trace_theta), 
                       1:nrow(trace_theta))

# 
colnames(theta_df_wide) <- c(1:K, "n")
theta_df_long <- gather(theta_df_wide, key = "topic", value = "prob", -n)

# 描画
ggplot(data = theta_df_long, mapping = aes(x = n, y = prob, color = topic)) + 
  geom_line() + 
  labs(title = "theta:PLSA")


## 単語分布
# データフレームを作成
phi_df_wide <- cbind(as.data.frame(trace_phi), 
                     1:nrow(trace_phi))

# 
colnames(phi_df_wide) <- c(1:V, "n")
phi_df_long <- gather(phi_df_wide, key = "word", value = "prob", -n)

# 描画
ggplot(data = phi_df_long, mapping = aes(x = n, y = prob, color = word)) + 
  geom_line(alpha = 0.5) + 
  theme(legend.position = "none") + 
  labs(title = "phi:PLSA")

# 資料作成用チャンク
  scale_fill_manual(values = c("#FFF33F", "#0233CB", "#00A968", "Orange")) + # 色
  scale_color_manual(values = c("#FFF33F", "#0233CB", "#00A968", "Orange")) + # 色
  