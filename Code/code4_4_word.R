
# Chapter4.4 トピックモデルの変分ベイズ推定：LDA(単語単位) ------------------------------------------------------------


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


# トピックの変分事後分布：q_dvk
q_dvk <- array(data = 0, dim = c(D, V, max(N_dv), K), 
               dimnames = list(paste0("d=", 1:D),  # 確認用の次元名
                               paste0("v=", 1:V), 
                               paste0("N_dv=", 1:max(N_dv)), 
                               paste0("k=", 1:K)))


# 事前分布のパラメータ：α,β
alpha <- 2 # 任意の値を指定する
beta  <- 2 # 任意の値を指定する


# トピック分布の変分事後分布のパラメータ：α_dk
alpha_dk <- seq(1, 3, 0.01) %>%                # 任意の範囲を指定する
            sample(size = D * K, replace = TRUE) %>%  # 値をランダムに割り振る
            matrix(nrow = D, ncol = K,         # 次元の設定
                   dimnames = list(paste0("d=", 1:D),  # 行名
                                   paste0("k=", 1:K))) # 列名


# 単語分布の変分事後分布のパラメータ：β_kv
beta_kv <- seq(1, 3, 0.01) %>%                # 任意の範囲を指定する
           sample(size = K * V, replace = TRUE) %>%  # 値をランダムに割り振る
           matrix(nrow = K, ncol = V,         # 次元の設定
                  dimnames = list(paste0("k=", 1:K),  # 行名
                                  paste0("v=", 1:V))) # 列名


# 変分ベイズ推定 -----------------------------------------------------------------

for(i in 1:30) {
  
  # パラメータを初期化
  new_alpha_dk <- matrix(data = alpha, nrow = D, ncol = K, 
                         dimnames = list(paste0("d=", 1:D), 
                                         paste0("k=", 1:K)))
  
  new_beta_kv <- matrix(data = beta, nrow = K, ncol = V, 
                        dimnames = list(paste0("k=", 1:K), 
                                        paste0("v=", 1:V)))
  
  for(d in 1:D) { # (文書1,...,D)
    
    for(v in 1:V) { # (語彙1,...,V)
      
      if(N_dv[d, v] > 0) {
        
        for(n in N_dv[d, v]) { # (出現回数1,...,N_dv)
          
         for(k in 1:K) { # (トピック1,...,K)
            
            # 負担率を計算
            tmp_q_alpha <- digamma(alpha_dk[d, k]) - digamma(sum(alpha_dk[d, ]))
            tmp_q_beta  <- digamma(beta_kv[k, v]) - digamma(sum(beta_kv[k, ]))
            q_dvk[d, v, n, k] <- exp(tmp_q_alpha + tmp_q_beta)
          }
          
          # 負担率を正規化
          q_dvk[d, v, n, ] <- q_dvk[d, v, n, ] / sum(q_dvk[d, v, n, ])
          
          for(k in 1:K) { # (トピック1,...,K)(続き)
            
            # α_dkを更新
            new_alpha_dk[d, k] <- new_alpha_dk[d, k] + q_dvk[d, v, n, k]
            
            # β_kvを更新
            new_beta_kv[k, v] <- new_beta_kv[k, v] + q_dvk[d, v, n, k]
            
          } # (/トピック1,...,K)
        } # (/出現回数1,...,N_dv)
      }
    } # (/語彙1,...,V)
  } # (/文書1,...,D)
  
  # パラメータを更新
  alpha_dk <- new_alpha_dk
  beta_kv  <- new_beta_kv
  
  # 動作確認用
  print(paste0(i, "th try..."))
}


# 推定結果の確認 -----------------------------------------------------------------


# トピック分布のパラメータ ------------------------------------------------------------

# dfの整形
alpha_df_wide <- cbind(as.data.frame(alpha_dk), as.factor(1:D))
colnames(alpha_df_wide) <- c(1:K, "doc")
alpha_df_long <- gather(alpha_df_wide, key = "topic", value = "alpha", -doc)
alpha_df_long$topic <- alpha_df_long$topic %>% 
  as.numeric() %>% 
  as.factor()

# 作図
ggplot(alpha_df_long, aes(topic, alpha, fill = topic)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~ doc, labeller = label_both) + 
  labs(title = "LDA")



# トピック分布(期待値) -------------------------------------------------------------

# パラメータαからΦの期待値を算出
theta_df_wide <- as.data.frame(alpha_dk)
for(d in 1:D) {
  theta_df_wide[d, ] <- theta_df_wide[d, ] / sum(theta_df_wide[d, ])
}
# 文書インデックス(v)の列を加える
theta_df_wide$doc <- as.factor(1:D)
# 行名を変更
colnames(theta_df_wide) <- c(1:K, "doc")

# データフレームをlong型に変換
theta_df_long <- gather(theta_df_wide, key = "topic", value = "prob", -doc)
theta_df_long$topic <- theta_df_long$topic %>% 
  as.numeric() %>% 
  as.factor()

# 作図
ggplot(data = theta_df_long, mapping = aes(x = topic, y = prob, fill = topic)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~ doc, labeller = label_both) + 
  labs(title = "LDA")




# トピック分布(最頻度) -------------------------------------------------------------

# パラメータαからΦの期待値を算出
theta_df_wide <- as.data.frame(alpha_dk)
for(d in 1:D) {
  theta_df_wide[d, ] <- (theta_df_wide[d, ] - 1) / (sum(theta_df_wide[d, ]) - V)
}
# 文書インデックス(v)の列を加える
theta_df_wide$doc <- as.factor(1:D)

# 行名を変更
colnames(theta_df_wide) <- c(1:K, "doc")

# データフレームをlong型に変換
theta_df_long <- gather(theta_df_wide, key = "topic", value = "prob", -doc)
theta_df_long$topic <- theta_df_long$topic %>% 
  as.numeric() %>% 
  as.factor()

# 作図
ggplot(data = theta_df_long, mapping = aes(x = topic, y = prob, fill = topic)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~ doc, labeller = label_both) + 
  labs(title = "LDA")



# 単語分布のパラメータ --------------------------------------------------------------

# dfの整形
beta_df_wide <- cbind(as.data.frame(beta_kv), as.factor(1:K))

# データフレームをlong型に変換
colnames(beta_df_wide) <- c(1:V, "topic")
beta_df_long <- gather(beta_df_wide, key = "word", value = "beta", -topic)
beta_df_long$word <- beta_df_long$word %>% 
  as.numeric() %>% 
  as.factor()

# 作図
ggplot(beta_df_long, aes(word, beta, fill = word)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~ topic, labeller = label_both) + 
  theme(legend.position = "none") + 
  labs(title = "LDA")


# try ---------------------------------------------------------------------

library(MCMCpack)
N_d
n <- 10
plsa_theta <- theta_df_wide[n, 1:K]
lda_theta <- rdirichlet(10000, as.vector(as.matrix(alpha_df_wide[n, 1:K]))) %>% 
  as.data.frame()
colnames(lda_theta) <- 1:K

lda_theta_long <- gather(lda_theta, key = "topic", value = "theta")
lda_theta_long$topic <- lda_theta_long$topic %>% 
  as.numeric() %>% 
  as.factor()


ggplot(lda_theta_long, aes(theta, y = ..count.., fill = topic)) + 
  geom_histogram(binwidth = 0.01, alpha = 1) + 
  geom_line(aes(color = topic), stat = "count")

ggplot(lda_theta_long, aes(theta)) + 
  geom_histogram(aes(y = ..density.., fill = topic), 
                 position = "dodge", binwidth = 0.01, alpha = 0.5) + 
  geom_line(aes(color = topic), stat = "density")


ggplot(alpha_df_long, aes(alpha, y = ..density.., fill = topic)) + 
  geom_histogram(binwidth = 1, alpha = 0.6) + 
  geom_line(aes(color = topic), stat = "density")
