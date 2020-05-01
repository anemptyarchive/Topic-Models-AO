
# Chapter4.5 LDA:ギブスサンプリング(多様) ------------------------------------------------------------

library(RMeCab)
library(tidyverse)


# テキスト処理 ---------------------------------------------------------------------

# 形態素解析
mecab_df <- docDF("text_data/lyrics_mor_single", type = 1) # テキストファイルの保存先を指定する


# 文書dの語彙vの出現回数：N_dv
N_dv <- mecab_df %>% 
  filter(grepl("名詞|^動詞|形容詞", POS1)) %>%  # 抽出する品詞を指定する
  filter(grepl("一般|^自立", POS2)) %>% 
  select(-c(TERM, POS1, POS2)) %>% 
  filter(apply(., 1, sum) >= 15) %>%           # 抽出する総出現回数を指定する
  t()

# 確認用の行列名
dimnames(N_dv) <- list(paste("d=", 1:nrow(N_dv), sep = ""), 
                       paste("v=", 1:ncol(N_dv), sep = ""))

# 文書dの単語数：N_d
N_d <- apply(N_dv, 1, sum)


# 文書数：D
D <- nrow(N_dv)

# 総語彙数：V
V <- ncol(N_dv)


# パラメータの初期設定 ----------------------------------------------------------------------

# トピック数：K
K <- 4 # 任意の値を指定する


# ハイパーパラメータ(alpha, beta)
alpha_k <- rep(2, K) # 任意の値を指定する
beta_v  <- rep(2, V) # 任意の値を指定する


# 文書dの語彙vに割り当てられたトピック：z_dnの初期化
z_dn <- array(rep(0, D * V * max(N_dv)), dim = c(D, V, max(N_dv)), 
              dimnames = list(paste0("d=", 1:D), 
                              paste0("v=", 1:V), 
                              paste0("N_dv=", 1:max(N_dv))))

# 文書dにおいてトピックkが割り当てられた単語数：N_dkの初期化
N_dk <- matrix(rep(0, D * K), nrow = D, ncol = K, 
               dimnames = list(paste0("d=", 1:D), 
                               paste0("k=", 1:K)))

# トピックkが割り当てられた語彙vの出現回数：N_kvの初期化
N_kv <- matrix(rep(0, K * V), nrow = K, ncol = V, 
               dimnames = list(paste0("k=", 1:K), 
                               paste0("v=", 1:V)))

# 全文書でトピックkが割り当てられた単語数：N_kの初期化
N_k <- rep(0, K)


# ギブスサンプリング ---------------------------------------------------------------

# 受け皿の用意
p <- NULL

# 結果の確認用
trace_alpha <- as.matrix(alpha_k)
trace_beta  <- as.matrix(beta_v)
trace_N_k <- as.matrix(N_k)

for(i in 1:1000) {
  
  ## 新たに割当られたトピックに関するカウントを初期化
  new_N_dk <- matrix(rep(0, D * K), nrow = D, ncol = K, 
                     dimnames = list(paste0("d=", 1:D), paste0("k=", 1:K)))
  new_N_kv <- matrix(rep(0, K * V), nrow = K, ncol = V, 
                     dimnames = list(paste0("k=", 1:K), paste0("v=", 1:V)))
  new_N_k  <- rep(0, K)
  
  for(d in 1:D) { ## 文書：1,...,D
    
    for(v in 1:V) { ## 各語彙：1,...,V
      
      if(N_dv[d, v] > 0) { ## 出現回数：N_dv > 0のときのみ
        
        for(ndv in 1:N_dv[d, v]) { ## 各語彙の出現回数：1,...,N_dv
          
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
            p[k] <- tmp_p_alpha * tmp_p_beta_numer / tmp_p_beta_denom
          }
          
          # サンプリング
          tmp_z_dn   <- rmultinom(n = 1, size = 1:K, prob = p)
          z_dn[d, v, ndv] <- which(tmp_z_dn == 1)
          
          # 新たに与えられたトピックを`k`に代入
          k <- z_dn[d, v, ndv]
          
          # 文書dの語彙vの分のカウントを加える
          new_N_dk[d, k] <- new_N_dk[d, k] + 1
          new_N_kv[k, v] <- new_N_kv[k, v] + 1
          new_N_k[k]     <- new_N_k[k] + 1
          
        } ## /各語彙の出現回数：1,...,N_dv
      } ## /出現回数：N_dv > 0のときのみ
    } ## /各語彙：1,...,V
  } ## /各文書：1,...,D
  
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
  trace_alpha <- cbind(trace_alpha, as.matrix(alpha_k))
  trace_beta  <- cbind(trace_beta, as.matrix(beta_v))
  trace_N_k   <- cbind(trace_N_k, as.matrix(N_k))
  
  # 動作確認用
  print(paste0(i, "th try..."))
}


# 推定結果の推移の確認 --------------------------------------------------------------


## トピック分布の推移の確認
# データフレームを作成
alpha_df_wide <- cbind(as.data.frame(t(trace_alpha)), 
                       as.factor(1:ncol(trace_alpha)))

# データフレームをlong型に変換
colnames(alpha_df_wide) <- c(1:K, "n") # key用の行名を付与
alpha_df_long <- gather(alpha_df_wide, 
                        key = "topic", value = "alpha", -n) # 変換
alpha_df_long$n <- as.numeric(alpha_df_long$n) # 文字列になるので数値に変換

# 描画
ggplot(alpha_df_long, aes(n, alpha, color = topic)) +  # データ
  geom_line() +                      # 折れ線グラフ
  labs(title = "LDA:Gibbs Sampling") # タイトル


## 単語分布のパラメータの推移の確認
# データフレームを作成
beta_df_wide <- cbind(as.data.frame(t(trace_beta)), 
                      as.factor(1:ncol(trace_beta))) # 推定回数

# データフレームをlong型に変換
colnames(beta_df_wide) <- c(1:V, "n") # key用の行名を付与
beta_df_long <- gather(beta_df_wide, 
                       key = "topic", value = "beta", -n) # 変換
beta_df_long$n <- as.numeric(beta_df_long$n) # 文字列になるので数値に変換

# 描画
ggplot(data = beta_df_long, mapping = aes(x = n, y = beta, color = topic)) +  # データ
  geom_line(alpha = 0.5) +           # 折れ線グラフ
  theme(legend.position = "none") +  # 凡例
  labs(title = "LDA:Gibbs Sampling") # タイトル


# 推定結果の確認 -----------------------------------------------------------------


## トピック分布のパラメータの推定結果の確認
# データフレームを作成
alpha_df <- data.frame(topic = as.factor(1:K), 
                       alpha = alpha_k)

# 描画
ggplot(data = alpha_df, mapping = aes(x = topic, y = alpha, fill = topic)) + # データ
  geom_bar(stat = "identity", position = "dodge") +  # 棒グラフ
  labs(title = "LDA:Gibbs Sampling")                 # タイトル


## 単語分布のパラメータの推定結果の確認
# データフレームを作成
beta_df <- data.frame(word = as.factor(1:V), 
                      beta = beta_v)

# 描画
ggplot(data = beta_df, mapping = aes(x = word, y = beta, fill = word)) +  # データ
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  theme(legend.position = "none") +                 # 凡例
  labs(title = "LDA:Gibbs Sampling")                # タイトル



# パラメータの推定結果(平均値)の確認 ------------------------------------------------------------


## トピック分布の推定結果(平均値)の確認
# データフレームを作成
theta_df <- data.frame(topic = as.factor(1:K), 
                       prob = alpha_k / sum(alpha_k))

# 描画
ggplot(data = theta_df, mapping = aes(x = topic, y = prob, fill = topic)) + # データ
  geom_bar(stat = "identity", position = "dodge") +  # 棒グラフ
  labs(title = "LDA:Gibbs Sampling")                 # タイトル


## 単語分布の推定結果(平均値)の確認
# データフレームを作成
phi_df <- data.frame(word = as.factor(1:V), 
                     prob = beta_v / sum(beta_v))

# 描画
ggplot(data = phi_df, mapping = aes(x = word, y = prob, fill = word)) +  # データ
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  theme(legend.position = "none") +                 # 凡例
  labs(title = "LDA:Gibbs Sampling")                # タイトル


# try ---------------------------------------------------------------------

# トピックごとの単語数の推移
N_k_df_wide <- cbind(as.data.frame(t(trace_N_k)), 
                     1:ncol(trace_N_k))
colnames(N_k_df_wide) <- c(1:K, "n")
N_k_df_long <- gather(N_k_df_wide, key = "topic", value = "N_k", -n)
ggplot(N_k_df_long, aes(n, N_k, color = topic)) + 
  geom_line()

