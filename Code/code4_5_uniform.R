
# Chapter4.5 LDA:ギブスサンプリング(一様) ------------------------------------------------------------

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
alpha <- 2 # 任意の値を指定する
beta  <- 2 # 任意の値を指定する


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
trace_alpha <- alpha
trace_beta  <- beta
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
            tmp_p_alpha      <- tmp_N_dk[d, k] + alpha
            tmp_p_beta_numer <- tmp_N_kv[k, v] + beta
            tmp_p_beta_denom <- tmp_N_k[k] + beta * V
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
  tmp_alpha_numer1 <- sum(digamma(N_dk + alpha))
  tmp_alpha_numer2 <- D * K * digamma(alpha)
  tmp_alpha_denom1 <- K * sum(digamma(N_d + alpha * K))
  tmp_alpha_denom2 <- D * K * digamma(alpha * K)
  alpha <- alpha * (tmp_alpha_numer1 - tmp_alpha_numer2) / (tmp_alpha_denom1 - tmp_alpha_denom2)
  
  # ハイパーパラメータβの更新
  tmp_beta_numer1 <- sum(digamma(N_kv + beta))
  tmp_beta_numer2 <- K * V * digamma(beta)
  tmp_beta_denom1 <- V * sum(digamma(N_k + beta * V))
  tmp_beta_denom2 <- K * V * digamma(beta * V)
  beta <- beta * (tmp_beta_numer1 - tmp_beta_numer2) / (tmp_beta_denom1 - tmp_beta_denom2)
  
  # 結果の確認用
  trace_alpha <- c(trace_alpha, alpha)
  trace_beta  <- c(trace_beta, beta)
  trace_N_k <- cbind(trace_N_k, as.matrix(N_k))
  
  # 動作確認用
  print(paste0(i, "th try..."))
}


# 推定結果の確認 -----------------------------------------------------------------


# データフレームを作成
ab_df_wide <- data.frame(n = seq_along(trace_alpha),  # 推定回数
                         alpha = trace_alpha, 
                         beta = trace_beta)

# データフレームをlong型に変換
ab_df_long <- gather(ab_df_wide, key = "parameter", value = "value", -n)

# 描画
ggplot(data = ab_df_long, mapping = aes(x = n, y = value, color = parameter)) +  # データ
  geom_line() +                      # 折れ線グラフ
  labs(title = "LDA:Gibbs Sampling") # タイトル



# try ---------------------------------------------------------------------

# トピックごとの単語数の推移
N_k_df_wide <- cbind(as.data.frame(t(trace_N_k)), 
                     1:ncol(trace_N_k))
names(N_k_df_wide) <- c(1:K, "n")
N_k_df_long <- gather(N_k_df_wide, key = "topic", value = "N_k", -n)
ggplot(N_k_df_long, aes(n, N_k, color = topic)) + 
  geom_line()


z_dn[, 20, ]

