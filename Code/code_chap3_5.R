
# 混合ユニグラムモデル：ギブスサンプリング ----------------------------------------------------

# 利用パッケージ
library(RMeCab)
library(tidyverse)


# テキスト処理 ---------------------------------------------------------------------

# テキストファイルの保存先を指定する
file_path <- "text_data/lyrics/juicejuice"

# 抽出しない単語を指定
stop_words <- "[a-z]"

# 形態素解析
mecab_df <- docDF(file_path, type = 1)


# 文書dの語彙vの出現回数(N_dv)の集合
N_dv <- mecab_df %>% 
        filter(grepl("名詞|^動詞|形容詞", POS1)) %>%  # 抽出する品詞(大分類)を指定する
        filter(grepl("一般|^自立", POS2)) %>%         # 抽出する品詞(細分類)を指定する
        filter(!grepl(stop_words, TERM)) %>%          # ストップワードを除く
        select(-c(TERM, POS1, POS2)) %>%              # 数値列のみを残す
        filter(apply(., 1, sum) >= 5) %>%             # 抽出する総出現回数を指定する
        t()                                           # 転置する

# 確認用の行列名
dimnames(N_dv) <- list(paste("d=", 1:nrow(N_dv), sep = ""), # 行名 
                       paste("v=", 1:ncol(N_dv), sep = "")) # 列名

# 文書dの単語数(N_d)のベクトル
N_d <- apply(N_dv, 1, sum)


# 文書数(D)
D <- nrow(N_dv)

# 総語彙数(V)
V <- ncol(N_dv)


# パラメータの初期設定 ----------------------------------------------------------------------

# トピック数(K)
K <- 3 # 任意の値を指定する

# ハイパーパラメータ(alpha, beta)
alpha <- 2 # 任意の値を指定する
beta  <- 2 # 任意の値を指定する

# トピックkが割り当てられた文書数(D_{z_d})のベクトル
D_k <- rep(0, K)

# トピックkが割り当てられた文書の語彙vの出現回数(N_{z_d,v})の集合
N_kv <- matrix(0, nrow = K, ncol = V, 
               dimnames = list(paste0("k=", 1:K), 
                               paste0("v=", 1:V)))

# トピックkが割り当てられた文書の単語数(N_{z_d})のベクトル
N_k <- rep(0, K)

# 文書dのトピック(z_d)のベクトル
z_d <- rep(0, D)


# ギブスサンプリング ---------------------------------------------------------------

# サンプル確率(p)
p <- NULL

# 結果の確認用
trace_alpha <- alpha
trace_beta  <- beta
trace_D_k <- as.matrix(D_k)
trace_N_k <- as.matrix(N_k)

for(i in 1:3000) { # 任意の回数を指定する
  
  # 新たに割り当られてるトピックに関するカウントを初期化
  new_D_k <- rep(0, K)
  new_N_kv <- matrix(0, nrow = K, ncol = V, 
                     dimnames = list(paste0("k=", 1:K), 
                                     paste0("v=", 1:V)))
  new_N_k <- rep(0, K)
  
  for(d in 1:D) { ## (各文書：1,...,D)
    
    # 現ステップの計算のために前ステップのカウントを移す
    tmp_D_k  <- D_k
    tmp_N_kv <- N_kv
    tmp_N_k  <- N_k
    
    if(z_d[d] > 0) { # 初回を飛ばす処理
      
      # 前ステップで文書dが与えられたトピックを`k`に代入
      k <- z_d[d]
      
      # 文書dに関する値をカウントから除く
      tmp_D_k[k]    <- D_k[k] - 1
      tmp_N_kv[k, ] <- N_kv[k, ] - N_dv[d, ]
      tmp_N_k[k]    <- N_k[k] - N_d[d]
    }
    
    for(k in 1:K) { ## (各トピック：1,...,K)
      
      # サンプリング確率の計算
      tmp_p_alpha <- log(tmp_D_k[k] + alpha) # 第1因子
      tmp_p_beta1 <- lgamma(tmp_N_k[k] + beta * V) - lgamma(tmp_N_k[k] + N_d[d] + beta * V)  # 第2因子
      tmp_p_beta2 <- lgamma(tmp_N_kv[k, ] + N_dv[d, ] + beta) - lgamma(tmp_N_kv[k, ] + beta) # 第3因子
      p[k] <- exp(tmp_p_alpha + tmp_p_beta1 + sum(tmp_p_beta2))
      
    } ## (/各トピック：1,...,K)
    
    # サンプリング
    tmp_z_d <- rmultinom(n = 1, size = 1, prob = p) # カテゴリ分布によりトピックを生成
    z_d[d]  <- which(tmp_z_d == 1)                    # サンプリングの結果を抽出
    
    # 新たに割り当てられたトピックを`k`に代入
    k <- z_d[d]
    
    # 文書dに関する値をカウントに加える
    new_D_k[k]    <- new_D_k[k] + 1
    new_N_kv[k, ] <- new_N_kv[k, ] + N_dv[d, ]
    new_N_k[k]    <- new_N_k[k] + N_d[d]
    
  } ## (/各文書：1,...,D)
  
  # カウントを更新
  D_k  <- new_D_k
  N_kv <- new_N_kv
  N_k  <- new_N_k
  
  # ハイパーパラメータ(α)を更新
  tmp_alpha_numer <- sum(digamma(D_k + alpha)) - K * digamma(alpha)      # 分子
  tmp_alpha_denom <- K * digamma(D + alpha * K) - K * digamma(alpha * K) # 分母
  alpha <- alpha * tmp_alpha_numer / tmp_alpha_denom
  
  # ハイパーパラメータ(β)を更新
  tmp_beta_numer <- sum(digamma(N_kv + beta)) - K * V * digamma(beta)            # 分子
  tmp_beta_denom <- V * sum(digamma(N_k + beta * V)) - K * V * digamma(beta * V) # 分母
  beta <- beta * tmp_beta_numer / tmp_beta_denom
  
  # 結果の確認用
  trace_alpha <- c(trace_alpha, alpha)
  trace_beta  <- c(trace_beta, beta)
  trace_D_k <- cbind(trace_D_k, as.matrix(D_k))
  trace_N_k <- cbind(trace_N_k, as.matrix(N_k))
  
  # 動作確認用
  print(paste0(i, "th try..."))
}


# 推移の確認 -------------------------------------------------------------------


## 推移を確認
# データフレームを作成
ab_df_wide <- data.frame(n = 1:length(trace_alpha), 
                         alpha = trace_alpha, 
                         beta = trace_beta)

# データフレームをlong型に変換
ab_df_long <- gather(ab_df_wide, key = "parameter", value = "value", -n)

# 描画
ggplot(ab_df_long, aes(n, value, color = parameter)) +     # データの指定
  geom_line() +                                            # 折れ線グラフ
  scale_color_manual(values = c("#00A968", "Orange")) +    # グラフの色
  labs(title = "Mixtuer of Unigram Models:Gibbs Sampling") # タイトル



# try ---------------------------------------------------------------------

# トピックごとの単語数の推移
N_k_df_wide <- cbind(as.data.frame(t(trace_N_k)), 
                     1:ncol(trace_N_k))
names(N_k_df_wide) <- c(1:K, "n")
N_k_df_long <- gather(N_k_df_wide[(nrow(N_k_df_wide)*0.9):nrow(N_k_df_wide), ], key = "topic", value = "N_k", -n)
ggplot(N_k_df_long, aes(n, N_k, color = topic)) + 
  geom_line(alpha = 0.5)


# トピックごとの文書数の推移
D_k_df_wide <- cbind(as.data.frame(t(trace_D_k)), 
                     1:ncol(trace_D_k))
names(D_k_df_wide) <- c(1:K, "n")
D_k_df_long <- gather(D_k_df_wide[(nrow(D_k_df_wide)*0.9):nrow(D_k_df_wide), ], key = "topic", value = "D_k", -n)
ggplot(D_k_df_long, aes(n, D_k, color = topic)) + 
  geom_line(alpha = 0.5)


rmultinom(1, 1, c(1, 1, 1, 1))
rmultinom()