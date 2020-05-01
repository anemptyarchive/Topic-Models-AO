
# 3.5 混合ユニグラムモデル：ギブスサンプリング ----------------------------------------------------

# 利用パッケージ
library(tidyverse)

## 簡易文書データ
# 文書数
D <- 10
# 語彙数
V <- 20
# 各文書における各語彙数
N_dv <- matrix(sample(0:10, D * V, replace = TRUE), D, V)


# パラメータの初期設定 ----------------------------------------------------------------------

# 試行回数を指定
Iter <- 1000

# トピック数(K)を指定
K <- 4

# ハイパーパラメータ(alpha, beta)を指定
alpha <- 2
beta  <- 2

# 各トピックが割り当てられた文書数(D_{z_d})
D_k <- rep(0, K)

# 全文書において各トピックが割り当てられた語彙ごとの出現回数(N_{z_d,v})
N_kv <- matrix(
  0, nrow = K, ncol = V, 
  dimnames = list(paste0("k=", 1:K), # 行名
                  paste0("v=", 1:V)) # 列名
)

# 全文書において各トピックが割り当てられた単語数(N_{z_d})
N_k <- rep(0, K)

# 各文書が割り当てられたトピック(z_d)
z_d <- rep(0, D)


# ギブスサンプリング ---------------------------------------------------------------

# サンプル確率(p)
p_z <- NULL

# 推移の確認用
trace_alpha <- rep(0, Iter + 1)
trace_beta <- rep(0, Iter + 1)
# 初期値を代入
trace_alpha[1] <- alpha
trace_beta[1]  <- beta

for(i in 1:Iter) {
  
  # 新たに割り当られてるトピックに関するカウントを初期化
  new_D_k <- rep(0, K)
  new_N_kv <- matrix(0, nrow = K, ncol = V)
  new_N_k <- rep(0, K)
  
  for(d in 1:D) { ## (各文書)
    
    # 現ステップの計算のために前ステップのカウントを移す
    D_k.d  <- D_k
    N_kv.d <- N_kv
    N_k.d  <- N_k
    
    if(z_d[d] > 0) { # 初回を飛ばす処理
      
      # 前ステップで文書dが与えられたトピックを`k`に代入
      k <- z_d[d]
      
      # 文書dに関する値をカウントから除く
      D_k.d[k]    <- D_k[k] - 1
      N_kv.d[k, ] <- N_kv[k, ] - N_dv[d, ]
      N_k.d[k]    <- N_k[k] - N_d[d]
    }
    
    for(k in 1:K) { ## (各トピック)
      
      # サンプリング確率の計算
      term_alpha <- log(D_k.d[k] + alpha) # 第1因子
      term_beta1 <- lgamma(N_k.d[k] + beta * V) - lgamma(N_k.d[k] + N_d[d] + beta * V)  # 第2因子
      term_beta2 <- lgamma(N_kv.d[k, ] + N_dv[d, ] + beta) - lgamma(N_kv.d[k, ] + beta) # 第3因子
      p_z[k] <- exp(term_alpha + term_beta1 + sum(term_beta2))
      
    } ## (/各トピック)
    
    # サンプリング
    res_z <- rmultinom(n = 1, size = 1, prob = p_z)
    z_d[d] <- which(res_z == 1) # サンプリングの結果を抽出
    
    # 新たに割り当てられたトピックを`k`に代入
    k <- z_d[d]
    
    # 文書dに関する値をカウントに加える
    new_D_k[k]    <- new_D_k[k] + 1
    new_N_kv[k, ] <- new_N_kv[k, ] + N_dv[d, ]
    new_N_k[k]    <- new_N_k[k] + N_d[d]
    
  } ## (/各文書)
  
  # カウントを更新
  D_k  <- new_D_k
  N_kv <- new_N_kv
  N_k  <- new_N_k
  
  # ハイパーパラメータ(α)を更新
  term_alpha_numer <- sum(digamma(D_k + alpha)) - K * digamma(alpha)      # 分子
  term_alpha_denom <- K * digamma(D + alpha * K) - K * digamma(alpha * K) # 分母
  alpha <- alpha * term_alpha_numer / term_alpha_denom
  
  # ハイパーパラメータ(β)を更新
  term_beta_numer <- sum(digamma(N_kv + beta)) - K * V * digamma(beta)            # 分子
  term_beta_denom <- V * sum(digamma(N_k + beta * V)) - K * V * digamma(beta * V) # 分母
  beta <- beta * term_beta_numer / term_beta_denom
  
  # 結果の確認用
  trace_alpha[i + 1] <- alpha
  trace_beta[i + 1]  <- beta
  
  # 動作確認用
  print(paste0(i, "th try..."))
}


# 推移の確認 -------------------------------------------------------------------

## 推移を確認
# データフレームを作成
ab_df_wide <- data.frame(
  alpha = trace_alpha, 
  beta = trace_beta, 
  Iteration = 0:Iter
)

# データフレームをlong型に変換
ab_df_long <- pivot_longer(
  ab_df_wide, 
  cols = -Iteration, 
  names_to = "parameter", 
  values_to = "value"
)

# 描画
ggplot(ab_df_long, aes(x = Iteration, y = value, color = parameter)) + 
  geom_line() + # 折れ線グラフ
  scale_color_manual(values = c("#00A968", "Orange")) + # グラフの色(不必要)
  labs(title = "Mixtuer of Unigram Models", 
       subtitle = "Gibbs Sampling") # ラベル


