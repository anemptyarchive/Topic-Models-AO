
# ch3.4 混合ユニグラムモデル：変分ベイズ推定 ----------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)

# チェック用
library(ggplot2)


# 文書データの簡易生成 --------------------------------------------------------------

## ch3_4.Rを参照


# 変分ベイズ推定 -----------------------------------------------------------------

# パラメータの初期設定 --------------------------------------------------------------

### ・パラメータの初期化 -----

# トピック数を指定
K <- 4

# 文書数と語彙数を取得
D <- nrow(N_dv)
V <- ncol(N_dv)

# 文書ごとの単語数を計算
N_d <- rowSums(N_dv)


# トピック分布θの事前分布のパラメータαを指定
alpha <- 1

# 単語分布Φの事前分布のパラメータβを指定
beta  <- 1

# トピック分布θの変分事後分布のパラメータαの初期化
alpha_k <- runif(n = K, min = 0.01, max = 2) # 範囲を指定

# 単語分布Φの変分事後分布のパラメータβの初期化
beta_kv <- runif(n = K*V, min = 0.01, max = 2) |> # 範囲を指定
  matrix(nrow = K, ncol = V)


### ・推論処理 -----

# 試行回数を指定
MaxIter <- 20

# 推移の確認用の受け皿を作成
trace_q_dki    <- array(NA, dim = c(D, K, MaxIter))
trace_alpha_ki <- matrix(NA, nrow = K, ncol = MaxIter+1)
trace_beta_kvi <- array(NA, dim = c(K, V, MaxIter+1))

# 初期値を保存
trace_alpha_ki[, 1]   <- alpha_k
trace_beta_kvi[, , 1] <- beta_kv

# 変分ベイズ推定
for(i in 1:MaxIter) {
  
  # 負担率qの計算:式(3.22)
  term_alpha_k <- digamma(alpha_k) - digamma(sum(alpha_k))
  term_beta_kd <- digamma(beta_kv) %*% t(N_dv) - digamma(rowSums(beta_kv)) %*% t(N_d)
  log_q_dk     <- t(term_alpha_k + term_beta_kd)
  log_q_dk <- log_q_dk - apply(log_q_dk, MARGIN = 1, FUN = min) # アンダーフロー対策
  log_q_dk <- log_q_dk - apply(log_q_dk, MARGIN = 1, FUN = max) # オーバーフロー対策
  q_dk     <- exp(log_q_dk)
  #q_dk[rowSums(q_dk) == 0, ] <- 1 / K # 全ての要素が0の場合は等確率(任意の同じ値)に設定
  q_dk     <- q_dk / rowSums(q_dk) # 正規化
  
  # トピック分布θの変分事後分布のパラメータαの計算:式(3.19')
  alpha_k <- alpha + colSums(q_dk)
  
  # 単語分布Φの変分事後分布のパラメータβの計算:式(3.20')
  beta_kv <- beta + t(q_dk) %*% N_dv
  
  # i回目の更新値を保存
  trace_q_dki[, , i]      <- q_dk
  trace_alpha_ki[, i+1]   <- alpha_k
  trace_beta_kvi[, , i+1] <- beta_kv
  
  # 動作確認
  message("\r", i, "/", MaxIter, appendLF = FALSE)
}


# 推定結果の可視化 ---------------------------------------------------------------

## ch3_4.Rを参照


