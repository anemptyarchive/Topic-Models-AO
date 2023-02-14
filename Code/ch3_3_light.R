
# ch3.3 混合ユニグラムモデル：EMアルゴリズム(最尤推定) ----------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)

# チェック用
library(ggplot2)


# 文書データの簡易生成 --------------------------------------------------------------

## ch3_1.Rを参照


# EMアルゴリズム(最尤推定) --------------------------------------------------------------------

### ・パラメータの初期化 -----

# トピック数を指定
K <- 4

# 文書数と語彙数を取得
D <- nrow(N_dv)
V <- ncol(N_dv)


# 負担率qの初期化
q_dk <- matrix(0, nrow = D, ncol = K)

# トピック分布θの初期化
theta_k <- runif(n = K, min = 0, max = 1) |> 
  (\(vec) {vec / sum(vec)})() # 正規化

# 単語分布Φの初期化
phi_kv <- runif(n = K*V, min = 0, max = 1) |> 
  matrix(nrow = K, ncol = V) |> 
  (\(mat) {mat / rowSums(mat)})() # 正規化


### ・推論処理 -----

# 試行回数を指定
MaxIter <- 20

# 推移の確認用の受け皿を作成
trace_theta_ki <- matrix(NA, nrow = K, ncol = MaxIter+1)
trace_phi_kvi  <- array(NA, dim = c(K, V, MaxIter+1))

# 初期値を保存
trace_theta_ki[, 1]  <- theta_k
trace_phi_kvi[, , 1] <- phi_kv

# EMアルゴリズムによる最尤推定
for(i in 1:MaxIter){
  
  # 負担率qの計算:式(3.3)
  log_term_dk <- t(log(theta_k + 1e-7) + log(phi_kv + 1e-7) %*% t(N_dv)) # 分子
  log_term_dk <- log_term_dk - apply(log_term_dk, MARGIN = 1, FUN = min) # アンダーフロー対策
  log_term_dk <- log_term_dk - apply(log_term_dk, MARGIN = 1, FUN = max) # オーバーフロー対策
  q_dk        <- exp(log_term_dk) / rowSums(exp(log_term_dk)) # 正規化
  
  # トピック分布θの計算:式(3.7)
  new_theta_k <- colSums(q_dk) # 分子
  theta_k     <- new_theta_k / sum(new_theta_k) # 正規化
  
  # 単語分布Φの計算:式(3.8)
  new_phi_kv <- t(q_dk) %*% N_dv # 分子
  phi_kv     <- new_phi_kv / rowSums(new_phi_kv) # 正規化
  
  # i回目の更新値を保存
  trace_theta_ki[, i+1]  <- theta_k
  trace_phi_kvi[, , i+1] <- phi_kv
  
  # 動作確認
  message("\r", i, "/", MaxIter, appendLF = FALSE)
}


# 推定結果の可視化 ---------------------------------------------------------------

## ch3_3.Rを参照


