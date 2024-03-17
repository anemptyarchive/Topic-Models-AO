
# chpter 4.5
# トピックモデル
# 崩壊型ギブスサンプリング
# パラメータが多様な場合

# %%

#　利用ライブラリ
import numpy as np
from scipy.special import digamma
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# %%

### 簡易文書データの作成：(LDA)
# (詳細はgenerative_process.pyを参照)

# 文書数を指定
D = 30

# 語彙数を指定
V = 100

# トピック数を指定
true_K = 10

# ハイパーパラメータを指定
true_alpha_k = np.repeat(1.0, repeats=true_K) # トピック分布
true_beta_v  = np.repeat(1.0, repeats=V)      # 語彙分布

# トピック分布を生成
true_theta_dk = np.random.dirichlet(alpha=true_alpha_k, size=D)

# 語彙分布を生成
true_phi_kv = np.random.dirichlet(alpha=true_beta_v, size=true_K)

# 受け皿を初期化
w_dic      = {} # 文書ごとの各単語の語彙
true_z_dic = {} # 文書ごとの各単語のトピック
N_d  = np.zeros(shape=D, dtype='int')      # 各文書の単語数
N_dv = np.zeros(shape=(D, V), dtype='int') # 各文書の語彙ごとの単語数

# 文書データを生成
for d in range(D): # 文書ごと

    # 単語数を生成
    N_d[d] = np.random.randint(low=100, high=200, size=1) # 下限・上限を指定
    
    # 受け皿を初期化
    tmp_w_n = np.repeat(np.nan, repeats=N_d[d]) # 各単語の語彙
    tmp_z_n = np.repeat(np.nan, repeats=N_d[d]) # 各単語のトピック
    
    for n in range(N_d[d]): # 単語ごと
        
        # トピックを生成
        k = np.random.choice(a=np.arange(true_K), size=1, p=true_theta_dk[d]).item() # (カテゴリ乱数)

        # トピックを割当
        tmp_z_n[n] = k

        # 語彙を生成
        v = np.random.choice(a=np.arange(V), size=1, p=true_phi_kv[k]).item() # (カテゴリ乱数)
        
        # 語彙を割当
        tmp_w_n[n] = v
        
        # 頻度をカウント
        N_dv[d, v] += 1
    
    # データを記録
    w_dic[d]      = tmp_w_n.astype('int').copy()
    true_z_dic[d] = tmp_z_n.astype('int').copy()
    
    # 途中経過を表示
    print(f'document: {d+1}, words: {N_d[d]}, topics: {np.unique(true_z_dic[d])}')

#%%

### パラメータの初期化

# 文書数を取得
D = N_dv.shape[0]

# 語彙数を取得
V = N_dv.shape[1]

# 各文書の単語数を取得
N_d = N_dv.sum(axis=1)

# トピック数を指定
K = 9

# ハイパーパラメータの初期値を指定
alpha_k = np.repeat(1.0, repeats=K) # トピック分布
beta_v  = np.repeat(1.0, repeats=V) # 語彙分布

# 文書ごとの各単語のトピックを初期化
z_dic = {d: np.repeat(np.nan, repeats=N_d[d]) for d in range(D)}

# 文書ごとの各トピックの割り当て単語数を初期化
N_dk = np.zeros(shape=(D, K))

# 語彙ごとの各トピックの割り当て単語数を初期化
N_kv = np.zeros(shape=(K, V))

# %%

### 推論

# 試行回数を指定
max_iter = 100

# 初期値を記録
trace_z_lt     = [z_dic.copy()]
trace_Ndk_lt   = [N_dk.copy()]
trace_Nkv_lt   = [N_kv.copy()]
trace_alpha_lt = [alpha_k.copy()]
trace_beta_lt  = [beta_v.copy()]

# 崩壊型ギブスサンプリング
for i in range(max_iter): # 繰り返し試行

    for d in range(D): # 文書ごと
        
        # 配列を取得
        tmp_w_n = w_dic[d]
        tmp_z_n = z_dic[d].copy()

        for n in range(N_d[d]): # 単語ごと

            # 語彙を取得
            v = tmp_w_n[n]

            if not np.isnan(tmp_z_n[n]): # (初回は不要)

                # 前ステップのトピックを取得
                k = tmp_z_n[n].copy()

                # ディスカウント
                N_dk[d, k] -= 1
                N_kv[k, v] -= 1

            # サンプリング確率を計算
            prob_z_k  = N_dk[d] + alpha_k
            prob_z_k *= N_kv[:, v] + beta_v[v]
            prob_z_k /= N_kv.sum(axis=1) + beta_v.sum()
            prob_z_k /= prob_z_k.sum() # 正規化
            
            # トピックをサンプリング
            k = np.random.choice(a=np.arange(K), size=1, p=prob_z_k).item() # (簡易版のカテゴリ乱数)
            
            # トピックを割当
            tmp_z_n[n] = k
            
            # カウント
            N_dk[d, k] += 1
            N_kv[k, v] += 1
        
        # 配列を格納
        z_dic[d] = tmp_z_n.astype('int').copy()
    
    # ハイパーパラメータを計算
    old_alpha_k = alpha_k.copy() + 1e-7 # (負の発散の回避用)
    alpha_k *= digamma(N_dk + old_alpha_k).sum(axis=0) - D * digamma(old_alpha_k)
    alpha_k /= digamma(N_dk.sum(axis=1) + old_alpha_k.sum()).sum() - D * digamma(old_alpha_k.sum())
    
    old_beta_v = beta_v.copy() + 1e-7 # (負の発散の回避用)
    beta_v *= digamma(N_kv + old_beta_v).sum(axis=0) - K * digamma(old_beta_v)
    beta_v /= digamma(N_kv.sum(axis=1) + old_beta_v.sum()).sum() - K * digamma(old_beta_v.sum())
    
    # 更新値を記録
    trace_z_lt.append(z_dic.copy())
    trace_Ndk_lt.append(N_dk.copy())
    trace_Nkv_lt.append(N_kv.copy())
    trace_alpha_lt.append(alpha_k.copy())
    trace_beta_lt.append(beta_v.copy())

    # 途中経過を表示
    print(
        f'iteration: {i+1}, alpha = ({alpha_k[0]:.3f}, ...), beta = ({beta_v[0]:.3f}, ...)'
    )

# %%

### 作図の準備

# 配色の共通化用のカラーマップを作成
cmap = plt.get_cmap("tab10")

# カラーマップの色数を設定:(カラーマップに応じて固定)
color_num = 10

# %%

### 推定したトピック集合の可視化

# 描画する文書数を指定
#doc_num = D
doc_num = 5

# グラフサイズを設定
u = 5
axis_Ndv_max = (np.ceil(N_dv[:doc_num].max() /u)*u).astype('int') # u単位で切り上げ
axis_Ndk_max = (np.ceil(N_dk[:doc_num].max() /u)*u).astype('int') # u単位で切り上げ

# 文書データを作図
fig, axes = plt.subplots(nrows=doc_num, ncols=2, constrained_layout=True, 
                         figsize=(20, 25), dpi=100, facecolor='white')

for d in range(doc_num):

    # 各語彙の単語数を集計・各単語のトピックを格納
    tmp_z_mv = np.tile(np.nan, reps=(axis_Ndv_max, V))
    for n in range(N_d[d]):
        v = w_dic[d][n]
        m = np.sum(w_dic[d][:n+1] == v)
        k = z_dic[d][n]
        tmp_z_mv[m-1, v] = k % color_num # (配色の共通化用)
    
    # 単語データを描画
    ax = axes[d, 0]
    ax.pcolor(tmp_z_mv, cmap=cmap, vmin=0, vmax=color_num-1) # 頻度・トピック
    ax.set_xlabel('vocabulary ($v$)')
    ax.set_ylabel('frequency ($N_{dv}$)')
    ax.set_title(f'iteration: {max_iter}, $d = {d+1}, N_d = {N_d[d]}$', loc='left')
    ax.grid()

    # 語彙の出現順を描画
    for n in range(N_d[d]):
        v = w_dic[d][n]
        m = np.sum(w_dic[d][:n+1] == v)
        ax.text(x=v+0.5, y=m-0.5, s=str(n+1), 
                size=5, ha='center', va='center') # 単語番号
    
    # トピックの割当を描画
    ax = axes[d, 1]
    ax.bar(x=np.arange(stop=K)+1, height=N_dk[d], 
           color=[cmap(k%color_num) for k in range(K)]) # 単語数
    ax.set_ylim(ymin=0, ymax=axis_Ndk_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('frequency ($N_{dk}$)')
    ax.set_title(f'iteration: {max_iter}, $d = {d+1}, N_d = {N_d[d]}$', loc='left')
    ax.grid()

fig.supylabel('document ($d$)')
fig.suptitle('document data', fontsize=20)
plt.show()

# %%

### 推定したトピック集合の推移の可視化

# フレーム数を指定
#frame_num = max_iter + 1
frame_num = 10

# 1フレーム当たりの試行回数を設定
iter_per_frame = (max_iter + 1) // frame_num

# 描画する文書数を指定
#doc_num = D
doc_num = 5

# グラフサイズを設定
u = 5
axis_Ndv_max = (np.ceil(N_dv[:doc_num].max() /u)*u).astype('int') # u単位で切り上げ
axis_Ndk_max = (np.ceil(np.array(trace_Ndk_lt)[:, :doc_num].max() /u)*u).astype('int') # u単位で切り上げ

# グラフオブジェクトを初期化
fig, axes = plt.subplots(nrows=doc_num, ncols=2, constrained_layout=True, 
                         figsize=(20, 30), dpi=100, facecolor='white')
fig.supylabel('document ($d$)')
fig.suptitle('document data', fontsize=20)

# 作図処理を定義
def update(i):
    
    # 試行回数を調整
    i *= iter_per_frame
    
    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes[:, 0]]
    [ax.cla() for ax in axes[:, 1]]

    # 更新値を取得
    z_dic = trace_z_lt[i]
    N_dk  = trace_Ndk_lt[i]
    
    for d in range(doc_num):
        
        # 各語彙の単語数を集計・各単語のトピックを格納
        z_mv = np.tile(np.nan, reps=(axis_Ndv_max, V))
        for n in range(N_d[d]):
            v = w_dic[d][n]
            m = np.sum(w_dic[d][:n+1] == v)
            k = z_dic[d][n]
            z_mv[m-1, v] = k % color_num # (配色の共通化用)
        
        # 単語データを描画
        ax = axes[d, 0]
        ax.pcolor(z_mv, cmap=cmap, vmin=0, vmax=color_num-1) # 頻度・トピック
        ax.set_xlabel('vocabulary ($v$)')
        ax.set_ylabel('frequency ($N_{dv}$)')
        ax.set_title(f'iteration: {i}, $d = {d+1}, N_d = {N_d[d]}$', loc='left')
        ax.grid()
        
        # トピックの割当を描画
        ax = axes[d, 1]
        ax.bar(x=np.arange(stop=K)+1, height=N_dk[d], 
            color=[cmap(k%color_num) for k in range(K)]) # 単語数
        ax.set_ylim(ymin=0, ymax=axis_Ndk_max)
        ax.set_xlabel('topic ($k$)')
        ax.set_ylabel('frequency ($N_{dk}$)')
        ax.set_title(f'iteration: {i}, $d = {d+1}, N_d = {N_d[d]}$', loc='left')
        ax.grid()

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/ch4/ch4_5_topic_set.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)

# %%

### 推定したトピック分布のハイパーパラメータの推移の可視化

# 配列に変換
trace_alpha_ik = np.array(trace_alpha_lt)

# グラフサイズを設定
u = 0.5
axis_size = np.ceil(trace_alpha_ik.max()/u)*u # u単位で切り上げ

# ハイパーパラメータの推移を作図
fig, ax = plt.subplots(figsize=(10, 5), dpi=100, facecolor='white')
for k in range(K):
    ax.plot(np.arange(max_iter+1), trace_alpha_ik[:, k]) # 更新値
ax.set_ylim(ymin=0, ymax=axis_size)
ax.set_xlabel('iteration')
ax.set_ylabel('value ($\\alpha_k$)')
ax.set_title(f'$K = {K}$', loc='left')
fig.suptitle('hyperparameter of topic distribution', fontsize=20)
ax.grid()
plt.show()

# %%

### 推定した単語分布のハイパーパラメータの推移の可視化

# 配列に変換
trace_beta_iv = np.array(trace_beta_lt)

# グラフサイズを設定
u = 0.5
axis_size = np.ceil(trace_beta_iv.max()/u)*u # u単位で切り上げ

# ハイパーパラメータの推移を作図
fig, ax = plt.subplots(figsize=(10, 5), dpi=100, facecolor='white')
for v in range(V):
    ax.plot(np.arange(max_iter+1), trace_beta_iv[:, v]) # 更新値
ax.set_xlabel('iteration')
ax.set_ylabel('value ($\\beta_v$)')
fig.suptitle('hyperparameter of word distribution', fontsize=20)
ax.set_title(f'$V = {V}$', loc='left')
ax.set_ylim(ymin=0, ymax=axis_size)
ax.grid()
plt.show()

# %%

### 推定したトピック分布のパラメータの推移の可視化

# 配列に変換
trace_theta_idk  = np.array(trace_Ndk_lt)
trace_theta_idk += np.array(trace_alpha_lt).reshape((max_iter+1, 1, K))
trace_theta_idk /= trace_theta_idk.sum(axis=2, keepdims=True) # 正規化

# 描画する文書数を指定
#doc_num = D
doc_num = 9

# グラフサイズを設定
u = 0.1
axis_size = np.ceil(trace_theta_idk[:, :doc_num].max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < D)
col_num = 3
row_num = np.ceil(doc_num / col_num).astype('int')

# パラメータの推移を作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                       figsize=(30, 15), dpi=100, facecolor='white')

for d in range(doc_num):
    
    # サブプロットを抽出
    r = d // col_num
    c = d % col_num
    ax = axes[r, c]

    # パラメータの推移を描画
    ax.plot(np.arange(max_iter+1), trace_theta_idk[:, d, :]) # 更新値
    ax.set_ylim(ymin=0, ymax=axis_size)
    ax.set_xlabel('iteration')
    ax.set_ylabel('probability ($\\theta_{dk}$)')
    ax.set_title(f'$d = {d+1}, K = {K}$', loc='left')
    ax.grid()

# 残りのサブプロットを非表示
for c in range(c+1, col_num):
    axes[r, c].axis('off')

fig.supxlabel('document ($d$)')
fig.supylabel('document ($d$)')
fig.suptitle('parameter of topic distribution', fontsize=20)
plt.show()

# %%

### 推定した単語分布のパラメータの推移の可視化

# 配列に変換
trace_phi_ikv  = np.array(trace_Nkv_lt)
trace_phi_ikv += np.array(trace_beta_lt).reshape((max_iter+1, 1, V))
trace_phi_ikv /= trace_phi_ikv.sum(axis=2, keepdims=True) # 正規化

# 描画するトピック数を指定
#doc_num = D
topic_num = 9

# グラフサイズを設定
u = 0.01
axis_size = np.ceil(trace_phi_ikv[:, :topic_num].max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < K)
col_num = 3
row_num = np.ceil(topic_num / col_num).astype('int')

# ハイパーパラメータの推移を作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(30, 15), dpi=100, facecolor='white')

for k in range(topic_num):
    
    # サブプロットを抽出
    r = k // col_num
    c = k % col_num
    ax = axes[r, c]

    # パラメータの推移を描画
    ax.plot(np.arange(max_iter+1), trace_phi_ikv[:, k]) # 更新値
    ax.set_ylim(ymin=0, ymax=axis_size)
    ax.set_xlabel('iteration')
    ax.set_ylabel('probability ($\\phi_{kv}$)')
    ax.set_title(f'$k = {k+1}, V = {V}$', loc='left')
    ax.grid()

# 残りのサブプロットを非表示
for c in range(c+1, col_num):
    axes[r, c].axis('off')

fig.supxlabel('topic ($k$)')
fig.supylabel('topic ($k$)')
fig.suptitle('parameter of word distribution', fontsize=20)
plt.show()

# %%

### 推定したトピック分布の可視化

# トピック分布を計算
theta_dk  = N_dk + alpha_k
theta_dk /= theta_dk.sum(axis=1, keepdims=True) # 正規化

# 描画する文書を指定
#doc_num = D
doc_num = 9

# グラフサイズを設定
u = 0.1
axis_size = np.ceil(theta_dk[:doc_num].max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < D)
col_num = 3
row_num = np.ceil(doc_num / col_num).astype('int')

# トピック分布を作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(24, 15), dpi=100, facecolor='white')

for d in range(doc_num):
    
    # サブプロットを抽出
    r = d // col_num
    c = d % col_num
    ax = axes[r, c]

    # トピック分布を描画
    ax.bar(x=np.arange(stop=K)+1, height=theta_dk[d], 
           color=[cmap(k%color_num) for k in range(K)]) # 確率
    ax.set_ylim(ymin=0, ymax=axis_size)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('probability ($\\theta_{dk}$)')
    ax.set_title(f'iteration: {max_iter}, $d = {d+1}$', loc='left')
    ax.grid()

# 残りのサブプロットを非表示
for c in range(c+1, col_num):
    axes[r, c].axis('off')

fig.supxlabel('document ($d$)')
fig.supylabel('document ($d$)')
fig.suptitle('topic distribution (estimated)', fontsize=20)
plt.show()

# %%

### 推定したトピック分布の推移の可視化

# フレーム数を指定
#frame_num = max_iter + 1
frame_num = 10

# 1フレーム当たりの試行回数を設定
iter_per_frame = (max_iter + 1) // frame_num

# 描画する文書数を指定
#doc_num = D
doc_num = 5

# グラフサイズを設定
u = 5
axis_val_max  = np.ceil(np.array(trace_alpha_lt).max() /u)*u # u単位で切り上げ
axis_freq_max = np.ceil(max([(trace_Ndk_lt[i][:doc_num] + trace_alpha_lt[i]).max() for i in range(max_iter+1)]) /u)*u # u単位で切り上げ
axis_prob_max = 0.5

# グラフオブジェクトを初期化
fig, axes = plt.subplots(nrows=doc_num+1, ncols=2, constrained_layout=True, 
                         figsize=(16, 30), facecolor='white')
fig.supylabel('document ($d$)')
fig.suptitle('topic distribution', fontsize=20)

# 作図処理を定義
def update(i):

    # 試行回数を調整
    i *= iter_per_frame

    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes[:, 0]]
    [ax.cla() for ax in axes[:, 1]]
    
    # 更新値を取得
    alpha_k = trace_alpha_lt[i]
    N_dk    = trace_Ndk_lt[i]
    
    # トピック分布を計算
    theta_dk  = N_dk + alpha_k
    theta_dk /= theta_dk.sum(axis=1, keepdims=True) # 正規化

    # 全文書で共通の値を描画
    ax = axes[0, 0]
    ax.bar(x=np.arange(stop=K)+1, height=alpha_k, 
           color=[cmap(k%color_num) for k in range(K)]) # ハイパラ
    ax.set_ylim(ymin=0, ymax=axis_val_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('value ($\\alpha_k$)')
    ax.set_title(f'iteration: {i}', loc='left')
    ax.grid()

    # 残りのサブプロットを非表示
    axes[0, 1].axis('off')
    
    for d in range(doc_num):
        
        # トピックごとの基準値を描画
        ax = axes[d+1, 0]
        ax.bar(x=np.arange(stop=K)+1, height=N_dk[d], 
               color=[cmap(k%color_num) for k in range(K)]) # 単語数
        ax.bar(x=np.arange(stop=K)+1, height=alpha_k, bottom=N_dk[d], 
               color=[cmap(k%color_num) for k in range(K)], 
               edgecolor=[cmap(k%color_num) for k in range(K)], 
               alpha=0.5, linewidth=1, linestyle='dashed') # ハイパラ
        ax.set_ylim(ymin=0, ymax=axis_freq_max)
        ax.set_xlabel('topic ($k$)')
        ax.set_ylabel('count ($N_{dk} + \\alpha_k$)')
        ax.set_title(f'iteration:{i}, $d = {d+1}$', loc='left')
        ax.grid()

        # トピック分布を描画
        ax = axes[d+1, 1]
        ax.bar(x=np.arange(stop=K)+1, height=theta_dk[d], 
               color=[cmap(k%color_num) for k in range(K)]) # 確率
        ax.set_ylim(ymin=0, ymax=axis_prob_max)
        ax.set_xlabel('topic ($k$)')
        ax.set_ylabel('probability ($\\theta_{dk}$)')
        ax.set_title(f'iteration: {i}, $d = {d+1}$', loc='left')
        ax.grid()

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/ch4/ch4_5_topic_dist.mp4', dpi=100, 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)

# %%

### 推定した単語分布の可視化

# 語彙分布を計算
phi_kv  = N_kv + beta_v
phi_kv /= phi_kv.sum(axis=1, keepdims=True) # 正規化

# 描画するトピック数を指定
#topic_num = K
topic_num = 9

# グラフサイズを設定
u = 0.05
axis_size = np.ceil(phi_kv[:topic_num].max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < K)
col_num = 3
row_num = np.ceil(topic_num / col_num).astype('int')

# 語彙分布を作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(30, 15), dpi=100, facecolor='white')

for k in range(topic_num):
    
    # サブプロットを抽出
    r = k // col_num
    c = k % col_num
    ax = axes[r, c]

    # 語彙分布を描画
    ax.bar(x=np.arange(stop=V)+1, height=phi_kv[k], 
           color=cmap(k%color_num)) # 確率
    ax.set_ylim(ymin=0, ymax=axis_size)
    ax.set_xlabel('vocabulary ($v$)')
    ax.set_ylabel('probability ($\phi_{kv}$)')
    ax.set_title(f'iteration: {max_iter}, $k = {k+1}$', loc='left')
    ax.grid()

# 残りのサブプロットを非表示
for c in range(c+1, col_num):
    axes[r, c].axis('off')

fig.supxlabel('topic ($k$)')
fig.supylabel('topic ($k$)')
fig.suptitle('word distribution', fontsize=20)
plt.show()

# %%

### 推定した単語分布の推移の可視化

# フレーム数を指定
#frame_num = max_iter + 1
frame_num = 10

# 1フレーム当たりの試行回数を設定
iter_per_frame = (max_iter + 1) // frame_num

# 描画するトピック数を指定
#topic_num = K
topic_num = 5

# グラフサイズを設定
u = 5
axis_val_max  = np.ceil(np.array(trace_beta_lt).max() /u)*u # u単位で切り上げ
axis_freq_max = np.ceil(max([(trace_Nkv_lt[i][:topic_num] + trace_beta_lt[i]).max() for i in range(max_iter+1)]) /u)*u # u単位で切り上げ
axis_prob_max = 0.1

# グラフオブジェクトを初期化
fig, axes = plt.subplots(nrows=topic_num+1, ncols=2, constrained_layout=True, 
                         figsize=(20, 30), facecolor='white')
fig.supylabel('topic ($k$)')
fig.suptitle('word distribution', fontsize=20)

# 作図処理を定義
def update(i):
    
    # 試行回数を調整
    i *= iter_per_frame

    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes[:, 0]]
    [ax.cla() for ax in axes[:, 1]]
    
    # 更新値を取得
    beta_v = trace_beta_lt[i]
    N_kv   = trace_Nkv_lt[i]
    
    # 語彙分布を計算
    phi_kv  = N_kv + beta_v
    phi_kv /= phi_kv.sum(axis=1, keepdims=True) # 正規化
    
    # 全トピックで共通の値を描画
    ax = axes[0, 0]
    ax.bar(x=np.arange(stop=V)+1, height=beta_v, 
           color='brown') # ハイパラ
    ax.set_ylim(ymin=0, ymax=axis_val_max)
    ax.set_xlabel('vocabulary ($v$)')
    ax.set_ylabel('value ($\\beta_v$)')
    ax.set_title(f'iteration: {i}', loc='left')
    ax.grid()

    # 残りのサブプロットを非表示
    axes[0, 1].axis('off')
    
    for k in range(topic_num):
        
        # 語彙ごとの共通の値を描画
        ax = axes[k+1, 0]
        ax.bar(x=np.arange(stop=V)+1, height=N_kv[k], 
               color=cmap(k%color_num)) # 単語数
        ax.bar(x=np.arange(stop=V)+1, height=beta_v, bottom=N_kv[k], 
               color='brown', edgecolor='brown', 
               alpha=0.5, linewidth=1, linestyle='dashed') # ハイパラ
        ax.set_ylim(ymin=0, ymax=axis_freq_max)
        ax.set_xlabel('vocabulary ($v$)')
        ax.set_ylabel('count ($N_{kv}$)')
        ax.set_title(f'iteration: {i}, $k = {k+1}$', loc='left')
        ax.grid()

        # 語彙分布を描画
        ax = axes[k+1, 1]
        ax.bar(x=np.arange(stop=V)+1, height=phi_kv[k], 
               color=cmap(k%color_num)) # 確率
        ax.set_ylim(ymin=0, ymax=axis_prob_max)
        ax.set_xlabel('vocabulary ($v$)')
        ax.set_ylabel('probability ($\\phi_{kv}$)')
        ax.set_title(f'iteration: {i}, $k = {k+1}$', loc='left')
        ax.grid()

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/ch4/ch4_5_word_dist.mp4', dpi=100, 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)

# %%


