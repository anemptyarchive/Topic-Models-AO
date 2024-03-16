
# chapter 3.5
# 混合ユニグラムモデル
# 崩壊型ギブスサンプリング
# パラメータが一様な場合

# %%

# 利用ライブラリ
import numpy as np
from scipy.special import loggamma, digamma
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# %%

### 簡易文書データの作成：(混合モデル)
# (詳細はgenerative_process.pyを参照)

# 文書数を指定
D = 25

# 語彙数を指定
V = 240

# トピック数を指定
true_K = 10

# ハイパーパラメータを指定
true_alpha = 1.0 # トピック分布
true_beta  = 1.0 # 語彙分布

# トピック分布を生成
true_theta_k = np.random.dirichlet(
    alpha=np.repeat(true_alpha, repeats=true_K), 
    size=1
).reshape(true_K)

# 語彙分布を生成
true_phi_kv = np.random.dirichlet(
    alpha=np.repeat(true_beta, repeats=V), 
    size=true_K
)

# 受け皿を初期化
w_dic = {} # 文書ごとの各単語の語彙
true_z_d = np.tile(np.nan, reps=D) # 各文書のトピック
N_d  = np.zeros(shape=D, dtype='int')      # 各文書の単語数
N_dv = np.zeros(shape=(D, V), dtype='int') # 文書ごとの各語彙の単語数

# 文書データを生成
for d in range(D): # 文書ごと
    
    # トピックを生成
    k = np.random.choice(a=np.arange(true_K), size=1, p=true_theta_k).item() # (カテゴリ乱数)
    
    # トピックを割当
    true_z_d[d] = k

    # 単語数を生成
    N_d[d] = np.random.randint(low=100, high=200, size=1) # 下限・上限を指定

    # 受け皿を初期化
    tmp_w_n = np.repeat(np.nan, repeats=N_d[d]) # 各単語の語彙
    
    for n in range(N_d[d]): # 単語ごと

        # 語彙を生成
        v = np.random.choice(a=np.arange(V), size=1, p=true_phi_kv[k]).item() # (カテゴリ乱数)
        
        # 語彙を割当
        tmp_w_n[n] = v

        # 頻度をカウント
        N_dv[d, v] += 1
    
    # データを記録
    w_dic[d] = tmp_w_n.copy()
    
    # 途中経過を表示
    print(
        'document: ' + str(d+1) + ', ' + 
        'words: '    + str(N_d[d]) + ', ' + 
        'topic: '    + str(k+1)
    )

# %%

### パラメータの初期化

# 文書数を取得
D = N_dv.shape[0]

# 語彙数を取得
V = N_dv.shape[1]

# 各文書の単語数を取得
N_d = N_dv.sum(axis=1)

# トピック数を指定
K = 10

# ハイパーパラメータの初期値を指定
alpha = 1.0 # トピック分布
beta  = 1.0 # 語彙分布

# 文書ごとのトピックを初期化
z_d = np.tile(np.nan, reps=D)

# 各トピックの割り当て文書数を初期化
D_k = np.zeros(shape=K)

# 語彙ごとの各トピックの割り当て単語数を初期化
N_kv = np.zeros(shape=(K, V))

# 各トピックの割り当て単語数を初期化
N_k = np.zeros(shape=K)

# %%

### 推論

# 試行回数を指定
max_iter = 1000

# 初期値を記録
trace_z_lt     = [z_d.copy()]
trace_Dk_lt    = [D_k.copy()]
trace_Nkv_lt   = [N_kv.copy()]
trace_alpha_lt = [alpha]
trace_beta_lt  = [beta]

# 崩壊型ギブスサンプリング
for i in range(max_iter): # 繰り返し試行

    for d in range(D): # 文書ごと
        if not np.isnan(z_d[d]): # (初回は不要)

            # 前ステップのトピックを取得
            k = z_d[d].astype('int')

            # ディスカウント
            D_k[k]  -= 1
            N_kv[k] -= N_dv[d]
            N_k[k]  -= N_d[d]
        
        # サンプリング確率を計算:式(3.27)
        log_prob_z_k  = np.log(D_k + alpha)
        log_prob_z_k += loggamma(N_k + V*beta)
        log_prob_z_k -= loggamma(N_k + N_d[d] + V*beta)
        log_prob_z_k += loggamma(N_kv + N_dv[d] + beta).sum(axis=1)
        log_prob_z_k -= loggamma(N_kv + beta).sum(axis=1)
        prob_z_k      = np.exp(log_prob_z_k - log_prob_z_k.min()) # (アンダーフロー対策)
        prob_z_k     /= prob_z_k.sum() # 正規化

        # トピックをサンプリング
        k = np.random.choice(a=np.arange(K), size=1, p=prob_z_k).item() # (簡易版のカテゴリ乱数)
        
        # トピックを割当
        z_d[d] = k
        
        # カウント
        D_k[k]  += 1
        N_kv[k] += N_dv[d]
        N_k[k]  += N_d[d]
    
    # トピック分布のパラメータを更新:式(3.28)
    old_alpha = alpha
    alpha *= digamma(D_k + old_alpha).sum() - K * digamma(old_alpha)
    alpha /= K * digamma(D + K*old_alpha) - K * digamma(K*old_alpha)
    
    # 語彙分布のパラメータを更新:式(3.29)
    old_beta = beta
    beta *= digamma(N_kv + old_beta).sum() - K*V * digamma(old_beta)
    beta /= V * digamma(N_k + V*old_beta).sum() - K*V * digamma(V*old_beta)
    
    # 更新値を記録
    trace_z_lt.append(z_d.astype('int').copy())
    trace_Dk_lt.append(D_k.copy())
    trace_Nkv_lt.append(N_kv.copy())
    trace_alpha_lt.append(alpha)
    trace_beta_lt.append(beta)

    # 途中経過を表示
    print(
        'iteration: ' + str(i+1) + ', ' + 
        'alpha = '    + str(alpha.round(2)) + ', ' + 
        'beta = '     + str(beta.round(2))
    )

# %%

### 作図の準備

# 作図用に変換
z_d = z_d.astype('int')

# 配色の共通化用のカラーマップを作成
cmap = plt.get_cmap("tab10")

# カラーマップの色数を設定:(カラーマップに応じて固定)
color_num = 10

# %%

### 推定したトピック集合の可視化

# グラフサイズを設定
u = 5
axis_size = np.ceil(N_dv.max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < D)
col_num = 5
row_num = np.ceil(D / col_num).astype('int')

# 文書データを作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(30, 20), dpi=100, facecolor='white')

for d in range(D):
    
    # サブプロットを抽出
    r = d // col_num
    c = d % col_num
    ax = axes[r, c]

    # トピック番号を取得
    k = z_d[d]
    
    # 語彙頻度を描画
    ax.bar(x=np.arange(stop=V)+1, height=N_dv[d], 
           color=cmap(k%color_num)) # 頻度
    ax.set_ylim(ymin=0, ymax=axis_size)
    ax.set_xlabel('vocabulary ($v$)')
    ax.set_ylabel('frequency ($N_{dv}$)')
    ax.set_title(f'$d = {d+1}, k = {k+1}$', loc='left')
    ax.grid()

# 残りのサブプロットを非表示
for c in range(c+1, col_num):
    axes[r, c].axis('off')

fig.supxlabel('document ($d$)')
fig.supylabel('document ($d$)')
fig.suptitle('document data', fontsize=20)
plt.show()

# %%

### 推定したトピック集合の推移の可視化

# フレーム数を指定
#frame_num = max_iter + 1
frame_num = 100

# 1フレーム当たりの試行回数を設定
iter_per_frame = (max_iter + 1) // frame_num

# グラフサイズを設定
u = 5
axis_Ndv_max = np.ceil(N_dv.max() /u)*u # u単位で切り上げ
axis_Dk_max  = np.ceil(np.max(trace_Dk_lt) /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < D)
col_num = 5
row_num = np.ceil((D+1) / col_num).astype('int')

# グラフオブジェクトを初期化
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(30, 20), dpi=100, facecolor='white')
fig.supxlabel('document ($d$)')
fig.supylabel('document ($d$)')
fig.suptitle('document data', fontsize=20)

# 作図処理を定義
def update(i):

    # 試行回数を調整
    i *= iter_per_frame

    # 前フレームのグラフを初期化
    for r in range(row_num):
        [ax.cla() for ax in axes[r]]
    
    # 更新値を取得
    z_d = trace_z_lt[i]
    D_k = trace_Dk_lt[i]
    
    for d in range(D):

        # サブプロットを抽出
        r = d // col_num
        c = d % col_num
        ax = axes[r, c]

        # トピック番号を取得
        k = z_d[d]

        # ラベル用の文字列を作成
        doc_label = f'$d = {d+1}, k = {k+1}$'
        if d == 0: # (左上図の場合)
            doc_label = f'iteration: {i}, ' + doc_label
        
        # 語彙頻度を描画
        ax.bar(x=np.arange(stop=V)+1, height=N_dv[d], 
            color=cmap(k%color_num)) # 頻度
        ax.set_ylim(ymin=0, ymax=axis_Ndv_max)
        ax.set_xlabel('vocabulary ($v$)')
        ax.set_ylabel('frequency ($N_{dv}$)')
        ax.set_title(doc_label, loc='left')
        ax.grid()

    # 残りのサブプロットを非表示
    if c == col_num-1: # (最後の文書が右端の列の場合)
        r = row_num - 1
        c = -1 
    for c in range(c+1, col_num-1):
        axes[r, c].axis('off')
        
    # トピックごとの文書数を描画
    ax = axes[row_num-1, col_num-1]
    ax.bar(x=np.arange(stop=K)+1, height=D_k, 
           color=[cmap(k%color_num) for k in range(K)]) # 文書数
    ax.set_ylim(ymin=0, ymax=axis_Dk_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('count ($D_k$)')
    ax.set_title(f'$iteration: {i}, D = {D}$', loc='left')
    ax.grid()

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/ch3/ch3_5_topic_set.mp4', dpi=100, 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)

# %%

### 推定したトピック分布のハイパーパラメータの推移の可視化

# 配列に変換
trace_alpha_i = np.array(trace_alpha_lt)

# グラフサイズを設定
u = 0.5
axis_size = np.ceil(trace_alpha_i.max()/u)*u # u単位で切り上げ

# ハイパーパラメータの推移を作図
fig, ax = plt.subplots(figsize=(8, 6), facecolor='white')
ax.plot(np.arange(max_iter+1), trace_alpha_i) # 更新値
ax.set_ylim(ymin=0, ymax=axis_size)
ax.set_xlabel('iteration')
ax.set_ylabel('value ($\\alpha$)')
fig.suptitle('hyperparameter of topic distribution', fontsize=20)
ax.grid()
plt.show()

# %%

### 推定した単語分布のハイパーパラメータの推移の可視化

# 配列に変換
trace_beta_i = np.array(trace_beta_lt)

# グラフサイズを設定
u = 0.5
axis_size = np.ceil(trace_beta_i.max()/u)*u # u単位で切り上げ

# ハイパーパラメータの推移を作図
fig, ax = plt.subplots(figsize=(8, 6), facecolor='white')
ax.plot(np.arange(max_iter+1), trace_beta_i) # 更新値
ax.set_xlabel('iteration')
ax.set_ylabel('value ($\\beta$)')
fig.suptitle('hyperparameter of word distribution', fontsize=20)
ax.set_ylim(ymin=0, ymax=axis_size)
ax.grid()
plt.show()

# %%

### 推定したトピック分布のパラメータの推移の可視化

# 配列に変換
trace_theta_ik  = np.array(trace_Dk_lt)
trace_theta_ik += np.array(trace_alpha_lt).reshape((max_iter+1, 1))
trace_theta_ik /= trace_theta_ik.sum(axis=1, keepdims=True) # 正規化

# グラフサイズを設定
u = 0.5
axis_size = np.ceil(trace_theta_ik.max()/u)*u # u単位で切り上げ

# パラメータの推移を作図
fig, ax = plt.subplots(figsize=(8, 6), facecolor='white')
ax.plot(np.arange(max_iter+1), trace_theta_ik) # 更新値
ax.set_ylim(ymin=0, ymax=axis_size)
ax.set_xlabel('iteration')
ax.set_ylabel('probability ($\\theta_k$)')
ax.set_title(f'$K = {K}$', loc='left')
fig.suptitle('parameter of topic distribution', fontsize=20)
ax.grid()
plt.show()

# %%

### 推定した単語分布のパラメータの推移の可視化

# 配列に変換
trace_phi_ikv  = np.array(trace_Nkv_lt)
trace_phi_ikv += np.array(trace_beta_i).reshape((max_iter+1, 1, 1))
trace_phi_ikv /= trace_phi_ikv.sum(axis=2, keepdims=True) # 正規化

# グラフサイズを設定
u = 0.01
axis_size = np.ceil(trace_phi_ikv.max()/u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < K)
col_num = 2
row_num = np.ceil(K / col_num).astype('int')

# ハイパーパラメータの推移を作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(16, 18), facecolor='white')

for k in range(K):
    
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
theta_k  = D_k + alpha
theta_k /= theta_k.sum() # 正規化

# グラフサイズを設定
u = 0.1
axis_size = np.ceil(theta_k.max() /u)*u # u単位で切り上げ

# トピック分布を作図
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
ax.bar(x=np.arange(stop=K)+1, height=theta_k, 
       color=[cmap(k%color_num) for k in range(K)]) # 確率
ax.set_ylim(ymin=0, ymax=axis_size)
ax.set_xlabel('topic ($k$)')
ax.set_ylabel('probability ($\\theta_k$)')
ax.set_title(f'$\\alpha = {alpha:.2f}$', loc='left')
fig.suptitle('topic distribution', fontsize=20)
ax.grid()
plt.show()

# %%

### 推定したトピック分布の推移の可視化

# フレーム数を指定
#frame_num = max_iter + 1
frame_num = 100

# 1フレーム当たりの試行回数を設定
iter_per_frame = (max_iter + 1) // frame_num

# グラフサイズを設定
u = 1
axis_val_max = np.ceil(max(trace_alpha_lt) /u)*u # u単位で切り上げ
u = 5
axis_freq_max = np.ceil(max([(trace_Dk_lt[i] + trace_alpha_lt[i]).max() for i in range(max_iter+1)]) /u)*u # u単位で切り上げ
axis_prob_max = 0.5

# グラフオブジェクトを初期化
fig, axes = plt.subplots(nrows=1, ncols=3, constrained_layout=True, 
                         figsize=(16, 6), facecolor='white')
fig.suptitle('topic distribution', fontsize=20)

# 作図処理を定義
def update(i):

    # 試行回数を調整
    i *= iter_per_frame

    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes]
    
    # 更新値を取得
    alpha = trace_alpha_lt[i]
    D_k   = trace_Dk_lt[i]

    # ハイパーパラメータを描画
    ax = axes[0]
    ax.bar(x=0, height=alpha, 
           color='brown') # ハイパラ
    ax.set_xticks(ticks=[0], labels=[''])
    ax.set_ylim(ymin=0, ymax=axis_val_max)
    ax.set_xlabel('')
    ax.set_ylabel('value ($\\alpha$)')
    ax.set_title(f'$iteration = {i}$', loc='left')
    ax.grid()

    # トピックごとの基準値を描画
    ax = axes[1]
    ax.bar(x=np.arange(stop=K)+1, height=D_k, 
           color=[cmap(k%color_num) for k in range(K)]) # 文書数
    ax.bar(x=np.arange(K)+1, height=alpha, bottom=D_k, 
           color='brown', alpha=0.5, linewidth=1, linestyle='dashed') # ハイパラ
    ax.set_ylim(ymin=0, ymax=axis_freq_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('count ($D_k$)')
    ax.set_title(f'$\\alpha = {alpha:.2f}$', loc='left')
    ax.grid()

    # トピック分布を計算
    theta_k  = D_k + alpha
    theta_k /= theta_k.sum() # 正規化

    # トピック分布を描画
    ax = axes[2]
    ax.bar(x=np.arange(stop=K)+1, height=theta_k, 
           color=[cmap(k%color_num) for k in range(K)]) # 確率
    ax.set_ylim(ymin=0, ymax=axis_prob_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('probability ($\\theta_k$)')
    ax.grid()

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/ch3/ch3_5_topic_dist.mp4', dpi=100, 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)

# %%

### 推定した単語分布の可視化

# 語彙分布を計算
phi_kv  = N_kv + beta
phi_kv /= phi_kv.sum(axis=1, keepdims=True) # 正規化

# グラフサイズを設定
u = 0.01
axis_size = np.ceil(phi_kv.max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < K)
col_num = 2
row_num = np.ceil(K / col_num).astype('int')

# 語彙分布を作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(16, 18), dpi=100, facecolor='white')

for k in range(K):
    
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
    ax.set_title(f'$k = {k+1}, \\beta = {beta:.2f}$', loc='left')
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
frame_num = 100

# 1フレーム当たりの試行回数を設定
iter_per_frame = (max_iter + 1) // frame_num

# グラフサイズを設定
u = 1
axis_val_max = np.ceil(max(trace_beta_lt) /u)*u # u単位で切り上げ
u = 5
axis_freq_max = np.ceil(max([(trace_Nkv_lt[i] + trace_beta_lt[i]).max() for i in range(max_iter+1)]) /u)*u # u単位で切り上げ
axis_prob_max = 0.1

# グラフオブジェクトを初期化
fig, axes = plt.subplots(nrows=K+1, ncols=2, constrained_layout=True, 
                         figsize=(16, 36), facecolor='white')
fig.suptitle('word distribution', fontsize=20)

# 作図処理を定義
def update(i):

    # 試行回数を調整
    i *= iter_per_frame

    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes[:, 0]]
    [ax.cla() for ax in axes[:, 1]]
    
    # 更新値を取得
    beta = trace_beta_lt[i]
    N_kv = trace_Nkv_lt[i]

    # ハイパーパラメータを描画
    ax = axes[0, 0]
    ax.bar(x=0, height=beta, 
           color='brown') # ハイパラ
    ax.set_xticks(ticks=[0], labels=[''])
    ax.set_ylim(ymin=0, ymax=axis_val_max)
    ax.set_xlabel('')
    ax.set_ylabel('value ($\\beta$)')
    ax.set_title(f'$iteration = {i}$', loc='left')
    ax.grid()

    # 残りのサブプロットを非表示
    axes[0, 1].axis('off')

    for k in range(K):

        # 語彙ごとの基準値を描画
        ax = axes[k+1, 0]
        ax.bar(x=np.arange(stop=V)+1, height=N_kv[k], 
               color=cmap(k%color_num)) # 単語数
        ax.bar(x=np.arange(V)+1, height=beta, bottom=N_kv[k], 
               color='brown', alpha=0.5, linewidth=1, linestyle='dashed') # ハイパラ
        ax.set_ylim(ymin=0, ymax=axis_freq_max)
        ax.set_xlabel('vocabulary ($v$)')
        ax.set_ylabel('count ($N_{kv}$)')
        ax.set_title(f'$\\beta = {beta:.2f}$', loc='left')
        ax.grid()
        
        # 語彙分布を計算
        phi_v  = N_kv[k] + beta
        phi_v /= phi_v.sum() # 正規化

        # 語彙分布を描画
        ax = axes[k+1, 1]
        ax.bar(x=np.arange(stop=V)+1, height=phi_v, 
               color=cmap(k%color_num)) # 確率
        ax.set_ylim(ymin=0, ymax=axis_prob_max)
        ax.set_xlabel('vocabulary ($v$)')
        ax.set_ylabel('probability ($\phi_{kv}$)')
        ax.set_title(f'$k = {k+1}$', loc='left')
        ax.grid()

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/ch3/ch3_5_word_dist.mp4', dpi=100, 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)

# %%


