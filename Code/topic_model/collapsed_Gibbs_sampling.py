
# chpter 4.5 トピックモデル(多様なパラメータ)：ギブスサンプリング

# %%

#　利用ライブラリ
import numpy as np
from scipy.special import digamma
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# %%

### 簡易文書データの作成：(LDA)

# 文書数を指定
D = 9

# 語彙数を指定
V = 150

# トピック数を指定
true_K = 5

# ハイパーパラメータを指定
true_alpha_k = np.repeat(1, repeats=true_K) # トピック分布
true_beta_v  = np.repeat(1, repeats=V)      # 語彙分布

# トピック分布を生成
true_theta_dk = np.random.dirichlet(alpha=true_alpha_k, size=D)

# 単語分布を生成
true_phi_kv = np.random.dirichlet(alpha=true_beta_v, size=true_K)

# 受け皿を初期化
w_dic      = {} # 各単語の語彙
true_z_dic = {} # 各単語のトピック
N_d  = np.zeros(shape=D, dtype='int')      # 各文書の単語数
N_dv = np.zeros(shape=(D, V), dtype='int') # 各文書の語彙ごとの単語数
true_doc_dic = {} # 文書データ

# 文書データを生成
for d in range(D): # 文書ごと

    # 単語数を生成
    N_d[d] = np.random.randint(low=100, high=200, size=1) # 下限・上限を指定

    # 受け皿を初期化
    tmp_w_n = np.repeat(np.nan, repeats=N_d[d]) # 各単語の語彙
    tmp_z_n = np.repeat(np.nan, repeats=N_d[d]) # 各単語のトピック
    tmp_word_dic = {} # 単語データ
    
    for n in range(N_d[d]): # 単語ごと
        
        # トピックを生成
        onehot_k = np.random.multinomial(n=1, pvals=true_theta_dk[d], size=1).reshape(true_K) # one-hot符号
        k, = np.where(onehot_k == 1) # トピック番号
        k  = k.item()
        
        # トピックを割当
        tmp_z_n[n] = k
    
        # 語彙を生成
        onehot_v = np.random.multinomial(n=1, pvals=true_phi_kv[k], size=1).reshape(V) # one-hot符号
        v, = np.where(onehot_v == 1) # 語彙番号
        v  = v.item()

        # 語彙を割当
        tmp_w_n[n] = v
        
        # 頻度をカウント
        N_dv[d, v] += 1

        # データを記録
        tmp_word_dic[n] = (k, v, N_dv[d, v]) # トピック番号, 語彙番号, 頻度
    
    # データを記録
    w_dic[d]        = tmp_w_n.astype('int').copy()
    true_z_dic[d]   = tmp_z_n.astype('int').copy()
    true_doc_dic[d] = tmp_word_dic.copy()
    
    # 途中経過を表示
    print(
        'document: ' + str(d+1) + ', ' + 
        'words: '    + str(N_d[d]) + ', ' + 
        'topics: '   + str(np.unique(true_z_dic[d]))
    )

# %%

### 真のトピック集合の可視化

# 配列に変換
N_dk = np.zeros(shape=(D, true_K)) # 各文書におけるトピックごとの単語数
for d in range(D):
    for n in range(N_d[d]):
        k, v, i = true_doc_dic[d][n]
        N_dk[d, k] += 1

# グラフサイズを設定
u = 5
axis_Ndv_max = (np.ceil(N_dv.max() /u)*u).astype('int') # u単位で切り上げ
axis_Ndk_max = (np.ceil(N_dk.max() /u)*u).astype('int') # u単位で切り上げ

# 配色の共通化用のカラーマップを作成
cmap = plt.get_cmap("tab10")
color_num = 10 # (カラーマップに応じて固定)

# 文書データを作図
fig, axes = plt.subplots(nrows=D, ncols=2, constrained_layout=True, 
                         figsize=(24, 30), facecolor='white')
fig.supylabel('document ($d$)')
fig.suptitle('document data (true topic)', fontsize=20)

for d in range(D):

    # 配列に変換
    z_nv = np.tile(np.nan, reps=(axis_Ndv_max, V))
    for n in range(N_d[d]):
        k, v, i = true_doc_dic[d][n]
        z_nv[i-1, v] = k % color_num # (配色の共通化用)
    
    # 単語のトピック・語彙の頻度を描画
    ax = axes[d, 0]
    ax.pcolor(z_nv, cmap=cmap, vmin=0, vmax=color_num-1) # 単語データ
    ax.set_xlabel('vocabulary ($v$)')
    ax.set_ylabel('frequency ($N_{dv}$)')
    ax.set_title('$d = {}, N_d = {}$'.format(d+1, N_d[d]), loc='left')
    ax.grid()

    # 語彙の出現順を描画
    for n in range(N_d[d]):
        k, v, i = true_doc_dic[d][n]
        ax.text(x=v+0.5, y=i-0.5, s=str(n+1), 
                size=7, ha='center', va='center') # 単語番号
    
    # トピックの割当を描画
    ax = axes[d, 1]
    ax.bar(x=np.arange(stop=true_K)+1, height=N_dk[d], 
           color=[cmap(k%color_num) for k in range(true_K)]) # 単語数
    ax.set_ylim(ymin=0, ymax=axis_Ndk_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('frequency ($N_{dk}$)')
    ax.set_title('$K = {}$'.format(true_K), loc='left')
    ax.grid()

plt.show()

# %%

### 真のトピック分布の可視化

# グラフサイズを設定
u = 0.1
axis_size = np.ceil(true_theta_dk.max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < D)
col_num = 4

# サブプロットの行数を計算:(1行or1列だとエラー)
row_num = np.ceil(D / col_num).astype('int')

# トピック分布を作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(15, 15), dpi=100, facecolor='white')

for d in range(D):
    
    # サブプロットを抽出
    r = d // col_num
    c = d % col_num
    ax = axes[r, c]

    # トピック分布を作図
    ax.bar(x=np.arange(stop=true_K)+1, height=true_theta_dk[d], 
           color=[cmap(k%color_num) for k in range(true_K)]) # 確率
    ax.set_ylim(ymin=0, ymax=axis_size)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('probability ($\\theta_{dk}$)')
    ax.set_title('$d = {}$'.format(d+1), loc='left')
    ax.grid()

# 余りを非表示化
for i in range(c+1, col_num):
    axes[r, i].axis('off')

fig.supxlabel('document ($d$)')
fig.supylabel('document ($d$)')
fig.suptitle('topic distribution (truth)', fontsize=20)
plt.show()

# %%

### 真の単語分布の可視化

# グラフサイズを設定
u = 0.01
axis_size = np.ceil(true_phi_kv.max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < K)
col_num = 3

# サブプロットの行数を計算:(1行or1列だとエラー)
row_num = np.ceil(true_K / col_num).astype('int')

# 語彙分布を作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(12, 9), dpi=100, facecolor='white')

for k in range(true_K):
    
    # サブプロットを抽出
    r = k // col_num
    c = k % col_num
    ax = axes[r, c]

    # 語彙分布を描画
    ax.bar(x=np.arange(stop=V)+1, height=true_phi_kv[k], 
           color=cmap(k%color_num)) # 確率
    ax.set_ylim(ymin=0, ymax=axis_size)
    ax.set_xlabel('vocabulary ($v$)')
    ax.set_ylabel('probability ($\phi_{kv}$)')
    ax.set_title('$k = {}$'.format(k+1), loc='left')
    ax.grid()

# 余りを非表示化
for i in range(c+1, col_num):
    axes[r, i].axis('off')

fig.supxlabel('topic ($k$)')
fig.supylabel('topic ($k$)')
fig.suptitle('word distribution (truth)', fontsize=20)
plt.show()

#%%

### パラメータの初期化

# 文書数を取得
D = N_dv.shape[0]

# 語彙数を取得
V = N_dv.shape[1]

# 各文書の単語数を取得
N_d = N_dv.sum(axis=1)

# トピック数を指定
K = 5

# ハイパーパラメータの初期値を指定
alpha_k = np.repeat(2.0, repeats=K) # トピック分布
beta_v  = np.repeat(2.0, repeats=V) # 語彙分布

# 単語ごとの割り当てトピックを初期化
z_dic = {d: np.repeat(np.nan, repeats=N_d[d]) for d in range(D)}

# 各文書における各トピックの割り当て数(単語数)を初期化
N_dk = np.zeros(shape=(D, K))

# 各語彙に対する各トピックの割り当て数(単語数)を初期化
N_kv = np.zeros(shape=(K, V))

# %%

### 推論

# 試行回数を指定
max_iter = 1000

# 初期値を記録
trace_alpha_lt = [alpha_k.copy()]
trace_beta_lt  = [beta_v.copy()]
trace_Ndk_lt   = [N_dk.copy()]
trace_Nkv_lt   = [N_kv.copy()]
trace_z_lt     = [z_dic.copy()]

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
                k = tmp_z_n[n]

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
    trace_alpha_lt.append(alpha_k.copy())
    trace_beta_lt.append(beta_v.copy())
    trace_Ndk_lt.append(N_dk.copy())
    trace_Nkv_lt.append(N_kv.copy())
    trace_z_lt.append(z_dic.copy())

    # 途中経過を表示
    print('iteration: '+str(i+1))

# %%

### 推定したトピック集合の可視化

# グラフサイズを設定
u = 5
axis_Ndv_max = (np.ceil(N_dv.max() /u)*u).astype('int') # u単位で切り上げ
axis_Ndk_max = (np.ceil(N_dk.max() /u)*u).astype('int') # u単位で切り上げ

# 配色の共通化用のカラーマップを作成
cmap = plt.get_cmap("tab10")
color_num = 10 # (カラーマップに応じて固定)

# 文書データを作図
fig, axes = plt.subplots(nrows=D, ncols=2, constrained_layout=True, 
                         figsize=(24, 30), facecolor='white')
fig.supylabel('document ($d$)')
fig.suptitle('document data (estimated topic)', fontsize=20)

for d in range(D):

    # 配列に変換
    z_nv = np.tile(np.nan, reps=(axis_Ndv_max, V))
    for n in range(N_d[d]):
        v = w_dic[d][n]
        k = z_dic[d][n]
        i = np.sum(w_dic[d][:n+1] == v)
        z_nv[i-1, v] = k % color_num # (配色の共通化用)
    
    # 単語のトピック・語彙の頻度を描画
    ax = axes[d, 0]
    ax.pcolor(z_nv, cmap=cmap, vmin=0, vmax=color_num-1) # 単語データ
    ax.set_xlabel('vocabulary ($v$)')
    ax.set_ylabel('frequency ($N_{dv}$)')
    ax.set_title('$d = {}, N_d = {}$'.format(d+1, N_d[d]), loc='left')
    ax.grid()

    # 語彙の出現順を描画
    for n in range(N_d[d]):
        v = w_dic[d][n]
        k = z_dic[d][n]
        i = np.sum(w_dic[d][:n+1] == v)
        ax.text(x=v+0.5, y=i-0.5, s=str(n+1), 
                size=7, ha='center', va='center') # 単語番号
    
    # トピックの割当を描画
    ax = axes[d, 1]
    ax.bar(x=np.arange(stop=K)+1, height=N_dk[d], 
           color=[cmap(k%color_num) for k in range(K)]) # 単語数
    ax.set_ylim(ymin=0, ymax=axis_Ndk_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('frequency ($N_{dk}$)')
    ax.set_title('$K = {}$'.format(K), loc='left')
    ax.grid()

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
axis_Ndv_max = (np.ceil(N_dv.max() /u)*u).astype('int') # u単位で切り上げ
axis_Ndk_max = (np.ceil(max([trace_Ndk_lt[i].max() for i in range(max_iter+1)]) /u)*u).astype('int') # u単位で切り上げ

# 配色の共通化用のカラーマップを作成
cmap = plt.get_cmap("tab10")
color_num = 10 # (カラーマップに応じて固定)

# グラフオブジェクトを初期化
fig, axes = plt.subplots(nrows=D, ncols=2, constrained_layout=True, 
                         figsize=(24, 30), facecolor='white')
fig.supylabel('document ($d$)')
fig.suptitle('document data (estimated topic)', fontsize=20)

# 作図処理を定義
def update(i):
    
    # 試行回数を調整
    i *= iter_per_frame
    
    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes[:, 0]]
    [ax.cla() for ax in axes[:, 1]]
    
    for d in range(D):
        
        # 配列に変換
        z_nv = np.tile(np.nan, reps=(axis_Ndv_max, V))
        for n in range(N_d[d]):
            v = w_dic[d][n]
            k = trace_z_lt[i][d][n]
            m = np.sum(w_dic[d][:n+1] == v)
            z_nv[m-1, v] = k % color_num # (配色の共通化用)
        
        # 単語のトピック・語彙の頻度を描画
        ax = axes[d, 0]
        ax.pcolor(z_nv, cmap=cmap, vmin=0, vmax=color_num-1) # 単語データ
        ax.set_xlabel('vocabulary ($v$)')
        ax.set_ylabel('frequency ($N_{dv}$)')
        ax.set_title('$d = {}, N_d = {}, iteration: {}$'.format(d+1, N_d[d], i), loc='left')
        ax.grid()

        # 語彙の出現順を描画
        for n in range(N_d[d]):
            v = w_dic[d][n]
            k = trace_z_lt[i][d][n]
            m = np.sum(w_dic[d][:n+1] == v)
            ax.text(x=v+0.5, y=m-0.5, s=str(n+1), 
                    size=7, ha='center', va='center') # 単語番号
        
        # トピックの割当を描画
        ax = axes[d, 1]
        ax.bar(x=np.arange(stop=K)+1, height=trace_Ndk_lt[i][d], 
            color=[cmap(k%color_num) for k in range(K)]) # 単語数
        ax.set_ylim(ymin=0, ymax=axis_Ndk_max)
        ax.set_xlabel('topic ($k$)')
        ax.set_ylabel('frequency ($N_{dk}$)')
        ax.set_title('$K = {}$'.format(K), loc='left')
        ax.grid()

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/ch4/ch4_5_doc_data.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)

# %%

### 推定したトピック分布のパラメータの推移の可視化

# 配列に変換
trace_alpha_ik = np.array(trace_alpha_lt)

# グラフサイズを設定
u = 0.5
axis_size = np.ceil(trace_alpha_ik.max()/u)*u # u単位で切り上げ

# ハイパーパラメータの推移を作図
fig, ax = plt.subplots(figsize=(8, 6), facecolor='white')
for k in range(K):
    ax.plot(np.arange(max_iter+1), trace_alpha_ik[:, k]) # 更新値
ax.set_ylim(ymin=0, ymax=axis_size)
ax.set_xlabel('iteration')
ax.set_ylabel('value ($\\alpha_k$)')
fig.suptitle('parameter of topic distribution', fontsize=20)
ax.grid()
plt.show()

# %%

### 推定した単語分布のパラメータの推移の可視化

# 配列に変換
trace_beta_iv = np.array(trace_beta_lt)

# グラフサイズを設定
u = 0.5
axis_size = np.ceil(trace_beta_iv.max()/u)*u # u単位で切り上げ

# ハイパーパラメータの推移を作図
fig, ax = plt.subplots(figsize=(8, 6), facecolor='white')
for v in range(V):
    ax.plot(np.arange(max_iter+1), trace_beta_iv[:, v]) # 更新値
ax.set_xlabel('iteration')
ax.set_ylabel('value ($\\beta_v$)')
fig.suptitle('parameter of word distribution', fontsize=20)
ax.set_ylim(ymin=0, ymax=axis_size)
ax.grid()
plt.show()

# %%

### 推定したトピック分布の可視化

# トピック分布を計算
theta_dk = (N_dk + alpha_k) / (N_d + alpha_k.sum()).reshape((D, 1))

# グラフサイズを設定
u = 0.1
axis_size = np.ceil(theta_dk.max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < D)
col_num = 4

# サブプロットの行数を計算:(1行or1列だとエラー)
row_num = np.ceil(D / col_num).astype('int')

# トピック分布を作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(15, 15), dpi=100, facecolor='white')

for d in range(D):
    
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
    ax.set_title('$d = {}$'.format(d+1), loc='left')
    ax.grid()

# 余りを非表示化
for i in range(c+1, col_num):
    axes[r, i].axis('off')

fig.supxlabel('document ($d$)')
fig.supylabel('document ($d$)')
fig.suptitle('topic distribution (estimated)', fontsize=20)
plt.show()

# %%

### 推定したトピック分布の推移の可視化

# フレーム数を指定
#frame_num = max_iter + 1
frame_num = 100

# 1フレーム当たりの試行回数を設定
iter_per_frame = (max_iter + 1) // frame_num

# グラフサイズを設定
u = 5
axis_freq_max = np.ceil(max([(trace_Ndk_lt[i] + trace_alpha_lt[i]).max() for i in range(max_iter+1)]) /u)*u # u単位で切り上げ
axis_prob_max = 1.0

# グラフオブジェクトを初期化
fig, axes = plt.subplots(nrows=D+1, ncols=2, constrained_layout=True, 
                         figsize=(12, 18), facecolor='white')
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

    # ハイパーパラメータを描画
    ax = axes[0, 0]
    ax.bar(x=np.arange(stop=K)+1, height=alpha_k, 
           color=[cmap(k%color_num) for k in range(K)]) # 全文書で共通の値
    ax.set_ylim(ymin=0, ymax=axis_freq_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('value ($\\alpha_k$)')
    ax.set_title('iteration: '+str(i), loc='left')
    ax.grid()

    # 余りを非表示化
    axes[0, 1].axis('off')
    
    for d in range(D):
        
        # トピックごとの単語数を描画
        ax = axes[d+1, 0]
        ax.bar(x=np.arange(stop=K)+1, height=N_dk[d], 
               color=[cmap(k%color_num) for k in range(K)]) # 度数
        ax.bar(x=np.arange(stop=K)+1, height=alpha_k, bottom=N_dk[d], 
               color=[cmap(k%color_num) for k in range(K)], 
               edgecolor=[cmap(k%color_num) for k in range(K)], 
               alpha=0.5, linewidth=1, linestyle='dashed') # ハイパラ
        ax.set_ylim(ymin=0, ymax=axis_freq_max)
        ax.set_xlabel('topic ($k$)')
        ax.set_ylabel('count ($N_{dk}$)')
        ax.set_title('$d = {}$'.format(d+1), loc='left')
        ax.grid()

        # トピック分布を描画
        ax = axes[d+1, 1]
        ax.bar(x=np.arange(stop=K)+1, height=theta_dk[d], 
               color=[cmap(k%color_num) for k in range(K)]) # 確率
        ax.set_ylim(ymin=0, ymax=axis_prob_max)
        ax.set_xlabel('topic ($k$)')
        ax.set_ylabel('probability ($\\theta_{dk}$)')
        ax.set_title('$d = {}$'.format(d+1), loc='left')
        ax.grid()

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/ch4/ch4_5_topic_dist.mp4', dpi=100, 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)

# %%

### 推定した単語分布の可視化

# 単語分布を計算
phi_kv = (N_kv + beta_v) / (N_kv.sum(axis=1) + beta_v.sum()).reshape((K, 1))

# グラフサイズを設定
u = 0.05
axis_size = np.ceil(phi_kv.max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < K)
col_num = 3

# サブプロットの行数を計算:(1行or1列だとエラー)
row_num = np.ceil(K / col_num).astype('int')

# 語彙分布を作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(12, 9), dpi=100, facecolor='white')

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
    ax.set_title('$k = {}$'.format(k+1), loc='left')
    ax.grid()

# 余りを非表示化
for i in range(c+1, col_num):
    axes[r, i].axis('off')

fig.supxlabel('topic ($k$)')
fig.supylabel('topic ($k$)')
fig.suptitle('word distribution (estimated)', fontsize=20)
plt.show()

# %%

### 推定した単語分布の推移の可視化

# フレーム数を指定
#frame_num = max_iter + 1
frame_num = 10

# 1フレーム当たりの試行回数を設定
iter_per_frame = (max_iter + 1) // frame_num

# グラフサイズを設定
u = 5
axis_freq_max = np.ceil(max([(trace_Nkv_lt[i] + trace_beta_lt[i]).max() for i in range(max_iter+1)]) /u)*u # u単位で切り上げ
axis_prob_max = 0.5

# グラフオブジェクトを初期化
fig, axes = plt.subplots(nrows=K+1, ncols=2, constrained_layout=True, 
                         figsize=(15, 15), facecolor='white')
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
    
    # 単語分布を計算
    phi_kv  = N_kv + beta_v
    phi_kv /= phi_kv.sum(axis=1, keepdims=True) # 正規化
    
    # ハイパーパラメータを描画
    ax = axes[0, 0]
    ax.bar(x=np.arange(stop=V)+1, height=beta_v, 
           color='brown') # 全文書で共通の値
    ax.set_ylim(ymin=0, ymax=axis_freq_max)
    ax.set_xlabel('vocabulary ($v$)')
    ax.set_ylabel('value ($\\beta_v$)')
    ax.set_title('iteration: '+str(i), loc='left')
    ax.grid()

    # 余りを非表示化
    axes[0, 1].axis('off')
    
    for k in range(K):
        
        # トピックごとの単語数を描画
        ax = axes[k+1, 0]
        ax.bar(x=np.arange(stop=V)+1, height=N_kv[k], 
               color=cmap(k%color_num)) # 度数
        ax.bar(x=np.arange(stop=V)+1, height=beta_v, bottom=N_kv[k], 
               color='brown', edgecolor='brown', 
               alpha=0.5, linewidth=1, linestyle='dashed') # ハイパラ
        ax.set_ylim(ymin=0, ymax=axis_freq_max)
        ax.set_xlabel('vocabulary ($v$)')
        ax.set_ylabel('count ($N_{kv}$)')
        ax.set_title('$k = {}$'.format(k+1), loc='left')
        ax.grid()

        # 語彙分布を描画
        ax = axes[k+1, 1]
        ax.bar(x=np.arange(stop=V)+1, height=phi_kv[k], 
               color=cmap(k%color_num)) # 確率
        ax.set_ylim(ymin=0, ymax=axis_prob_max)
        ax.set_xlabel('vocabulary ($v$)')
        ax.set_ylabel('probability ($\\phi_{kv}$)')
        ax.set_title('$k = {}$'.format(k+1), loc='left')
        ax.grid()

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/ch4/ch4_5_word_dist.mp4', dpi=100, 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)

# %%


