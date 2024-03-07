
# chapter 8.1 無限混合ユニグラムモデル：崩壊型ギブスサンプリング

# %%

# 利用ライブラリ
import numpy as np
from scipy.special import loggamma
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# %%

### 簡易文書データの作成：(中華料理店過程)

# 文書数を指定
D = 25

# 語彙数を指定
V = 240

# ハイパーパラメータを指定
true_alpha = 2.0 # トピック分布
true_beta  = 1.0 # 語彙分布

# トピック数を初期化
true_K = 0

# 受け皿を初期化
true_z_d = np.tile(np.nan, reps=D) # 各文書のトピック
true_D_k = np.zeros(shape=true_K)  # 各トピックが割り当てられた文書数
N_d  = np.zeros(shape=D, dtype='int')      # 各文書の単語数
N_dv = np.zeros(shape=(D, V), dtype='int') # 各文書の語彙ごとの単語数

# 文書データを生成
for d in range(D): # 文書ごと
    
    # トピック分布を生成
    if true_K == 0: # (初回の場合)
        true_theta_k = np.array([1.0])
    else:
        true_theta_k  = np.hstack([true_D_k, true_alpha]) / (d + true_alpha) # 既存・新規の確率を結合
    
    # トピックを生成
    onehot_k = np.random.multinomial(n=1, pvals=true_theta_k, size=1).reshape(true_K+1) # one-hot符号
    k, = np.where(onehot_k == 1) # トピック番号
    k  = k.item()
    
    # トピックを割当
    true_z_d[d] = k

    if k == true_K: # (新規で割り当てられた場合)
        
        # トピックを追加
        true_K  += 1
        true_D_k = np.hstack([true_D_k, 0])

        # 語彙分布を生成
        if true_K == 1: # (初回の場合)
            true_phi_kv = np.random.dirichlet(alpha=np.repeat(true_beta, repeats=V), size=1)
        else:
            true_phi_kv = np.vstack(
                [true_phi_kv, np.random.dirichlet(alpha=np.repeat(true_beta, repeats=V), size=1)] # 既存・新規の確率を結合
            )
    
    # 割り当て数をカウント
    true_D_k[k] += 1

    # 単語数を生成
    N_d[d] = np.random.randint(low=100, high=200, size=1) # 下限・上限を指定

    for n in range(N_d[d]): # 単語ごと

        # 語彙を生成
        onehot_v = np.random.multinomial(n=1, pvals=true_phi_kv[k], size=1).reshape(V) # one-hot符号
        v, = np.where(onehot_v == 1) # 語彙番号
        v  = v.item()

        # 頻度をカウント
        N_dv[d, v] += 1
    
    # 途中経過を表示
    print(
        'document: ' + str(d+1) + ', ' + 
        'words: '    + str(N_d[d]) + ', ' + 
        'topic: '    + str(k+1)
    )

# 作図用に変換
true_z_d = true_z_d.astype('int')

# %%

### 真のトピック集合の可視化

# グラフサイズを設定
u = 5
axis_size = np.ceil(N_dv.max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < D)
col_num = 5

# サブプロットの行数を計算:(1行or1列だとエラー)
row_num = np.ceil(D / col_num).astype('int')

# 配色の共通化用のカラーマップを作成
cmap = plt.get_cmap("tab10")
color_num = 10 # (カラーマップに応じて固定)

# 文書データを作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(30, 20), dpi=100, facecolor='white')

for d in range(D):
    
    # サブプロットを抽出
    r = d // col_num
    c = d % col_num
    ax = axes[r, c]

    # トピック番号を取得
    k = true_z_d[d]
    
    # 語彙頻度を描画
    ax.bar(x=np.arange(stop=V)+1, height=N_dv[d], 
           color=cmap(k%color_num)) # 頻度
    ax.set_ylim(ymin=0, ymax=axis_size)
    ax.set_xlabel('vocabulary ($v$)')
    ax.set_ylabel('frequency ($N_{dv}$)')
    ax.set_title('$d = {}, k = {}$'.format(d+1, k+1), loc='left')
    ax.grid()

# 余りを非表示化
for i in range(c+1, col_num):
    axes[r, i].axis('off')

fig.supxlabel('document ($d$)')
fig.supylabel('document ($d$)')
fig.suptitle('document data (true topic)', fontsize=20)

plt.show()

# %%

### 真のトピック分布の可視化

# D+1番目の文書のトピック分布を計算
true_D_k = np.array([np.sum(true_z_d == k) for k in range(true_K)])
true_theta_k = np.hstack(
    [true_D_k / (D + true_alpha)] + [true_alpha / (D + true_alpha)] # 既存・新規の確率を結合
)
true_theta_k /= true_theta_k.sum() # 正規化

# グラフサイズを設定
u = 0.1
axis_size = np.ceil(true_theta_k.max() /u)*u # u単位で切り上げ

# トピック分布を作図
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
ax.bar(x=np.arange(stop=true_K+1)+1, height=true_theta_k, 
       color=[cmap(k%color_num) for k in range(true_K)]+['whitesmoke'], 
       edgecolor='black', linestyle='dashed', linewidth=[0]*true_K+[1]) # 確率
ax.set_ylim(ymin=0, ymax=axis_size)
ax.set_xlabel('topic ($k$)')
ax.set_ylabel('probability ($\\theta_k$)')
ax.set_title('$K = {}, \\alpha = {}$'.format(true_K, true_alpha), loc='left')
fig.suptitle('topic distribution (truth)', fontsize=20)
ax.grid()
plt.show()

# %%

### 真のトピック分布の推移の可視化

# グラフサイズを設定
u = 5
axis_freq_max = np.ceil(true_D_k.max() /u)*u # u単位で切り上げ
axis_prob_max = 1.0

# グラフオブジェクトを初期化
fig, axes = plt.subplots(nrows=1, ncols=2, constrained_layout=True, 
                         figsize=(12, 9), dpi=100, facecolor='white')
fig.suptitle('topic distribution (truth)', fontsize=20)

# 作図処理を定義
def update(d):
    
    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes]
    
    # トピック数を設定
    if d == 0: # (初回の場合)
        tmp_K = 0
    else:
        tmp_K = true_z_d[:d].max() + 1
    
    # トピックごとの文書数を集計
    tmp_D_k = np.array([np.sum(true_z_d[:d] == k) for k in range(true_K)])
    
    # トピックごとの文書数を描画
    ax = axes[0]
    ax.bar(x=np.arange(stop=true_K)+1, height=tmp_D_k, 
           color=[cmap(k%color_num) for k in range(true_K)]) # 文書数
    ax.set_xlim(xmin=0, xmax=true_K+2)
    ax.set_ylim(ymin=0, ymax=axis_freq_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('frequency ($D_k$)')
    ax.set_title('$D = {}, K = {}$'.format(d, tmp_K), loc='left')
    ax.grid()

    # トピック分布を計算
    if tmp_K == 0: # (初回の場合)
        tmp_theta_k = np.array([1.0])
    else:
        tmp_theta_k = np.hstack(
            [tmp_D_k[:tmp_K] / (d + true_alpha)] + [true_alpha / (d + true_alpha)] # 既存・新規の確率を結合
        )
        tmp_theta_k /= tmp_theta_k.sum() # 正規化
    
    # トピック分布を描画
    ax = axes[1]
    ax.bar(x=np.arange(stop=tmp_K+1)+1, height=tmp_theta_k, 
           color=[cmap(k%color_num) for k in range(tmp_K)]+['whitesmoke'], 
           edgecolor='black', linestyle='dashed', linewidth=[0]*tmp_K+[1]) # 確率
    ax.set_xlim(xmin=0, xmax=true_K+2)
    ax.set_ylim(ymin=0, ymax=axis_prob_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('probability ($\\theta_k$)')
    ax.set_title('$\\alpha = {}$'.format(true_alpha), loc='left')
    ax.grid()

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=D+1, interval=500)

# 動画を書出
ani.save(
    filename='../figure/ch8/ch8_1_tocip_dist.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)

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

# %%

### パラメータの初期化

# 文書数を取得
D = N_dv.shape[0]

# 語彙数を取得
V = N_dv.shape[1]

# 各文書の単語数を取得
N_d = N_dv.sum(axis=1)

# ハイパーパラメータを指定
alpha = 2.0 # トピック分布
beta  = 2.0 # 語彙分布

# トピック数を初期化
K = 0

# 文書ごとのトピックを初期化
z_d = np.tile(np.nan, reps=D)

# 各文書に対する各トピックの割り当て数(文書数)を初期化
D_k = np.zeros(shape=K)

# 各語彙に対する各トピックの割り当て数(単語数)を初期化
N_kv = np.zeros(shape=(K, V))

# %%

### 推論

# 試行回数を指定
max_iter = 1000

# 記録用の受け皿を初期化
trace_z_lt = []

# 崩壊型ギブスサンプリング
for i in range(max_iter):

    for d in range(D):
        if not np.isnan(z_d[d]): # (初回は不要)

            # 前ステップのトピックを取得
            k = z_d[d].astype('int')

            # ディスカウント
            D_k[k]  -= 1
            N_kv[k] -= N_dv[d]

            # トピックを削除
            if D_k[k] == 0: # (既存の割り当てがなくなった場合)
                K   -= 1
                D_k  = np.delete(D_k, obj=k)
                N_kv = np.delete(N_kv, obj=k, axis=0)
                del_k = k
                z_d  = np.array([k if k < del_k else k-1 for k in z_d])
        
        # 既存トピックのサンプリング確率を計算
        log_prob_z_k  = np.log(D_k)
        log_prob_z_k += loggamma(N_kv.sum(axis=1) + V*beta) - loggamma(N_kv.sum(axis=1) + N_d[d] + V*beta)
        log_prob_z_k += (loggamma(N_kv + N_dv[d] + beta) - loggamma(N_kv + beta)).sum(axis=1)

        # 新規トピックのサンプリング確率を計算
        log_prob_z_val  = np.log(alpha)
        log_prob_z_val += loggamma(V*beta) - loggamma(N_d[d] + V*beta)
        log_prob_z_val += (loggamma(N_dv[d] + beta) - loggamma(beta)).sum()

        # サンプリング確率を作成
        prob_z_k = np.hstack([log_prob_z_k, log_prob_z_val]) # 既存・新規の確率を結合
        prob_z_k = np.exp(prob_z_k - prob_z_k.min()) # (アンダーフロー対策)
        prob_z_k /= prob_z_k.sum() # 正規化
        
        # トピックをサンプリング
        k = np.random.choice(a=np.arange(K+1), size=1, p=prob_z_k).item() # カテゴリ乱数

        # トピックを割当
        z_d[d] = k
        
        # トピックを追加
        if k == K: # (新規で割り当てられた場合)
            K   += 1
            D_k  = np.hstack([D_k, 0])
            N_kv = np.vstack([N_kv, np.zeros(shape=(1, V))])
        
        # カウント
        D_k[k]  += 1
        N_kv[k] += N_dv[d]
    
    # 結果を記録
    trace_z_lt.append(z_d.astype('int').copy())
    
    # 途中経過を表示
    print('iteration: '+str(i+1) + ', K = '+str(K))

# 作図用に整数型に変換
z_d = z_d.astype('int')

# %%
    
### トピック集合の可視化

# グラフサイズを設定
u = 5
axis_size = np.ceil(N_dv.max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < D)
col_num = 5

# サブプロットの行数を計算:(1行or1列だとエラー)
row_num = np.ceil(D / col_num).astype('int')

# 配色の共通化用のカラーマップを作成
cmap = plt.get_cmap("tab10")
color_num = 10 # (カラーマップに応じて固定)

# 文書データを作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(30, 20), dpi=100, facecolor='white')

for d in range(D):
    
    # サブプロットを抽出
    r = d // col_num
    c = d % col_num
    ax = axes[r, c]

    # トピック番号を取得
    k = true_z_d[d]
    
    # 語彙頻度を描画
    ax.bar(x=np.arange(stop=V)+1, height=N_dv[d], 
           color=cmap(k%color_num)) # 頻度
    ax.set_ylim(ymin=0, ymax=axis_size)
    ax.set_xlabel('vocabulary ($v$)')
    ax.set_ylabel('frequency ($N_{dv}$)')
    ax.set_title('$d = {}, k = {}$'.format(d+1, k+1), loc='left')
    ax.grid()

# 余りを非表示化
for i in range(c+1, col_num):
    axes[r, i].axis('off')

fig.supxlabel('document ($d$)')
fig.supylabel('document ($d$)')
fig.suptitle('document data (estimated topic)', fontsize=20)
plt.show()

# %%

### トピック集合の推移の可視化

# 配列に変換
trace_z_di = np.array(trace_z_lt).T

# グラフサイズを設定
u = 1
axis_size = trace_z_di.max()+1 + u # 余白uを追加

# トピックの推移を作図
fig, ax = plt.subplots(figsize=(15, 15), dpi=100, facecolor='white', 
                       subplot_kw={'projection': '3d'})
for d in range(D):
    ax.plot(np.arange(max_iter), np.repeat(d+1, repeats=max_iter), trace_z_di[d]+1, 
            color=cmap(z_d[d].astype('int')%color_num)) # トピック番号
ax.set_zlim(zmin=0, zmax=axis_size)
ax.set_xlabel('iteration')
ax.set_ylabel('document ($d$)')
ax.set_zlabel('topic ($z_d$)')
fig.suptitle('Topic assigned to document', fontsize=20)
ax.view_init(elev=5, azim=300) # 表示アングル
plt.show()

# %%
