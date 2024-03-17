
# chapter 8.1
# 無限混合ユニグラムモデル
# 崩壊型ギブスサンプリング
# パラメータ部分未完

# %%

# 利用ライブラリ
import numpy as np
from scipy.special import loggamma
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# %%

### 簡易文書データの作成：(中華料理店過程)

# 文書数を指定
D = 30

# 語彙数を指定
V = 100

# ハイパーパラメータを指定
true_alpha  = 2.0 # トピック分布
true_beta  = 1.0 # 語彙分布

# トピック数を初期化
true_K = 0

# 受け皿を初期化
w_dic = {} # 文書ごとの各単語の語彙
true_z_d = np.tile(np.nan, reps=D) # 各文書のトピック
true_D_k = np.array([])  # 各トピックが割り当てられた文書数
N_d  = np.zeros(shape=D, dtype='int')      # 各文書の単語数
N_dv = np.zeros(shape=(D, V), dtype='int') # 文書ごとの各語彙の単語数

# 文書データを生成
for d in range(D): # 文書ごと
    
    # トピック分布を生成
    if true_K == 0: # (初回の場合)
        true_theta_k = np.array([1.0])
    else:
        true_theta_k  = np.hstack([true_D_k, true_alpha]) / (d + true_alpha) # 既存・新規の確率を結合
    
    # トピックを生成
    k = np.random.choice(a=np.arange(true_K+1), size=1, p=true_theta_k).item() # (カテゴリ乱数)
    
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
                [
                    true_phi_kv, 
                    np.random.dirichlet(alpha=np.repeat(true_beta, repeats=V), size=1)
                ]
            ) # 既存・新規の確率を結合
    
    # 割り当て数をカウント
    true_D_k[k] += 1

    # 単語数を生成
    N_d[d] = np.random.randint(low=100, high=200, size=1) # 下限・上限を指定

    # 各単語の語彙を初期化
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
    print(f'document: {d+1}, words: {N_d[d]}, topic: {k+1}')

# %%

### パラメータの初期化

# 文書数を取得
D = N_dv.shape[0]

# 語彙数を取得
V = N_dv.shape[1]

# 各文書の単語数を取得
N_d = N_dv.sum(axis=1)

# ハイパーパラメータの初期値を指定
alpha = 1.0 # トピック分布
beta  = 1.0 # 語彙分布

# トピック数を初期化
K = 0

# 各文書のトピックを初期化
z_d = np.tile(np.nan, reps=D)

# 各トピックの割り当て文書数を初期化
D_k = np.array([])

# 語彙ごとの各トピックの割り当て単語数を初期化
N_kv = np.zeros(shape=(K, V))

# 各トピックの割り当て単語数を初期化
N_k = np.array([])

# %%

### 推論

# 試行回数を指定
max_iter = 1000

# 記録用の受け皿を初期化
trace_z_lt   = [z_d.copy()]
trace_Dk_lt  = [D_k.copy()]
trace_Nkv_lt = [N_kv.copy()]

# 崩壊型ギブスサンプリング
for i in range(max_iter): # 繰り返し試行

    for d in range(D): # 文書ごと
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
        k = np.random.choice(a=np.arange(K+1), size=1, p=prob_z_k).item() # (カテゴリ乱数)

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
    
    # 更新値を記録
    trace_z_lt.append(z_d.astype('int').copy())
    trace_Dk_lt.append(D_k.copy())
    trace_Nkv_lt.append(N_kv.copy())
     
    # 途中経過を表示
    print(f'iteration: {i+1}, K = {K}')

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

# 描画する文書数を指定
#doc_num = D
doc_num = 10

# グラフサイズを設定
u = 5
axis_Ndv_max = np.ceil(N_dv[:doc_num].max() /u)*u # u単位で切り上げ
axis_Dk_max  = np.ceil(D_k.max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < D+1)
col_num = 3
row_num = np.ceil((doc_num+1) / col_num).astype('int')

# 文書データを作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(30, 20), dpi=100, facecolor='white')

for d in range(doc_num):
    
    # サブプロットを抽出
    r = d // col_num
    c = d % col_num
    ax = axes[r, c]

    # トピック番号を取得
    k = z_d[d]
    
    # 語彙頻度を描画
    ax.bar(x=np.arange(stop=V)+1, height=N_dv[d], 
           color=cmap(k%color_num)) # 頻度
    ax.set_ylim(ymin=0, ymax=axis_Ndv_max)
    ax.set_xlabel('vocabulary ($v$)')
    ax.set_ylabel('frequency ($N_{dv}$)')
    ax.set_title(f'iteration: {max_iter}, $d = {d+1}, k = {k+1}$', loc='left')
    ax.grid()

# 残りのサブプロットを非表示
if c == col_num-1: # (最後の文書が右端の列の場合)
    r = row_num - 1
    c = -1 
for c in range(c+1, col_num-1):
    axes[r, c].axis('off')

# トピックの割当を描画
ax = axes[row_num-1, col_num-1]
ax.bar(x=np.arange(stop=K)+1, height=D_k, 
        color=[cmap(k%color_num) for k in range(K)]) # 文書数
ax.set_ylim(ymin=0, ymax=axis_Dk_max)
ax.set_xlabel('topic ($k$)')
ax.set_ylabel('count ($D_k$)')
ax.set_title(f'iteration: {max_iter}, $D = {D}, K = {K}$', loc='left')
ax.grid()

fig.supxlabel('document ($d$)')
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
doc_num = 10

# グラフサイズを設定
u = 5
axis_Ndv_max = np.ceil(N_dv[:doc_num].max() /u)*u # u単位で切り上げ
axis_Dk_max  = np.ceil(max([trace_Dk_lt[i].max() for i in range(1, max_iter+1)]) /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < D+1)
col_num = 3
row_num = np.ceil((doc_num+1) / col_num).astype('int')

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
    tmp_K = len(D_k)

    for d in range(doc_num):
        
        # サブプロットを抽出
        r = d // col_num
        c = d % col_num
        ax = axes[r, c]

        # トピック番号を取得
        k = z_d[d]
        
        # 語彙頻度を描画
        ax.bar(x=np.arange(stop=V)+1, height=N_dv[d], 
            color=cmap(k%color_num)) # 頻度
        ax.set_ylim(ymin=0, ymax=axis_Ndv_max)
        ax.set_xlabel('vocabulary ($v$)')
        ax.set_ylabel('frequency ($N_{dv}$)')
        ax.set_title(f'iteration: {i}, $d = {d+1}, k = {k+1}$', loc='left')
        ax.grid()

    # 残りのサブプロットを非表示
    if c == col_num-1: # (最後の文書が右端の列の場合)
        r = row_num - 1
        c = -1 
    for c in range(c+1, col_num-1):
        axes[r, c].axis('off')

    # トピックの割当を描画
    ax = axes[row_num-1, col_num-1]
    ax.bar(x=np.arange(stop=tmp_K)+1, height=D_k, 
            color=[cmap(k%color_num) for k in range(tmp_K)]) # 文書数
    ax.set_xlim(xmin=0.5, xmax=K+0.5)
    ax.set_ylim(ymin=0, ymax=axis_Dk_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('count ($D_k$)')
    ax.set_title(f'iteration: {i}, $D = {D}, K = {tmp_K}$', loc='left')
    ax.grid()

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# 動画を書出
ani.save(
    filename='../figure/ch8/ch8_1_topic_set.mp4', dpi=100, 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)

# %%


