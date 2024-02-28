
# chapter 8.1 無限混合ユニグラムモデル：ギブスサンプリング

# %%

# 利用ライブラリ
import numpy as np
from scipy.special import loggamma
import matplotlib.pyplot as plt

# %%

### 簡易文書データの作成

# 文書数を指定
D = 25

# 語彙数を指定
V = 240

# ハイパーパラメータを指定
alpha = 2.0


# 真のトピック数を初期化
true_K = 0

# 受け皿を初期化
true_z_d = np.tile(-1, reps=D) # 各文書の真のトピック
D_k  = np.zeros(shape=true_K)  # 各トピックが割り当てられた文書数
N_d  = np.zeros(shape=D)       # 各文書の単語数
N_dv = np.zeros(shape=(D, V))  # 各文書・語彙の単語数

# 文書データを生成
for d in range(D):
    
    # 真のトピック分布を生成
    if d == 0: # (初回の場合)
        true_theta_k = np.array([1.0])
    else:
        true_theta_k = np.array(
            [D_k[k] / (d + alpha) for k in range(true_K)] + [alpha / (d + alpha)] # 既存・新規の確率を結合
        )
        true_theta_k /= true_theta_k.sum() # 正規化
    
    # 真のトピックを生成
    onehot_k = np.random.multinomial(n=1, pvals=true_theta_k, size=1).reshape(true_K+1) # one-hot符号
    k, = np.where(onehot_k == 1) # トピック番号
    k  = k.item()
    
    # 真のトピックを割り当て
    true_z_d[d] = k

    # トピックを追加
    if k == true_K: # (新規で割り当てられた場合)
        true_K += 1
        D_k     = np.hstack([D_k, 0])

        # 真の単語分布を生成
        if d == 0: # (初回の場合)
            true_phi_kv = np.random.dirichlet(alpha=np.repeat(1, repeats=V), size=1) # ハイパーパラメータを指定
        else:
            true_phi_kv = np.vstack(
                [true_phi_kv] + [np.random.dirichlet(alpha=np.repeat(1, repeats=V), size=1)] # 既存・新規の確率を結合
            )
    
    # カウント
    D_k[k] += 1

    # 単語数を生成
    N_d[d] = np.random.randint(low=100, high=200, size=1) # 下限・上限を指定

    # 単語を生成
    N_dv[d] = np.random.multinomial(n=N_d[d], pvals=true_phi_kv[k], size=1)[0]

    # 途中経過を表示
    print('document: '+str(d+1) + ', topic: '+str(k+1) + ', words: '+str(N_d[d].astype('int')))

# %%

### 真のトピック集合の可視化

# グラフサイズを設定
axis_size = np.ceil(N_dv.max() /10)*10 # 10の位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < D)
col_num = 5

# サブプロットの行数を計算:(1行or1列だとエラー)
row_num = np.ceil(D / col_num).astype('int')

# 配色の共通化用のカラーマップを作成
cmap = plt.get_cmap("tab10")
cnum = 10 # (カラーマップごとに固定)

# 文書データを作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(30, 20), facecolor='white')

for d in range(D):
    
    # サブプロットを抽出
    r = d // col_num
    c = d % col_num
    ax = axes[r, c]

    # ラベル用の文字列を作成
    doc_label = 'document ($d$): '+str(d+1) + ', true topic ($k$): '+str(true_z_d[d]+1)
    
    # 単語頻度を作図
    ax.bar(x=np.arange(start=1, stop=V+1), height=N_dv[d], 
           color=cmap(true_z_d[d]%cnum)) # 頻度
    ax.set_ylim(ymin=0, ymax=axis_size)
    ax.set_title(doc_label, loc='left')
    ax.grid()

# 余ったサブプロットを非表示化
for i in range(c+1, col_num):
    axes[r, i].axis('off')

fig.supxlabel('vocabulary ($v$)')
fig.supylabel('frequency ($N_{dv}$)')
fig.suptitle('document data', fontsize=20)

# グラフを書出
plt.savefig(fname='../figure/ch8_1/doc_topic_true.png', dpi=100)

plt.show()

# %%

### 真のトピック分布の可視化

# 最後に新規トピックが割り当てられた場合は確率を追加
if len(true_theta_k) == true_K:
    true_theta_k = np.array(
        [D_k[k] / (d + alpha) for k in range(true_K)] + [alpha / (d + alpha)] # 既存・新規の確率を結合
    )
    true_theta_k /= true_theta_k.sum() # 正規化

# グラフサイズを設定
axis_size = np.ceil(true_theta_k.max() *10)/10 # 小数第1位で切り上げ

# トピック分布を作図
fig, ax = plt.subplots(figsize=(8, 6), facecolor='white')
ax.bar(x=np.arange(start=1, stop=true_K+2), height=true_theta_k, 
       color=[cmap(k%cnum) for k in range(true_K)]+['gray']) # 確率
ax.set_ylim(ymin=0, ymax=axis_size)
ax.set_xlabel('topic ($k$)')
ax.set_ylabel('probability ($\\theta_k$)')
ax.set_title('$\\alpha = {}, K = {}'.format(alpha, true_K), loc='left')
fig.suptitle('topic distribution (truth)', fontsize=20)
ax.grid()

# グラフを書出
plt.savefig(fname='../figure/ch8_1/topic_dist_true.png', dpi=100)

plt.show()

# %%

### 真の単語分布の可視化

# グラフサイズを設定
axis_size = np.ceil(true_phi_kv.max() *10)/10 # 小数第1位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < K)
col_num = 3

# サブプロットの行数を計算:(1行or1列だとエラー)
row_num = np.ceil(true_K / col_num).astype('int')

# 単語分布を作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(12, 9), facecolor='white')

for k in range(true_K):
    
    # サブプロットを抽出
    r = k // col_num
    c = k % col_num
    ax = axes[r, c]

    # 分布を作図
    ax.bar(x=np.arange(start=1, stop=V+1), height=true_phi_kv[k], 
           color=cmap(k%cnum)) # 確率
    ax.set_ylim(ymin=0, ymax=axis_size)
    ax.set_title('topic ($k$): '+str(k+1), loc='left')
    ax.grid()

# 余ったサブプロットを非表示化
for i in range(c+1, col_num):
    axes[r, i].axis('off')

fig.supxlabel('vocabulary ($v$)')
fig.supylabel('probability ($\phi_{kv}$)')
fig.suptitle('word distribution (trueth)', fontsize=20)

# グラフを書出
plt.savefig(fname='../figure/ch8_1/word_dist_true.png', dpi=100)

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
alpha = 2.0
beta  = 2.0

# トピック数を初期化
K = 0

# 文書ごとの割り当てトピックを初期化
z_d = np.tile(-1, reps=D) # (整数型の欠損値を使えなかったので-1で対応)

# 各文書に対する各トピックの割り当て数(文書数)を初期化
D_k = np.zeros(shape=K)

# 各語彙に対する各トピックの割り当て数(単語数)を初期化
N_kv = np.zeros(shape=(K, V))

# %%

### 推論

# 試行回数を指定
max_iter = 1000

# 記録用の受け皿を初期化
trace_z = []

# ギブスサンプリング
for i in range(max_iter):

    for d in range(D):
        if z_d[d] > -1: # (初回は不要)

            # 前ステップのトピックを取得
            k = z_d[d]

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

        # トピックを割り当て
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
    trace_z.append(z_d.copy())
    
    # 途中経過を表示
    print('iteration: '+str(i+1) + ', K = '+str(K))

# %%
    
### トピック集合の可視化

# グラフサイズを設定
axis_size = np.ceil(N_dv.max() /10)*10 # 10の位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < D)
col_num = 5

# サブプロットの行数を計算:(1行or1列だとエラー)
row_num = np.ceil(D / col_num).astype('int')

# 配色の共通化用のカラーマップを作成
cmap = plt.get_cmap("tab10")
cnum = 10 # (カラーマップごとに固定)

# 文書データを作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(30, 20), facecolor='white')

for d in range(D):
    
    # サブプロットを抽出
    r = d // col_num
    c = d % col_num
    ax = axes[r, c]

    # ラベル用の文字列を作成
    doc_label = 'document ($d$): '+str(d+1) + ', topic ($k$): '+str(z_d[d]+1)
    
    # 単語頻度を作図
    ax.bar(x=np.arange(start=1, stop=V+1), height=N_dv[d], 
           color=cmap(z_d[d]%cnum)) # 頻度
    ax.set_ylim(ymin=0, ymax=axis_size)
    ax.set_title(doc_label, loc='left')
    ax.grid()

# 余ったサブプロットを非表示化
for i in range(c+1, col_num):
    axes[r, i].axis('off')

fig.supxlabel('vocabulary ($v$)')
fig.supylabel('frequency ($N_{dv}$)')
fig.suptitle('document data', fontsize=20)

# グラフを書出
plt.savefig(fname='../figure/ch8_1/doc_topic_estimated.png', dpi=100)

plt.show()

# %%


