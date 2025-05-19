
# chapter 5.5
# トピック追跡モデル

# グラフィカルモデル表現 ---------------------------------------------------------------

# 利用パッケージ
library(DiagrammeR)
library(DiagrammeRsvg)


# グラフィカルモデルの作図 ------------------------------------------------------------

### ・1時刻のパラメータの場合 -----

# トピック追跡モデルのグラフィカルモデルを作図
graph <- DiagrammeR::grViz("
  digraph dot{
    
    graph [rankdir = LR, newrank = true]
    node  [shape = circle, fixedsize = ture, height = 0.6, fontname = 'Times-Italic']
    edge  []
    
    subgraph cluster_t_fin{
      label = ' '
      color = white
      
      theta_fin [label = ' ', shape = none]
      phi_fin   [label = ' ', shape = none]
      
      theta_fin -> phi_fin [penwidth = 0, dir = none];
    }
    
    
    subgraph cluster_d{
      label = 'D'
      
      alpha [label = '&alpha;@_{d}@^{(t)}']
      theta [label = <<B>&theta;</B>@_{d}@^{(t)}>]
      
      subgraph cluster_n{
        label = 'N@_{d}'
        
        z [label = 'z@_{dn}@^{(t)}']
        w [label = 'w@_{dn}@^{(t)}', style = filled, filledcolor = gray]
      }
    }
    
    subgraph cluster_k{
      label = 'K'
      
      beta [label = '&beta;@_{k}@^{(t)}']
      phi [label = <<B>&phi;</B>@_{'k}@^{(t)}>]
    }
    
    
    subgraph cluster_d_old{
      label = 'D'
      
      alpha_old [label = '&alpha;@_{d}@^{(t-1)}']
      theta_old [label = <<B>&theta;</B>@_{d}@^{(t-1)}>]
      
      subgraph cluster_n_old{
        label = 'N@_{d}'
        
        z_old [label = 'z@_{dn}@^{(t-1)}']
        w_old [label = 'w@_{dn}@^{(t-1)}', style = filled, filledcolor = gray]
      }
    }
    
    
    subgraph cluster_k_old{
      label = 'K'
      
      beta_old [label = '&beta;@_{k}@^{(t-1)}']
      phi_old [label = <<B>&phi;</B>@_{'k}@^{(t-1)}>]
    }
    
    subgraph cluster_t_init{
      color = white
      
      label    = 'topic tracking model'
      labelloc = c
      fontsize = 20
      
      theta_init [label = ' ', shape = none]
      phi_init   [label = ' ', shape = none]
      
      theta_init -> phi_init [penwidth = 0, dir = none];
    }
    
    {rank = same; w_old; w}
    {rank = same; theta_init; theta_old; theta; theta_fin}
    {rank = same; phi_init; phi_old; phi; phi_fin}
    
    alpha_old -> theta_old -> z_old -> w_old;
    w_old -> phi_old -> beta_old [dir = back];
    
    alpha -> theta -> z -> w;
    w -> phi -> beta [dir = back];
    
    theta_init -> theta_old -> theta -> theta_fin;
    phi_init -> phi_old -> phi -> phi_fin;
  }
")

## ( `{'k}` は書き出し時にφの添字が重なってしまう対策用の小細工)
## ( `_init, _fin` のクラスタは時間軸方向のエッジの長さ調整用の小細工)
## (ダミークラスタの余白が空くので図タイトルをクラスタタイトルで代用)
## ( 変数の定義順と上下の表示順が逆順になるのが謎)

# グラフを書出
DiagrammeRsvg::export_svg(gv = graph) |> # svgファイルに変換
  charToRaw() |> 
  rsvg::rsvg(height = 1500) |> # ビットマップに変換
  png::writePNG(target = "figure/graphical_model/topic_tracking_model.png", dpi = 100) # pngファイルに変換


### ・複数時刻のパラメータの場合 -----

# トピック追跡モデルのグラフィカルモデルを作図
graph <- DiagrammeR::grViz("
  digraph dot{
    
    graph [rankdir = LR, newrank = true]
    node  [shape = circle, fixedsize = ture, height = 0.6, fontname = 'Times-Italic']
    edge  []
    
    subgraph cluster_t_fin{
      label = ' '
      color = 'white'
      
      theta_fin [label = ' ', shape = none]
      phi_fin   [label = ' ', shape = none]
      
      theta_fin -> phi_fin [penwidth = 0, dir = none];
    }
    
    
    subgraph cluster_t_dummy{
      color = 'white'
      
      subgraph cluster_d_t{
        label = 'D'
        color = 'black'
        
        alpha_t [label = '&alpha;@_{d}@^{(t)}']
        theta_t [label = <<B>&theta;</B>@_{d}@^{(t)}>]
        
        subgraph cluster_n_t{
          label = 'N@_{d}'
          color = 'black'
          
          z_t [label = 'z@_{dn}@^{(t)}']
          w_t [label = 'w@_{dn}@^{(t)}', style = filled, filledcolor = gray]
        }
      }
      
      subgraph cluster_k_t{
        label = 'K'
        color = 'black'
        
        beta_t [label = '&beta;@_{k}@^{(t)}']
        phi_t [label = <<B>&phi;</B>@_{'k}@^{(t)}>]
      }
    }
    
    subgraph cluster_t1_dummy{
      color = 'white'
      
      subgraph cluster_d_t1{
        label = 'D'
        color = 'black'
        
        alpha_t1 [label = '&alpha;@_{d}@^{(t-1)}']
        theta_t1 [label = <<B>&theta;</B>@_{d}@^{(t-1)}>]
        
        subgraph cluster_n_t1{
          label = 'N@_{d}'
          color = 'black'
          
          z_t1 [label = 'z@_{dn}@^{(t-1)}']
          w_t1 [label = 'w@_{dn}@^{(t-1)}', style = filled, filledcolor = gray]
        }
      }
      
      subgraph cluster_k_t1{
        label = 'K'
        color = 'black'
        
        beta_t1 [label = '&beta;@_{k}@^{(t-1)}']
        phi_t1 [label = <<B>&phi;</B>@_{'k}@^{(t-1)}>]
      }
    }
    
    
    subgraph cluster_t2_dummy{
      color = 'white'
      
      subgraph cluster_d_t2{
        label = 'D'
        color = 'black'
        
        alpha_t2 [label = '&alpha;@_{d}@^{(t-2)}']
        theta_t2 [label = <<B>&theta;</B>@_{d}@^{(t-2)}>]
        
        subgraph cluster_n_t2{
          label = 'N@_{d}'
          color = 'black'
          
          z_t2 [label = 'z@_{dn}@^{(t-2)}']
          w_t2 [label = 'w@_{dn}@^{(t-2)}', style = filled, filledcolor = gray]
        }
      }
      
      subgraph cluster_k_t2{
        label = 'K'
        color = 'black'
        
        beta_t2 [label = '&beta;@_{k}@^{(t-2)}']
        phi_t2 [label = <<B>&phi;</B>@_{'k}@^{(t-2)}>]
      }
    }
    
    subgraph cluster_t_init{
      color = 'white'
      
      label    = 'topic tracking model'
      labelloc = c
      fontsize = 20
      
      theta_init [label = ' ', shape = none]
      phi_init   [label = ' ', shape = none]
      
      theta_init -> phi_init [penwidth = 0, dir = none];
    }
    
    
    {rank = same; theta_init; theta_t2; theta_t1; theta_t; theta_fin}
    {rank = same; phi_init; phi_t2; phi_t1; phi_t; phi_fin}
    
    alpha_t2 -> theta_t2 -> z_t2 -> w_t2;
    w_t2 -> phi_t2 -> beta_t2 [dir = back];
    
    alpha_t1 -> theta_t1 -> z_t1 -> w_t1;
    w_t1 -> phi_t1 -> beta_t1 [dir = back];
    
    alpha_t -> theta_t -> z_t -> w_t;
    w_t -> phi_t -> beta_t [dir = back];
    
    theta_init -> theta_t2 -> theta_t1;
    {theta_t2, theta_t1} -> theta_t -> theta_fin;
    phi_init -> phi_t2 -> phi_t1;
    {phi_t2, phi_t1} -> phi_t -> phi_fin;
  }
")

## ( `{'k}` は書き出し時にφの添字が重なってしまう対策用の小細工)
## ( `_init, _fin` のクラスタは時間軸方向のエッジの長さ調整用の小細工)
## (ダミークラスタの余白が空くので図タイトルをクラスタタイトルで代用)
## ( 変数の定義順と上下の表示順が逆順になるのが謎)
## ( dummy clusterは時刻ごとの縦方向のズレの調整用の小細工)

# グラフを書出
DiagrammeRsvg::export_svg(gv = graph) |> # svgファイルに変換
  charToRaw() |> 
  rsvg::rsvg(height = 2000) |> # ビットマップに変換
  png::writePNG(target = "figure/graphical_model/topic_tracking_model_LongDeps.png", dpi = 100) # pngファイルに変換


