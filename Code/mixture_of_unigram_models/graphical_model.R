
# chapter 3
# 混合ユニグラムモデル

# グラフィカルモデル表現 ---------------------------------------------------------------

# 利用パッケージ
library(DiagrammeR)
library(DiagrammeRsvg)


# グラフィカルモデルの作図 ------------------------------------------------------------

### ・事前分布なしの場合 -----

# 混合ユニグラムモデル(事前分布なし)のグラフィカルモデルを作図
graph <- DiagrammeR::grViz("
  digraph dot{
    graph [rankdir = LR]
    node  [shape = circle]
      
    theta [label = <<B>&theta;</B>>]
    
    subgraph cluster_D{
      label = D
      
      z [label = 'z@_{d}']
      
      subgraph cluster_N{
        label = 'N@_{d}'
        
        w [label = 'w@_{dn}', style = filled, filledcolor = 'gray']
      }
    }
    
    edge []
      theta -> z -> w;
    
    subgraph cluster_K{
      label = K
      
      phi [label = <<B>&phi;</B>@_{'k}>]
    }
    
    edge []
      phi -> w;
  }
")

## ( `{'k}` は書き出し時にφの添字が重なってしまう対策用の小細工)

# グラフを書出
DiagrammeRsvg::export_svg(gv = graph) |> # svgファイルに変換
  charToRaw() |> 
  rsvg::rsvg(height = 1000) |> # ビットマップに変換
  png::writePNG(target = "figure/graphical_model/mixunigram_model.png", dpi = 100) # pngファイルに変換


### ・事前分布ありの場合 -----

# 混合ユニグラムモデル(事前分布あり)のグラフィカルモデルを作図
graph <- DiagrammeR::grViz("
  digraph dot{
    graph [rankdir = LR]
    node  [shape = circle]
    
    alpha [label = <<B>&alpha;</B>>]
    theta [label = <<B>&theta;</B>>]
    
    subgraph cluster_D{
      label = D
      
      z [label = 'z@_{d}']
      
      subgraph cluster_N{
        label = 'N@_{d}'
        
        w [label = 'w@_{dn}', style = filled, filledcolor = 'gray']
      }
    }
    
    edge []
      alpha -> theta -> z -> w;
    
    beta [label = <<B>&beta;</B>>]
    
    subgraph cluster_K{
      label = K
      
      phi [label = <<B>&phi;</B>@_{'k}>]
    }
    
    edge []
      beta -> phi -> w;
  }
")

## ( `{'k}` は書き出し時にφの添字が重なってしまう対策用の小細工)

# グラフを書出
DiagrammeRsvg::export_svg(gv = graph) |> # svgファイルに変換
  charToRaw() |> 
  rsvg::rsvg(height = 1000) |> # ビットマップに変換
  png::writePNG(target = "figure/graphical_model/mixunigram_model_prior.png", dpi = 100) # pngファイルに変換


