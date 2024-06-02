
# chapter 4
# トピックモデル

# グラフィカルモデル表現 ---------------------------------------------------------------

# 利用パッケージ
library(DiagrammeR)
library(DiagrammeRsvg)


# グラフィカルモデルの作図 ------------------------------------------------------------

### ・事前分布なしの場合 -----

# トピックモデル(事前分布なし)のグラフィカルモデルを作図
graph <- DiagrammeR::grViz("
  digraph dot{
    graph [rankdir = LR]
    node  [shape = circle]
    
    subgraph cluster_D{
      label = D
      
      theta [label = <<B>&theta;</B>@_{d}>]
      
      subgraph cluster_N{
        label = 'N@_{d}'

        z [label = 'z@_{dn}']
        w [label = 'w@_{dn}', style = filled, filledcolor = 'gray']
      }
      
      edge []
        theta -> z -> w;
    }
    
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
  png::writePNG(target = "figure/graphical_model/topic_model.png", dpi = 100) # pngファイルに変換


### ・事前分布ありの場合 -----

# トピックモデル(事前分布あり)のグラフィカルモデルを作図
graph <- DiagrammeR::grViz("
  digraph dot{
    graph [rankdir = LR]
    node  [shape = circle]
    
    alpha [label = <<B>&alpha;</B>>]
    
    subgraph cluster_D{
      label = D
      
      theta [label = <<B>&theta;</B>@_{d}>]
      
      subgraph cluster_N{
        label = 'N@_{d}'
        
        z [label = 'z@_{dn}']
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
  png::writePNG(target = "figure/graphical_model/topic_model_prior.png", dpi = 100) # pngファイルに変換



