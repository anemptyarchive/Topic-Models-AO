
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
    label    = 'topic model'
    labelloc = t
    fontsize = 20
    
    graph [rankdir = LR]
    node  [shape = circle, fixedsize = ture, height = 0.6, fontname = 'Times-Italic']
    edge  []
    
    subgraph cluster_d{
      label    = 'D'
      fontsize = 14
      
      theta [label = <<B>&theta;</B>@_{d}>]
      
      subgraph cluster_n{
        label    = 'N@_{d}'
        fontsize = 14

        z [label = 'z@_{dn}']
        w [label = 'w@_{dn}', style = filled, filledcolor = gray]
      }
    }
    
    subgraph cluster_k{
      label    = 'K'
      fontsize = 14
      
      phi [label = <<B>&phi;</B>@_{'k}>]
    }
    
    theta -> z -> w;
    w -> phi[dir = back];
  }
")

## ( `{'k}` は書き出し時にφの添字が重なってしまう対策用の小細工)

# グラフを書出
DiagrammeRsvg::export_svg(gv = graph) |> # svgファイルに変換
  charToRaw() |> 
  rsvg::rsvg(height = 500) |> # ビットマップに変換
  png::writePNG(target = "figure/graphical_model/topic_model_non_hyparam.png", dpi = 100) # pngファイルに変換


### ・事前分布ありの場合 -----

# トピックモデルのグラフィカルモデルを作図
graph <- DiagrammeR::grViz("
  digraph dot{
    label    = 'topic model'
    labelloc = t
    fontsize = 20
    
    graph [rankdir = LR]
    node  [shape = circle, fixedsize = ture, height = 0.6, fontname = 'Times-Italic']
    edge  []
    
    alpha [label = <<B>&alpha;</B>>]
    beta  [label = <<B>&beta;</B>>]
    
    subgraph cluster_d{
      label    = 'D'
      fontsize = 14
      
      theta [label = <<B>&theta;</B>@_{d}>]
      
      subgraph cluster_n{
        label    = 'N@_{d}'
        fontsize = 14
        
        z [label = 'z@_{dn}']
        w [label = 'w@_{dn}', style = filled, filledcolor = gray]
      }
    }
    
    subgraph cluster_k{
      label    = 'K'
      fontsize = 14
      
      phi [label = <<B>&phi;</B>@_{'k}>]
    }
    
    alpha -> theta -> z -> w;
    w -> phi -> beta [dir = back];
  }
")

## ( `{'k}` は書き出し時にφの添字が重なってしまう対策用の小細工)

# グラフを書出
DiagrammeRsvg::export_svg(gv = graph) |> # svgファイルに変換
  charToRaw() |> 
  rsvg::rsvg(height = 500) |> # ビットマップに変換
  png::writePNG(target = "figure/graphical_model/topic_model.png", dpi = 100) # pngファイルに変換


