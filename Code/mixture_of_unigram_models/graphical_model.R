
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
    label    = 'mixture of unigram models'
    labelloc = t
    fontsize = 20
    
    graph [rankdir = LR]
    node  [shape = circle, fixedsize = ture, fontname = 'Times-Italic']
    edge []
    
    theta [label = <<B>&theta;</B>>]
    
    subgraph cluster_d{
      label    = 'D'
      fontsize = 14
      
      z [label = 'z@_{d}']
      
      subgraph cluster_n{
        label    = 'N@_{d}'
        fontsize = 14
        
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
  png::writePNG(target = "figure/graphical_model/mixture_of_unigram_models_non_hyparam.png", dpi = 100) # pngファイルに変換


### ・事前分布ありの場合 -----

# 混合ユニグラムモデルのグラフィカルモデルを作図
graph <- DiagrammeR::grViz("
  digraph dot{
    label    = 'mixture of unigram models'
    labelloc = t
    fontsize = 20
    
    graph [rankdir = LR]
    node  [shape = circle, fixedsize = ture, fontname = 'Times-Italic']
    edge  []
    
    alpha [label = <<B>&alpha;</B>>]
    beta  [label = <<B>&beta;</B>>]
    theta [label = <<B>&theta;</B>>]
    
    subgraph cluster_d{
      label    = 'D'
      fontsize = 14
      
      z [label = 'z@_{d}']
      
      subgraph cluster_n{
        label    = 'N@_{d}'
        fontsize = 14
        
        w [label = 'w@_{dn}', style = filled, filledcolor = gray]
      }
    }
    
    subgraph cluster_k{
      label    = 'K'
      fontsize = 14
      
      phi [label = <<B>&phi;</B>@_{'k}>]
    }
    
    alpha -> theta -> z -> w;
    w -> phi -> beta[dir = back];
  }
")

## ( `{'k}` は書き出し時にφの添字が重なってしまう対策用の小細工)

# グラフを書出
DiagrammeRsvg::export_svg(gv = graph) |> # svgファイルに変換
  charToRaw() |> 
  rsvg::rsvg(height = 500) |> # ビットマップに変換
  png::writePNG(target = "figure/graphical_model/mixture_of_unigram_models.png", dpi = 100) # pngファイルに変換


