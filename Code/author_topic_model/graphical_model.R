
# chapter 5.4
# 著者トピックモデル

# グラフィカルモデル表現 ---------------------------------------------------------------

# 利用パッケージ
library(DiagrammeR)
library(DiagrammeRsvg)


# グラフィカルモデルの作図 ------------------------------------------------------------

# 著者トピックモデルのグラフィカルモデルを作図
graph <- DiagrammeR::grViz("
  digraph dot{
    label    = 'author topic model'
    labelloc = t
    fontsize = 20
    
    graph [rankdir = LR]
    node  [shape = circle, fixedsize = ture, height = 0.6, fontname = 'Times-Italic']
    edge  []
    
    alpha [label = <<B>&alpha;</B>>]
    beta  [label = <<B>&beta;</B>>]
    
    subgraph cluster_s{
      label    = 'S'
      fontsize = 14
      
      theta [label = <<B>&theta;</B>@_{s}>]
    }
    
    subgraph cluster_d{
      label    = 'D'
      fontsize = 14
      
      a [label = <<B>a</B>@_{-d}>, style = filled, filledcolor = gray]
      
      subgraph cluster_n{
        label    = 'N@_{d}'
        fontsize = 14
        
        y [label = 'y@_{dn}']
        z [label = 'z@_{dn}']
        w [label = 'w@_{dn}', style = filled, filledcolor = gray]
      }
    }
    
    subgraph cluster_k{
      label    = 'K'
      fontsize = 14
      
      phi [label = <<B>&phi;</B>@_{'k}>]
    }
    
    alpha -> theta;
    a -> y
    {theta, y} -> z -> w;
    w -> phi -> beta [dir = back];
  }
")

## ( `{'k}, {-d}` は書き出し時にφ,ψ,aの添字が重なってしまう対策用の小細工)

# グラフを書出
DiagrammeRsvg::export_svg(gv = graph) |> # svgファイルに変換
  charToRaw() |> 
  rsvg::rsvg(height = 1000) |> # ビットマップに変換
  png::writePNG(target = "figure/graphical_model/author_topic_model.png", dpi = 100) # pngファイルに変換


