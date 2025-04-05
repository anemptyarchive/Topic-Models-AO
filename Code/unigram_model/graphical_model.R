
# chapter 2
# ユニグラムモデル

# グラフィカルモデル表現 ---------------------------------------------------------------

# 利用パッケージ
library(DiagrammeR)
library(DiagrammeRsvg)


# グラフィカルモデルの作図 ------------------------------------------------------------

### ・事前分布なしの場合 -----

# ユニグラムモデル(事前分布なし)のグラフィカルモデルを作図
graph <- DiagrammeR::grViz("
  digraph dot{
    label    = 'unigram model'
    labelloc = t
    fontsize = 20
    
    graph [rankdir = LR]
    node  [shape = circle, fixedsize = ture, height = 0.6, fontname = 'Times-Italic']
    edge  []
    
    phi [label = <<B>&phi;</B>>]
    
    subgraph cluster_d{
      label    = 'D'
      fontsize = 14
      
      subgraph cluster_n{
        label    = 'N@_{d}'
        fontsize = 14
        
        w [label = 'w@_{dn}', style = filled, filledcolor = gray]
      }
    }
    
    phi -> w;
  }
")

# グラフを書出
DiagrammeRsvg::export_svg(gv = graph) |> # svgファイルに変換
  charToRaw() |> 
  rsvg::rsvg(height = 500) |> # ビットマップに変換
  png::writePNG(target = "figure/graphical_model/unigram_model_non_hyparam.png", dpi = 100) # pngファイルに変換


### ・事前分布ありの場合 -----

# ユニグラムモデルのグラフィカルモデルを作図
graph <- DiagrammeR::grViz("
  digraph dot{
    label    = 'unigram model'
    labelloc = t
    fontsize = 20
    
    graph [rankdir = LR]
    node  [shape = circle, fixedsize = ture, height = 0.6, fontname = 'Times-Italic']
    edge  []
    
    beta [label = <<B>&beta;</B>>]
    phi  [label = <<B>&phi;</B>>]
    
    subgraph cluster_d{
      label    = 'D'
      fontsize = 14
      
      subgraph cluster_n{
        label    = 'N@_{d}'
        fontsize = 14
        
        w [label = 'w@_{dn}', style = filled, filledcolor = gray]
      }
    }
    
    beta -> phi -> w
  }
")

# グラフを書出
DiagrammeRsvg::export_svg(gv = graph) |> # svgファイルに変換
  charToRaw() |> 
  rsvg::rsvg(height = 500) |> # ビットマップに変換
  png::writePNG(target = "figure/graphical_model/unigram_model.png", dpi = 100) # pngファイルに変換


