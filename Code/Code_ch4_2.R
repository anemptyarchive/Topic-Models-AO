
# ch4.2 グラフィカルモデル ---------------------------------------------------------

# 利用パッケージ
library(DiagrammeR)


# ユニグラムモデル ----------------------------------------------------------------

# ユニグラムモデルの事前分布のグラフィカルモデル
DiagrammeR::grViz("
  digraph dot{
    graph [rankdir = LR]
    
    node [shape = circle]
    
    beta [label = '&beta;']
    phi [label = <<B>&phi;</B>>]
    
    edge []
      beta -> phi;
  }
")


# ユニグラムモデルの尤度のグラフィカルモデル
DiagrammeR::grViz("
  digraph dot{
    graph [rankdir = LR]
    
    node [shape = circle]
    
    phi [label = <<B>&phi;</B>>]
    
    subgraph cluster_D{
      label = D
      
      subgraph cluster_N{
        label = N
        
        w [label = 'w@_{dn}', style = filled, filledcolor = 'gray']
      }
    }
    
    edge []
    
    phi -> w;
  }
")

# ユニグラムモデルのグラフィカルモデル
DiagrammeR::grViz("
  digraph dot{
    graph [rankdir = LR]
    
    node [shape = circle]
    
    beta [label = '&beta;']
    phi [label = <<B>&phi;</B>>]
    
    subgraph cluster_D{
      label = D
      
      subgraph cluster_N{
        label = N
        
        w [label = 'w@_{dn}', style = filled, filledcolor = 'gray']
      }
    }

    edge []
      beta -> phi -> w
  }
")


# 混合ユニグラムモデル --------------------------------------------------------------

# 混合ユニグラムモデルのグラフィカルモデル
DiagrammeR::grViz("
  digraph dot{
    graph [rankdir = LR]
    
    node [shape = circle]
    
    subgraph Cluster_alpha{
      alpha [label = '&alpha;']
      theta [label = <<B>&theta;</B>>]
      
      subgraph cluster_D{
        label = D
        
        z [label = 'z@_{dn}']
        
        subgraph cluster_N{
          label = N
          
          w [label = 'w@_{dn}', style = filled, filledcolor = 'gray']
        }
      }
      
      edge []
      alpha -> theta -> z -> w;
    }
    
    subgraph Cluster_beta{
      beta [label = '&beta;']
      
      subgraph cluster_K{
        label = K
        
        phi [label = <<B>&phi;</B>@_{k}>]
      }
      
      edge []
      beta -> phi -> w;
    }
  }
")


# トピックモデル -----------------------------------------------------------------

# トピックモデルのグラフィカルモデル
DiagrammeR::grViz("
  digraph dot{
    graph [rankdir = LR]

    node [shape = circle]

    subgraph Cluster_alpha{
      alpha [label = '&alpha;']

      subgraph cluster_D{
        label = D

        theta [label = <<B>&theta;</B>@_{d}>]

        subgraph cluster_N{
          label = N

          z [label = 'z@_{dn}']
          w [label = 'w@_{dn}', style = filled, filledcolor = 'gray']
        }
      }

      edge []
        alpha -> theta -> z -> w;
    }

    subgraph Cluster_beta{
        beta [label = '&beta;']

      subgraph cluster_K{
        label = K

        phi [label = <<B>&phi;</B>@_{k}>]
      }

      edge []
        beta -> phi -> w;
    }
  }
")

