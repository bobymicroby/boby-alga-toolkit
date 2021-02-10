module Dot
  ( renderGraphWithDefaultStyle,
    renderGraph,
    renderSuggestGraph,
    renderFollowingGraph,
    renderFollowersGraph,
  )
where

import Algebra.Graph.Export.Dot
import Lib
  ( Graph (..),
    Request (..),
    User (..),
    followersGraph,
    followingGraph,
    suggestGraph,
  )
import Network.URI.Encode
import Algebra.Graph.ToGraph (toGraph)

toDot :: Graph User -> Style User String -> String
toDot graph style = export style graph

toHtmlLink :: Graph User -> Style User String -> String
toHtmlLink graph style = "https://dreampuf.github.io/GraphvizOnline/#" ++ encode (toDot graph style)

style :: Style User String
style =
  Style
    { graphName = "bobydev",
      preamble = ["", ""],
      graphAttributes = [],
      defaultVertexAttributes = ["shape" := "circle"],
      defaultEdgeAttributes = mempty,
      vertexName = \(User phone ) -> phone,
      vertexAttributes = \a -> [],
      edgeAttributes = \a b -> []
    }

renderGraph :: Graph User -> Style User String -> String
renderGraph g s = toHtmlLink (toGraph g) s

renderGraphWithDefaultStyle :: Graph User -> String
renderGraphWithDefaultStyle g = renderGraph g style

renderFollowersGraph :: User -> Graph User -> String
renderFollowersGraph u g = renderGraph (followersGraph u g) style {graphAttributes = ["label" := ("followers of " ++ phone  u), "labelloc" := "top"]}

renderFollowingGraph :: User -> Graph User -> String
renderFollowingGraph u g = renderGraph (followingGraph u g) style {graphAttributes = ["label" := (phone u ++ " is following"), "labelloc" := "top"]}

renderSuggestGraph :: User -> Int -> Graph User -> String
renderSuggestGraph u i g = renderGraph (suggestGraph u i g) style {graphAttributes = ["label" := ("Does " ++ phone u ++ " knows this people "), "labelloc" := "top"]}
