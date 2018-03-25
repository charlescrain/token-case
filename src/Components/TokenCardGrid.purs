module Components.TokenCardGrid where

import Data.Foreign
import Data.Options
import Prelude

import Data.Foldable (fold)
import Data.Lens (Lens', Prism', over, lens, prism)
import Data.List (List(..), deleteAt, filter, fromFoldable, length, toUnfoldable)
import MaterialUI as M
import MaterialUI.Grid as Grid
import MaterialUI.Paper as Paper
import React (ReactElement) as R
import React.DOM (text, p', td', input, tr', tbody', th, thead', table, div, h1', button) as R
import React.DOM.Props as RP
import Thermite as T

data TokenCardGridAction = SomeAction

type TokenCardGridState = 
  { tokenCards :: List TokenCard
  }

type TokenCard = Number


initialTokenCardGridState :: TokenCardGridState
initialTokenCardGridState = { tokenCards: fromFoldable ([1.0,2.0,3.0,4.0,5.0,11341.0,5.0,235.0])
                            }

-- | A `Lens` which corresponds to the `tokenCards` property.
_tokenCards :: Lens' TokenCardGridState (List TokenCard)
_tokenCards = lens _.tokenCards (_ {tokenCards = _ })

tokenCardGrid :: forall props eff. T.Spec eff TokenCardGridState props TokenCardGridAction
tokenCardGrid = container $ fold
    [ header
    , cardGrid
    ]
  where
    -- | A function which wraps a `Spec`'s `Render` function with a `container` element.
    container :: forall state action. T.Spec eff state props action -> T.Spec eff state props action
    container = over T._render \render d p s c ->
      [ R.div [ RP.className "container" ] (render d p s c) ]
      
    -- The header component contains a button which will create a new task.
    header :: T.Spec eff TokenCardGridState props TokenCardGridAction
    header = T.simpleSpec T.defaultPerformAction render
      where
        render :: T.Render TokenCardGridState props TokenCardGridAction
        render _ _ s _ =
          [ R.h1' [ R.text "Collectible Grid" ]
          ]
    cardGrid :: T.Spec eff TokenCardGridState props TokenCardGridAction
    cardGrid = T.simpleSpec T.defaultPerformAction render
      where
        containerGrid tcs = Grid.grid (Grid.container := true
                                  <> Grid.spacing := M.UnknownType (toForeign 8))
                            (map gridItem (toUnfoldable tcs)) 
        gridItem tc = Grid.grid (Grid.item := true
                              <> Grid.xs := M.UnknownType (toForeign 4))
                      [ Paper.paper (Paper.className := "paper") [R.text  (show tc)] ]
        render :: T.Render TokenCardGridState props TokenCardGridAction
        render _ _ s _ =
          [containerGrid (s.tokenCards)
          ]

