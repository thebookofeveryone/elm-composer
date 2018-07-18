module Composer.Scene
    exposing
        ( Scene
        )

{-| A `Scene` is set of graphical entities arranged in a spatial representation.
Entities are themselves trees, so a Scene only need to keep the root entity in
order to build complex screens.

A `Scene` also contains the size of the viewport. The viewport is the part
of the scene that is visible. The position is always set to the origin (0, 0).
We can translate the position of the root node to create a camera of a scroll
effect.

The `compose` function translate this scene representation into a flatten set
of primitives that can be consumed by other backend renderers (pdf, svg, canvas,
webgl, etc).

@docs Scene

-}

import Composer.Geometry.Size exposing (Size)
import Composer.Scene.Entity exposing (Entity)


{-| -}
type alias Scene =
    { root : Entity
    , size : Size
    }
