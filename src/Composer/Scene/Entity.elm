module Composer.Scene.Entity
    exposing
        ( -- Basics --
          Entity
        , Id
        , empty
        , identifier
        , children
        , setChildren
        , addChildren
          -- Transform --
        , transform
        , setTransform
          -- Color --
        , color
        , setColor
          -- Opacity --
        , opacity
        , setOpacity
          -- Shape --
        , Shape
        , shape
        , setShape
          -- Texture --
        , Uri
        , texture
        , setTexture
        )

{-| A graphical entity. By default an entity is just a named node of a tree.
We can create new entities using `empty` with a unique identifier and then, we
can add other entities as children using `setChildren` or `addChildren`.

But as said, entities by themselves do not say anything about their graphical
representation. Entities can also have a set of components that are responsible
to provide graphical meaning to an entity. For example, we can set the component
`texture` to draw an image and then set `transform` component to translate the
image to the center of the screen.


# Basics

@docs Entity, Id, empty, identifier, children, addChildren, setChildren


# Components


## Transform

@docs transform, setTransform


## Color

@docs color, setColor


## Opacity

@docs opacity, setOpacity


## Shape

@docs Shape, shape, setShape


## Texture

@docs Uri, texture, setTexture

-}

import Color exposing (Color)
import Composer.Geometry.Size exposing (Size)
import Composer.Geometry.Transform as Transform exposing (Transform)


{-| -}
type Entity
    = Entity
        { id : Id
        , children : Children
        , transform : Transform
        , shape : Maybe Shape
        , color : Color
        , opacity : Float
        , texture : Maybe Uri
        }


{-| A node identifier.
-}
type alias Id =
    String


{-| Creates an empty entity, with no components and no children. You must
guarantee id uniqueness in the whole entity set.
-}
empty : Id -> Entity
empty id =
    Entity
        { id = id
        , children = Children []
        , transform = Transform.identity
        , shape = Nothing
        , color = Color.white
        , opacity = 1
        , texture = Nothing
        }


{-| Returns the identifier of an entity.
-}
identifier : Entity -> Id
identifier (Entity { id }) =
    id


type Children
    = Children (List Entity)


{-| Returns the children of an entity.
-}
children : Entity -> List Entity
children (Entity entity) =
    let
        (Children list) =
            entity.children
    in
        list


{-| Sets the children of an entity. Previous children are removed.
-}
setChildren : List Entity -> Entity -> Entity
setChildren children (Entity entity) =
    Entity { entity | children = Children children }


{-| Add a child to an entity.
-}
addChildren : Entity -> Entity -> Entity
addChildren child (Entity entity) =
    let
        (Children children) =
            entity.children
    in
        Entity { entity | children = Children (child :: children) }



-- Transform --


{-| Return the transform of an entity.
-}
transform : Entity -> Transform
transform (Entity { transform }) =
    transform


{-| Sets the transform of an entity. You can use functions provided by the
`Transform` module to translate, scale and rotate the entity.
-}
setTransform : Transform -> Entity -> Entity
setTransform transform (Entity entity) =
    Entity { entity | transform = transform }



-- Color --


{-| Returns the color of an entity.
-}
color : Entity -> Color
color (Entity entity) =
    entity.color


{-| Sets the color of an entity. By itself this components does nothing, but
other components require a color.
-}
setColor : Color -> Entity -> Entity
setColor color (Entity entity) =
    Entity { entity | color = color }



-- Opacity --


{-| Returns the opacity of an entity.
-}
opacity : Entity -> Float
opacity (Entity entity) =
    entity.opacity


{-| Sets the opacity of an entity.
-}
setOpacity : Float -> Entity -> Entity
setOpacity opacity (Entity entity) =
    Entity { entity | opacity = opacity }



-- Shape --


{-| A set of shapes that can be draw as entities.
-}
type Shape
    = RectangleShape Size


{-| Return the shape of an entity, if any.
-}
shape : Entity -> Maybe Shape
shape (Entity entity) =
    entity.shape


{-| Sets or removes the shape of an entity.
-}
setShape : Maybe Shape -> Entity -> Entity
setShape shape (Entity entity) =
    Entity { entity | shape = shape }


{-| A uniform resource identifier.
-}
type alias Uri =
    String


{-| Return the texture of an entity, if any.
-}
texture : Entity -> Maybe Uri
texture (Entity entity) =
    entity.texture


{-| Sets or removes the shape of an entity.
-}
setTexture : Maybe Uri -> Entity -> Entity
setTexture texture (Entity entity) =
    Entity { entity | texture = texture }
