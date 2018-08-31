module EntitiesCanvas exposing (main)

import Composer.Scene as Scene exposing (Scene)
import Html as H exposing (Html)
import Html.Attributes as H
import Helpers.Entities as Entities exposing (Unit, Case)
import Composer.Backend.Immediate as Immediate
import Composer.Backend.Immediate.Command as Command
import Json.Encode as JE


main : Html msg
main =
    H.div
        []
        [ H.node "script"
            [ H.id "custom-elements-polyfill"
            , H.src "https://cdnjs.cloudflare.com/ajax/libs/custom-elements/1.2.0/custom-elements.min.js"
            ]
            []
        , H.node "script"
            [ H.id "canvas-2d-custom-element"
            ]
            [ H.text """
              // Given a list of commands, return a set assets sources to be
              // loaded. This actually return a map instead of set, where values
              // are null.
              function getAssetIndex(commandList) {
                let index = new Map();
                commandList.forEach(command => {
                  if (command.type == "drawImage") {
                    index.set(command.src, null);
                  }
                });
                return index;
              }

              // Given a list of commands and a callback, calls the callback
              // when all assets used in the commands are loaded. The callback
              // receives a map where keys are the source of the asset, and the
              // values the assets themselves.
              // NOTE: You probably also want to wait custom fonts to be loaded.
              function loadAssets(commandList, cb) {
                let promiseList = [];
                getAssetIndex(commandList)
                  .forEach((value, key) => {
                    promiseList.push(new Promise ((resolve, reject) => {
                      let image = new Image();
                      image.src = key;
                      image.onload = () => resolve(image);
                      image.onerror = () => resolve(null);
                    }));
                  });
                Promise.all(promiseList).then((values) => {
                  let assets = new Map();
                  values.forEach(img => assets.set(img.src, img));
                  cb(assets);
                });
              }

              // You can use MutationObserver to keep commands updated if needed
              window.addEventListener("load", () => {
                let canvasList = document.getElementsByTagName("canvas");
                for (let canvas of canvasList) {
                  let ctx = canvas.getContext("2d");
                  let commandList = JSON.parse(
                    canvas.attributes["data-commands"].value);
                  loadAssets(commandList, assets => {
                    commandList.forEach(command => {
                      switch (command.type) {
                        // State
                        case "saveState":
                          ctx.save();
                          break;
                        case "restoreState":
                          ctx.restore();
                          break;
                        // Transformations
                        case "transform":
                          ctx.setTransform.apply(ctx, command.transform);
                          break;
                        // Compositing
                        case "opacity":
                          ctx.globalAlpha = command.opacity;
                          break;
                        // Colors and Styles
                        case "fillStyle":
                          ctx.fillStyle = command.style;
                          break;
                        case "fillRect":
                          ctx.fillRect.apply(ctx, command.rect);
                          break;
                        // Text
                        case "font":
                          ctx.font = `${command.size}px ${command.name}`;
                          break;
                        case "fillText":
                          ctx.fillText(command.text, command.origin[0],
                            command.origin[1]);
                          break;
                        // Drawing Images
                        case "drawImage":
                          let image = assets.get(command.src);
                          ctx.drawImage(image, command.rect[0], command.rect[1],
                            command.rect[2], command.rect[3]);
                          break;
                        default:
                          console.error("unknown command", command);
                      }
                    });
                  });
                }
              }, {once: true});
              """
            ]
        , H.h1
            [ H.style [ ( "width", "100%" ), ( "text-align", "center" ) ]
            ]
            [ H.text "Examples Using Immediate Backend / 2D Canvas" ]
        , H.div [] (List.map unitView Entities.all)
        ]


unitView : Unit -> Html msg
unitView { title, cases } =
    H.section
        [ H.style
            [ ( "display", "flex" )
            , ( "flex-wrap", "wrap" )
            , ( "justify-content", "space-around" )
            ]
        ]
        (H.h2 [ H.style [ ( "width", "100%" ), ( "text-align", "center" ) ] ] [ H.text title ]
            :: List.map caseView cases
        )


caseView : Case -> Html msg
caseView { title, scene } =
    H.div [ H.style [ ( "width", "512px" ) ] ]
        [ H.h3 [ H.style [ ( "width", "100%" ), ( "text-align", "center" ) ] ] [ H.text title ]
        , H.canvas
            [ H.style
                [ ( "width", "512px" )
                , ( "height", "512px" )
                , ( "border", "1px solid black" )
                ]
            , scene
                |> Scene.compose
                |> Immediate.render
                |> List.map Command.encode
                |> JE.list
                |> JE.encode 0
                |> H.attribute "data-commands"
            , H.attribute "width" "512px"
            , H.attribute "height" "512px"
            ]
            []
        ]
