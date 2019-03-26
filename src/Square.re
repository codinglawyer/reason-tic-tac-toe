open SharedTypes;

open Utils;

let getClass = (gameState: gameState, field: field) =>
  switch (gameState) {
  | Winner(player) => field == Marked(player) ? "winner square" : "square"
  | _ => "square"
  };

let isFinished = (value: gameState) =>
  switch (value) {
  | Winner(_) => true
  | _ => false
  };

let toValue = (field: field) =>
  switch (field) {
  | Marked(Cross) => "X"
  | Marked(Circle) => "O"
  | Empty => ""
  };

let component = ReasonReact.statelessComponent("Square");

let make = (~value: field, ~gameState: gameState, ~onMark, _children) => {
  ...component,
  render: _self =>
    <button
      className=(getClass(gameState, value))
      disabled=(gameState |> isFinished)
      onClick=(_evt => onMark())>
      (value |> toValue |> toString)
    </button>,
};