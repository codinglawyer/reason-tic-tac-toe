open SharedTypes;

let str = ReasonReact.stringToElement;

let setStatus = (gameState: gameState) =>
  switch (gameState) {
  | Playing(Cross) => "Cross is playing"
  | Playing(Circle) => "Circle is playing"
  | Winner(Cross) => "The winner is Cross"
  | Winner(Circle) => "The winner is Circle"
  | Draw => "Draw"
  };

let component = ReasonReact.statelessComponent("Board");

let make = (~state: state, ~onMark, ~onRestart, _children) => {
  ...component,
  render: (_) =>
    <div className="game-board">
      (
        state.fields
        |> List.mapi((i: int, field: row) =>
             <div className="board-row" key=(string_of_int(i))>
               (
                 field
                 |> List.mapi((ind: int, value: field) => {
                      let id = string_of_int(i) ++ string_of_int(ind);
                      <Square
                        key=id
                        value
                        onMark=(() => onMark(id))
                        gameState=state.gameState
                      />;
                    })
                 |> Array.of_list
                 |> ReasonReact.arrayToElement
               )
             </div>
           )
        |> Array.of_list
        |> ReasonReact.arrayToElement
      )
      <div className="status"> (state.gameState |> setStatus |> str) </div>
      (
        switch (state.gameState) {
        | Playing(_) => ReasonReact.nullElement
        | _ =>
          <button className="restart" onClick=onRestart>
            (str("Restart"))
          </button>
        }
      )
    </div>,
};