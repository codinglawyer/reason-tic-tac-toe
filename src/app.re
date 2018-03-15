[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

let toString = ReasonReact.stringToElement;

type playerType =
  | Cross
  | Circle;

type fieldType =
  | Empty
  | Marked(playerType);

type gameStateType =
  | Playing(playerType)
  | Winner(playerType)
  | Draw;

let typeToValue = (tp: fieldType) =>
  switch (tp) {
  | Marked(Cross) => "X"
  | Marked(Circle) => "O"
  | Empty => ""
  };

let isGameFinished = (value: gameStateType) =>
  switch (value) {
  | Winner(_) => true
  | _ => false
  };

let getBackgroundClass = (gameState, value) =>
  switch (gameState) {
  | Winner(playerType) =>
    value == Marked(playerType) ? "winner square" : "square"
  | _ => "square"
  };

module Square = {
  let component = ReasonReact.statelessComponent("Square");
  let make = (~value, ~onToggle, ~gameState, _children) => {
    ...component,
    render: _self =>
      <button
        className=(getBackgroundClass(gameState, value))
        disabled=(Js.Boolean.to_js_boolean(isGameFinished(gameState)))
        onClick=(_evt => onToggle())>
        (toString(typeToValue(value)))
      </button>,
  };
};

let isDraw = fields =>
  List.for_all(
    field => field == Marked(Circle) || field == Marked(Cross),
    fields,
  );

let checkGameState = (fields, gameState) => {
  let flattenFields = List.flatten(fields);
  let winningRows = [
    [0, 1, 2],
    [3, 4, 5],
    [6, 7, 8],
    [0, 3, 6],
    [1, 4, 7],
    [2, 5, 8],
    [0, 4, 8],
    [2, 4, 6],
  ];
  let rec check = remainder => {
    let head = List.hd(remainder);
    let tail = List.tl(remainder);
    switch (
      tail,
      List.nth(flattenFields, List.nth(head, 0)),
      List.nth(flattenFields, List.nth(head, 1)),
      List.nth(flattenFields, List.nth(head, 2)),
    ) {
    | (_, Marked(Cross), Marked(Cross), Marked(Cross)) => Winner(Cross)
    | (_, Marked(Circle), Marked(Circle), Marked(Circle)) =>
      Winner(Circle)
    | ([], _, _, _) =>
      isDraw(flattenFields) ?
        Draw :
        (
          switch (gameState) {
          | Playing(Cross) => Playing(Circle)
          | _ => Playing(Cross)
          }
        )
    | _ => check(tail)
    };
  };
  check(winningRows);
};

module Board = {
  type row = list(fieldType);
  type state = {
    fields: list(row),
    gameState: gameStateType,
  };
  type action =
    | SquareClick(string)
    | Restart;
  let setStatus = (gameState: gameStateType) =>
    switch (gameState) {
    | Playing(Cross) => "Cross is playing"
    | Playing(Circle) => "Circle is playing"
    | Winner(Cross) => "The winner is Cross"
    | Winner(Circle) => "The winner is Circle"
    | Draw => "Draw"
    };
  let initialState = {
    fields: [
      [Empty, Empty, Empty],
      [Empty, Empty, Empty],
      [Empty, Empty, Empty],
    ],
    gameState: Playing(Cross),
  };
  let component = ReasonReact.reducerComponent("Board");
  let make = _children => {
    ...component,
    initialState: () => initialState,
    reducer: (action, state) =>
      switch (action) {
      | Restart => ReasonReact.Update(initialState)
      | SquareClick((i: string)) =>
        let updatedFields =
          state.fields
          |> List.mapi((ind, row: row) =>
               row
               |> List.mapi((index, value) =>
                    string_of_int(ind) ++ string_of_int(index) === i ?
                      switch (state.gameState, value) {
                      | (_, Marked(_)) => value
                      | (Playing(playerType), Empty) => Marked(playerType)
                      | (_, _) => Empty
                      } :
                      value
                  )
             );
        ReasonReact.Update({
          fields: updatedFields,
          gameState:
            state.fields == updatedFields ?
              state.gameState : checkGameState(updatedFields, state.gameState),
        });
      },
    render: ({state, reduce}) =>
      <div>
        (
          state.fields
          |> List.mapi((i, field) =>
               <div className="board-row" key=(string_of_int(i))>
                 (
                   field
                   |> List.mapi((ind, value) =>
                        <Square
                          key=(string_of_int(i) ++ string_of_int(ind))
                          value
                          onToggle=(
                            reduce(() =>
                              SquareClick(
                                string_of_int(i) ++ string_of_int(ind),
                              )
                            )
                          )
                          gameState=state.gameState
                        />
                      )
                   |> Array.of_list
                   |> ReasonReact.arrayToElement
                 )
               </div>
             )
          |> Array.of_list
          |> ReasonReact.arrayToElement
        )
        <div className="status">
          (setStatus(state.gameState) |> toString)
        </div>
        (
          switch (state.gameState) {
          | Playing(_) => ReasonReact.nullElement
          | _ =>
            <button className="restart" onClick=(reduce(_evt => Restart))>
              (toString("Restart"))
            </button>
          }
        )
      </div>,
  };
};

module Game = {
  let component = ReasonReact.statelessComponent("Game");
  let make = _children => {
    ...component,
    render: (_) =>
      <div className="game">
        <div className="game-board"> <Board /> </div>
      </div>,
  };
};

module App = {
  let component = ReasonReact.statelessComponent("App");
  let make = _children => {
    ...component,
    render: _self => <div className="App"> <Game /> </div>,
  };
};