[%bs.raw {|require('./game.css')|}];

type player =
  | Cross
  | Circle;

type field =
  | Empty
  | Marked(player);

type gameState =
  | Playing(player)
  | Winner(player)
  | Draw;

let str = ReasonReact.stringToElement;

let toValue = (field: field) =>
  switch (field) {
  | Marked(Cross) => "X"
  | Marked(Circle) => "O"
  | Empty => ""
  };

let isFinished = (value: gameState) =>
  switch (value) {
  | Winner(_) => true
  | _ => false
  };

let getClass = (gameState: gameState, field: field) =>
  switch (gameState) {
  | Winner(player) => field == Marked(player) ? "winner square" : "square"
  | _ => "square"
  };

let isDraw = fields =>
  List.for_all(
    field => field == Marked(Circle) || field == Marked(Cross),
    fields,
  );

module Square = {
  let component = ReasonReact.statelessComponent("Square");
  let make = (~value: field, ~gameState: gameState, ~onToggle, _children) => {
    ...component,
    render: _self =>
      <button
        className=(getClass(gameState, value))
        disabled=(gameState |> isFinished |> Js.Boolean.to_js_boolean)
        onClick=(_evt => onToggle())>
        (value |> toValue |> str)
      </button>,
  };
};

type row = list(field);

type fields = list(row);

type state = {
  fields,
  gameState,
};

type action =
  | SquareClick(string)
  | Restart;

let setStatus = (gameState: gameState) =>
  switch (gameState) {
  | Playing(Cross) => "Cross is playing"
  | Playing(Circle) => "Circle is playing"
  | Winner(Cross) => "The winner is Cross"
  | Winner(Circle) => "The winner is Circle"
  | Draw => "Draw"
  };

module Board = {
  let component = ReasonReact.statelessComponent("Game");
  let make = (~state, ~onToggle, ~onClick, _children) => {
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
                          onToggle=(() => onToggle(id))
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
            <button className="restart" onClick> (str("Restart")) </button>
          }
        )
      </div>,
  };
};

module Game = {
  let checkGameState = (fields: fields, gameState: gameState) => {
    let flattenFields = List.flatten(fields);
    let winningCombs = [
      [0, 1, 2],
      [3, 4, 5],
      [6, 7, 8],
      [0, 3, 6],
      [1, 4, 7],
      [2, 5, 8],
      [0, 4, 8],
      [2, 4, 6],
    ];
    let rec check = rest => {
      let head = List.hd(rest);
      let tail = List.tl(rest);
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
    check(winningCombs);
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
    reducer: (action: action, state: state) =>
      switch (action) {
      | Restart => ReasonReact.Update(initialState)
      | SquareClick((i: string)) =>
        let updatedFields =
          state.fields
          |> List.mapi((ind: int, row: row) =>
               row
               |> List.mapi((index: int, value: field) =>
                    string_of_int(ind) ++ string_of_int(index) === i ?
                      switch (state.gameState, value) {
                      | (_, Marked(_)) => value
                      | (Playing(player), Empty) => Marked(player)
                      | (_, Empty) => Empty
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
      <div className="game">
        <Board
          state
          onClick=(reduce(_evt => Restart))
          onToggle=(reduce(id => SquareClick(id)))
        />
      </div>,
  };
};