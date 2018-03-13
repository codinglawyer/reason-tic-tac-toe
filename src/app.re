[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

let toString = ReasonReact.stringToElement;

type playerType =
  | Cross
  | Circle;

type fieldType =
  | Empty
  | Filled(playerType)
  | Break;

type gameStateType =
  | Playing(playerType)
  | Won(playerType)
  | Tie;

let typeToValue = (tp: fieldType) =>
  switch tp {
  | Filled(Cross) => "X"
  | Filled(Circle) => "O"
  | _ => ""
  };

let typeToBool = (value: gameStateType) =>
  switch value {
  | Won(Cross) => true
  | Won(Circle) => true
  | _ => false
  };

module Square = {
  let component = ReasonReact.statelessComponent("Square");
  let make = (~value, ~onToggle, ~gameState, _children) => {
    ...component,
    render: _self =>
      <button
        className="square"
        disabled=(Js.Boolean.to_js_boolean(typeToBool(gameState)))
        onClick=(_evt => onToggle())>
        (toString(typeToValue(value)))
      </button>
  };
};

let checkWinner = (fields, gameState) => {
  let winningRows = [
    [0, 1, 2],
    [4, 5, 6],
    [8, 9, 10],
    [0, 4, 8],
    [1, 5, 9],
    [2, 6, 10],
    [0, 5, 10],
    [2, 5, 8]
  ];
  let rec check = remainder => {
    let head = List.hd(remainder);
    let tail = List.tl(remainder);
    switch (
      tail,
      List.nth(fields, List.nth(head, 0)),
      List.nth(fields, List.nth(head, 1)),
      List.nth(fields, List.nth(head, 2))
    ) {
    | (_, Filled(Cross), Filled(Cross), Filled(Cross)) => Won(Cross)
    | (_, Filled(Circle), Filled(Circle), Filled(Circle)) => Won(Circle)
    | ([], _, _, _) => switch gameState {
      | Playing(Cross) => Playing(Circle)
      | _ => Playing(Cross)
    }
    | _ => check(tail)
    };
  };
  check(winningRows);
};

module Board = {
  type state = {
    fields: list(fieldType),
    gameState: gameStateType
  };

  type action =
    | SquareClick(string)
    | Restart;

  let setStatus = (gameState: gameStateType) => switch gameState{
  | Playing(Cross) => "Cross is playing"
  | Playing(Circle) => "Circle is playing"
  | Won(Cross) => "The winner is Cross"
  | Won(Circle) => "The winner is Circle"
  | Tie => "Tie"
  };

  let initialState = {
    fields: [
      Empty,
      Empty,
      Empty,
      Break,
      Empty,
      Empty,
      Empty,
      Break,
      Empty,
      Empty,
      Empty
    ],
    gameState: Playing(Cross)
  };

  let component = ReasonReact.reducerComponent("Board");

  let make = _children => {
    ...component,
    initialState: () => initialState,
    reducer: (action, state) =>
      switch action {
      | Restart => ReasonReact.Update(initialState)
      | SquareClick((i: string)) =>
        let updatedFields =
          state.fields |>
          List.mapi(
            (index, value: fieldType) =>
              string_of_int(index) === i ?
                switch state.gameState{
                | Playing(playerType) => Filled(playerType)
                | _ => Empty
                }
                : value,
          );
        ReasonReact.Update({
          fields: updatedFields,
          gameState: checkWinner(updatedFields, state.gameState)
        });
      },
    render: ({state,reduce}) =>
      <div>
        <div className="status">
          (setStatus(state.gameState) |> toString)
           </div>
        (
          state.fields
          |> List.mapi((i, field) =>
               switch field {
               | Break => <div key=(string_of_int(i)) />
               | _ =>
                 <Square
                   key=(string_of_int(i))
                   value=field
                   onToggle=(reduce(() => SquareClick(string_of_int(i))))
                   gameState=state.gameState
                 />
               }
             )
          |> Array.of_list
          |> ReasonReact.arrayToElement
        )
        <div>
        (switch state.gameState{
        | Playing(_playerType) => ReasonReact.nullElement
        | _ => <button onClick=(reduce((_evt) => Restart))>(toString("Restart"))</button>
        })
        </div>
      </div>
  };
};

module Game = {
  let component = ReasonReact.statelessComponent("Game");
  let make = _children => {
    ...component,
    render: (_) =>
      <div className="game">
        <div className="game-board"> <Board /> </div>
        <div className="game-info"> <div /> <ol /> </div>
      </div>
  };
};

let component = ReasonReact.statelessComponent("App");

let make = _children => {
  ...component,
  render: _self => <div className="App"> <Game /> </div>
};