[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

let str = ReasonReact.stringToElement;

let valueOr = (foo: option('a), default: 'a) =>
  switch foo {
  | Some(value) => value
  | _ => default
  };

  let toBool = value =>
  switch value {
  | "X" => true
  | "O" => true
  | _ => false
  };

module Square = {
  let component = ReasonReact.statelessComponent("Square");
  let make = (~value, ~onToggle, ~isWinner, _children) => {
    ...component,
    render: _self =>
      <button className="square" disabled={Js.Boolean.to_js_boolean(toBool(isWinner))} onClick=(_evt => onToggle())>
        (str(valueOr(value, "-")))
      </button>
  };
};

type state = {
  fields: list(option(string)),
  isXPlaying: bool,
  winner: string
};

  let checkWinner = fields => {
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
    let rec exp = remainder =>
        {
          let head = List.hd(remainder);
          let tail = List.tl(remainder);
          switch (tail, List.nth(fields, List.nth(head, 0)), List.nth(fields, List.nth(head, 1)), List.nth(fields, List.nth(head, 2))){
          | (_, Some("X"), Some("X"), Some("X")) => "X"
          | (_, Some("O"), Some("O"), Some("O")) => "O"
          | ([], _, _, _) => "NON"
          | _ => exp(tail)
          };
        };
    exp(winningRows);
  };



module Board = {
  type action =
    | ClickSquare(string);
  let component = ReasonReact.reducerComponent("Board");
  let setStatus = (isXplaying, winner) => ((winner === "X") || (winner === "O")) ? ("The Winner is:" ++ winner) :
  "Next player:" ++ (isXplaying ? "X" : "O");
  let make = _children => {
    ...component,
    initialState: () => {
      fields: [None, None, None, Some("break"), None, None, None, Some("break"), None, None, None],
      isXPlaying: true,
      winner: "NON"
    },
    reducer: (action, state: state) =>
      switch action {
      | ClickSquare(i) =>
        let updatedFields =
          List.mapi(
            (index, value) => {
              Js.log(index);
              Js.log(str(i));
              string_of_int(index) === i ?
                state.isXPlaying ? Some("X") : Some("O") : value;
            },
            state.fields
          );

          let winner = checkWinner(updatedFields);


        /* Js.log(updatedFields); */
        ReasonReact.Update({isXPlaying: ! state.isXPlaying, fields: updatedFields, winner: winner});
      },
    render: ({state, reduce}) =>
      <div>
        <div className="status"> (str(setStatus(state.isXPlaying, state.winner))) </div>
        (
          ReasonReact.arrayToElement(
            Array.of_list(
              List.mapi(
                (i, num) =>
                switch num{
                | Some("break") => <div key=(string_of_int(i))/>
                | _ =>
                  <Square
                    key=(string_of_int(i))
                    value=num
                    onToggle=(reduce(() => ClickSquare(string_of_int(i))))
                    isWinner=state.winner
                  />},
                state.fields
              )
            )
          )
        )
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