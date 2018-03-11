[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

let str = ReasonReact.stringToElement;

let valueOr = (foo: option('a), default: 'a) =>
  switch foo {
  | Some(value) => value
  | _ => default
  };

module Square = {
  let component = ReasonReact.statelessComponent("Square");
  let make = (~value, ~onToggle, _children) => {
    ...component,
    render: _self =>
      <div className="square" onClick=(_evt => onToggle())>
        (str(valueOr(value, "-")))
      </div>
  };
};

type state = {
  fields: list(option(string)),
  isXPlaying: bool
};



let toBool = ex =>
  switch ex {
  | None => false
  | _ => true
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
          let isWinner =
            toBool(List.nth(fields, List.nth(head, 0)))
            &&
            toBool(List.nth(fields, List.nth(head, 0))) === toBool(
                                                             List.nth(
                                                               fields,
                                                               List.nth(head, 1)
                                                             )
                                                           )
            &&
            toBool(List.nth(fields, List.nth(head, 0))) === toBool(
                                                             List.nth(
                                                               fields,
                                                               List.nth(head, 2)
                                                             )
                                                           ) ?
              true : false;
          Js.log(isWinner);
          switch (tail, isWinner) {
          | ([], false) => "FAIL"
          | (_, true) => "PASS"
          | _ => exp(tail)
          };
        };
    exp(winningRows);
  };



module Board = {
  type action =
    | ClickSquare(string);
  let component = ReasonReact.reducerComponent("Board");
  let setStatus = isXplaying => "Next player:" ++ (isXplaying ? "X" : "O");
  let make = _children => {
    ...component,
    initialState: () => {
      fields: [None, None, None, Some("break"), None, None, None, Some("break"), None, None, None],
      isXPlaying: true
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

          Js.log(checkWinner(updatedFields));


        Js.log(updatedFields);
        ReasonReact.Update({isXPlaying: ! state.isXPlaying, fields: updatedFields});
      },
    render: ({state, reduce}) =>
      <div>
        <div className="status"> (str(setStatus(state.isXPlaying))) </div>
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