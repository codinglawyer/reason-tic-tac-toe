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
        Js.log(i);
        let res =
          List.mapi(
            (index, value) => {
              Js.log(index);
              Js.log(str(i));
              string_of_int(index) === i ?
                state.isXPlaying ? Some("X") : Some("O") : value;
            },
            state.fields
          );
        Js.log(res);
        ReasonReact.Update({isXPlaying: ! state.isXPlaying, fields: res});
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
                | Some("break") => <div />
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