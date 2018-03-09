[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

let str = ReasonReact.stringToElement;

let valueOr = (foo: option('a), default: 'a) => {
  switch (foo) {
    | Some(value) => value
    | _ => default
  };
};

module Square = {
  let component = ReasonReact.statelessComponent("Square");
  let make = (~value, ~onToggle, _children) => {
    ...component,
    render: _self =>
      <button className="square" onClick=(_evt => onToggle())>
      ( str((valueOr(value, "-"))))
      </button>
  };
};

module Board = {
  /* type turn = option(string); */
  type action =
    | ClickSquare(string);
  let component = ReasonReact.reducerComponent("Board");
  type state = list(option(string));
  let status = "Next player: X";
  /* let renderSquare = value => <Square value=(value)/>; */
  let make = _children => {
    ...component,
    initialState: () => [None, None, None, None, None, None, None, None, None],
    reducer: (action, state: state) =>
      switch action {
      | ClickSquare(i) => {
        Js.log(i);
        let res = List.mapi((index, value) => {Js.log(index); Js.log(str(i)); string_of_int(index) === i ? Some("X") : value}, state);
        Js.log(res);
        ReasonReact.Update(res)
      }
      },
    render: self =>
      <div>
        <div className="status"> (str(status)) </div>
        (
          ReasonReact.arrayToElement(
            Array.of_list(
              List.mapi((i, num) =>
                <Square
                  key=(string_of_int(i))
                  value=(num)
                  onToggle=(self.reduce(() => ClickSquare(string_of_int(i))))
                />, self.state
              )
            )
          )
        )
        <div
          className="board-row">
          /* <Square value=(string_of_int(1)) onToggle=(self.reduce (()=> ClickSquare)) /> */
          /* (renderSquare(string_of_int(1))) */
          /* <Square value=(string_of_int(2)) />
             <Square value=(string_of_int(3)) /> */
        </div>
        <div
          className="board-row">
          /* <Square value=(string_of_int(4)) />
             <Square value=(string_of_int(5)) />
             <Square value=(string_of_int(6)) /> */
             </div>
        <div
          className="board-row">
          /* <Square value=(string_of_int(7)) />
             <Square value=(string_of_int(8)) />
             <Square value=(string_of_int(9)) /> */
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