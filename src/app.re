[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

module Square = {
  let component = ReasonReact.statelessComponent("Square");
  let make = _children => {
    ...component,
    render: _self => <button className="square" />
  };
};

module Board = {
  let component = ReasonReact.statelessComponent("Board");
  let status = "Next player: X";
  let renderSquare = (_) => <Square />;
  let str = ReasonReact.stringToElement;
  let make = _children => {
    ...component,
    render: _self =>
      <div>
        <div className="status"> (str(status)) </div>
        <div className="board-row">
          (renderSquare())
          (renderSquare())
          (renderSquare())
        </div>
        <div className="board-row">
          (renderSquare())
          (renderSquare())
          (renderSquare())
        </div>
        <div className="board-row">
          (renderSquare())
          (renderSquare())
          (renderSquare())
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