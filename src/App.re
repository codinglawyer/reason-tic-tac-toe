open Utils;

let component = ReasonReact.statelessComponent("App");

let make = _children => {
  ...component,
  render: _self =>
    <div>
      <div className="title"> (toString("Tic Tac Toe")) </div>
      <Game />
    </div>,
};