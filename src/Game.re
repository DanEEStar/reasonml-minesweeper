let boardWidth = 20;
let boardHeight = 20;
let numBombs = 50;

module Tile = {
  type tileState =
    | Hidden
    | Revealed;

  type t = {
    index: int,
    numNeighbourBombs: int,
    state: tileState,
    hasBomb: bool,
  };

  let stringOfTile = tile =>
    if (tile.hasBomb) {
      "X";
    } else if (tile.numNeighbourBombs > 0) {
      string_of_int(tile.numNeighbourBombs);
    } else {
      "";
    };

  let reactOfTile = tile => {
    <span> {React.string(stringOfTile(tile))} </span>;
  };

  let noneTile = {
    index: (-1),
    numNeighbourBombs: 0,
    state: Hidden,
    hasBomb: false,
  };

  let getNeighbours = (tiles, tile) => {
    let tileIndex = tile.index;
    let x = tileIndex mod boardWidth;
    let y = tileIndex / boardWidth;
    Belt.List.map([(-1), 0, 1], i =>
      Belt.List.map(
        [(-1), 0, 1],
        j => {
          let x2 = x + i;
          let y2 = y + j;
          if (i == 0
              && j == 0
              || x2 < 0
              || x2 >= boardWidth
              || y2 < 0
              || y2 >= boardHeight) {
            noneTile;
          } else {
            let neighbourIndex = y2 * boardWidth + x2;
            Belt.Map.Int.getWithDefault(tiles, neighbourIndex, noneTile);
          };
        },
      )
    )
    ->Belt.List.flatten
    ->Belt.List.keep(t => t != noneTile);
  };

  let calcNumNeighbourBombs = (tiles, tile) => {
    getNeighbours(tiles, tile)
    ->Belt.List.keep(t => t.hasBomb)
    ->Belt.List.length;
  };

  let initTiles = numBombs => {
    let bombIndexes =
      Belt.Array.makeByAndShuffle(boardWidth * boardHeight, i => i)
      ->Belt.Array.slice(~offset=0, ~len=numBombs);

    let tiles =
      Belt.Array.make(boardHeight * boardWidth, 0)
      ->Belt.Array.mapWithIndex((index, _value) =>
          (
            index,
            {
              index,
              state: Hidden,
              numNeighbourBombs: 0,
              hasBomb:
                Belt.Array.some(bombIndexes, bombIndex => bombIndex == index),
            },
          )
        )
      ->Belt.Map.Int.fromArray;

    Belt.Map.Int.map(tiles, t =>
      {...t, numNeighbourBombs: calcNumNeighbourBombs(tiles, t)}
    );
  };

  [@react.component]
  let make = (~onClick, ~tile) => {
    <button className="square" onClick> {reactOfTile(tile)} </button>;
  };
};

type action =
  | NextMove(int)
  | JumpToHistory(int);

type state = {tiles: Belt.Map.Int.t(Tile.t)};

module Board = {
  [@react.component]
  let make = (~onClick, ~tiles) => {
    let renderTile = tile => {
      <Tile onClick={evt => onClick(evt, tile)} tile />;
    };

    ReasonReact.array(
      Belt.Array.map(Belt.Array.makeBy(boardHeight, i => i), row =>
        <div className="board-row" key={string_of_int(row)}>
          {Belt.Array.map(
             Belt.Array.makeBy(boardWidth, i => i),
             col => {
               let tileIndex = boardWidth * row + col;
               renderTile(
                 Belt.Map.Int.getWithDefault(tiles, tileIndex, Tile.noneTile),
               );
             },
           )
           ->ReasonReact.array}
        </div>
      ),
    );
  };
};

let handleClick = (_event, tile) => Js.log(tile);

[@react.component]
let make = (~message) => {
  let (state, dispatch) =
    React.useReducer(
      (state, action) =>
        switch (action) {
        | NextMove(squareIndex) => state
        | JumpToHistory(historyIndex) => state
        },
      {tiles: Tile.initTiles(numBombs)},
    );

  let neighbourDebug = _evt => {
    Js.log("click neighbour debug");
    let tile = Belt.Map.Int.getExn(state.tiles, 19);
    Js.log(tile);
    let neighbours = Tile.getNeighbours(state.tiles, tile);
    Js.log(Belt.List.toArray(neighbours));
  };

  <div className="game-board">
    <Board onClick=handleClick tiles={state.tiles} />
    <button onClick=neighbourDebug>
      {React.string("neighbour debug")}
    </button>
  </div>;
};