// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Belt_MapInt = require("bs-platform/lib/js/belt_MapInt.js");

function stringOfTile(tile) {
  if (tile[/* hasBomb */4]) {
    return "X";
  } else if (tile[/* numNeighbourBombs */1] > 0) {
    return String(tile[/* numNeighbourBombs */1]);
  } else {
    return "0";
  }
}

function reactOfTile(tile, showHidden) {
  if (tile[/* state */3] === /* Hidden */0 && !showHidden) {
    return null;
  } else {
    return React.createElement("span", undefined, stringOfTile(tile));
  }
}

var noneTile = /* record */[
  /* index */-1,
  /* numNeighbourBombs */0,
  /* neighbours : [] */0,
  /* state : Hidden */0,
  /* hasBomb */false
];

function getNeighbours(tiles, tile) {
  var tileIndex = tile[/* index */0];
  var x = tileIndex % 20;
  var y = tileIndex / 20 | 0;
  return Belt_List.keep(Belt_List.flatten(Belt_List.map(/* :: */[
                      -1,
                      /* :: */[
                        0,
                        /* :: */[
                          1,
                          /* [] */0
                        ]
                      ]
                    ], (function (i) {
                        return Belt_List.map(/* :: */[
                                    -1,
                                    /* :: */[
                                      0,
                                      /* :: */[
                                        1,
                                        /* [] */0
                                      ]
                                    ]
                                  ], (function (j) {
                                      var x2 = x + i | 0;
                                      var y2 = y + j | 0;
                                      if (i === 0 && j === 0 || x2 < 0 || x2 >= 20 || y2 < 0 || y2 >= 20) {
                                        return noneTile;
                                      } else {
                                        var neighbourIndex = Caml_int32.imul(y2, 20) + x2 | 0;
                                        return Belt_MapInt.getWithDefault(tiles, neighbourIndex, noneTile);
                                      }
                                    }));
                      }))), (function (t) {
                return Caml_obj.caml_notequal(t, noneTile);
              }));
}

function calcNumNeighbourBombs(tiles, tile) {
  return Belt_List.length(Belt_List.keep(getNeighbours(tiles, tile), (function (t) {
                    return t[/* hasBomb */4];
                  })));
}

function initTiles(numBombs) {
  var bombIndexes = Belt_Array.slice(Belt_Array.makeByAndShuffle(400, (function (i) {
              return i;
            })), 0, numBombs);
  var tiles = Belt_MapInt.fromArray(Belt_Array.mapWithIndex(Belt_Array.make(400, 0), (function (index, _value) {
              return /* tuple */[
                      index,
                      /* record */[
                        /* index */index,
                        /* numNeighbourBombs */0,
                        /* neighbours : [] */0,
                        /* state : Hidden */0,
                        /* hasBomb */Belt_Array.some(bombIndexes, (function (bombIndex) {
                                return bombIndex === index;
                              }))
                      ]
                    ];
            })));
  var tilesWithCalcBombs = Belt_MapInt.map(tiles, (function (t) {
          return /* record */[
                  /* index */t[/* index */0],
                  /* numNeighbourBombs */calcNumNeighbourBombs(tiles, t),
                  /* neighbours */t[/* neighbours */2],
                  /* state */t[/* state */3],
                  /* hasBomb */t[/* hasBomb */4]
                ];
        }));
  return Belt_MapInt.map(tilesWithCalcBombs, (function (t) {
                return /* record */[
                        /* index */t[/* index */0],
                        /* numNeighbourBombs */t[/* numNeighbourBombs */1],
                        /* neighbours */getNeighbours(tilesWithCalcBombs, t),
                        /* state */t[/* state */3],
                        /* hasBomb */t[/* hasBomb */4]
                      ];
              }));
}

function Game$Tile(Props) {
  var onClick = Props.onClick;
  var tile = Props.tile;
  var showHidden = Props.showHidden;
  return React.createElement("button", {
              className: "square",
              onClick: onClick
            }, reactOfTile(tile, showHidden));
}

var Tile = /* module */[
  /* stringOfTile */stringOfTile,
  /* reactOfTile */reactOfTile,
  /* noneTile */noneTile,
  /* getNeighbours */getNeighbours,
  /* calcNumNeighbourBombs */calcNumNeighbourBombs,
  /* initTiles */initTiles,
  /* make */Game$Tile
];

function Game$Board(Props) {
  var onClick = Props.onClick;
  var state = Props.state;
  return Belt_Array.map(Belt_Array.makeBy(20, (function (i) {
                    return i;
                  })), (function (row) {
                return React.createElement("div", {
                            key: String(row),
                            className: "board-row"
                          }, Belt_Array.map(Belt_Array.makeBy(20, (function (i) {
                                      return i;
                                    })), (function (col) {
                                  var tileIndex = Caml_int32.imul(20, row) + col | 0;
                                  var tile = Belt_MapInt.getWithDefault(state[/* tiles */0], tileIndex, noneTile);
                                  return React.createElement(Game$Tile, {
                                              onClick: (function (evt) {
                                                  return Curry._2(onClick, evt, tile);
                                                }),
                                              tile: tile,
                                              showHidden: state[/* showHidden */1]
                                            });
                                })));
              }));
}

var Board = /* module */[/* make */Game$Board];

function revealTile(tile, state) {
  var revealNeighbourTiles = function (tile, tiles) {
    var tiles$1 = Belt_MapInt.set(state[/* tiles */0], tile[/* index */0], /* record */[
          /* index */tile[/* index */0],
          /* numNeighbourBombs */tile[/* numNeighbourBombs */1],
          /* neighbours */tile[/* neighbours */2],
          /* state : Revealed */1,
          /* hasBomb */tile[/* hasBomb */4]
        ]);
    if (tile[/* numNeighbourBombs */1] === 0) {
      return Belt_List.reduce(tile[/* neighbours */2], tiles$1, (function (tiles, t) {
                    console.log(t[/* index */0]);
                    console.log(t[/* hasBomb */4]);
                    console.log(t[/* numNeighbourBombs */1]);
                    return Belt_MapInt.set(tiles, t[/* index */0], /* record */[
                                /* index */t[/* index */0],
                                /* numNeighbourBombs */t[/* numNeighbourBombs */1],
                                /* neighbours */t[/* neighbours */2],
                                /* state : Revealed */1,
                                /* hasBomb */t[/* hasBomb */4]
                              ]);
                  }));
    } else {
      return tiles$1;
    }
  };
  return /* record */[
          /* tiles */revealNeighbourTiles(tile, state[/* tiles */0]),
          /* showHidden */state[/* showHidden */1]
        ];
}

function Game(Props) {
  Props.message;
  var match = React.useReducer((function (state, action) {
          if (action) {
            return revealTile(action[0], state);
          } else {
            return /* record */[
                    /* tiles */state[/* tiles */0],
                    /* showHidden */!state[/* showHidden */1]
                  ];
          }
        }), /* record */[
        /* tiles */initTiles(10),
        /* showHidden */false
      ]);
  var dispatch = match[1];
  var handleClick = function (_event, tile) {
    return Curry._1(dispatch, /* RevealTile */[tile]);
  };
  var toggleHidden = function (_evt) {
    return Curry._1(dispatch, /* ToggleHidden */0);
  };
  return React.createElement("div", {
              className: "game-board"
            }, React.createElement(Game$Board, {
                  onClick: handleClick,
                  state: match[0]
                }), React.createElement("button", {
                  onClick: toggleHidden
                }, "toggle hidden"));
}

var boardWidth = 20;

var boardHeight = 20;

var numBombs = 10;

var make = Game;

exports.boardWidth = boardWidth;
exports.boardHeight = boardHeight;
exports.numBombs = numBombs;
exports.Tile = Tile;
exports.Board = Board;
exports.revealTile = revealTile;
exports.make = make;
/* react Not a pure module */
