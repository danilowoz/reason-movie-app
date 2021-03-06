// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Json_decode = require("@glennsl/bs-json/src/Json_decode.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");

function genre(json) {
  return /* record */[
          /* id */Json_decode.field("id", Json_decode.$$int, json),
          /* name */Json_decode.field("name", Json_decode.string, json)
        ];
}

function list(json) {
  return Json_decode.field("genres", (function (param) {
                return Json_decode.array(genre, param);
              }), json);
}

var Decode = /* module */[
  /* genre */genre,
  /* list */list
];

var getList = "https://api.themoviedb.org/3/genre/movie/list?api_key=cee134a9d530ca1dc02dd7058c66352f";

var component = ReasonReact.reducerComponent("Genres");

function make(getId, _children) {
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */(function (self) {
              return Curry._1(self[/* send */3], /* GenreFetch */0);
            }),
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (self) {
              var match = self[/* state */1][/* pageState */0];
              return React.createElement("div", undefined, React.createElement("h1", undefined, "Genres"), typeof match === "number" ? (
                            match !== 0 ? React.createElement("div", undefined, "An error occurred!") : React.createElement("div", undefined, "Loading...")
                          ) : React.createElement("div", undefined, Belt_Array.map(match[0], (function (g) {
                                      return React.createElement("button", {
                                                  key: String(g[/* id */0]),
                                                  onClick: (function (_e) {
                                                      return Curry._1(self[/* send */3], /* HandleId */Block.__(1, [g[/* id */0]]));
                                                    })
                                                }, g[/* name */1]);
                                    }))));
            }),
          /* initialState */(function (_state) {
              return /* record */[
                      /* pageState : Loading */0,
                      /* selectedId */0
                    ];
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, _state) {
              if (typeof action === "number") {
                if (action === 0) {
                  return /* UpdateWithSideEffects */Block.__(2, [
                            /* record */[
                              /* pageState : Loading */0,
                              /* selectedId */_state[/* selectedId */1]
                            ],
                            (function (self) {
                                fetch(getList).then((function (prim) {
                                            return prim.json();
                                          })).then((function (json) {
                                          var listGenre = list(json);
                                          return Promise.resolve(Curry._1(self[/* send */3], /* GenreFetched */Block.__(0, [listGenre])));
                                        })).catch((function (_err) {
                                        console.log(_err);
                                        return Promise.resolve(Curry._1(self[/* send */3], /* GenreFailedToFetch */1));
                                      }));
                                return /* () */0;
                              })
                          ]);
                } else {
                  return /* Update */Block.__(0, [/* record */[
                              /* pageState : Error */1,
                              /* selectedId */_state[/* selectedId */1]
                            ]]);
                }
              } else if (action.tag) {
                return /* UpdateWithSideEffects */Block.__(2, [
                          /* record */[
                            /* pageState */_state[/* pageState */0],
                            /* selectedId */action[0]
                          ],
                          (function (self) {
                              return Curry._1(getId, self[/* state */1][/* selectedId */1]);
                            })
                        ]);
              } else {
                return /* Update */Block.__(0, [/* record */[
                            /* pageState : Loaded */[action[0]],
                            /* selectedId */_state[/* selectedId */1]
                          ]]);
              }
            }),
          /* jsElementWrapped */component[/* jsElementWrapped */13]
        ];
}

exports.Decode = Decode;
exports.getList = getList;
exports.component = component;
exports.make = make;
/* component Not a pure module */
