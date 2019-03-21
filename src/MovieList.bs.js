// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Json_decode = require("@glennsl/bs-json/src/Json_decode.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var Genres$ReactTemplate = require("./Genres.bs.js");

function movie(json) {
  var __x = Json_decode.field("genre_ids", (function (param) {
          return Json_decode.array(Json_decode.$$int, param);
        }), json);
  return /* record */[
          /* title */Json_decode.field("original_title", Json_decode.string, json),
          /* genres */Belt_Array.map(__x, (function (g) {
                  return g;
                })),
          /* image */Json_decode.field("poster_path", Json_decode.string, json)
        ];
}

function listMovie(json) {
  return Json_decode.field("results", (function (param) {
                return Json_decode.array(movie, param);
              }), json);
}

var Decode = /* module */[
  /* movie */movie,
  /* listMovie */listMovie
];

var searchUrl = "https://api.themoviedb.org/3/discover/movie?api_key=cee134a9d530ca1dc02dd7058c66352f";

var component = ReasonReact.reducerComponent("MovieList");

function make(_children) {
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */(function (self) {
              return Curry._1(self[/* send */3], /* MovieFetch */0);
            }),
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (self) {
              var match = self[/* state */1][/* pageState */0];
              return React.createElement("div", undefined, ReasonReact.element(undefined, undefined, Genres$ReactTemplate.make((function (id) {
                                    return Curry._1(self[/* send */3], /* HandleId */Block.__(1, [id]));
                                  }), /* array */[])), typeof match === "number" ? (
                            match !== 0 ? React.createElement("div", undefined, "An error occurred!") : React.createElement("div", undefined, "Loading...")
                          ) : React.createElement("div", undefined, React.createElement("h1", undefined, "Movie"), React.createElement("ul", undefined, Belt_Array.map(match[0], (function (movie) {
                                          return React.createElement("div", {
                                                      style: {
                                                        display: "inline-block",
                                                        verticalAlign: "top",
                                                        width: "200px"
                                                      }
                                                    }, React.createElement("img", {
                                                          src: "https://image.tmdb.org/t/p/w500/" + movie[/* image */2],
                                                          width: "150px"
                                                        }), React.createElement("p", {
                                                          key: movie[/* title */0]
                                                        }, movie[/* title */0]));
                                        })))));
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
                                var hasId = self[/* state */1][/* selectedId */1] !== 0;
                                var url = hasId ? searchUrl + ("&with_genres=" + String(self[/* state */1][/* selectedId */1])) : searchUrl;
                                fetch(url).then((function (prim) {
                                            return prim.json();
                                          })).then((function (json) {
                                          var listMovie$1 = listMovie(json);
                                          return Promise.resolve(Curry._1(self[/* send */3], /* MovieFetched */Block.__(0, [listMovie$1])));
                                        })).catch((function (_err) {
                                        console.log(_err);
                                        return Promise.resolve(Curry._1(self[/* send */3], /* MovieFailedToFetch */1));
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
                              return Curry._1(self[/* send */3], /* MovieFetch */0);
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
exports.searchUrl = searchUrl;
exports.component = component;
exports.make = make;
/* component Not a pure module */
