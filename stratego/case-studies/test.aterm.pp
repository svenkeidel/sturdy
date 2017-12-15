Specification(
  [ Signature(
      [ Constructors(
          [ ExtOpDecl("Nil", ConstType(Sort("List", [SortVar("a")])))
          , ExtOpDecl(
              "Cons"
            , FunType(
                [ConstType(SortVar("a")), ConstType(Sort("List", [SortVar("a")]))]
              , ConstType(Sort("List", [SortVar("a")]))
              )
            )
          , OpDecl("DR_DUMMY", ConstType(Sort("ATerm", [])))
          , ExtOpDecl(
              ""
            , FunType(
                [ConstType(SortVar("a")), ConstType(SortVar("b")), ConstType(SortVar("c"))]
              , ConstType(
                  Sort(
                    "Tuple"
                  , [ Sort(
                        "Cons"
                      , [ SortVar("a")
                        , Sort(
                            "Cons"
                          , [ SortVar("b")
                            , Sort("Cons", [SortVar("c"), Sort("Nil", [])])
                            ]
                          )
                        ]
                      )
                    ]
                  )
                )
              )
            )
          , OpDecl(
              "App"
            , FunType(
                [ConstType(Sort("String", [])), ConstType(Sort("Expr", []))]
              , ConstType(Sort("Expr", []))
              )
            )
          , OpDecl(
              "Function"
            , FunType(
                [ ConstType(Sort("String", []))
                , ConstType(Sort("String", []))
                , ConstType(Sort("Expr", []))
                ]
              , ConstType(Sort("Expr", []))
              )
            )
          ]
        )
      ]
    )
  , Strategies(
      [ SDefT(
          "DefineUnfoldCall_0_0"
        , []
        , []
        , Scope(
            [ "v_0"
            , "w_0"
            , "x_0"
            , "y_0"
            , "z_0"
            , "a_1"
            , "b_1"
            , "c_1"
            , "d_1"
            , "e_1"
            ]
          , Seq(
              Match(
                Anno(
                  Op(
                    "Function"
                  , [Var("v_0"), Var("w_0"), Var("x_0")]
                  )
                , Wld()
                )
              )
            , Seq(
                Match(Var("y_0"))
              , Seq(
                  Match(Var("c_1"))
                , Seq(
                    Build(Anno(Str("UnfoldCall"), Op("Nil", [])))
                  , Seq(
                      Match(Var("z_0"))
                    , Seq(
                        Build(Var("c_1"))
                      , Seq(
                          Match(Var("d_1"))
                        , Seq(
                            Build(
                              Anno(
                                Op(
                                  "App"
                                , [ Var("v_0")
                                  , Anno(
                                      Op(
                                        "Cons"
                                      , [ Anno(Op("DR_DUMMY", []), Op("Nil", []))
                                        , Anno(Op("Nil", []), Op("Nil", []))
                                        ]
                                      )
                                    , Op("Nil", [])
                                    )
                                  ]
                                )
                              , Op(
                                  "Cons"
                                , [ Anno(Op("DR_DUMMY", []), Op("Nil", []))
                                  , Anno(Op("Nil", []), Op("Nil", []))
                                  ]
                                )
                              )
                            )
                          , Seq(
                              Match(Var("a_1"))
                            , Seq(
                                Build(Var("d_1"))
                              , Seq(
                                  Match(Var("e_1"))
                                , Seq(
                                    Build(
                                      Anno(
                                        Op(
                                          ""
                                        , [ Anno(Str("-590029"), Op("Nil", []))
                                          , Var("w_0")
                                          , Var("x_0")
                                          ]
                                        )
                                      , Op("Nil", [])
                                      )
                                    )
                                  , Seq(
                                      Match(Var("b_1"))
                                    , Seq(
                                        Build(Var("e_1"))
                                      , Seq(
                                          CallT(
                                            SVar("dr_set_rule_0_3")
                                          , []
                                          , [Var("z_0"), Var("a_1"), Var("b_1")]
                                          )
                                        , Build(Var("y_0"))
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "main_0_0"
        , []
        , []
        , CallT(SVar("DefineUnfoldCall_0_0"), [], [])
        )
      , ExtSDef(
          "dr_set_rule_0_3"
        , []
        , [ VarDec("h_33", ConstType(Sort("ATerm", [])))
          , VarDec("i_33", ConstType(Sort("ATerm", [])))
          , VarDec("j_33", ConstType(Sort("ATerm", [])))
          ]
        )
      ]
    )
  ]
)
