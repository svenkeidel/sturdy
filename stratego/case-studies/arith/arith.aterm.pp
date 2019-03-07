Specification(
  [ Signature(
      [ Constructors(
          [ OpDecl("Zero", ConstType(SortNoArgs("Exp")))
          , OpDecl(
              "Succ"
            , FunType([ConstType(SortNoArgs("Exp"))], ConstType(SortNoArgs("Exp")))
            )
          , OpDecl(
              "Mul"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("Exp"))
              )
            )
          , OpDecl(
              "Add"
            , FunType(
                [ConstType(SortNoArgs("Exp")), ConstType(SortNoArgs("Exp"))]
              , ConstType(SortNoArgs("Exp"))
              )
            )
          , OpDecl("Nil", ConstType(Sort("List", [SortVar("a")])))
          , OpDecl(
              "Cons"
            , FunType(
                [ConstType(SortVar("a")), ConstType(Sort("List", [SortVar("a")]))]
              , ConstType(Sort("List", [SortVar("a")]))
              )
            )
          , OpDeclInj(ConstType(SortTuple([])))
          , OpDeclInj(
              FunType(
                [ConstType(SortVar("a"))]
              , ConstType(SortTuple([SortVar("a")]))
              )
            )
          , OpDeclInj(
              FunType(
                [ConstType(SortVar("a")), ConstType(SortVar("b"))]
              , ConstType(SortTuple([SortVar("a"), SortVar("b")]))
              )
            )
          , OpDeclInj(
              FunType(
                [ConstType(SortVar("a")), ConstType(SortVar("b")), ConstType(SortVar("c"))]
              , ConstType(
                  SortTuple([SortVar("a"), SortVar("b"), SortVar("c")])
                )
              )
            )
          ]
        )
      ]
    )
  , Strategies(
      [ SDefT(
          "eval_0_0"
        , []
        , []
        , GuardedLChoice(
            Seq(
              Match(Anno(Op("Zero", []), Wld()))
            , Build(Anno(Op("Zero", []), Op("Nil", [])))
            )
          , Id()
          , GuardedLChoice(
              Scope(
                ["m_1", "n_1", "o_1"]
              , Seq(
                  Match(Anno(Op("Succ", [Var("m_1")]), Wld()))
                , Seq(
                    Match(Var("o_1"))
                  , Seq(
                      Build(Var("m_1"))
                    , Seq(
                        CallT(SVar("eval_0_0"), [], [])
                      , Seq(
                          Match(Var("n_1"))
                        , Seq(
                            Build(Var("o_1"))
                          , Build(
                              Anno(
                                Op("Succ", [Var("n_1")])
                              , Op("Nil", [])
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            , Id()
            , GuardedLChoice(
                Seq(
                  Match(
                    Anno(
                      Op(
                        "Mul"
                      , [Anno(Op("Zero", []), Wld()), Wld()]
                      )
                    , Wld()
                    )
                  )
                , Build(Anno(Op("Zero", []), Op("Nil", [])))
                )
              , Id()
              , GuardedLChoice(
                  Scope(
                    ["k_1", "l_1"]
                  , Seq(
                      Match(
                        Anno(
                          Op(
                            "Mul"
                          , [Anno(Op("Succ", [Var("k_1")]), Wld()), Var("l_1")]
                          )
                        , Wld()
                        )
                      )
                    , Seq(
                        Build(
                          Anno(
                            Op(
                              "Add"
                            , [ Anno(
                                  Op("Mul", [Var("k_1"), Var("l_1")])
                                , Op("Nil", [])
                                )
                              , Var("l_1")
                              ]
                            )
                          , Op("Nil", [])
                          )
                        )
                      , CallT(SVar("eval_0_0"), [], [])
                      )
                    )
                  )
                , Id()
                , GuardedLChoice(
                    Scope(
                      ["e_1", "f_1", "g_1", "i_1", "h_1", "j_1"]
                    , Seq(
                        Match(
                          Anno(
                            Op("Mul", [Var("e_1"), Var("f_1")])
                          , Wld()
                          )
                        )
                      , Seq(
                          Match(Var("i_1"))
                        , Seq(
                            Build(Var("e_1"))
                          , Seq(
                              CallT(SVar("eval_0_0"), [], [])
                            , Seq(
                                Match(Var("g_1"))
                              , Seq(
                                  Build(Var("i_1"))
                                , Seq(
                                    Match(Var("j_1"))
                                  , Seq(
                                      Build(Var("f_1"))
                                    , Seq(
                                        CallT(SVar("eval_0_0"), [], [])
                                      , Seq(
                                          Match(Var("h_1"))
                                        , Seq(
                                            Build(Var("j_1"))
                                          , Seq(
                                              Build(
                                                Anno(
                                                  Op("Mul", [Var("g_1"), Var("h_1")])
                                                , Op("Nil", [])
                                                )
                                              )
                                            , CallT(SVar("eval_0_0"), [], [])
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
                  , Id()
                  , GuardedLChoice(
                      Scope(
                        ["d_1"]
                      , Seq(
                          Match(
                            Anno(
                              Op(
                                "Add"
                              , [Anno(Op("Zero", []), Wld()), Var("d_1")]
                              )
                            , Wld()
                            )
                          )
                        , Seq(
                            Build(Var("d_1"))
                          , CallT(SVar("eval_0_0"), [], [])
                          )
                        )
                      )
                    , Id()
                    , GuardedLChoice(
                        Scope(
                          ["b_1", "c_1"]
                        , Seq(
                            Match(
                              Anno(
                                Op(
                                  "Add"
                                , [Anno(Op("Succ", [Var("b_1")]), Wld()), Var("c_1")]
                                )
                              , Wld()
                              )
                            )
                          , Seq(
                              Build(
                                Anno(
                                  Op(
                                    "Succ"
                                  , [ Anno(
                                        Op("Add", [Var("b_1"), Var("c_1")])
                                      , Op("Nil", [])
                                      )
                                    ]
                                  )
                                , Op("Nil", [])
                                )
                              )
                            , CallT(SVar("eval_0_0"), [], [])
                            )
                          )
                        )
                      , Id()
                      , Scope(
                          ["v_0", "w_0", "x_0", "z_0", "y_0", "a_1"]
                        , Seq(
                            Match(
                              Anno(
                                Op("Add", [Var("v_0"), Var("w_0")])
                              , Wld()
                              )
                            )
                          , Seq(
                              Match(Var("z_0"))
                            , Seq(
                                Build(Var("v_0"))
                              , Seq(
                                  CallT(SVar("eval_0_0"), [], [])
                                , Seq(
                                    Match(Var("x_0"))
                                  , Seq(
                                      Build(Var("z_0"))
                                    , Seq(
                                        Match(Var("a_1"))
                                      , Seq(
                                          Build(Var("w_0"))
                                        , Seq(
                                            CallT(SVar("eval_0_0"), [], [])
                                          , Seq(
                                              Match(Var("y_0"))
                                            , Seq(
                                                Build(Var("a_1"))
                                              , Build(
                                                  Anno(
                                                    Op("Add", [Var("x_0"), Var("y_0")])
                                                  , Op("Nil", [])
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
                )
              )
            )
          )
        )
      , SDefT(
          "Anno__Cong_____2_0"
        , [ VarDec(
              "t_1"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "u_1"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["p_1", "q_1", "r_1", "s_1"]
          , Seq(
              Match(Anno(Var("p_1"), Var("q_1")))
            , Seq(
                Build(Var("p_1"))
              , Seq(
                  CallT(SVar("t_1"), [], [])
                , Seq(
                    Match(Var("r_1"))
                  , Seq(
                      Build(Var("q_1"))
                    , Seq(
                        CallT(SVar("u_1"), [], [])
                      , Seq(
                          Match(Var("s_1"))
                        , Build(Anno(Var("r_1"), Var("s_1")))
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
          "Zero_0_0"
        , []
        , []
        , Match(Anno(Op("Zero", []), Wld()))
        )
      , SDefT(
          "Succ_1_0"
        , [ VarDec(
              "v_1"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["k_2", "j_2", "l_2"]
          , Seq(
              Match(
                Anno(Op("Succ", [Var("j_2")]), Var("k_2"))
              )
            , Seq(
                Build(Var("j_2"))
              , Seq(
                  CallT(SVar("v_1"), [], [])
                , Seq(
                    Match(Var("l_2"))
                  , Build(
                      Anno(Op("Succ", [Var("l_2")]), Var("k_2"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "Mul_2_0"
        , [ VarDec(
              "w_1"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "x_1"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["o_2", "m_2", "n_2", "p_2", "q_2"]
          , Seq(
              Match(
                Anno(
                  Op("Mul", [Var("m_2"), Var("n_2")])
                , Var("o_2")
                )
              )
            , Seq(
                Build(Var("m_2"))
              , Seq(
                  CallT(SVar("w_1"), [], [])
                , Seq(
                    Match(Var("p_2"))
                  , Seq(
                      Build(Var("n_2"))
                    , Seq(
                        CallT(SVar("x_1"), [], [])
                      , Seq(
                          Match(Var("q_2"))
                        , Build(
                            Anno(
                              Op("Mul", [Var("p_2"), Var("q_2")])
                            , Var("o_2")
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
          "Add_2_0"
        , [ VarDec(
              "y_1"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "z_1"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["t_2", "r_2", "s_2", "u_2", "v_2"]
          , Seq(
              Match(
                Anno(
                  Op("Add", [Var("r_2"), Var("s_2")])
                , Var("t_2")
                )
              )
            , Seq(
                Build(Var("r_2"))
              , Seq(
                  CallT(SVar("y_1"), [], [])
                , Seq(
                    Match(Var("u_2"))
                  , Seq(
                      Build(Var("s_2"))
                    , Seq(
                        CallT(SVar("z_1"), [], [])
                      , Seq(
                          Match(Var("v_2"))
                        , Build(
                            Anno(
                              Op("Add", [Var("u_2"), Var("v_2")])
                            , Var("t_2")
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
          "Nil_0_0"
        , []
        , []
        , Match(Anno(Op("Nil", []), Wld()))
        )
      , SDefT(
          "Cons_2_0"
        , [ VarDec(
              "a_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "b_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["y_2", "w_2", "x_2", "z_2", "a_3"]
          , Seq(
              Match(
                Anno(
                  Op("Cons", [Var("w_2"), Var("x_2")])
                , Var("y_2")
                )
              )
            , Seq(
                Build(Var("w_2"))
              , Seq(
                  CallT(SVar("a_2"), [], [])
                , Seq(
                    Match(Var("z_2"))
                  , Seq(
                      Build(Var("x_2"))
                    , Seq(
                        CallT(SVar("b_2"), [], [])
                      , Seq(
                          Match(Var("a_3"))
                        , Build(
                            Anno(
                              Op("Cons", [Var("z_2"), Var("a_3")])
                            , Var("y_2")
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
          "_0_0"
        , []
        , []
        , Match(Anno(Op("", []), Wld()))
        )
      , SDefT(
          "_1_0"
        , [ VarDec(
              "c_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["c_3", "b_3", "d_3"]
          , Seq(
              Match(
                Anno(Op("", [Var("b_3")]), Var("c_3"))
              )
            , Seq(
                Build(Var("b_3"))
              , Seq(
                  CallT(SVar("c_2"), [], [])
                , Seq(
                    Match(Var("d_3"))
                  , Build(
                      Anno(Op("", [Var("d_3")]), Var("c_3"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "_2_0"
        , [ VarDec(
              "d_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "e_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["g_3", "e_3", "f_3", "h_3", "i_3"]
          , Seq(
              Match(
                Anno(
                  Op("", [Var("e_3"), Var("f_3")])
                , Var("g_3")
                )
              )
            , Seq(
                Build(Var("e_3"))
              , Seq(
                  CallT(SVar("d_2"), [], [])
                , Seq(
                    Match(Var("h_3"))
                  , Seq(
                      Build(Var("f_3"))
                    , Seq(
                        CallT(SVar("e_2"), [], [])
                      , Seq(
                          Match(Var("i_3"))
                        , Build(
                            Anno(
                              Op("", [Var("h_3"), Var("i_3")])
                            , Var("g_3")
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
          "_3_0"
        , [ VarDec(
              "f_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "g_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "h_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["m_3", "j_3", "k_3", "l_3", "n_3", "o_3", "p_3"]
          , Seq(
              Match(
                Anno(
                  Op(
                    ""
                  , [Var("j_3"), Var("k_3"), Var("l_3")]
                  )
                , Var("m_3")
                )
              )
            , Seq(
                Build(Var("j_3"))
              , Seq(
                  CallT(SVar("f_2"), [], [])
                , Seq(
                    Match(Var("n_3"))
                  , Seq(
                      Build(Var("k_3"))
                    , Seq(
                        CallT(SVar("g_2"), [], [])
                      , Seq(
                          Match(Var("o_3"))
                        , Seq(
                            Build(Var("l_3"))
                          , Seq(
                              CallT(SVar("h_2"), [], [])
                            , Seq(
                                Match(Var("p_3"))
                              , Build(
                                  Anno(
                                    Op(
                                      ""
                                    , [Var("n_3"), Var("o_3"), Var("p_3")]
                                    )
                                  , Var("m_3")
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
          "DR__UNDEFINE_1_0"
        , [ VarDec(
              "i_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["r_3", "q_3", "s_3"]
          , Seq(
              Match(
                Anno(Op("DR_UNDEFINE", [Var("q_3")]), Var("r_3"))
              )
            , Seq(
                Build(Var("q_3"))
              , Seq(
                  CallT(SVar("i_2"), [], [])
                , Seq(
                    Match(Var("s_3"))
                  , Build(
                      Anno(Op("DR_UNDEFINE", [Var("s_3")]), Var("r_3"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "DR__DUMMY_0_0"
        , []
        , []
        , Match(Anno(Op("DR_DUMMY", []), Wld()))
        )
      ]
    )
  ]
)
