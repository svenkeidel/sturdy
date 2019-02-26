Specification(
  [ Signature(
      [ Constructors(
          [ OpDecl(
              "Atom"
            , FunType([ConstType(SortNoArgs("String"))], ConstType(SortNoArgs("Formula")))
            )
          , OpDecl(
              "And"
            , FunType(
                [ConstType(SortNoArgs("Formula")), ConstType(SortNoArgs("Formula"))]
              , ConstType(SortNoArgs("Formula"))
              )
            )
          , OpDecl(
              "Or"
            , FunType(
                [ConstType(SortNoArgs("Formula")), ConstType(SortNoArgs("Formula"))]
              , ConstType(SortNoArgs("Formula"))
              )
            )
          , OpDecl(
              "Imp"
            , FunType(
                [ConstType(SortNoArgs("Formula")), ConstType(SortNoArgs("Formula"))]
              , ConstType(SortNoArgs("Formula"))
              )
            )
          , OpDecl(
              "Iff"
            , FunType(
                [ConstType(SortNoArgs("Formula")), ConstType(SortNoArgs("Formula"))]
              , ConstType(SortNoArgs("Formula"))
              )
            )
          , OpDecl(
              "Neg"
            , FunType([ConstType(SortNoArgs("Formula"))], ConstType(SortNoArgs("Formula")))
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
          "nnf_0_0"
        , []
        , []
        , GuardedLChoice(
            Scope(
              ["l_1", "m_1"]
            , Seq(
                Match(
                  Anno(
                    Op(
                      "Neg"
                    , [Anno(
                         Op("Or", [Var("l_1"), Var("m_1")])
                       , Wld()
                       )]
                    )
                  , Wld()
                  )
                )
              , Build(
                  Anno(
                    Op(
                      "And"
                    , [ Anno(
                          Op("Neg", [Var("l_1")])
                        , Op("Nil", [])
                        )
                      , Anno(
                          Op("Neg", [Var("m_1")])
                        , Op("Nil", [])
                        )
                      ]
                    )
                  , Op("Nil", [])
                  )
                )
              )
            )
          , Id()
          , GuardedLChoice(
              Scope(
                ["j_1", "k_1"]
              , Seq(
                  Match(
                    Anno(
                      Op(
                        "Neg"
                      , [Anno(
                           Op("And", [Var("j_1"), Var("k_1")])
                         , Wld()
                         )]
                      )
                    , Wld()
                    )
                  )
                , Build(
                    Anno(
                      Op(
                        "Or"
                      , [ Anno(
                            Op("Neg", [Var("j_1")])
                          , Op("Nil", [])
                          )
                        , Anno(
                            Op("Neg", [Var("k_1")])
                          , Op("Nil", [])
                          )
                        ]
                      )
                    , Op("Nil", [])
                    )
                  )
                )
              )
            , Id()
            , GuardedLChoice(
                Scope(
                  ["h_1", "i_1"]
                , Seq(
                    Match(
                      Anno(
                        Op(
                          "Neg"
                        , [Anno(
                             Op("Imp", [Var("h_1"), Var("i_1")])
                           , Wld()
                           )]
                        )
                      , Wld()
                      )
                    )
                  , Build(
                      Anno(
                        Op(
                          "And"
                        , [ Var("h_1")
                          , Anno(
                              Op("Neg", [Var("i_1")])
                            , Op("Nil", [])
                            )
                          ]
                        )
                      , Op("Nil", [])
                      )
                    )
                  )
                )
              , Id()
              , GuardedLChoice(
                  Scope(
                    ["f_1", "g_1"]
                  , Seq(
                      Match(
                        Anno(
                          Op(
                            "Neg"
                          , [Anno(
                               Op("Iff", [Var("f_1"), Var("g_1")])
                             , Wld()
                             )]
                          )
                        , Wld()
                        )
                      )
                    , Build(
                        Anno(
                          Op(
                            "Or"
                          , [ Anno(
                                Op(
                                  "And"
                                , [ Var("f_1")
                                  , Anno(
                                      Op("Neg", [Var("g_1")])
                                    , Op("Nil", [])
                                    )
                                  ]
                                )
                              , Op("Nil", [])
                              )
                            , Anno(
                                Op(
                                  "And"
                                , [ Anno(
                                      Op("Neg", [Var("f_1")])
                                    , Op("Nil", [])
                                    )
                                  , Var("g_1")
                                  ]
                                )
                              , Op("Nil", [])
                              )
                            ]
                          )
                        , Op("Nil", [])
                        )
                      )
                    )
                  )
                , Id()
                , GuardedLChoice(
                    Scope(
                      ["e_1"]
                    , Seq(
                        Match(
                          Anno(
                            Op(
                              "Neg"
                            , [Anno(Op("Neg", [Var("e_1")]), Wld())]
                            )
                          , Wld()
                          )
                        )
                      , Build(Var("e_1"))
                      )
                    )
                  , Id()
                  , GuardedLChoice(
                      Scope(
                        ["c_1", "d_1"]
                      , Seq(
                          Match(
                            Anno(
                              Op("Imp", [Var("c_1"), Var("d_1")])
                            , Wld()
                            )
                          )
                        , Build(
                            Anno(
                              Op(
                                "Or"
                              , [ Anno(
                                    Op("Neg", [Var("c_1")])
                                  , Op("Nil", [])
                                  )
                                , Var("d_1")
                                ]
                              )
                            , Op("Nil", [])
                            )
                          )
                        )
                      )
                    , Id()
                    , Scope(
                        ["a_1", "b_1"]
                      , Seq(
                          Match(
                            Anno(
                              Op("Iff", [Var("a_1"), Var("b_1")])
                            , Wld()
                            )
                          )
                        , Build(
                            Anno(
                              Op(
                                "Or"
                              , [ Anno(
                                    Op("And", [Var("a_1"), Var("b_1")])
                                  , Op("Nil", [])
                                  )
                                , Anno(
                                    Op(
                                      "And"
                                    , [ Anno(
                                          Op("Neg", [Var("a_1")])
                                        , Op("Nil", [])
                                        )
                                      , Anno(
                                          Op("Neg", [Var("b_1")])
                                        , Op("Nil", [])
                                        )
                                      ]
                                    )
                                  , Op("Nil", [])
                                  )
                                ]
                              )
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
      , SDefT(
          "topdown_1_0"
        , [ VarDec(
              "n_1"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Seq(
            CallT(SVar("n_1"), [], [])
          , All(
              CallT(
                SVar("topdown_1_0")
              , [CallT(SVar("n_1"), [], [])]
              , []
              )
            )
          )
        )
      , SDefT(
          "try_1_0"
        , [ VarDec(
              "o_1"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , GuardedLChoice(CallT(SVar("o_1"), [], []), Id(), Id())
        )
      , SDefT(
          "main_0_0"
        , []
        , []
        , CallT(
            SVar("topdown_1_0")
          , [GuardedLChoice(CallT(SVar("nnf_0_0"), [], []), Id(), Id())]
          , []
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
          "Atom_1_0"
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
            ["p_2", "o_2", "q_2"]
          , Seq(
              Match(
                Anno(Op("Atom", [Var("o_2")]), Var("p_2"))
              )
            , Seq(
                Build(Var("o_2"))
              , Seq(
                  CallT(SVar("v_1"), [], [])
                , Seq(
                    Match(Var("q_2"))
                  , Build(
                      Anno(Op("Atom", [Var("q_2")]), Var("p_2"))
                    )
                  )
                )
              )
            )
          )
        )
      , SDefT(
          "And_2_0"
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
            ["t_2", "r_2", "s_2", "u_2", "v_2"]
          , Seq(
              Match(
                Anno(
                  Op("And", [Var("r_2"), Var("s_2")])
                , Var("t_2")
                )
              )
            , Seq(
                Build(Var("r_2"))
              , Seq(
                  CallT(SVar("w_1"), [], [])
                , Seq(
                    Match(Var("u_2"))
                  , Seq(
                      Build(Var("s_2"))
                    , Seq(
                        CallT(SVar("x_1"), [], [])
                      , Seq(
                          Match(Var("v_2"))
                        , Build(
                            Anno(
                              Op("And", [Var("u_2"), Var("v_2")])
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
          "Or_2_0"
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
            ["y_2", "w_2", "x_2", "z_2", "a_3"]
          , Seq(
              Match(
                Anno(
                  Op("Or", [Var("w_2"), Var("x_2")])
                , Var("y_2")
                )
              )
            , Seq(
                Build(Var("w_2"))
              , Seq(
                  CallT(SVar("y_1"), [], [])
                , Seq(
                    Match(Var("z_2"))
                  , Seq(
                      Build(Var("x_2"))
                    , Seq(
                        CallT(SVar("z_1"), [], [])
                      , Seq(
                          Match(Var("a_3"))
                        , Build(
                            Anno(
                              Op("Or", [Var("z_2"), Var("a_3")])
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
          "Imp_2_0"
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
            ["d_3", "b_3", "c_3", "e_3", "f_3"]
          , Seq(
              Match(
                Anno(
                  Op("Imp", [Var("b_3"), Var("c_3")])
                , Var("d_3")
                )
              )
            , Seq(
                Build(Var("b_3"))
              , Seq(
                  CallT(SVar("a_2"), [], [])
                , Seq(
                    Match(Var("e_3"))
                  , Seq(
                      Build(Var("c_3"))
                    , Seq(
                        CallT(SVar("b_2"), [], [])
                      , Seq(
                          Match(Var("f_3"))
                        , Build(
                            Anno(
                              Op("Imp", [Var("e_3"), Var("f_3")])
                            , Var("d_3")
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
          "Iff_2_0"
        , [ VarDec(
              "c_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "d_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["i_3", "g_3", "h_3", "j_3", "k_3"]
          , Seq(
              Match(
                Anno(
                  Op("Iff", [Var("g_3"), Var("h_3")])
                , Var("i_3")
                )
              )
            , Seq(
                Build(Var("g_3"))
              , Seq(
                  CallT(SVar("c_2"), [], [])
                , Seq(
                    Match(Var("j_3"))
                  , Seq(
                      Build(Var("h_3"))
                    , Seq(
                        CallT(SVar("d_2"), [], [])
                      , Seq(
                          Match(Var("k_3"))
                        , Build(
                            Anno(
                              Op("Iff", [Var("j_3"), Var("k_3")])
                            , Var("i_3")
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
          "Neg_1_0"
        , [ VarDec(
              "e_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["m_3", "l_3", "n_3"]
          , Seq(
              Match(
                Anno(Op("Neg", [Var("l_3")]), Var("m_3"))
              )
            , Seq(
                Build(Var("l_3"))
              , Seq(
                  CallT(SVar("e_2"), [], [])
                , Seq(
                    Match(Var("n_3"))
                  , Build(
                      Anno(Op("Neg", [Var("n_3")]), Var("m_3"))
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
          ]
        , []
        , Scope(
            ["q_3", "o_3", "p_3", "r_3", "s_3"]
          , Seq(
              Match(
                Anno(
                  Op("Cons", [Var("o_3"), Var("p_3")])
                , Var("q_3")
                )
              )
            , Seq(
                Build(Var("o_3"))
              , Seq(
                  CallT(SVar("f_2"), [], [])
                , Seq(
                    Match(Var("r_3"))
                  , Seq(
                      Build(Var("p_3"))
                    , Seq(
                        CallT(SVar("g_2"), [], [])
                      , Seq(
                          Match(Var("s_3"))
                        , Build(
                            Anno(
                              Op("Cons", [Var("r_3"), Var("s_3")])
                            , Var("q_3")
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
              "h_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["u_3", "t_3", "v_3"]
          , Seq(
              Match(
                Anno(Op("", [Var("t_3")]), Var("u_3"))
              )
            , Seq(
                Build(Var("t_3"))
              , Seq(
                  CallT(SVar("h_2"), [], [])
                , Seq(
                    Match(Var("v_3"))
                  , Build(
                      Anno(Op("", [Var("v_3")]), Var("u_3"))
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
              "i_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "j_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["y_3", "w_3", "x_3", "z_3", "a_4"]
          , Seq(
              Match(
                Anno(
                  Op("", [Var("w_3"), Var("x_3")])
                , Var("y_3")
                )
              )
            , Seq(
                Build(Var("w_3"))
              , Seq(
                  CallT(SVar("i_2"), [], [])
                , Seq(
                    Match(Var("z_3"))
                  , Seq(
                      Build(Var("x_3"))
                    , Seq(
                        CallT(SVar("j_2"), [], [])
                      , Seq(
                          Match(Var("a_4"))
                        , Build(
                            Anno(
                              Op("", [Var("z_3"), Var("a_4")])
                            , Var("y_3")
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
              "k_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "l_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          , VarDec(
              "m_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["e_4", "b_4", "c_4", "d_4", "f_4", "g_4", "h_4"]
          , Seq(
              Match(
                Anno(
                  Op(
                    ""
                  , [Var("b_4"), Var("c_4"), Var("d_4")]
                  )
                , Var("e_4")
                )
              )
            , Seq(
                Build(Var("b_4"))
              , Seq(
                  CallT(SVar("k_2"), [], [])
                , Seq(
                    Match(Var("f_4"))
                  , Seq(
                      Build(Var("c_4"))
                    , Seq(
                        CallT(SVar("l_2"), [], [])
                      , Seq(
                          Match(Var("g_4"))
                        , Seq(
                            Build(Var("d_4"))
                          , Seq(
                              CallT(SVar("m_2"), [], [])
                            , Seq(
                                Match(Var("h_4"))
                              , Build(
                                  Anno(
                                    Op(
                                      ""
                                    , [Var("f_4"), Var("g_4"), Var("h_4")]
                                    )
                                  , Var("e_4")
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
              "n_2"
            , FunType(
                [ConstType(Sort("ATerm", []))]
              , ConstType(Sort("ATerm", []))
              )
            )
          ]
        , []
        , Scope(
            ["j_4", "i_4", "k_4"]
          , Seq(
              Match(
                Anno(Op("DR_UNDEFINE", [Var("i_4")]), Var("j_4"))
              )
            , Seq(
                Build(Var("i_4"))
              , Seq(
                  CallT(SVar("n_2"), [], [])
                , Seq(
                    Match(Var("k_4"))
                  , Build(
                      Anno(Op("DR_UNDEFINE", [Var("k_4")]), Var("j_4"))
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
