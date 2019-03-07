import org.strategoxt.stratego_lib.*;
import org.strategoxt.lang.*;
import org.spoofax.interpreter.terms.*;
import static org.strategoxt.lang.Term.*;
import org.spoofax.interpreter.library.AbstractPrimitive;
import java.util.ArrayList;
import java.lang.ref.WeakReference;

@SuppressWarnings("all") public class arrows  
{ 
  protected static final boolean TRACES_ENABLED = true;

  protected static ITermFactory constantFactory;

  private static WeakReference<Context> initedContext;

  private static boolean isIniting;

  protected static IStrategoTerm constConstr2;

  protected static IStrategoTerm constConstrPat0;

  protected static IStrategoTerm constUnit0;

  protected static IStrategoTerm constAppBin0;

  protected static IStrategoTerm constVar3;

  protected static IStrategoTerm const7;

  protected static IStrategoTerm constVar2;

  protected static IStrategoTerm const6;

  protected static IStrategoTerm const5;

  protected static IStrategoTerm constConstr1;

  protected static IStrategoTerm const4;

  protected static IStrategoTerm constConstr0;

  protected static IStrategoTerm const3;

  protected static IStrategoTerm constVar1;

  protected static IStrategoTerm const2;

  protected static IStrategoTerm const1;

  protected static IStrategoTerm constNil0;

  protected static IStrategoTerm constVar0;

  protected static IStrategoTerm const0;

  public static IStrategoConstructor _consConc_2;

  public static IStrategoConstructor _consNone_0;

  public static IStrategoConstructor _consSome_1;

  public static IStrategoConstructor _consUnit_0;

  public static IStrategoConstructor _consVar_1;

  public static IStrategoConstructor _consBinCon_1;

  public static IStrategoConstructor _consArrLetStmt_1;

  public static IStrategoConstructor _consArrBindStmt_2;

  public static IStrategoConstructor _consArrCmdStmt_1;

  public static IStrategoConstructor _consArrStmtList_1;

  public static IStrategoConstructor _consArrStmtSeq_2;

  public static IStrategoConstructor _consArrProcedure_2;

  public static IStrategoConstructor _consConstr_1;

  public static IStrategoConstructor _consProduct_1;

  public static IStrategoConstructor _consAbs_2;

  public static IStrategoConstructor _consLet_2;

  public static IStrategoConstructor _consIf_3;

  public static IStrategoConstructor _consAppBin_2;

  public static IStrategoConstructor _consOpApp_3;

  public static IStrategoConstructor _consArrFirst_2;

  public static IStrategoConstructor _consArrHigher_2;

  public static IStrategoConstructor _consArrAbs_2;

  public static IStrategoConstructor _consArrLet_2;

  public static IStrategoConstructor _consArrIf_3;

  public static IStrategoConstructor _consArrDo_1;

  public static IStrategoConstructor _consArrAppBin_2;

  public static IStrategoConstructor _consArrForm_2;

  public static IStrategoConstructor _consArrOpApp_3;

  public static IStrategoConstructor _consECons_2;

  public static IStrategoConstructor _consVarFunLHS_2;

  public static IStrategoConstructor _consConstrPat_1;

  public static IStrategoConstructor _consTuplePat_2;

  public static IStrategoConstructor _consCons_2;

  public static IStrategoConstructor _consNil_0;

  public static Context init(Context context)
  { 
    synchronized(arrows.class)
    { 
      if(isIniting)
        return null;
      try
      { 
        isIniting = true;
        ITermFactory termFactory = context.getFactory();
        if(constantFactory == null)
        { 
          initConstructors(termFactory);
          initConstants(termFactory);
        }
        if(initedContext == null || initedContext.get() != context)
        { 
          org.strategoxt.stratego_lib.Main.init(context);
          context.registerComponent("arrows");
        }
        initedContext = new WeakReference<Context>(context);
        constantFactory = termFactory;
      }
      finally
      { 
        isIniting = false;
      }
      return context;
    }
  }

  public static Context init()
  { 
    return init(new Context());
  }

  public static void main(String args[])
  { 
    Context context = init();
    context.setStandAlone(true);
    try
    { 
      IStrategoTerm result;
      try
      { 
        result = context.invokeStrategyCLI(main_0_0.instance, "arrows", args);
      }
      finally
      { 
        context.getIOAgent().closeAllFiles();
      }
      if(result == null)
      { 
        System.err.println("arrows" + (TRACES_ENABLED ? ": rewriting failed, trace:" : ": rewriting failed"));
        context.printStackTrace();
        context.setStandAlone(false);
        System.exit(1);
      }
      else
      { 
        System.out.println(result);
        context.setStandAlone(false);
        System.exit(0);
      }
    }
    catch(StrategoErrorExit exit)
    { 
      context.setStandAlone(false);
      System.err.println(exit.getLocalizedMessage());
      System.exit(exit.getValue());
    }
    catch(StrategoExit exit)
    { 
      context.setStandAlone(false);
      System.exit(exit.getValue());
    }
  }

  public static IStrategoTerm mainNoExit(String ... args) throws StrategoExit
  { 
    return mainNoExit(new Context(), args);
  }

  public static IStrategoTerm mainNoExit(Context context, String ... args) throws StrategoExit
  { 
    try
    { 
      init(context);
      return context.invokeStrategyCLI(main_0_0.instance, "arrows", args);
    }
    finally
    { 
      context.getIOAgent().closeAllFiles();
    }
  }

  public static Strategy getMainStrategy()
  { 
    return main_0_0.instance;
  }

  public static void initConstructors(ITermFactory termFactory)
  { 
    _consConc_2 = termFactory.makeConstructor("Conc", 2);
    _consNone_0 = termFactory.makeConstructor("None", 0);
    _consSome_1 = termFactory.makeConstructor("Some", 1);
    _consUnit_0 = termFactory.makeConstructor("Unit", 0);
    _consVar_1 = termFactory.makeConstructor("Var", 1);
    _consBinCon_1 = termFactory.makeConstructor("BinCon", 1);
    _consArrLetStmt_1 = termFactory.makeConstructor("ArrLetStmt", 1);
    _consArrBindStmt_2 = termFactory.makeConstructor("ArrBindStmt", 2);
    _consArrCmdStmt_1 = termFactory.makeConstructor("ArrCmdStmt", 1);
    _consArrStmtList_1 = termFactory.makeConstructor("ArrStmtList", 1);
    _consArrStmtSeq_2 = termFactory.makeConstructor("ArrStmtSeq", 2);
    _consArrProcedure_2 = termFactory.makeConstructor("ArrProcedure", 2);
    _consConstr_1 = termFactory.makeConstructor("Constr", 1);
    _consProduct_1 = termFactory.makeConstructor("Product", 1);
    _consAbs_2 = termFactory.makeConstructor("Abs", 2);
    _consLet_2 = termFactory.makeConstructor("Let", 2);
    _consIf_3 = termFactory.makeConstructor("If", 3);
    _consAppBin_2 = termFactory.makeConstructor("AppBin", 2);
    _consOpApp_3 = termFactory.makeConstructor("OpApp", 3);
    _consArrFirst_2 = termFactory.makeConstructor("ArrFirst", 2);
    _consArrHigher_2 = termFactory.makeConstructor("ArrHigher", 2);
    _consArrAbs_2 = termFactory.makeConstructor("ArrAbs", 2);
    _consArrLet_2 = termFactory.makeConstructor("ArrLet", 2);
    _consArrIf_3 = termFactory.makeConstructor("ArrIf", 3);
    _consArrDo_1 = termFactory.makeConstructor("ArrDo", 1);
    _consArrAppBin_2 = termFactory.makeConstructor("ArrAppBin", 2);
    _consArrForm_2 = termFactory.makeConstructor("ArrForm", 2);
    _consArrOpApp_3 = termFactory.makeConstructor("ArrOpApp", 3);
    _consECons_2 = termFactory.makeConstructor("ECons", 2);
    _consVarFunLHS_2 = termFactory.makeConstructor("VarFunLHS", 2);
    _consConstrPat_1 = termFactory.makeConstructor("ConstrPat", 1);
    _consTuplePat_2 = termFactory.makeConstructor("TuplePat", 2);
    _consCons_2 = termFactory.makeConstructor("Cons", 2);
    _consNil_0 = termFactory.makeConstructor("Nil", 0);
  }

  public static void initConstants(ITermFactory termFactory)
  { 
    const0 = termFactory.makeString("arr");
    constVar0 = termFactory.makeAppl(arrows._consVar_1, new IStrategoTerm[]{arrows.const0});
    constNil0 = (IStrategoTerm)termFactory.makeList(Term.NO_TERMS);
    const1 = termFactory.makeString(">>>");
    const2 = termFactory.makeString("app");
    constVar1 = termFactory.makeAppl(arrows._consVar_1, new IStrategoTerm[]{arrows.const2});
    const3 = termFactory.makeString("Left");
    constConstr0 = termFactory.makeAppl(arrows._consConstr_1, new IStrategoTerm[]{arrows.const3});
    const4 = termFactory.makeString("Right");
    constConstr1 = termFactory.makeAppl(arrows._consConstr_1, new IStrategoTerm[]{arrows.const4});
    const5 = termFactory.makeString("|||");
    const6 = termFactory.makeString("first");
    constVar2 = termFactory.makeAppl(arrows._consVar_1, new IStrategoTerm[]{arrows.const6});
    const7 = termFactory.makeString("snd");
    constVar3 = termFactory.makeAppl(arrows._consVar_1, new IStrategoTerm[]{arrows.const7});
    constAppBin0 = termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constVar0, arrows.constVar3});
    constUnit0 = termFactory.makeAppl(arrows._consUnit_0, NO_TERMS);
    constConstrPat0 = termFactory.makeAppl(arrows._consConstrPat_1, new IStrategoTerm[]{arrows.constUnit0});
    constConstr2 = termFactory.makeAppl(arrows._consConstr_1, new IStrategoTerm[]{arrows.constUnit0});
  }

  @SuppressWarnings("all") public static class desugar_arrow_0_0 extends Strategy 
  { 
    public static desugar_arrow_0_0 instance = new desugar_arrow_0_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      ITermFactory termFactory = context.getFactory();
      context.push("desugar_arrow_0_0");
      Fail0:
      { 
        IStrategoTerm p_12 = null;
        IStrategoTerm q_12 = null;
        IStrategoTerm r_12 = null;
        IStrategoTerm t_12 = null;
        if(term.getTermType() != IStrategoTerm.APPL || arrows._consArrProcedure_2 != ((IStrategoAppl)term).getConstructor())
          break Fail0;
        q_12 = term.getSubterm(0);
        p_12 = term.getSubterm(1);
        term = free_pat_vars_0_0.instance.invoke(context, q_12);
        if(term == null)
          break Fail0;
        r_12 = term;
        term = tuple_0_0.instance.invoke(context, r_12);
        if(term == null)
          break Fail0;
        t_12 = term;
        term = desugar_arrow_p__0_1.instance.invoke(context, p_12, r_12);
        if(term == null)
          break Fail0;
        term = termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constVar0, termFactory.makeAppl(arrows._consAbs_2, new IStrategoTerm[]{(IStrategoTerm)termFactory.makeListCons(q_12, (IStrategoList)arrows.constNil0), t_12})}), arrows.const1, term});
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class desugar_arrow_p__0_1 extends Strategy 
  { 
    public static desugar_arrow_p__0_1 instance = new desugar_arrow_p__0_1();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, IStrategoTerm ref_o_32)
    { 
      ITermFactory termFactory = context.getFactory();
      TermReference o_32 = new TermReference(ref_o_32);
      context.push("desugar_arrow_p__0_1");
      Fail1:
      { 
        IStrategoTerm term0 = term;
        IStrategoConstructor cons2 = term.getTermType() == IStrategoTerm.APPL ? ((IStrategoAppl)term).getConstructor() : null;
        Success0:
        { 
          if(cons2 == arrows._consArrFirst_2)
          { 
            Fail2:
            { 
              IStrategoTerm j_17 = null;
              IStrategoTerm k_17 = null;
              k_17 = term.getSubterm(0);
              j_17 = term.getSubterm(1);
              if(o_32.value == null)
                break Fail2;
              term = tuple_pat_0_0.instance.invoke(context, o_32.value);
              if(term == null)
                break Fail2;
              term = termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constVar0, termFactory.makeAppl(arrows._consAbs_2, new IStrategoTerm[]{(IStrategoTerm)termFactory.makeListCons(term, (IStrategoList)arrows.constNil0), j_17})}), arrows.const1, k_17});
              if(true)
                break Success0;
            }
            term = term0;
          }
          Success1:
          { 
            if(cons2 == arrows._consArrHigher_2)
            { 
              Fail3:
              { 
                IStrategoTerm e_17 = null;
                IStrategoTerm f_17 = null;
                e_17 = term.getSubterm(0);
                f_17 = term.getSubterm(1);
                if(o_32.value == null)
                  break Fail3;
                term = tuple_pat_0_0.instance.invoke(context, o_32.value);
                if(term == null)
                  break Fail3;
                term = termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constVar0, termFactory.makeAppl(arrows._consAbs_2, new IStrategoTerm[]{(IStrategoTerm)termFactory.makeListCons(term, (IStrategoList)arrows.constNil0), termFactory.makeAppl(arrows._consProduct_1, new IStrategoTerm[]{termFactory.makeAppl(arrows._consECons_2, new IStrategoTerm[]{e_17, (IStrategoTerm)termFactory.makeListCons(f_17, (IStrategoList)arrows.constNil0)})})})}), arrows.const1, arrows.constVar1});
                if(true)
                  break Success1;
              }
              term = term0;
            }
            Success2:
            { 
              if(cons2 == arrows._consArrIf_3)
              { 
                Fail4:
                { 
                  IStrategoTerm q_16 = null;
                  IStrategoTerm r_16 = null;
                  IStrategoTerm s_16 = null;
                  IStrategoTerm t_16 = null;
                  IStrategoTerm y_16 = null;
                  IStrategoTerm u_16 = null;
                  IStrategoTerm z_16 = null;
                  IStrategoTerm v_16 = null;
                  IStrategoTerm w_16 = null;
                  q_16 = term.getSubterm(0);
                  r_16 = term.getSubterm(1);
                  s_16 = term.getSubterm(2);
                  y_16 = term;
                  if(o_32.value == null)
                    break Fail4;
                  term = tuple_pat_0_0.instance.invoke(context, o_32.value);
                  if(term == null)
                    break Fail4;
                  t_16 = term;
                  term = y_16;
                  z_16 = y_16;
                  if(o_32.value == null)
                    break Fail4;
                  term = tuple_0_0.instance.invoke(context, o_32.value);
                  if(term == null)
                    break Fail4;
                  u_16 = term;
                  term = z_16;
                  if(o_32.value == null)
                    break Fail4;
                  term = tuple_0_0.instance.invoke(context, o_32.value);
                  if(term == null)
                    break Fail4;
                  v_16 = term;
                  term = this.invoke(context, r_16, o_32.value);
                  if(term == null)
                    break Fail4;
                  w_16 = term;
                  term = this.invoke(context, s_16, o_32.value);
                  if(term == null)
                    break Fail4;
                  term = termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constVar0, termFactory.makeAppl(arrows._consAbs_2, new IStrategoTerm[]{(IStrategoTerm)termFactory.makeListCons(t_16, (IStrategoList)arrows.constNil0), termFactory.makeAppl(arrows._consIf_3, new IStrategoTerm[]{q_16, termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constConstr0, u_16}), termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constConstr1, v_16})})})}), arrows.const1, termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{w_16, arrows.const5, term})});
                  if(true)
                    break Success2;
                }
                term = term0;
              }
              Success3:
              { 
                if(cons2 == arrows._consArrLet_2)
                { 
                  Fail5:
                  { 
                    IStrategoTerm e_16 = null;
                    IStrategoTerm f_16 = null;
                    IStrategoTerm g_16 = null;
                    IStrategoTerm h_16 = null;
                    IStrategoTerm i_16 = null;
                    IStrategoTerm j_16 = null;
                    IStrategoTerm k_16 = null;
                    f_16 = term.getSubterm(0);
                    e_16 = term.getSubterm(1);
                    i_16 = term;
                    term = free_decls_vars_0_0.instance.invoke(context, f_16);
                    if(term == null)
                      break Fail5;
                    g_16 = term;
                    if(o_32.value == null)
                      break Fail5;
                    term = termFactory.makeTuple(o_32.value, g_16);
                    term = conc_0_0.instance.invoke(context, term);
                    if(term == null)
                      break Fail5;
                    h_16 = term;
                    term = i_16;
                    if(o_32.value == null)
                      break Fail5;
                    term = tuple_pat_0_0.instance.invoke(context, o_32.value);
                    if(term == null)
                      break Fail5;
                    j_16 = term;
                    term = tuple_0_0.instance.invoke(context, h_16);
                    if(term == null)
                      break Fail5;
                    k_16 = term;
                    term = this.invoke(context, e_16, h_16);
                    if(term == null)
                      break Fail5;
                    term = termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constVar0, termFactory.makeAppl(arrows._consAbs_2, new IStrategoTerm[]{(IStrategoTerm)termFactory.makeListCons(j_16, (IStrategoList)arrows.constNil0), termFactory.makeAppl(arrows._consLet_2, new IStrategoTerm[]{f_16, k_16})})}), arrows.const1, term});
                    if(true)
                      break Success3;
                  }
                  term = term0;
                }
                Success4:
                { 
                  if(cons2 == arrows._consArrAbs_2)
                  { 
                    Fail6:
                    { 
                      IStrategoTerm s_15 = null;
                      IStrategoTerm t_15 = null;
                      IStrategoTerm u_15 = null;
                      IStrategoTerm v_15 = null;
                      IStrategoTerm w_15 = null;
                      IStrategoTerm x_15 = null;
                      IStrategoTerm y_15 = null;
                      IStrategoTerm arg0 = term.getSubterm(0);
                      if(arg0.getTermType() != IStrategoTerm.LIST || ((IStrategoList)arg0).isEmpty())
                        break Fail6;
                      t_15 = ((IStrategoList)arg0).head();
                      IStrategoTerm arg1 = ((IStrategoList)arg0).tail();
                      if(arg1.getTermType() != IStrategoTerm.LIST || !((IStrategoList)arg1).isEmpty())
                        break Fail6;
                      s_15 = term.getSubterm(1);
                      w_15 = term;
                      term = free_pat_vars_0_0.instance.invoke(context, t_15);
                      if(term == null)
                        break Fail6;
                      u_15 = term;
                      if(o_32.value == null)
                        break Fail6;
                      term = termFactory.makeTuple(o_32.value, u_15);
                      term = conc_0_0.instance.invoke(context, term);
                      if(term == null)
                        break Fail6;
                      v_15 = term;
                      term = w_15;
                      if(o_32.value == null)
                        break Fail6;
                      term = tuple_pat_0_0.instance.invoke(context, o_32.value);
                      if(term == null)
                        break Fail6;
                      x_15 = term;
                      term = tuple_0_0.instance.invoke(context, v_15);
                      if(term == null)
                        break Fail6;
                      y_15 = term;
                      term = this.invoke(context, s_15, v_15);
                      if(term == null)
                        break Fail6;
                      term = termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constVar0, termFactory.makeAppl(arrows._consAbs_2, new IStrategoTerm[]{(IStrategoTerm)termFactory.makeListCons(termFactory.makeAppl(arrows._consTuplePat_2, new IStrategoTerm[]{x_15, (IStrategoTerm)termFactory.makeListCons(termFactory.makeAppl(arrows._consVar_1, new IStrategoTerm[]{t_15}), (IStrategoList)arrows.constNil0)}), (IStrategoList)arrows.constNil0), y_15})}), arrows.const1, term});
                      if(true)
                        break Success4;
                    }
                    term = term0;
                  }
                  Success5:
                  { 
                    if(cons2 == arrows._consArrAppBin_2)
                    { 
                      Fail7:
                      { 
                        IStrategoTerm j_15 = null;
                        IStrategoTerm k_15 = null;
                        IStrategoTerm l_15 = null;
                        IStrategoTerm o_15 = null;
                        IStrategoTerm m_15 = null;
                        k_15 = term.getSubterm(0);
                        j_15 = term.getSubterm(1);
                        o_15 = term;
                        if(o_32.value == null)
                          break Fail7;
                        term = tuple_pat_0_0.instance.invoke(context, o_32.value);
                        if(term == null)
                          break Fail7;
                        l_15 = term;
                        term = o_15;
                        if(o_32.value == null)
                          break Fail7;
                        term = tuple_0_0.instance.invoke(context, o_32.value);
                        if(term == null)
                          break Fail7;
                        m_15 = term;
                        term = this.invoke(context, k_15, o_32.value);
                        if(term == null)
                          break Fail7;
                        term = termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constVar0, termFactory.makeAppl(arrows._consAbs_2, new IStrategoTerm[]{(IStrategoTerm)termFactory.makeListCons(l_15, (IStrategoList)arrows.constNil0), termFactory.makeAppl(arrows._consProduct_1, new IStrategoTerm[]{termFactory.makeAppl(arrows._consECons_2, new IStrategoTerm[]{m_15, (IStrategoTerm)termFactory.makeListCons(j_15, (IStrategoList)arrows.constNil0)})})})}), arrows.const1, term});
                        if(true)
                          break Success5;
                      }
                      term = term0;
                    }
                    Success6:
                    { 
                      if(cons2 == arrows._consArrForm_2)
                      { 
                        Fail8:
                        { 
                          IStrategoTerm y_14 = null;
                          IStrategoTerm z_14 = null;
                          IStrategoTerm a_15 = null;
                          IStrategoTerm c_15 = null;
                          IStrategoTerm e_15 = null;
                          y_14 = term.getSubterm(0);
                          z_14 = term.getSubterm(1);
                          e_15 = term;
                          if(o_32.value == null)
                            break Fail8;
                          term = tuple_pat_0_0.instance.invoke(context, o_32.value);
                          if(term == null)
                            break Fail8;
                          c_15 = term;
                          term = e_15;
                          if(o_32.value == null)
                            break Fail8;
                          term = tuple_0_0.instance.invoke(context, o_32.value);
                          if(term == null)
                            break Fail8;
                          term = termFactory.makeAppl(arrows._consAbs_2, new IStrategoTerm[]{(IStrategoTerm)termFactory.makeListCons(c_15, (IStrategoList)arrows.constNil0), term});
                          a_15 = term;
                          term = z_14;
                          lifted0 lifted00 = new lifted0();
                          lifted00.o_32 = o_32;
                          term = map_1_0.instance.invoke(context, term, lifted00);
                          if(term == null)
                            break Fail8;
                          term = termFactory.makeTuple(y_14, term);
                          term = apply_all_0_1.instance.invoke(context, term, a_15);
                          if(term == null)
                            break Fail8;
                          if(true)
                            break Success6;
                        }
                        term = term0;
                      }
                      Success7:
                      { 
                        if(cons2 == arrows._consArrOpApp_3)
                        { 
                          Fail9:
                          { 
                            IStrategoTerm u_14 = null;
                            IStrategoTerm v_14 = null;
                            IStrategoTerm w_14 = null;
                            v_14 = term.getSubterm(0);
                            u_14 = term.getSubterm(1);
                            w_14 = term.getSubterm(2);
                            term = termFactory.makeAppl(arrows._consArrForm_2, new IStrategoTerm[]{termFactory.makeAppl(arrows._consBinCon_1, new IStrategoTerm[]{u_14}), (IStrategoTerm)termFactory.makeListCons(v_14, termFactory.makeListCons(w_14, (IStrategoList)arrows.constNil0))});
                            term = this.invoke(context, term, o_32.value);
                            if(term == null)
                              break Fail9;
                            if(true)
                              break Success7;
                          }
                          term = term0;
                        }
                        Success8:
                        { 
                          if(cons2 == arrows._consArrDo_1)
                          { 
                            Fail10:
                            { 
                              IStrategoTerm s_14 = null;
                              IStrategoTerm arg2 = term.getSubterm(0);
                              if(arg2.getTermType() != IStrategoTerm.APPL || arrows._consArrStmtList_1 != ((IStrategoAppl)arg2).getConstructor())
                                break Fail10;
                              IStrategoTerm arg3 = arg2.getSubterm(0);
                              if(arg3.getTermType() != IStrategoTerm.APPL || arrows._consArrCmdStmt_1 != ((IStrategoAppl)arg3).getConstructor())
                                break Fail10;
                              s_14 = arg3.getSubterm(0);
                              term = this.invoke(context, s_14, o_32.value);
                              if(term == null)
                                break Fail10;
                              if(true)
                                break Success8;
                            }
                            term = term0;
                          }
                          Success9:
                          { 
                            if(cons2 == arrows._consArrDo_1)
                            { 
                              Fail11:
                              { 
                                IStrategoTerm g_14 = null;
                                IStrategoTerm h_14 = null;
                                IStrategoTerm i_14 = null;
                                IStrategoTerm j_14 = null;
                                IStrategoTerm k_14 = null;
                                IStrategoTerm l_14 = null;
                                IStrategoTerm m_14 = null;
                                IStrategoTerm arg4 = term.getSubterm(0);
                                if(arg4.getTermType() != IStrategoTerm.APPL || arrows._consArrStmtList_1 != ((IStrategoAppl)arg4).getConstructor())
                                  break Fail11;
                                IStrategoTerm arg5 = arg4.getSubterm(0);
                                if(arg5.getTermType() != IStrategoTerm.APPL || arrows._consArrStmtSeq_2 != ((IStrategoAppl)arg5).getConstructor())
                                  break Fail11;
                                IStrategoTerm arg6 = arg5.getSubterm(0);
                                if(arg6.getTermType() != IStrategoTerm.APPL || arrows._consArrLetStmt_1 != ((IStrategoAppl)arg6).getConstructor())
                                  break Fail11;
                                h_14 = arg6.getSubterm(0);
                                g_14 = arg5.getSubterm(1);
                                k_14 = term;
                                term = free_decls_vars_0_0.instance.invoke(context, h_14);
                                if(term == null)
                                  break Fail11;
                                i_14 = term;
                                if(o_32.value == null)
                                  break Fail11;
                                term = termFactory.makeTuple(o_32.value, i_14);
                                term = conc_0_0.instance.invoke(context, term);
                                if(term == null)
                                  break Fail11;
                                j_14 = term;
                                term = k_14;
                                if(o_32.value == null)
                                  break Fail11;
                                term = tuple_pat_0_0.instance.invoke(context, o_32.value);
                                if(term == null)
                                  break Fail11;
                                l_14 = term;
                                term = tuple_0_0.instance.invoke(context, j_14);
                                if(term == null)
                                  break Fail11;
                                m_14 = term;
                                term = termFactory.makeAppl(arrows._consArrDo_1, new IStrategoTerm[]{termFactory.makeAppl(arrows._consArrStmtList_1, new IStrategoTerm[]{g_14})});
                                term = this.invoke(context, term, j_14);
                                if(term == null)
                                  break Fail11;
                                term = termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constVar0, termFactory.makeAppl(arrows._consAbs_2, new IStrategoTerm[]{(IStrategoTerm)termFactory.makeListCons(l_14, (IStrategoList)arrows.constNil0), termFactory.makeAppl(arrows._consLet_2, new IStrategoTerm[]{h_14, m_14})})}), arrows.const1, term});
                                if(true)
                                  break Success9;
                              }
                              term = term0;
                            }
                            Success10:
                            { 
                              if(cons2 == arrows._consArrDo_1)
                              { 
                                Fail12:
                                { 
                                  IStrategoTerm t_13 = null;
                                  IStrategoTerm u_13 = null;
                                  IStrategoTerm v_13 = null;
                                  IStrategoTerm a_14 = null;
                                  IStrategoTerm w_13 = null;
                                  IStrategoTerm b_14 = null;
                                  IStrategoTerm x_13 = null;
                                  IStrategoTerm y_13 = null;
                                  IStrategoTerm arg7 = term.getSubterm(0);
                                  if(arg7.getTermType() != IStrategoTerm.APPL || arrows._consArrStmtList_1 != ((IStrategoAppl)arg7).getConstructor())
                                    break Fail12;
                                  IStrategoTerm arg8 = arg7.getSubterm(0);
                                  if(arg8.getTermType() != IStrategoTerm.APPL || arrows._consArrStmtSeq_2 != ((IStrategoAppl)arg8).getConstructor())
                                    break Fail12;
                                  IStrategoTerm arg9 = arg8.getSubterm(0);
                                  if(arg9.getTermType() != IStrategoTerm.APPL || arrows._consArrCmdStmt_1 != ((IStrategoAppl)arg9).getConstructor())
                                    break Fail12;
                                  t_13 = arg9.getSubterm(0);
                                  u_13 = arg8.getSubterm(1);
                                  a_14 = term;
                                  if(o_32.value == null)
                                    break Fail12;
                                  term = tuple_pat_0_0.instance.invoke(context, o_32.value);
                                  if(term == null)
                                    break Fail12;
                                  v_13 = term;
                                  term = a_14;
                                  b_14 = a_14;
                                  if(o_32.value == null)
                                    break Fail12;
                                  term = tuple_0_0.instance.invoke(context, o_32.value);
                                  if(term == null)
                                    break Fail12;
                                  w_13 = term;
                                  term = b_14;
                                  if(o_32.value == null)
                                    break Fail12;
                                  term = tuple_0_0.instance.invoke(context, o_32.value);
                                  if(term == null)
                                    break Fail12;
                                  x_13 = term;
                                  term = this.invoke(context, t_13, o_32.value);
                                  if(term == null)
                                    break Fail12;
                                  y_13 = term;
                                  term = termFactory.makeAppl(arrows._consArrDo_1, new IStrategoTerm[]{termFactory.makeAppl(arrows._consArrStmtList_1, new IStrategoTerm[]{u_13})});
                                  term = this.invoke(context, term, o_32.value);
                                  if(term == null)
                                    break Fail12;
                                  term = termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constVar0, termFactory.makeAppl(arrows._consAbs_2, new IStrategoTerm[]{(IStrategoTerm)termFactory.makeListCons(v_13, (IStrategoList)arrows.constNil0), termFactory.makeAppl(arrows._consProduct_1, new IStrategoTerm[]{termFactory.makeAppl(arrows._consECons_2, new IStrategoTerm[]{w_13, (IStrategoTerm)termFactory.makeListCons(x_13, (IStrategoList)arrows.constNil0)})})})}), arrows.const1, termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constVar2, y_13}), arrows.const1, termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{arrows.constAppBin0, arrows.const1, term})})});
                                  if(true)
                                    break Success10;
                                }
                                term = term0;
                              }
                              if(cons2 == arrows._consArrDo_1)
                              { 
                                IStrategoTerm y_12 = null;
                                IStrategoTerm z_12 = null;
                                IStrategoTerm a_13 = null;
                                IStrategoTerm b_13 = null;
                                IStrategoTerm c_13 = null;
                                IStrategoTerm d_13 = null;
                                IStrategoTerm e_13 = null;
                                IStrategoTerm l_13 = null;
                                IStrategoTerm f_13 = null;
                                IStrategoTerm m_13 = null;
                                IStrategoTerm g_13 = null;
                                IStrategoTerm n_13 = null;
                                IStrategoTerm h_13 = null;
                                IStrategoTerm o_13 = null;
                                IStrategoTerm i_13 = null;
                                IStrategoTerm j_13 = null;
                                IStrategoTerm arg10 = term.getSubterm(0);
                                if(arg10.getTermType() != IStrategoTerm.APPL || arrows._consArrStmtList_1 != ((IStrategoAppl)arg10).getConstructor())
                                  break Fail1;
                                IStrategoTerm arg11 = arg10.getSubterm(0);
                                if(arg11.getTermType() != IStrategoTerm.APPL || arrows._consArrStmtSeq_2 != ((IStrategoAppl)arg11).getConstructor())
                                  break Fail1;
                                IStrategoTerm arg12 = arg11.getSubterm(0);
                                if(arg12.getTermType() != IStrategoTerm.APPL || arrows._consArrBindStmt_2 != ((IStrategoAppl)arg12).getConstructor())
                                  break Fail1;
                                a_13 = arg12.getSubterm(0);
                                y_12 = arg12.getSubterm(1);
                                z_12 = arg11.getSubterm(1);
                                d_13 = term;
                                term = free_pat_vars_0_0.instance.invoke(context, a_13);
                                if(term == null)
                                  break Fail1;
                                b_13 = term;
                                if(o_32.value == null)
                                  break Fail1;
                                term = termFactory.makeTuple(b_13, o_32.value);
                                term = conc_0_0.instance.invoke(context, term);
                                if(term == null)
                                  break Fail1;
                                c_13 = term;
                                term = d_13;
                                l_13 = d_13;
                                if(o_32.value == null)
                                  break Fail1;
                                term = tuple_pat_0_0.instance.invoke(context, o_32.value);
                                if(term == null)
                                  break Fail1;
                                e_13 = term;
                                term = l_13;
                                m_13 = l_13;
                                if(o_32.value == null)
                                  break Fail1;
                                term = tuple_0_0.instance.invoke(context, o_32.value);
                                if(term == null)
                                  break Fail1;
                                f_13 = term;
                                term = m_13;
                                n_13 = m_13;
                                if(o_32.value == null)
                                  break Fail1;
                                term = tuple_0_0.instance.invoke(context, o_32.value);
                                if(term == null)
                                  break Fail1;
                                g_13 = term;
                                o_13 = n_13;
                                term = this.invoke(context, y_12, o_32.value);
                                if(term == null)
                                  break Fail1;
                                h_13 = term;
                                term = o_13;
                                if(o_32.value == null)
                                  break Fail1;
                                term = tuple_pat_0_0.instance.invoke(context, o_32.value);
                                if(term == null)
                                  break Fail1;
                                i_13 = term;
                                term = tuple_0_0.instance.invoke(context, c_13);
                                if(term == null)
                                  break Fail1;
                                j_13 = term;
                                term = termFactory.makeAppl(arrows._consArrDo_1, new IStrategoTerm[]{termFactory.makeAppl(arrows._consArrStmtList_1, new IStrategoTerm[]{z_12})});
                                term = this.invoke(context, term, c_13);
                                if(term == null)
                                  break Fail1;
                                term = termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constVar0, termFactory.makeAppl(arrows._consAbs_2, new IStrategoTerm[]{(IStrategoTerm)termFactory.makeListCons(e_13, (IStrategoList)arrows.constNil0), termFactory.makeAppl(arrows._consProduct_1, new IStrategoTerm[]{termFactory.makeAppl(arrows._consECons_2, new IStrategoTerm[]{f_13, (IStrategoTerm)termFactory.makeListCons(g_13, (IStrategoList)arrows.constNil0)})})})}), arrows.const1, termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constVar2, h_13}), arrows.const1, termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constVar0, termFactory.makeAppl(arrows._consAbs_2, new IStrategoTerm[]{(IStrategoTerm)termFactory.makeListCons(termFactory.makeAppl(arrows._consTuplePat_2, new IStrategoTerm[]{a_13, (IStrategoTerm)termFactory.makeListCons(i_13, (IStrategoList)arrows.constNil0)}), (IStrategoList)arrows.constNil0), j_13})}), arrows.const1, term})})});
                              }
                              else
                              { 
                                break Fail1;
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class tuple_pat_0_0 extends Strategy 
  { 
    public static tuple_pat_0_0 instance = new tuple_pat_0_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      ITermFactory termFactory = context.getFactory();
      Fail13:
      { 
        IStrategoTerm term11 = term;
        Success11:
        { 
          Fail14:
          { 
            if(term.getTermType() != IStrategoTerm.LIST || !((IStrategoList)term).isEmpty())
              break Fail14;
            term = arrows.constConstrPat0;
            if(true)
              break Success11;
          }
          term = term11;
          IStrategoTerm term12 = term;
          Success12:
          { 
            Fail15:
            { 
              IStrategoTerm p_17 = null;
              if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
                break Fail15;
              p_17 = ((IStrategoList)term).head();
              IStrategoTerm arg13 = ((IStrategoList)term).tail();
              if(arg13.getTermType() != IStrategoTerm.LIST || !((IStrategoList)arg13).isEmpty())
                break Fail15;
              term = p_17;
              if(true)
                break Success12;
            }
            term = term12;
            IStrategoTerm n_17 = null;
            IStrategoTerm o_17 = null;
            if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
              break Fail13;
            n_17 = ((IStrategoList)term).head();
            o_17 = ((IStrategoList)term).tail();
            term = termFactory.makeAppl(arrows._consTuplePat_2, new IStrategoTerm[]{n_17, o_17});
          }
        }
        if(true)
          return term;
      }
      context.push("tuple_pat_0_0");
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class tuple_0_0 extends Strategy 
  { 
    public static tuple_0_0 instance = new tuple_0_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      ITermFactory termFactory = context.getFactory();
      Fail16:
      { 
        IStrategoTerm term13 = term;
        Success13:
        { 
          Fail17:
          { 
            if(term.getTermType() != IStrategoTerm.LIST || !((IStrategoList)term).isEmpty())
              break Fail17;
            term = arrows.constConstr2;
            if(true)
              break Success13;
          }
          term = term13;
          IStrategoTerm term14 = term;
          Success14:
          { 
            Fail18:
            { 
              IStrategoTerm s_17 = null;
              if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
                break Fail18;
              s_17 = ((IStrategoList)term).head();
              IStrategoTerm arg14 = ((IStrategoList)term).tail();
              if(arg14.getTermType() != IStrategoTerm.LIST || !((IStrategoList)arg14).isEmpty())
                break Fail18;
              term = s_17;
              if(true)
                break Success14;
            }
            term = term14;
            IStrategoTerm q_17 = null;
            IStrategoTerm r_17 = null;
            if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
              break Fail16;
            q_17 = ((IStrategoList)term).head();
            r_17 = ((IStrategoList)term).tail();
            term = termFactory.makeAppl(arrows._consProduct_1, new IStrategoTerm[]{termFactory.makeAppl(arrows._consECons_2, new IStrategoTerm[]{q_17, r_17})});
          }
        }
        if(true)
          return term;
      }
      context.push("tuple_0_0");
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class free_pat_vars_0_0 extends Strategy 
  { 
    public static free_pat_vars_0_0 instance = new free_pat_vars_0_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      context.push("free_pat_vars_0_0");
      Fail19:
      { 
        term = collect_all_1_0.instance.invoke(context, term, lifted1.instance);
        if(term == null)
          break Fail19;
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class free_decls_vars_0_0 extends Strategy 
  { 
    public static free_decls_vars_0_0 instance = new free_decls_vars_0_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      context.push("free_decls_vars_0_0");
      Fail20:
      { 
        term = collect_all_3_0.instance.invoke(context, term, lifted2.instance, union_0_0.instance, lifted4.instance);
        if(term == null)
          break Fail20;
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class apply_all_0_1 extends Strategy 
  { 
    public static apply_all_0_1 instance = new apply_all_0_1();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, IStrategoTerm p_32)
    { 
      ITermFactory termFactory = context.getFactory();
      context.push("apply_all_0_1");
      Fail21:
      { 
        IStrategoTerm term15 = term;
        Success15:
        { 
          Fail22:
          { 
            IStrategoTerm z_17 = null;
            if(term.getTermType() != IStrategoTerm.TUPLE || term.getSubtermCount() != 2)
              break Fail22;
            z_17 = term.getSubterm(0);
            IStrategoTerm arg18 = term.getSubterm(1);
            if(arg18.getTermType() != IStrategoTerm.LIST || !((IStrategoList)arg18).isEmpty())
              break Fail22;
            term = z_17;
            if(true)
              break Success15;
          }
          term = term15;
          IStrategoTerm v_17 = null;
          IStrategoTerm w_17 = null;
          IStrategoTerm x_17 = null;
          if(term.getTermType() != IStrategoTerm.TUPLE || term.getSubtermCount() != 2)
            break Fail21;
          v_17 = term.getSubterm(0);
          IStrategoTerm arg19 = term.getSubterm(1);
          if(arg19.getTermType() != IStrategoTerm.LIST || ((IStrategoList)arg19).isEmpty())
            break Fail21;
          w_17 = ((IStrategoList)arg19).head();
          x_17 = ((IStrategoList)arg19).tail();
          term = termFactory.makeTuple(termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{v_17, termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constVar0, p_32}), arrows.const1, w_17})}), x_17);
          term = this.invoke(context, term, p_32);
          if(term == null)
            break Fail21;
        }
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class map_1_0 extends Strategy 
  { 
    public static map_1_0 instance = new map_1_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy f_18)
    { 
      context.push("map_1_0");
      Fail23:
      { 
        g_18 g_180 = new g_18();
        g_180.f_18 = f_18;
        term = g_180.invoke(context, term);
        if(term == null)
          break Fail23;
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class collect_all_1_0 extends Strategy 
  { 
    public static collect_all_1_0 instance = new collect_all_1_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy h_18)
    { 
      context.push("collect_all_1_0");
      Fail24:
      { 
        term = collect_all_2_0.instance.invoke(context, term, h_18, union_0_0.instance);
        if(term == null)
          break Fail24;
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class collect_all_2_0 extends Strategy 
  { 
    public static collect_all_2_0 instance = new collect_all_2_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy i_18, Strategy j_18)
    { 
      context.push("collect_all_2_0");
      Fail25:
      { 
        k_18 k_180 = new k_18();
        k_180.i_18 = i_18;
        k_180.j_18 = j_18;
        term = k_180.invoke(context, term);
        if(term == null)
          break Fail25;
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class collect_all_3_0 extends Strategy 
  { 
    public static collect_all_3_0 instance = new collect_all_3_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy p_18, Strategy q_18, Strategy r_18)
    { 
      context.push("collect_all_3_0");
      Fail26:
      { 
        s_18 s_180 = new s_18();
        s_180.p_18 = p_18;
        s_180.r_18 = r_18;
        s_180.q_18 = q_18;
        term = s_180.invoke(context, term);
        if(term == null)
          break Fail26;
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class crush_3_0 extends Strategy 
  { 
    public static crush_3_0 instance = new crush_3_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy y_18, Strategy z_18, Strategy a_19)
    { 
      context.push("crush_3_0");
      Fail27:
      { 
        IStrategoTerm args0 = context.invokePrimitive("SSL_get_arguments", term, NO_STRATEGIES, new IStrategoTerm[]{term});
        term = foldr_3_0.instance.invoke(context, args0, y_18, z_18, a_19);
        if(term == null)
          break Fail27;
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class foldr_3_0 extends Strategy 
  { 
    public static foldr_3_0 instance = new foldr_3_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy d_19, Strategy e_19, Strategy f_19)
    { 
      ITermFactory termFactory = context.getFactory();
      context.push("foldr_3_0");
      Fail28:
      { 
        IStrategoTerm term20 = term;
        Success16:
        { 
          Fail29:
          { 
            if(term.getTermType() != IStrategoTerm.LIST || !((IStrategoList)term).isEmpty())
              break Fail29;
            term = d_19.invoke(context, term);
            if(term == null)
              break Fail29;
            if(true)
              break Success16;
          }
          term = term20;
          IStrategoTerm b_19 = null;
          IStrategoTerm c_19 = null;
          IStrategoTerm g_19 = null;
          if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
            break Fail28;
          b_19 = ((IStrategoList)term).head();
          c_19 = ((IStrategoList)term).tail();
          term = f_19.invoke(context, b_19);
          if(term == null)
            break Fail28;
          g_19 = term;
          term = this.invoke(context, c_19, d_19, e_19, f_19);
          if(term == null)
            break Fail28;
          term = termFactory.makeTuple(g_19, term);
          term = e_19.invoke(context, term);
          if(term == null)
            break Fail28;
        }
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class conc_0_0 extends Strategy 
  { 
    public static conc_0_0 instance = new conc_0_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      context.push("conc_0_0");
      Fail30:
      { 
        IStrategoTerm term21 = term;
        Success17:
        { 
          Fail31:
          { 
            IStrategoTerm k_19 = null;
            TermReference l_19 = new TermReference();
            if(term.getTermType() != IStrategoTerm.TUPLE || term.getSubtermCount() != 2)
              break Fail31;
            k_19 = term.getSubterm(0);
            if(l_19.value == null)
              l_19.value = term.getSubterm(1);
            else
              if(l_19.value != term.getSubterm(1) && !l_19.value.match(term.getSubterm(1)))
                break Fail31;
            term = k_19;
            lifted10 lifted100 = new lifted10();
            lifted100.l_19 = l_19;
            term = at_end_1_0.instance.invoke(context, term, lifted100);
            if(term == null)
              break Fail31;
            if(true)
              break Success17;
          }
          term = term21;
          IStrategoTerm cons1 = context.invokePrimitive("SSL_get_constructor", term, NO_STRATEGIES, new IStrategoTerm[]{term});
          if(cons1.getTermType() != IStrategoTerm.STRING || !"".equals(((IStrategoString)cons1).stringValue()))
            break Fail30;
          IStrategoTerm args1 = context.invokePrimitive("SSL_get_arguments", term, NO_STRATEGIES, new IStrategoTerm[]{term});
          term = concat_0_0.instance.invoke(context, args1);
          if(term == null)
            break Fail30;
        }
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class at_end_1_0 extends Strategy 
  { 
    public static at_end_1_0 instance = new at_end_1_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy s_19)
    { 
      context.push("at_end_1_0");
      Fail32:
      { 
        t_19 t_190 = new t_19();
        t_190.s_19 = s_19;
        term = t_190.invoke(context, term);
        if(term == null)
          break Fail32;
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class concat_0_0 extends Strategy 
  { 
    public static concat_0_0 instance = new concat_0_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      context.push("concat_0_0");
      Fail33:
      { 
        term = w_19.instance.invoke(context, term);
        if(term == null)
          break Fail33;
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class union_0_0 extends Strategy 
  { 
    public static union_0_0 instance = new union_0_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      context.push("union_0_0");
      Fail34:
      { 
        TermReference x_19 = new TermReference();
        IStrategoTerm y_19 = null;
        if(term.getTermType() != IStrategoTerm.TUPLE || term.getSubtermCount() != 2)
          break Fail34;
        y_19 = term.getSubterm(0);
        if(x_19.value == null)
          x_19.value = term.getSubterm(1);
        else
          if(x_19.value != term.getSubterm(1) && !x_19.value.match(term.getSubterm(1)))
            break Fail34;
        term = y_19;
        e_20 e_200 = new e_20();
        e_200.x_19 = x_19;
        term = e_200.invoke(context, term);
        if(term == null)
          break Fail34;
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class $Hd$Member_1_0 extends Strategy 
  { 
    public static $Hd$Member_1_0 instance = new $Hd$Member_1_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy i_20)
    { 
      context.push("HdMember_1_0");
      Fail35:
      { 
        IStrategoTerm f_20 = null;
        TermReference g_20 = new TermReference();
        if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
          break Fail35;
        if(g_20.value == null)
          g_20.value = ((IStrategoList)term).head();
        else
          if(g_20.value != ((IStrategoList)term).head() && !g_20.value.match(((IStrategoList)term).head()))
            break Fail35;
        f_20 = ((IStrategoList)term).tail();
        term = i_20.invoke(context, term);
        if(term == null)
          break Fail35;
        lifted13 lifted130 = new lifted13();
        lifted130.g_20 = g_20;
        term = fetch_1_0.instance.invoke(context, term, lifted130);
        if(term == null)
          break Fail35;
        term = f_20;
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class fetch_1_0 extends Strategy 
  { 
    public static fetch_1_0 instance = new fetch_1_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy u_20)
    { 
      context.push("fetch_1_0");
      Fail36:
      { 
        v_20 v_200 = new v_20();
        v_200.u_20 = u_20;
        term = v_200.invoke(context, term);
        if(term == null)
          break Fail36;
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class eq_0_0 extends Strategy 
  { 
    public static eq_0_0 instance = new eq_0_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail37:
      { 
        IStrategoTerm w_20 = null;
        if(term.getTermType() != IStrategoTerm.TUPLE || term.getSubtermCount() != 2)
          break Fail37;
        w_20 = term.getSubterm(0);
        if(term.getSubterm(1) != w_20 && !w_20.match(term.getSubterm(1)))
          break Fail37;
        if(true)
          return term;
      }
      context.push("eq_0_0");
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class oncetd_1_0 extends Strategy 
  { 
    public static oncetd_1_0 instance = new oncetd_1_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy x_20)
    { 
      context.push("oncetd_1_0");
      Fail38:
      { 
        y_20 y_200 = new y_20();
        y_200.x_20 = x_20;
        term = y_200.invoke(context, term);
        if(term == null)
          break Fail38;
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class main_0_0 extends Strategy 
  { 
    public static main_0_0 instance = new main_0_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      context.push("main_0_0");
      Fail39:
      { 
        term = oncetd_1_0.instance.invoke(context, term, desugar_arrow_0_0.instance);
        if(term == null)
          break Fail39;
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") private static final class y_20 extends Strategy 
  { 
    Strategy x_20;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail40:
      { 
        IStrategoTerm term27 = term;
        Success18:
        { 
          Fail41:
          { 
            term = x_20.invoke(context, term);
            if(term == null)
              break Fail41;
            if(true)
              break Success18;
          }
          term = SRTS_one.instance.invoke(context, term27, this);
          if(term == null)
            break Fail40;
        }
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class v_20 extends Strategy 
  { 
    Strategy u_20;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      ITermFactory termFactory = context.getFactory();
      Fail42:
      { 
        IStrategoTerm term26 = term;
        Success19:
        { 
          Fail43:
          { 
            IStrategoTerm k_20 = null;
            IStrategoTerm l_20 = null;
            IStrategoTerm m_20 = null;
            IStrategoTerm n_20 = null;
            IStrategoTerm o_20 = null;
            if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
              break Fail43;
            k_20 = ((IStrategoList)term).head();
            l_20 = ((IStrategoList)term).tail();
            IStrategoList annos3 = term.getAnnotations();
            o_20 = annos3;
            term = u_20.invoke(context, k_20);
            if(term == null)
              break Fail43;
            m_20 = term;
            term = l_20;
            n_20 = l_20;
            IStrategoList list5;
            list5 = checkListTail(n_20);
            if(list5 == null)
              break Fail43;
            term = termFactory.annotateTerm((IStrategoTerm)termFactory.makeListCons(m_20, list5), checkListAnnos(termFactory, o_20));
            if(true)
              break Success19;
          }
          term = term26;
          IStrategoTerm p_20 = null;
          IStrategoTerm q_20 = null;
          IStrategoTerm r_20 = null;
          IStrategoTerm s_20 = null;
          IStrategoTerm t_20 = null;
          if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
            break Fail42;
          p_20 = ((IStrategoList)term).head();
          q_20 = ((IStrategoList)term).tail();
          IStrategoList annos4 = term.getAnnotations();
          t_20 = annos4;
          r_20 = p_20;
          term = this.invoke(context, q_20);
          if(term == null)
            break Fail42;
          s_20 = term;
          IStrategoList list6;
          list6 = checkListTail(s_20);
          if(list6 == null)
            break Fail42;
          term = termFactory.annotateTerm((IStrategoTerm)termFactory.makeListCons(r_20, list6), checkListAnnos(termFactory, t_20));
        }
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class lifted13 extends Strategy 
  { 
    TermReference g_20;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      ITermFactory termFactory = context.getFactory();
      Fail44:
      { 
        IStrategoTerm h_20 = null;
        h_20 = term;
        if(g_20.value == null)
          break Fail44;
        term = termFactory.makeTuple(g_20.value, h_20);
        term = eq_0_0.instance.invoke(context, term);
        if(term == null)
          break Fail44;
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class e_20 extends Strategy 
  { 
    TermReference x_19;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      ITermFactory termFactory = context.getFactory();
      Fail45:
      { 
        IStrategoTerm term24 = term;
        Success20:
        { 
          Fail46:
          { 
            if(term.getTermType() != IStrategoTerm.LIST || !((IStrategoList)term).isEmpty())
              break Fail46;
            if(x_19.value == null)
              break Fail46;
            term = x_19.value;
            if(true)
              break Success20;
          }
          term = term24;
          IStrategoTerm term25 = term;
          Success21:
          { 
            Fail47:
            { 
              lifted12 lifted120 = new lifted12();
              lifted120.x_19 = x_19;
              term = $Hd$Member_1_0.instance.invoke(context, term, lifted120);
              if(term == null)
                break Fail47;
              term = this.invoke(context, term);
              if(term == null)
                break Fail47;
              if(true)
                break Success21;
            }
            term = term25;
            IStrategoTerm z_19 = null;
            IStrategoTerm a_20 = null;
            IStrategoTerm b_20 = null;
            IStrategoTerm c_20 = null;
            IStrategoTerm d_20 = null;
            if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
              break Fail45;
            z_19 = ((IStrategoList)term).head();
            a_20 = ((IStrategoList)term).tail();
            IStrategoList annos2 = term.getAnnotations();
            d_20 = annos2;
            b_20 = z_19;
            term = this.invoke(context, a_20);
            if(term == null)
              break Fail45;
            c_20 = term;
            IStrategoList list4;
            list4 = checkListTail(c_20);
            if(list4 == null)
              break Fail45;
            term = termFactory.annotateTerm((IStrategoTerm)termFactory.makeListCons(b_20, list4), checkListAnnos(termFactory, d_20));
          }
        }
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class lifted12 extends Strategy 
  { 
    TermReference x_19;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail48:
      { 
        if(x_19.value == null)
          break Fail48;
        term = x_19.value;
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class w_19 extends Strategy 
  { 
    public static final w_19 instance = new w_19();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail49:
      { 
        IStrategoTerm term23 = term;
        Success22:
        { 
          Fail50:
          { 
            if(term.getTermType() != IStrategoTerm.LIST || !((IStrategoList)term).isEmpty())
              break Fail50;
            if(true)
              break Success22;
          }
          term = term23;
          IStrategoTerm u_19 = null;
          TermReference v_19 = new TermReference();
          if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
            break Fail49;
          u_19 = ((IStrategoList)term).head();
          if(v_19.value == null)
            v_19.value = ((IStrategoList)term).tail();
          else
            if(v_19.value != ((IStrategoList)term).tail() && !v_19.value.match(((IStrategoList)term).tail()))
              break Fail49;
          term = u_19;
          lifted11 lifted110 = new lifted11();
          lifted110.v_19 = v_19;
          term = at_end_1_0.instance.invoke(context, term, lifted110);
          if(term == null)
            break Fail49;
        }
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class lifted11 extends Strategy 
  { 
    TermReference v_19;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail51:
      { 
        if(v_19.value == null)
          break Fail51;
        term = w_19.instance.invoke(context, v_19.value);
        if(term == null)
          break Fail51;
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class t_19 extends Strategy 
  { 
    Strategy s_19;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      ITermFactory termFactory = context.getFactory();
      Fail52:
      { 
        IStrategoTerm term22 = term;
        Success23:
        { 
          Fail53:
          { 
            IStrategoTerm n_19 = null;
            IStrategoTerm o_19 = null;
            IStrategoTerm p_19 = null;
            IStrategoTerm q_19 = null;
            IStrategoTerm r_19 = null;
            if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
              break Fail53;
            n_19 = ((IStrategoList)term).head();
            o_19 = ((IStrategoList)term).tail();
            IStrategoList annos1 = term.getAnnotations();
            r_19 = annos1;
            p_19 = n_19;
            term = this.invoke(context, o_19);
            if(term == null)
              break Fail53;
            q_19 = term;
            IStrategoList list3;
            list3 = checkListTail(q_19);
            if(list3 == null)
              break Fail53;
            term = termFactory.annotateTerm((IStrategoTerm)termFactory.makeListCons(p_19, list3), checkListAnnos(termFactory, r_19));
            if(true)
              break Success23;
          }
          term = term22;
          if(term.getTermType() != IStrategoTerm.LIST || !((IStrategoList)term).isEmpty())
            break Fail52;
          term = s_19.invoke(context, term);
          if(term == null)
            break Fail52;
        }
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class lifted10 extends Strategy 
  { 
    TermReference l_19;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail54:
      { 
        if(l_19.value == null)
          break Fail54;
        term = l_19.value;
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class s_18 extends Strategy 
  { 
    Strategy p_18;

    Strategy r_18;

    Strategy q_18;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      ITermFactory termFactory = context.getFactory();
      Fail55:
      { 
        IStrategoTerm term18 = term;
        Success24:
        { 
          Fail56:
          { 
            IStrategoTerm t_18 = null;
            IStrategoTerm v_18 = null;
            IStrategoTerm u_18 = null;
            IStrategoTerm w_18 = null;
            v_18 = term;
            term = p_18.invoke(context, term);
            if(term == null)
              break Fail56;
            t_18 = term;
            term = v_18;
            w_18 = v_18;
            term = crush_3_0.instance.invoke(context, term, lifted8.instance, q_18, this);
            if(term == null)
              break Fail56;
            u_18 = term;
            term = w_18;
            IStrategoList list2;
            list2 = checkListTail(u_18);
            if(list2 == null)
              break Fail56;
            term = (IStrategoTerm)termFactory.makeListCons(t_18, list2);
            if(true)
              break Success24;
          }
          term = term18;
          IStrategoTerm term19 = term;
          Success25:
          { 
            Fail57:
            { 
              term = r_18.invoke(context, term);
              if(term == null)
                break Fail57;
              term = this.invoke(context, term);
              if(term == null)
                break Fail57;
              if(true)
                break Success25;
            }
            term = crush_3_0.instance.invoke(context, term19, lifted9.instance, q_18, this);
            if(term == null)
              break Fail55;
          }
        }
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class lifted9 extends Strategy 
  { 
    public static final lifted9 instance = new lifted9();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail58:
      { 
        term = arrows.constNil0;
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class lifted8 extends Strategy 
  { 
    public static final lifted8 instance = new lifted8();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail59:
      { 
        term = arrows.constNil0;
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class k_18 extends Strategy 
  { 
    Strategy i_18;

    Strategy j_18;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      ITermFactory termFactory = context.getFactory();
      Fail60:
      { 
        IStrategoTerm term17 = term;
        Success26:
        { 
          Fail61:
          { 
            IStrategoTerm l_18 = null;
            IStrategoTerm n_18 = null;
            IStrategoTerm m_18 = null;
            IStrategoTerm o_18 = null;
            n_18 = term;
            term = i_18.invoke(context, term);
            if(term == null)
              break Fail61;
            l_18 = term;
            term = n_18;
            o_18 = n_18;
            term = crush_3_0.instance.invoke(context, term, lifted6.instance, j_18, this);
            if(term == null)
              break Fail61;
            m_18 = term;
            term = o_18;
            IStrategoList list1;
            list1 = checkListTail(m_18);
            if(list1 == null)
              break Fail61;
            term = (IStrategoTerm)termFactory.makeListCons(l_18, list1);
            if(true)
              break Success26;
          }
          term = crush_3_0.instance.invoke(context, term17, lifted7.instance, j_18, this);
          if(term == null)
            break Fail60;
        }
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class lifted7 extends Strategy 
  { 
    public static final lifted7 instance = new lifted7();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail62:
      { 
        term = arrows.constNil0;
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class lifted6 extends Strategy 
  { 
    public static final lifted6 instance = new lifted6();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail63:
      { 
        term = arrows.constNil0;
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class g_18 extends Strategy 
  { 
    Strategy f_18;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      ITermFactory termFactory = context.getFactory();
      Fail64:
      { 
        IStrategoTerm term16 = term;
        Success27:
        { 
          Fail65:
          { 
            if(term.getTermType() != IStrategoTerm.LIST || !((IStrategoList)term).isEmpty())
              break Fail65;
            if(true)
              break Success27;
          }
          term = term16;
          IStrategoTerm a_18 = null;
          IStrategoTerm b_18 = null;
          IStrategoTerm c_18 = null;
          IStrategoTerm d_18 = null;
          IStrategoTerm e_18 = null;
          if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
            break Fail64;
          a_18 = ((IStrategoList)term).head();
          b_18 = ((IStrategoList)term).tail();
          IStrategoList annos0 = term.getAnnotations();
          e_18 = annos0;
          term = f_18.invoke(context, a_18);
          if(term == null)
            break Fail64;
          c_18 = term;
          term = this.invoke(context, b_18);
          if(term == null)
            break Fail64;
          d_18 = term;
          IStrategoList list0;
          list0 = checkListTail(d_18);
          if(list0 == null)
            break Fail64;
          term = termFactory.annotateTerm((IStrategoTerm)termFactory.makeListCons(c_18, list0), checkListAnnos(termFactory, e_18));
        }
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class lifted4 extends Strategy 
  { 
    public static final lifted4 instance = new lifted4();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail66:
      { 
        IStrategoTerm t_17 = null;
        if(term.getTermType() != IStrategoTerm.APPL || arrows._consVarFunLHS_2 != ((IStrategoAppl)term).getConstructor())
          break Fail66;
        t_17 = term.getSubterm(0);
        term = t_17;
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class lifted2 extends Strategy 
  { 
    public static final lifted2 instance = new lifted2();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail67:
      { 
        if(term.getTermType() != IStrategoTerm.APPL || arrows._consVar_1 != ((IStrategoAppl)term).getConstructor())
          break Fail67;
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class lifted1 extends Strategy 
  { 
    public static final lifted1 instance = new lifted1();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail68:
      { 
        if(term.getTermType() != IStrategoTerm.APPL || arrows._consVar_1 != ((IStrategoAppl)term).getConstructor())
          break Fail68;
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class lifted0 extends Strategy 
  { 
    TermReference o_32;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail69:
      { 
        term = desugar_arrow_p__0_1.instance.invoke(context, term, o_32.value);
        if(term == null)
          break Fail69;
        if(true)
          return term;
      }
      return null;
    }
  }

  public static void registerInterop(org.spoofax.interpreter.core.IContext context, Context compiledContext)
  { 
    new InteropRegisterer().registerLazy(context, compiledContext, InteropRegisterer.class.getClassLoader());
  }

  @SuppressWarnings("unused") public static class InteropRegisterer extends org.strategoxt.lang.InteropRegisterer 
  { 
    @Override public void register(org.spoofax.interpreter.core.IContext context, Context compiledContext)
    { 
      register(context, compiledContext, context.getVarScope());
    }

    @Override public void registerLazy(org.spoofax.interpreter.core.IContext context, Context compiledContext, ClassLoader classLoader)
    { 
      registerLazy(context, compiledContext, classLoader, context.getVarScope());
    }

    private void register(org.spoofax.interpreter.core.IContext context, Context compiledContext, org.spoofax.interpreter.core.VarScope varScope)
    { 
      compiledContext.registerComponent("arrows");
      arrows.init(compiledContext);
      varScope.addSVar("desugar_arrow_0_0", new InteropSDefT(desugar_arrow_0_0.instance, context));
      varScope.addSVar("desugar_arrow_p__0_1", new InteropSDefT(desugar_arrow_p__0_1.instance, context));
      varScope.addSVar("tuple_pat_0_0", new InteropSDefT(tuple_pat_0_0.instance, context));
      varScope.addSVar("tuple_0_0", new InteropSDefT(tuple_0_0.instance, context));
      varScope.addSVar("free_pat_vars_0_0", new InteropSDefT(free_pat_vars_0_0.instance, context));
      varScope.addSVar("free_decls_vars_0_0", new InteropSDefT(free_decls_vars_0_0.instance, context));
      varScope.addSVar("apply_all_0_1", new InteropSDefT(apply_all_0_1.instance, context));
      varScope.addSVar("map_1_0", new InteropSDefT(map_1_0.instance, context));
      varScope.addSVar("collect_all_1_0", new InteropSDefT(collect_all_1_0.instance, context));
      varScope.addSVar("collect_all_2_0", new InteropSDefT(collect_all_2_0.instance, context));
      varScope.addSVar("collect_all_3_0", new InteropSDefT(collect_all_3_0.instance, context));
      varScope.addSVar("crush_3_0", new InteropSDefT(crush_3_0.instance, context));
      varScope.addSVar("foldr_3_0", new InteropSDefT(foldr_3_0.instance, context));
      varScope.addSVar("conc_0_0", new InteropSDefT(conc_0_0.instance, context));
      varScope.addSVar("at_end_1_0", new InteropSDefT(at_end_1_0.instance, context));
      varScope.addSVar("concat_0_0", new InteropSDefT(concat_0_0.instance, context));
      varScope.addSVar("union_0_0", new InteropSDefT(union_0_0.instance, context));
      varScope.addSVar("HdMember_1_0", new InteropSDefT($Hd$Member_1_0.instance, context));
      varScope.addSVar("fetch_1_0", new InteropSDefT(fetch_1_0.instance, context));
      varScope.addSVar("eq_0_0", new InteropSDefT(eq_0_0.instance, context));
      varScope.addSVar("oncetd_1_0", new InteropSDefT(oncetd_1_0.instance, context));
      varScope.addSVar("main_0_0", new InteropSDefT(main_0_0.instance, context));
    }

    private void registerLazy(org.spoofax.interpreter.core.IContext context, Context compiledContext, ClassLoader classLoader, org.spoofax.interpreter.core.VarScope varScope)
    { 
      compiledContext.registerComponent("arrows");
      arrows.init(compiledContext);
      varScope.addSVar("desugar_arrow_0_0", new InteropSDefT(classLoader, "arrows$desugar_arrow_0_0", context));
      varScope.addSVar("desugar_arrow_p__0_1", new InteropSDefT(classLoader, "arrows$desugar_arrow_p__0_1", context));
      varScope.addSVar("tuple_pat_0_0", new InteropSDefT(classLoader, "arrows$tuple_pat_0_0", context));
      varScope.addSVar("tuple_0_0", new InteropSDefT(classLoader, "arrows$tuple_0_0", context));
      varScope.addSVar("free_pat_vars_0_0", new InteropSDefT(classLoader, "arrows$free_pat_vars_0_0", context));
      varScope.addSVar("free_decls_vars_0_0", new InteropSDefT(classLoader, "arrows$free_decls_vars_0_0", context));
      varScope.addSVar("apply_all_0_1", new InteropSDefT(classLoader, "arrows$apply_all_0_1", context));
      varScope.addSVar("map_1_0", new InteropSDefT(classLoader, "arrows$map_1_0", context));
      varScope.addSVar("collect_all_1_0", new InteropSDefT(classLoader, "arrows$collect_all_1_0", context));
      varScope.addSVar("collect_all_2_0", new InteropSDefT(classLoader, "arrows$collect_all_2_0", context));
      varScope.addSVar("collect_all_3_0", new InteropSDefT(classLoader, "arrows$collect_all_3_0", context));
      varScope.addSVar("crush_3_0", new InteropSDefT(classLoader, "arrows$crush_3_0", context));
      varScope.addSVar("foldr_3_0", new InteropSDefT(classLoader, "arrows$foldr_3_0", context));
      varScope.addSVar("conc_0_0", new InteropSDefT(classLoader, "arrows$conc_0_0", context));
      varScope.addSVar("at_end_1_0", new InteropSDefT(classLoader, "arrows$at_end_1_0", context));
      varScope.addSVar("concat_0_0", new InteropSDefT(classLoader, "arrows$concat_0_0", context));
      varScope.addSVar("union_0_0", new InteropSDefT(classLoader, "arrows$union_0_0", context));
      varScope.addSVar("HdMember_1_0", new InteropSDefT(classLoader, "arrows$$Hd$Member_1_0", context));
      varScope.addSVar("fetch_1_0", new InteropSDefT(classLoader, "arrows$fetch_1_0", context));
      varScope.addSVar("eq_0_0", new InteropSDefT(classLoader, "arrows$eq_0_0", context));
      varScope.addSVar("oncetd_1_0", new InteropSDefT(classLoader, "arrows$oncetd_1_0", context));
      varScope.addSVar("main_0_0", new InteropSDefT(classLoader, "arrows$main_0_0", context));
    }
  }
}