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

  protected static IStrategoTerm const4;

  protected static IStrategoTerm constConstr1;

  protected static IStrategoTerm const3;

  protected static IStrategoTerm constConstr0;

  protected static IStrategoTerm const2;

  protected static IStrategoTerm const1;

  protected static IStrategoTerm constNil0;

  protected static IStrategoTerm constVar0;

  protected static IStrategoTerm const0;

  public static IStrategoConstructor _consConc_2;

  public static IStrategoConstructor _consNone_0;

  public static IStrategoConstructor _consSome_1;

  public static IStrategoConstructor _consUnit_0;

  public static IStrategoConstructor _consVarVar_1;

  public static IStrategoConstructor _consArrProcedure_2;

  public static IStrategoConstructor _consVar_1;

  public static IStrategoConstructor _consConstr_1;

  public static IStrategoConstructor _consProduct_1;

  public static IStrategoConstructor _consAbs_2;

  public static IStrategoConstructor _consIf_3;

  public static IStrategoConstructor _consAppBin_2;

  public static IStrategoConstructor _consOpApp_3;

  public static IStrategoConstructor _consArrIf_3;

  public static IStrategoConstructor _consECons_2;

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
    _consVarVar_1 = termFactory.makeConstructor("VarVar", 1);
    _consArrProcedure_2 = termFactory.makeConstructor("ArrProcedure", 2);
    _consVar_1 = termFactory.makeConstructor("Var", 1);
    _consConstr_1 = termFactory.makeConstructor("Constr", 1);
    _consProduct_1 = termFactory.makeConstructor("Product", 1);
    _consAbs_2 = termFactory.makeConstructor("Abs", 2);
    _consIf_3 = termFactory.makeConstructor("If", 3);
    _consAppBin_2 = termFactory.makeConstructor("AppBin", 2);
    _consOpApp_3 = termFactory.makeConstructor("OpApp", 3);
    _consArrIf_3 = termFactory.makeConstructor("ArrIf", 3);
    _consECons_2 = termFactory.makeConstructor("ECons", 2);
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
    const2 = termFactory.makeString("Left");
    constConstr0 = termFactory.makeAppl(arrows._consConstr_1, new IStrategoTerm[]{arrows.const2});
    const3 = termFactory.makeString("Right");
    constConstr1 = termFactory.makeAppl(arrows._consConstr_1, new IStrategoTerm[]{arrows.const3});
    const4 = termFactory.makeString("|||");
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

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, IStrategoTerm x_12)
    { 
      ITermFactory termFactory = context.getFactory();
      context.push("desugar_arrow_p__0_1");
      Fail1:
      { 
        IStrategoTerm y_12 = null;
        IStrategoTerm z_12 = null;
        IStrategoTerm a_13 = null;
        IStrategoTerm b_13 = null;
        IStrategoTerm c_13 = null;
        IStrategoTerm d_13 = null;
        IStrategoTerm e_13 = null;
        if(term.getTermType() != IStrategoTerm.APPL || arrows._consArrIf_3 != ((IStrategoAppl)term).getConstructor())
          break Fail1;
        y_12 = term.getSubterm(0);
        z_12 = term.getSubterm(1);
        a_13 = term.getSubterm(2);
        term = tuple_pat_0_0.instance.invoke(context, x_12);
        if(term == null)
          break Fail1;
        b_13 = term;
        term = tuple_0_0.instance.invoke(context, x_12);
        if(term == null)
          break Fail1;
        c_13 = term;
        term = tuple_0_0.instance.invoke(context, x_12);
        if(term == null)
          break Fail1;
        d_13 = term;
        term = this.invoke(context, z_12, x_12);
        if(term == null)
          break Fail1;
        e_13 = term;
        term = this.invoke(context, a_13, x_12);
        if(term == null)
          break Fail1;
        term = termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constVar0, termFactory.makeAppl(arrows._consAbs_2, new IStrategoTerm[]{(IStrategoTerm)termFactory.makeListCons(b_13, (IStrategoList)arrows.constNil0), termFactory.makeAppl(arrows._consIf_3, new IStrategoTerm[]{y_12, termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constConstr0, c_13}), termFactory.makeAppl(arrows._consAppBin_2, new IStrategoTerm[]{arrows.constConstr1, d_13})})})}), arrows.const1, termFactory.makeAppl(arrows._consOpApp_3, new IStrategoTerm[]{e_13, arrows.const4, term})});
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
      Fail2:
      { 
        IStrategoTerm term0 = term;
        Success0:
        { 
          Fail3:
          { 
            if(term.getTermType() != IStrategoTerm.LIST || !((IStrategoList)term).isEmpty())
              break Fail3;
            term = arrows.constConstrPat0;
            if(true)
              break Success0;
          }
          term = term0;
          IStrategoTerm term1 = term;
          Success1:
          { 
            Fail4:
            { 
              IStrategoTerm n_13 = null;
              if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
                break Fail4;
              n_13 = ((IStrategoList)term).head();
              IStrategoTerm arg0 = ((IStrategoList)term).tail();
              if(arg0.getTermType() != IStrategoTerm.LIST || !((IStrategoList)arg0).isEmpty())
                break Fail4;
              term = n_13;
              if(true)
                break Success1;
            }
            term = term1;
            IStrategoTerm l_13 = null;
            IStrategoTerm m_13 = null;
            if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
              break Fail2;
            l_13 = ((IStrategoList)term).head();
            m_13 = ((IStrategoList)term).tail();
            term = termFactory.makeAppl(arrows._consTuplePat_2, new IStrategoTerm[]{l_13, m_13});
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
      Fail5:
      { 
        IStrategoTerm term2 = term;
        Success2:
        { 
          Fail6:
          { 
            if(term.getTermType() != IStrategoTerm.LIST || !((IStrategoList)term).isEmpty())
              break Fail6;
            term = arrows.constConstr2;
            if(true)
              break Success2;
          }
          term = term2;
          IStrategoTerm term3 = term;
          Success3:
          { 
            Fail7:
            { 
              IStrategoTerm q_13 = null;
              if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
                break Fail7;
              q_13 = ((IStrategoList)term).head();
              IStrategoTerm arg1 = ((IStrategoList)term).tail();
              if(arg1.getTermType() != IStrategoTerm.LIST || !((IStrategoList)arg1).isEmpty())
                break Fail7;
              term = q_13;
              if(true)
                break Success3;
            }
            term = term3;
            IStrategoTerm o_13 = null;
            IStrategoTerm p_13 = null;
            if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
              break Fail5;
            o_13 = ((IStrategoList)term).head();
            p_13 = ((IStrategoList)term).tail();
            term = termFactory.makeAppl(arrows._consProduct_1, new IStrategoTerm[]{termFactory.makeAppl(arrows._consECons_2, new IStrategoTerm[]{o_13, p_13})});
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
      Fail8:
      { 
        term = collect_all_1_0.instance.invoke(context, term, lifted0.instance);
        if(term == null)
          break Fail8;
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

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy f_14)
    { 
      context.push("collect_all_1_0");
      Fail9:
      { 
        term = collect_all_2_0.instance.invoke(context, term, f_14, union_0_0.instance);
        if(term == null)
          break Fail9;
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

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy g_14, Strategy h_14)
    { 
      context.push("collect_all_2_0");
      Fail10:
      { 
        i_14 i_140 = new i_14();
        i_140.g_14 = g_14;
        i_140.h_14 = h_14;
        term = i_140.invoke(context, term);
        if(term == null)
          break Fail10;
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

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy w_14, Strategy x_14, Strategy y_14)
    { 
      context.push("crush_3_0");
      Fail11:
      { 
        IStrategoTerm args0 = context.invokePrimitive("SSL_get_arguments", term, NO_STRATEGIES, new IStrategoTerm[]{term});
        term = foldr_3_0.instance.invoke(context, args0, w_14, x_14, y_14);
        if(term == null)
          break Fail11;
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

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy b_15, Strategy c_15, Strategy d_15)
    { 
      ITermFactory termFactory = context.getFactory();
      context.push("foldr_3_0");
      Fail12:
      { 
        IStrategoTerm term5 = term;
        Success4:
        { 
          Fail13:
          { 
            if(term.getTermType() != IStrategoTerm.LIST || !((IStrategoList)term).isEmpty())
              break Fail13;
            term = b_15.invoke(context, term);
            if(term == null)
              break Fail13;
            if(true)
              break Success4;
          }
          term = term5;
          IStrategoTerm z_14 = null;
          IStrategoTerm a_15 = null;
          IStrategoTerm e_15 = null;
          if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
            break Fail12;
          z_14 = ((IStrategoList)term).head();
          a_15 = ((IStrategoList)term).tail();
          term = d_15.invoke(context, z_14);
          if(term == null)
            break Fail12;
          e_15 = term;
          term = this.invoke(context, a_15, b_15, c_15, d_15);
          if(term == null)
            break Fail12;
          term = termFactory.makeTuple(e_15, term);
          term = c_15.invoke(context, term);
          if(term == null)
            break Fail12;
        }
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
      Fail14:
      { 
        TermReference v_15 = new TermReference();
        IStrategoTerm w_15 = null;
        if(term.getTermType() != IStrategoTerm.TUPLE || term.getSubtermCount() != 2)
          break Fail14;
        w_15 = term.getSubterm(0);
        if(v_15.value == null)
          v_15.value = term.getSubterm(1);
        else
          if(v_15.value != term.getSubterm(1) && !v_15.value.match(term.getSubterm(1)))
            break Fail14;
        term = w_15;
        c_16 c_160 = new c_16();
        c_160.v_15 = v_15;
        term = c_160.invoke(context, term);
        if(term == null)
          break Fail14;
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

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy g_16)
    { 
      context.push("HdMember_1_0");
      Fail15:
      { 
        IStrategoTerm d_16 = null;
        TermReference e_16 = new TermReference();
        if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
          break Fail15;
        if(e_16.value == null)
          e_16.value = ((IStrategoList)term).head();
        else
          if(e_16.value != ((IStrategoList)term).head() && !e_16.value.match(((IStrategoList)term).head()))
            break Fail15;
        d_16 = ((IStrategoList)term).tail();
        term = g_16.invoke(context, term);
        if(term == null)
          break Fail15;
        lifted5 lifted50 = new lifted5();
        lifted50.e_16 = e_16;
        term = fetch_1_0.instance.invoke(context, term, lifted50);
        if(term == null)
          break Fail15;
        term = d_16;
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

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy s_16)
    { 
      context.push("fetch_1_0");
      Fail16:
      { 
        t_16 t_160 = new t_16();
        t_160.s_16 = s_16;
        term = t_160.invoke(context, term);
        if(term == null)
          break Fail16;
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
      Fail17:
      { 
        IStrategoTerm u_16 = null;
        if(term.getTermType() != IStrategoTerm.TUPLE || term.getSubtermCount() != 2)
          break Fail17;
        u_16 = term.getSubterm(0);
        if(term.getSubterm(1) != u_16 && !u_16.match(term.getSubterm(1)))
          break Fail17;
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

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy v_16)
    { 
      context.push("oncetd_1_0");
      Fail18:
      { 
        w_16 w_160 = new w_16();
        w_160.v_16 = v_16;
        term = w_160.invoke(context, term);
        if(term == null)
          break Fail18;
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
      Fail19:
      { 
        term = oncetd_1_0.instance.invoke(context, term, desugar_arrow_0_0.instance);
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

  @SuppressWarnings("all") private static final class w_16 extends Strategy 
  { 
    Strategy v_16;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail20:
      { 
        IStrategoTerm term9 = term;
        Success5:
        { 
          Fail21:
          { 
            term = v_16.invoke(context, term);
            if(term == null)
              break Fail21;
            if(true)
              break Success5;
          }
          term = SRTS_one.instance.invoke(context, term9, this);
          if(term == null)
            break Fail20;
        }
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class t_16 extends Strategy 
  { 
    Strategy s_16;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      ITermFactory termFactory = context.getFactory();
      Fail22:
      { 
        IStrategoTerm term8 = term;
        Success6:
        { 
          Fail23:
          { 
            IStrategoTerm i_16 = null;
            IStrategoTerm j_16 = null;
            IStrategoTerm k_16 = null;
            IStrategoTerm l_16 = null;
            IStrategoTerm m_16 = null;
            if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
              break Fail23;
            i_16 = ((IStrategoList)term).head();
            j_16 = ((IStrategoList)term).tail();
            IStrategoList annos1 = term.getAnnotations();
            m_16 = annos1;
            term = s_16.invoke(context, i_16);
            if(term == null)
              break Fail23;
            k_16 = term;
            term = j_16;
            l_16 = j_16;
            IStrategoList list2;
            list2 = checkListTail(l_16);
            if(list2 == null)
              break Fail23;
            term = termFactory.annotateTerm((IStrategoTerm)termFactory.makeListCons(k_16, list2), checkListAnnos(termFactory, m_16));
            if(true)
              break Success6;
          }
          term = term8;
          IStrategoTerm n_16 = null;
          IStrategoTerm o_16 = null;
          IStrategoTerm p_16 = null;
          IStrategoTerm q_16 = null;
          IStrategoTerm r_16 = null;
          if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
            break Fail22;
          n_16 = ((IStrategoList)term).head();
          o_16 = ((IStrategoList)term).tail();
          IStrategoList annos2 = term.getAnnotations();
          r_16 = annos2;
          p_16 = n_16;
          term = this.invoke(context, o_16);
          if(term == null)
            break Fail22;
          q_16 = term;
          IStrategoList list3;
          list3 = checkListTail(q_16);
          if(list3 == null)
            break Fail22;
          term = termFactory.annotateTerm((IStrategoTerm)termFactory.makeListCons(p_16, list3), checkListAnnos(termFactory, r_16));
        }
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class lifted5 extends Strategy 
  { 
    TermReference e_16;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      ITermFactory termFactory = context.getFactory();
      Fail24:
      { 
        IStrategoTerm f_16 = null;
        f_16 = term;
        if(e_16.value == null)
          break Fail24;
        term = termFactory.makeTuple(e_16.value, f_16);
        term = eq_0_0.instance.invoke(context, term);
        if(term == null)
          break Fail24;
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class c_16 extends Strategy 
  { 
    TermReference v_15;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      ITermFactory termFactory = context.getFactory();
      Fail25:
      { 
        IStrategoTerm term6 = term;
        Success7:
        { 
          Fail26:
          { 
            if(term.getTermType() != IStrategoTerm.LIST || !((IStrategoList)term).isEmpty())
              break Fail26;
            if(v_15.value == null)
              break Fail26;
            term = v_15.value;
            if(true)
              break Success7;
          }
          term = term6;
          IStrategoTerm term7 = term;
          Success8:
          { 
            Fail27:
            { 
              lifted4 lifted40 = new lifted4();
              lifted40.v_15 = v_15;
              term = $Hd$Member_1_0.instance.invoke(context, term, lifted40);
              if(term == null)
                break Fail27;
              term = this.invoke(context, term);
              if(term == null)
                break Fail27;
              if(true)
                break Success8;
            }
            term = term7;
            IStrategoTerm x_15 = null;
            IStrategoTerm y_15 = null;
            IStrategoTerm z_15 = null;
            IStrategoTerm a_16 = null;
            IStrategoTerm b_16 = null;
            if(term.getTermType() != IStrategoTerm.LIST || ((IStrategoList)term).isEmpty())
              break Fail25;
            x_15 = ((IStrategoList)term).head();
            y_15 = ((IStrategoList)term).tail();
            IStrategoList annos0 = term.getAnnotations();
            b_16 = annos0;
            z_15 = x_15;
            term = this.invoke(context, y_15);
            if(term == null)
              break Fail25;
            a_16 = term;
            IStrategoList list1;
            list1 = checkListTail(a_16);
            if(list1 == null)
              break Fail25;
            term = termFactory.annotateTerm((IStrategoTerm)termFactory.makeListCons(z_15, list1), checkListAnnos(termFactory, b_16));
          }
        }
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class lifted4 extends Strategy 
  { 
    TermReference v_15;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail28:
      { 
        if(v_15.value == null)
          break Fail28;
        term = v_15.value;
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class i_14 extends Strategy 
  { 
    Strategy g_14;

    Strategy h_14;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      ITermFactory termFactory = context.getFactory();
      Fail29:
      { 
        IStrategoTerm term4 = term;
        Success9:
        { 
          Fail30:
          { 
            IStrategoTerm j_14 = null;
            IStrategoTerm l_14 = null;
            IStrategoTerm k_14 = null;
            IStrategoTerm m_14 = null;
            l_14 = term;
            term = g_14.invoke(context, term);
            if(term == null)
              break Fail30;
            j_14 = term;
            term = l_14;
            m_14 = l_14;
            term = crush_3_0.instance.invoke(context, term, lifted2.instance, h_14, this);
            if(term == null)
              break Fail30;
            k_14 = term;
            term = m_14;
            IStrategoList list0;
            list0 = checkListTail(k_14);
            if(list0 == null)
              break Fail30;
            term = (IStrategoTerm)termFactory.makeListCons(j_14, list0);
            if(true)
              break Success9;
          }
          term = crush_3_0.instance.invoke(context, term4, lifted3.instance, h_14, this);
          if(term == null)
            break Fail29;
        }
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class lifted3 extends Strategy 
  { 
    public static final lifted3 instance = new lifted3();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail31:
      { 
        term = arrows.constNil0;
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
      Fail32:
      { 
        term = arrows.constNil0;
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class lifted0 extends Strategy 
  { 
    public static final lifted0 instance = new lifted0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail33:
      { 
        if(term.getTermType() != IStrategoTerm.APPL || arrows._consVarVar_1 != ((IStrategoAppl)term).getConstructor())
          break Fail33;
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
      varScope.addSVar("collect_all_1_0", new InteropSDefT(collect_all_1_0.instance, context));
      varScope.addSVar("collect_all_2_0", new InteropSDefT(collect_all_2_0.instance, context));
      varScope.addSVar("crush_3_0", new InteropSDefT(crush_3_0.instance, context));
      varScope.addSVar("foldr_3_0", new InteropSDefT(foldr_3_0.instance, context));
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
      varScope.addSVar("collect_all_1_0", new InteropSDefT(classLoader, "arrows$collect_all_1_0", context));
      varScope.addSVar("collect_all_2_0", new InteropSDefT(classLoader, "arrows$collect_all_2_0", context));
      varScope.addSVar("crush_3_0", new InteropSDefT(classLoader, "arrows$crush_3_0", context));
      varScope.addSVar("foldr_3_0", new InteropSDefT(classLoader, "arrows$foldr_3_0", context));
      varScope.addSVar("union_0_0", new InteropSDefT(classLoader, "arrows$union_0_0", context));
      varScope.addSVar("HdMember_1_0", new InteropSDefT(classLoader, "arrows$$Hd$Member_1_0", context));
      varScope.addSVar("fetch_1_0", new InteropSDefT(classLoader, "arrows$fetch_1_0", context));
      varScope.addSVar("eq_0_0", new InteropSDefT(classLoader, "arrows$eq_0_0", context));
      varScope.addSVar("oncetd_1_0", new InteropSDefT(classLoader, "arrows$oncetd_1_0", context));
      varScope.addSVar("main_0_0", new InteropSDefT(classLoader, "arrows$main_0_0", context));
    }
  }
}