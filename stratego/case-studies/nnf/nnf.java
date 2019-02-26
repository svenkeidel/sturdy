import org.strategoxt.stratego_lib.*;
import org.strategoxt.lang.*;
import org.spoofax.interpreter.terms.*;
import static org.strategoxt.lang.Term.*;
import org.spoofax.interpreter.library.AbstractPrimitive;
import java.util.ArrayList;
import java.lang.ref.WeakReference;

@SuppressWarnings("all") public class nnf  
{ 
  protected static final boolean TRACES_ENABLED = true;

  protected static ITermFactory constantFactory;

  private static WeakReference<Context> initedContext;

  private static boolean isIniting;

  public static IStrategoConstructor _consConc_2;

  public static IStrategoConstructor _consNone_0;

  public static IStrategoConstructor _consSome_1;

  public static IStrategoConstructor _consNil_0;

  public static IStrategoConstructor _consNeg_1;

  public static IStrategoConstructor _consIff_2;

  public static IStrategoConstructor _consImp_2;

  public static IStrategoConstructor _consOr_2;

  public static IStrategoConstructor _consAnd_2;

  public static Context init(Context context)
  { 
    synchronized(nnf.class)
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
          context.registerComponent("nnf");
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
        result = context.invokeStrategyCLI(main_0_0.instance, "nnf", args);
      }
      finally
      { 
        context.getIOAgent().closeAllFiles();
      }
      if(result == null)
      { 
        System.err.println("nnf" + (TRACES_ENABLED ? ": rewriting failed, trace:" : ": rewriting failed"));
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
      return context.invokeStrategyCLI(main_0_0.instance, "nnf", args);
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
    _consNil_0 = termFactory.makeConstructor("Nil", 0);
    _consNeg_1 = termFactory.makeConstructor("Neg", 1);
    _consIff_2 = termFactory.makeConstructor("Iff", 2);
    _consImp_2 = termFactory.makeConstructor("Imp", 2);
    _consOr_2 = termFactory.makeConstructor("Or", 2);
    _consAnd_2 = termFactory.makeConstructor("And", 2);
  }

  public static void initConstants(ITermFactory termFactory)
  { }

  @SuppressWarnings("all") public static class nnf_0_0 extends Strategy 
  { 
    public static nnf_0_0 instance = new nnf_0_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      ITermFactory termFactory = context.getFactory();
      Fail0:
      { 
        IStrategoTerm term0 = term;
        IStrategoConstructor cons0 = term.getTermType() == IStrategoTerm.APPL ? ((IStrategoAppl)term).getConstructor() : null;
        Success0:
        { 
          if(cons0 == nnf._consNeg_1)
          { 
            Fail1:
            { 
              IStrategoTerm l_1 = null;
              IStrategoTerm m_1 = null;
              IStrategoTerm arg0 = term.getSubterm(0);
              if(arg0.getTermType() != IStrategoTerm.APPL || nnf._consOr_2 != ((IStrategoAppl)arg0).getConstructor())
                break Fail1;
              l_1 = arg0.getSubterm(0);
              m_1 = arg0.getSubterm(1);
              term = termFactory.makeAppl(nnf._consAnd_2, new IStrategoTerm[]{termFactory.makeAppl(nnf._consNeg_1, new IStrategoTerm[]{l_1}), termFactory.makeAppl(nnf._consNeg_1, new IStrategoTerm[]{m_1})});
              if(true)
                break Success0;
            }
            term = term0;
          }
          Success1:
          { 
            if(cons0 == nnf._consNeg_1)
            { 
              Fail2:
              { 
                IStrategoTerm j_1 = null;
                IStrategoTerm k_1 = null;
                IStrategoTerm arg1 = term.getSubterm(0);
                if(arg1.getTermType() != IStrategoTerm.APPL || nnf._consAnd_2 != ((IStrategoAppl)arg1).getConstructor())
                  break Fail2;
                j_1 = arg1.getSubterm(0);
                k_1 = arg1.getSubterm(1);
                term = termFactory.makeAppl(nnf._consOr_2, new IStrategoTerm[]{termFactory.makeAppl(nnf._consNeg_1, new IStrategoTerm[]{j_1}), termFactory.makeAppl(nnf._consNeg_1, new IStrategoTerm[]{k_1})});
                if(true)
                  break Success1;
              }
              term = term0;
            }
            Success2:
            { 
              if(cons0 == nnf._consNeg_1)
              { 
                Fail3:
                { 
                  IStrategoTerm h_1 = null;
                  IStrategoTerm i_1 = null;
                  IStrategoTerm arg2 = term.getSubterm(0);
                  if(arg2.getTermType() != IStrategoTerm.APPL || nnf._consImp_2 != ((IStrategoAppl)arg2).getConstructor())
                    break Fail3;
                  h_1 = arg2.getSubterm(0);
                  i_1 = arg2.getSubterm(1);
                  term = termFactory.makeAppl(nnf._consAnd_2, new IStrategoTerm[]{h_1, termFactory.makeAppl(nnf._consNeg_1, new IStrategoTerm[]{i_1})});
                  if(true)
                    break Success2;
                }
                term = term0;
              }
              Success3:
              { 
                if(cons0 == nnf._consNeg_1)
                { 
                  Fail4:
                  { 
                    IStrategoTerm f_1 = null;
                    IStrategoTerm g_1 = null;
                    IStrategoTerm arg3 = term.getSubterm(0);
                    if(arg3.getTermType() != IStrategoTerm.APPL || nnf._consIff_2 != ((IStrategoAppl)arg3).getConstructor())
                      break Fail4;
                    f_1 = arg3.getSubterm(0);
                    g_1 = arg3.getSubterm(1);
                    term = termFactory.makeAppl(nnf._consOr_2, new IStrategoTerm[]{termFactory.makeAppl(nnf._consAnd_2, new IStrategoTerm[]{f_1, termFactory.makeAppl(nnf._consNeg_1, new IStrategoTerm[]{g_1})}), termFactory.makeAppl(nnf._consAnd_2, new IStrategoTerm[]{termFactory.makeAppl(nnf._consNeg_1, new IStrategoTerm[]{f_1}), g_1})});
                    if(true)
                      break Success3;
                  }
                  term = term0;
                }
                Success4:
                { 
                  if(cons0 == nnf._consNeg_1)
                  { 
                    Fail5:
                    { 
                      IStrategoTerm e_1 = null;
                      IStrategoTerm arg4 = term.getSubterm(0);
                      if(arg4.getTermType() != IStrategoTerm.APPL || nnf._consNeg_1 != ((IStrategoAppl)arg4).getConstructor())
                        break Fail5;
                      e_1 = arg4.getSubterm(0);
                      term = e_1;
                      if(true)
                        break Success4;
                    }
                    term = term0;
                  }
                  Success5:
                  { 
                    if(cons0 == nnf._consImp_2)
                    { 
                      Fail6:
                      { 
                        IStrategoTerm c_1 = null;
                        IStrategoTerm d_1 = null;
                        c_1 = term.getSubterm(0);
                        d_1 = term.getSubterm(1);
                        term = termFactory.makeAppl(nnf._consOr_2, new IStrategoTerm[]{termFactory.makeAppl(nnf._consNeg_1, new IStrategoTerm[]{c_1}), d_1});
                        if(true)
                          break Success5;
                      }
                      term = term0;
                    }
                    if(cons0 == nnf._consIff_2)
                    { 
                      IStrategoTerm a_1 = null;
                      IStrategoTerm b_1 = null;
                      a_1 = term.getSubterm(0);
                      b_1 = term.getSubterm(1);
                      term = termFactory.makeAppl(nnf._consOr_2, new IStrategoTerm[]{termFactory.makeAppl(nnf._consAnd_2, new IStrategoTerm[]{a_1, b_1}), termFactory.makeAppl(nnf._consAnd_2, new IStrategoTerm[]{termFactory.makeAppl(nnf._consNeg_1, new IStrategoTerm[]{a_1}), termFactory.makeAppl(nnf._consNeg_1, new IStrategoTerm[]{b_1})})});
                    }
                    else
                    { 
                      break Fail0;
                    }
                  }
                }
              }
            }
          }
        }
        if(true)
          return term;
      }
      context.push("nnf_0_0");
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class topdown_1_0 extends Strategy 
  { 
    public static topdown_1_0 instance = new topdown_1_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term, Strategy n_1)
    { 
      context.push("topdown_1_0");
      Fail7:
      { 
        term = n_1.invoke(context, term);
        if(term == null)
          break Fail7;
        lifted0 lifted00 = new lifted0();
        lifted00.n_1 = n_1;
        term = SRTS_all.instance.invoke(context, term, lifted00);
        if(term == null)
          break Fail7;
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
      Fail8:
      { 
        term = topdown_1_0.instance.invoke(context, term, lifted1.instance);
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

  @SuppressWarnings("all") private static final class lifted1 extends Strategy 
  { 
    public static final lifted1 instance = new lifted1();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail9:
      { 
        IStrategoTerm term6 = term;
        Success6:
        { 
          Fail10:
          { 
            term = nnf_0_0.instance.invoke(context, term);
            if(term == null)
              break Fail10;
            if(true)
              break Success6;
          }
          term = term6;
        }
        if(true)
          return term;
      }
      return null;
    }
  }

  @SuppressWarnings("all") private static final class lifted0 extends Strategy 
  { 
    Strategy n_1;

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      Fail11:
      { 
        term = topdown_1_0.instance.invoke(context, term, n_1);
        if(term == null)
          break Fail11;
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
      compiledContext.registerComponent("nnf");
      nnf.init(compiledContext);
      varScope.addSVar("nnf_0_0", new InteropSDefT(nnf_0_0.instance, context));
      varScope.addSVar("topdown_1_0", new InteropSDefT(topdown_1_0.instance, context));
      varScope.addSVar("main_0_0", new InteropSDefT(main_0_0.instance, context));
    }

    private void registerLazy(org.spoofax.interpreter.core.IContext context, Context compiledContext, ClassLoader classLoader, org.spoofax.interpreter.core.VarScope varScope)
    { 
      compiledContext.registerComponent("nnf");
      nnf.init(compiledContext);
      varScope.addSVar("nnf_0_0", new InteropSDefT(classLoader, "nnf$nnf_0_0", context));
      varScope.addSVar("topdown_1_0", new InteropSDefT(classLoader, "nnf$topdown_1_0", context));
      varScope.addSVar("main_0_0", new InteropSDefT(classLoader, "nnf$main_0_0", context));
    }
  }
}