<section id="about" class="page">
<div class="page-header"><h1>About</h1></div>

<div class="page-content">

<p>This webpage makes use of the typechecker provided by <a href="https://github.com/tomgr/libcspm">libcspm</a>. This package provides two commands, <code>cspmchecker</code> and <code>cspmcheckeri</code>. The first, <code>cspmchecker</code> simply takes a list of files and then prints any type errors to the console. The following example illustrates its usage.</p>

<pre>$ cspmchecker burglar.csp 
Checking burglar.csp.....
./burglar.csp:225:57-73:
    Couldn't match expected type (({(Identifiers, Int)}, a, b)) -> (&lt;Int>, &lt;needs>)
            with actual type (({(Identifiers, Int)}, c, d)) -> (Int, &lt;needs>)>
    In the second argument of snocf, namely &lt;Ival(lastdigs)>
    In the expression: snocf(Ival(key), &lt;Ival(lastdigs)>)
    In the expression: (lastdigs, snocf(Ival(key), &lt;Ival(lastdigs)>))

./statechartcompiler.csp:124:57-80:
    Couldn't match expected type {a} with actual type ({Int}, b)
    In the first argument of Set, namely (Statelabel, Statechart)
    In the expression: Set((Statelabel, Statechart))
    In the expression: Set(ActLabel).Set((Statelabel, Statechart))</pre>

<p>The second command, <code>cspmcheckeri</code>, provides an interactive environment, much like <code>ghci</code> or <code>hugs</code>. The following example shows some of the functionality available.</p>

<pre>$ cspmcheckeri
> :load phils.csp 
Ok, loaded phils.csp
phils.csp> 
ASPHILS    AlphaF     FORK       LPHILs     PHILS      SYSTEMs    picks
ASPHILSs   AlphaP     FORKNAMES  N          PHILs      T          putsdown
ASSYSTEM   BSYSTEM    FORKS      PHIL       SYSTEM     eats       sits
ASSYSTEMs  BUTLER     LPHIL      PHILNAMES  SYSTEM'    getsup     thinks
phils.csp> :type PHIL
PHIL :: (Int) -> Proc
phils.csp> :type thinks
thinks :: Int=>Event
phils.csp> PHIL(0)
PHIL(0)
phils.csp> :
:load       :printProc  :quit       :reload     :type
phils.csp> :printProc PHIL(0)
PHIL(0) =
thinks.0 -> sits.0 -> picks.0.0 -> picks.0.1 -> eats.0 -> putsdown.0.1 -> putsdown.0.0 -> getsup.0 -> PHIL(0)

PHIL(0)
phils.csp> :printProc eats.0.0 -> STOP
&lt;interactive&gt;:1:1-9:
Couldn't match expected type Int with actual type Int.Int
whilst matching expected type Event with actual type Event.Int
In the expression: eats.0.0
In the expression: eats.0.0 -> STOP</pre>

<p>These tools can either be installed by downloading a pre-compiled binary from <a href="https://github.com/tomgr/libcspm/downloads">here</a>, however, the recommened route is to install using <a href="http://www.haskell.org/cabal">Cabal</a>, the Haskell package manager. This can be done by firstly installing the <a href="http://www.haskell.org/platform">Haskell Platform</a> (ensuring that GHC is at least version 7), then by running <code>cabal install cspmchecker</code> on the command line.</p>

<h2>Credits</h2>
<p>This website makes use of <a href="https://github.com/tomgr/libcspm">libcspm</a>, the <a href="http://www.snapframework.com/">Snap Framework</a>, <a href="http://twitter.github.com/bootstrap/">Twitter Bootstrap</a> and <a href="https://github.com/deplorableword/textmate-solarized">Solarized</a>. Source code available from <a href="https://github.com/tomgr/webcspm">GitHub</a>.</p>

</div>
</section>