<section id="help" class="page">
<div class="page-header"><h1>Type System Help</h1></div>
<div class="page-content">

<!-- ********************************************************************* -->
<!-- Introduction -->

<ol id="help_nav">
    <li>
        <a href="#type_system">Type System</a>
        <ol>
            <li><a href="#ground_types">Ground Types</a></li>
            <li><a href="#constructor_types">Constructor Types</a></li>
            <li><a href="#channels_datatypes">Channels and Datatypes</a></li>
            <li><a href="#type_constraints">Type Constraints</a></li>
        </ol>
    </li>
    <li><a href="#common_errors">Common Errors</a></li>
    <li><a href="#type_checking_difficulties">Type Checking Difficulties</a></li>
</ol>

<p>The type system used in libcspm is very essentially a simplified version of Haskell's, along with suitable modifications to enable processes and events to be represented. More formally, it is a rank-1 polymorphic type system, meaning that polymorphic functions (like <cspm>id(x) = x</cspm>) can be defined, but any universal quantifications must be on the outside.</p>

<p>In this section we give a brief overview of the <a href="#type_system">type system</a> and then give <a href="#common_errors">several examples</a> to show how some of the most common errors present in CSPM scripts can be detected. Lastly, <a href="#type_checking_difficulties">we explain</a> some of the difficulties involved in type checking CSPM, and explain why it is, in general, undecidable.</p>


<!-- ********************************************************************* -->
<h2 id="type_system">The Type System</h2>

<p>In this section we present the type system, along with several examples to illustrate it. Throughout this section we write <cspm>f :: t</cspm> to mean that <cspm>f</cspm> is of type <type>t</type>. We write type variables using lowercase letters, like <type>a</type>.</p>


<!-- ********************************************************************* -->
<h3 id="ground_types">The Ground Types</h3>

<p>The built-in ground types are consist of type variables, like <type>a</type> along with <type>Bool</type>, <type>Event</type>, <type>Int</type> and <type>Proc</type>, which represent the type of booleans, events, integers and processes respectively. There is also a special type, <type>Eventable</type>, that represents something that is either an Event, or which can be made into an event by dotting it with suitable values. For example, in the expression <cspm>{| c |}</cspm>, <cspm>c</cspm> is merely required to be <type>Eventable</type>.</p>

<p>As CSPM allows the user to define new datatypes, ground types can be defined within the users source. For example, if the user writes <cspm>datatype A = ...</cspm> within their file, then a new ground type <type>A</type> is defined.</p>

<p>The example below contains some variables that are of ground types. Each variable's type is given on the line above.</p>

<blockcspm id="example_ground_types"><script>-- f :: Int
f = 2

channel c : {0}
-- g :: Event
g = c.0

-- h :: Bool
h = true or false

-- P :: Proc
P = c.0 -> STOP

datatype A = X
-- k :: A
k = X</script></blockcspm>


<!-- ********************************************************************* -->
<h3 id="constructor_types">Constructor Types</h3>

<p>Type constructors take a type argument, and then return a new type. In CSPM the <em>simple</em> type constructor are <type>&lt;a&gt;</type>, <type>{a}</type> and <type>(a,b,c,...,z)</type>, representing something of type list of <type>a</type>, set of <type>a</type>, or a tuple of values. For example, consider the following.</p>

<blockcspm id="example_basic_constructor_types"><script>-- m :: <Int>
g = <0>

-- n :: {Int}
n = {0}

-- mn -- {{Int}}
mn = {{i} | i <- {0..}}

-- k :: (Int, Bool)
k = (0, true)</script></blockcspm>

<p>There is also a function constructor type. We say <cspm>f</cspm> has type <type>(a,b,c) -> d</type> iff <cspm>f</cspm> takes 3 arguments, of type <type>a</type>, <type>b</type> and <type>c</type> respectively, and returns something of type <type>d</type>. For example, consider the following.</p>

<blockcspm id="example_function_types"><script>-- f :: (Int, Event) -> Process
f(0,ev) = ev -> STOP
f(n,ev) = ev -> f(n-1, ev)

-- g :: (Int) -> ((Int) -> Int) -> Int
g(n)(h) = h(n)</script></blockcspm>

<p>Note that, so far, the type system is exactly like Haskell's, just with a slightly different syntax and a couple of different ground types.</p>


<!-- ********************************************************************* -->
<h3 id="channels_datatypes">Channels and Datatypes <small>Or How Dot Causes Problems</small></h3>

<p>We now consider how to type channels and datatypes. Suppose, for instance, that we have the following channel declaration and consider what type we might assign to <cspm>c</cspm>.</p>

<blockcspm id="basic_channel_definition">channel c : {0}</blockcspm>

<p>Observe that when <cspm>c</cspm> is placed on the left of a variable, say <cspm>x</cspm> that is of type <type>Int</type>, (i.e. as in <cspm>c.x</cspm>) then it <em>yields</em> something of type <type>Event</type>. Therefore, this suggests the type of <cspm>c</cspm> should be &ldquo;something that, when dotted (i.e. placed on the left of a dot) with something of type Int, yields an event&rdquo;. Therefore, we introduce a new type constructor, <em>yields</em>, written <type>=></type>, and say that <cspm>c</cspm> is of type <type>Int => Event</type> (pronounced &ldquo;<cspm>c</cspm> is of type <type>Int</type> yield <type>Event</type>&rdquo;).</p>

<p>This lets us type almost any CSPM program, with one particular exception. Consider the expression <cspm>0.0</cspm>; this is perfectly valid, and whilst not in general advisable, it can be useful under certain circumstances. Therefore, we introduce one further type constructor, <em>dot</em>. If something is of type <type>t1.t2</type> then it consists of two things dotted together, where the left hand side is of type <type>t1</type> and the right hand side is of type <type>t2</type>.<p>

<p>We now consider several examples of the above.</p>

<blockcspm id="example_yield_types"><script>-- B :: A, C :: Int=>Bool=>A
datatype A = B | C.{0}.Bool

-- c :: A=>Int=>Event
channel c : A.{0}

-- p :: A.Int 
p = B.0

-- q :: A.Int.Proc
q = C.0.true.0.STOP</script></blockcspm>

<p>Note that whilst all of the above definitions are well-typed, some of them (in particular <cspm>p</cspm> and <cspm>q</cspm>) are inadvisable and may be rejected by a future, stricter, type checker. For further discussion see the <a href="#type_checking_difficulties">section on difficulties with type checking</a>.</p>


<!-- ********************************************************************* -->
<h3 id="type_constraints">Constraints</h3>

<p>Like in Haskell we allow type variables to be constrained in certain ways. In general, if a type variable <type>a</type> has a constraint <type>c</type> then <type>a</type> may only be instantiated with types that satisfy the constraint <type>c</type>.</p>

<p>There are three possible constraints on type variables: <type>Eq</type>, <type>Ord</type> and <type>Inputable</type>. If a type variable <type>a</type> has the constraint <type>Eq a</type>, then this means that the type variable can be anything, providing equality is defined for values of type <type>a</type>. For example, this means that the type <type>Int</type> would be admissible, but no function types would be as functions are not comparable.</p>

<p>A type <type>t</type> satisfies the constraint <type>Ord</type> when the type represents something that can be ordered (i.e. using <cspm>&lt;</cspm> or similar). For example, <type>Int</type> satisfies the constraint <type>Ord</type>, but functions or processes would not.</p>

<p>As an example of these consider the following.</p>

<blockcspm id="example_type_constraints"><script>-- f :: Eq a => (a, a) -> Bool
f(x,y) = x == y
-- g :: Ord a => (a, a) -> Bool
g(x,y) = x < y

-- Sets require their elements to be comparable.
-- h :: Eq a => a -> {a}
h(x) = {x}

-- This would be allowed, as Int satisfies Eq.
f(0,1)
-- This would be allowed, as Int satisfies Ord.
g(0,1)
-- These would not be allowed, as no function type satisfies Eq or Ord.
f(f, g)
g(f, g)</script></blockcspm>

<p>The last type variable constraint is more complicated, and appears only very rarely. We say that a type <type>t</type> satisfies the constraint <type>Inputable a</type> if it is not a yield type. To see why this is necessary consider the following example.</p>

<blockcspm id="example_inputable_type_constraint"><script>-- Z :: A
datatype A = Z
-- c :: Int => Event
channel c : A

f(X) = c?x:X!0 -> STOP</script></blockcspm>

<p>When evaluating <cspm>c?x!0</cspm>, the evaluator has to pick a set of elements for <cspm>x</cspm> to range over. Therefore, as <cspm>c</cspm> has only one channel component, of type <type>A</type>, this set would need to consist of elements of type <type>Int=>A</type> (as then <cspm>x.0</cspm> would be of type <type>A</type>, as required). However, the rules that the evaluator uses for picking this set essentially say that it matches one field. Therefore, in the above example, <cspm>x</cspm> would range over everything of type <type>A</type>.</p>

<typecheck>
<script>f(d) = d?x!0 -> STOP

datatype A = X.Int
channel c : A
g = f(c)</script>
    <output id="typecheck_example_inputable_type_constraint">
        <p>For example, consider the following script.</p>
        <source/>
        <p>Type checking this script produces the following error.</p>
        <errors/>
    </output>
</typecheck>


<!-- ********************************************************************* -->
<h2 id="common_errors">Common Errors</h2>

<div class="alert-message block-message error">
    <p><strong>Forthcoming.</strong></p>
</div>


<!-- ********************************************************************* -->
<h2 id="type_checking_difficulties">Difficulties Type Checking CSPM</h2>

<div class="alert-message block-message error">
    <p><strong>Presently incomplete.</strong></p>
</div>

<p>Note that, in general, typechecking CSPM code is undecidable. For example, consider the following program.</p>

<blockcspm id="example_cspm_undecidable"><script>f(x) = x.f({x})</script></blockcspm>

<p>Clearly <cspm>f</cspm> is well typed. Consider the type of <cspm>f</cspm>; clearly <cspm>f</cspm> takes one argument, say of type <type>a</type>. However, the value it returns is of type <type>a.{a}.{{a}}...</type>. Hence, this function's type is necessarily infinite.</p>

</div> <!-- // page_content -->
</section>
