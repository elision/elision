/*======================================================================
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 * The Elision Term Rewriter
 * 
 * Copyright (c) 2012 by UT-Battelle, LLC.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 
 * Collection of administrative costs for redistribution of the source code or
 * binary form is allowed. However, collection of a royalty or other fee in excess
 * of good faith amount for cost recovery for such redistribution is prohibited.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER, THE DOE, OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
======================================================================*/
package ornl.elision.core

import scala.compat.Platform
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ Set => MSet }
import scala.collection.mutable.Stack
import scala.collection.mutable.Queue
import scala.tools.nsc.interpreter.Results

import ornl.elision.context.NativeCompiler
import ornl.elision.context.Executor
import ornl.elision.core.matcher.SequenceMatcher
import ornl.elision.util.ElisionException
import ornl.elision.util.OmitSeq
import ornl.elision.util.Debugger
import ornl.elision.util.Loc
import ornl.elision.util.FastLinkedList

/**
 * An incorrect argument list was supplied to an operator.
 *
 * @param loc The locatin of the bad application.
 * @param msg	The human-readable message describing the problem.
 */
class ArgumentListException(loc: Loc, msg: String)
  extends ElisionException(loc, msg)

/**
 * The native handler could not be parsed.
 *
 * @param loc Location of the bad operator or native handler.
 * @param msg	The human-readable message describing the problem.
 */
class NativeHandlerException(loc: Loc, msg: String)
  extends ElisionException(loc, msg)

/**
 * A little class to use to pass data back and forth from the subordinate
 * interpreter.  Since a native handler must return an atom, this is the
 * return value of the passed closure.
 *
 * A native handler is parsed by a subordinate Scala interpreter.  This has
 * to bind something available in *this* scope - specifically it binds up
 * an instance of `HandHolder` and passes it back.  Initially this holds
 * `None`, but if the handler can be parsed it returns `Some` closure.
 *
 * @param handler		The handler.
 */
class HandHolder(
  var handler: Option[ApplyData => BasicAtom])

/**
 * Data block and special functions provided to a native handler.  A native
 * handler takes an instance of this class and hands back an atom.
 *
 * Certain information is populated based on the current __implicit__
 * `Executor` instance.  This is done at construction time.
 *
 * @param op			The operator.
 * @param args		The argument list.
 * @param binds		Bindings of parameter to argument value.
 * @param exec		An executor.
 */
class ApplyData(val op: SymbolicOperator, val args: AtomSeq,
  val binds: Bindings)(implicit val exec: Executor) {
  /** Provide fast access to the context from the executor. */
  val context = exec.context

  /** Provide fast access to the console from the executor. */
  val console = exec.console

  /** Just preserve the apply as it is. */
  def as_is = Apply(op, args, true)
}

/**
 * Additional constants and support methods for native handlers.
 */
object ApplyData {
  /** A special literal that we never show, or save as a binding. */
  val _no_show = Literal(Symbol(" NO SHOW "))
}

/**
 * Encapsulate an operator.
 *
 * This is the common base class for all operator classes.
 *
 * == Purpose ==
 * An operator, by itself, is simply a function that maps from some
 * domain to some codomain.
 *
 * == Use ==
 * The companion object provides methods to create operators.
 *
 * @param name				The operator name.
 * @param typ					The type of the fully-applied operator.
 * @param definition	A definition for the operator.
 * @param description	An optional short description for the operator.
 * @param detail			Optional detailed help for the operator.
 * @param evenMeta		Apply this operator even to meta-terms.  This is false
 * 										by default, and you should probably leave it alone.
 */
abstract class Operator(
  sfh: SpecialFormHolder,
  val name: String,
  val typ: BasicAtom,
  definition: AtomSeq,
  val description: String,
  val detail: String,
  override val evenMeta: Boolean = false)
  extends SpecialForm(sfh.loc, sfh.tag, sfh.content) with Applicable {
  /**
   * Apply the operator to the given sequence of basic atoms as arguments.
   *
   * @param atoms   The arguments, in order.
   * @return  The constructed atom.
   */
  def apply(atoms: BasicAtom*): BasicAtom

  /**
   * Proxy to the parent match method, but turn this operator into an operator
   * reference in the process.
   */
  override def tryMatchWithoutTypes(
    subject: BasicAtom, binds: Bindings, hints: Option[Any]) =
    super.tryMatchWithoutTypes(subject, binds, Some(OperatorRef(this)))
}

/**
 * Provide construction and matching for operators.
 */
object Operator {
  /** Tag for this special form. */
  val tag = Literal('operator)

  /**
   * Construct an operator from the provided special form data.
   *
   * @param sfh		The parsed special form data.
   * @return	An operator.
   */
  def apply(sfh: SpecialFormHolder): Operator = {
    val bh = sfh.requireBindings
    bh.check(Map("name" -> true, "cases" -> false, "params" -> false,
      "type" -> false, "description" -> false, "detail" -> false,
      "evenmeta" -> false, "handler" -> false))
    if (bh.either("cases", "params") == "cases") {
      CaseOperator(sfh)
    } else {
      TypedSymbolicOperator(sfh)
    }
  }

  /**
   * Extract the parts of an operator.
   *
   * @param op		The operator.
   * @return	The triple of name, type, and definition.
   */
  def unapply(op: Operator) = op match {
    case so: SymbolicOperator => Some((so.name, so.theType, so.params))
    case co: CaseOperator => Some((co.name, co.theType, co.cases))
  }
}

/**
 * Encapsulate a reference to an operator.
 *
 * == Purpose ==
 * Operators are just atoms, so they can be matched and rewritten.  This is
 * not always desirable; we want the operator to remain fixed.  This class
 * provides a level of indirection.
 *
 * @param operator	The referenced operator.
 */
class OperatorRef(val operator: Operator) extends BasicAtom with Applicable {
  val depth = 0
  val deBruijnIndex = 0
  val isTerm = true
  val isConstant = true
  val theType = OPREF
  /** The operator name. */
  val name = operator.name

  /**
   * Apply the referenced operator to the given atom.
   *
   * @param atom		The atom.
   * @param bypass	Whether to bypass native handlers.
   * @return	The result of applying the referenced operator to the given atom.
   */
  def doApply(atom: BasicAtom, bypass: Boolean) = operator.doApply(atom, bypass)

  /**
   * Operator references cannot be rewritten.  This is actually why they exist!
   */
  def rewrite(binds: Bindings) = (this, false)

  def replace(map: Map[BasicAtom, BasicAtom]) = map.get(this) match {
    case None => (this, false)
    case Some(atom) => (atom, true)
  }

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
    hints: Option[Any]) =
    if (subject == this) {
      Match(binds)
    } else subject match {
      case OperatorRef(oop) if (oop == operator) => Match(binds)
      case oop: Operator if (oop == operator) => Match(binds)
      case _ => Fail("Operator reference does not match subject.", this, subject)
    }

  /**
   * Apply the referenced operator to a sequence of atoms.
   *
   * @param atoms		The arguments.
   * @return	The result of applying the referenced operator to the sequence
   * 					of atoms.
   */
  def apply(atoms: BasicAtom*) = operator(atoms: _*)

  /**
   * Operator references are equal iff the referenced operators are equal.
   */
  override def equals(other: Any) = other match {
    case OperatorRef(oop) if (oop == operator) => true
    case _ => false
  }

  override lazy val hashCode = 12289 * operator.hashCode
  override lazy val otherHashCode = 8191 * operator.otherHashCode
}

/**
 * Make and match operator references.
 */
object OperatorRef {

  /**
   * Extract the operator from the reference.
   *
   * @param ref		The operator reference.
   * @return	The referenced operator.
   */
  def unapply(ref: OperatorRef) = Some(ref.operator)

  /**
   * Make a reference for an operator.
   *
   * @param op	The operator.
   * @return	A reference to the operator.
   */
  def apply(op: Operator) = new OperatorRef(op) {
    override val evenMeta = op.evenMeta
  }
}

/**
 * Construction and matching of macros (case operators).
 */
object CaseOperator {
  /**
   * Make a case operator from the given special form data.
   *
   * @param sfh	The special form data.
   * @return	The case operator.
   */
  def apply(sfh: SpecialFormHolder): CaseOperator = {
    val bh = sfh.requireBindings
    bh.check(Map("name" -> true, "cases" -> true, "type" -> false,
      "description" -> false, "detail" -> false, "evenmeta" -> false))
    val name = bh.fetchAs[SymbolLiteral]("name").value.name
    val cases = bh.fetchAs[AtomSeq]("cases")
    val typ = bh.fetchAs[BasicAtom]("type", Some(ANY))
    var description = bh.fetchAs[StringLiteral]("description", Some("No description."))
    if (description.value(0) == '|') description = description.value.stripMargin('|')
    val detail = bh.fetchAs[StringLiteral]("detail", Some("No detail."))
    val evenMeta = bh.fetchAs[BooleanLiteral]("evenmeta", Some(false)).value
    return new CaseOperator(sfh, name, typ, cases, description, detail, evenMeta)
  }

  /**
   * Make a case operator from the components.
   *
   * @param loc           Location of the definition of this operator.
   * @param name					Operator name.
   * @param typ						The operator type (may be `ANY`).
   * @param cases					The cases, as a sequence of atoms.
   * @param description		An optional short description for the operator.
   * @param detail				Optional detailed help for the operator.
   * @param evenMeta			Apply this operator even when the arguments contain
   * 											meta-terms.  This is not advisable, and you should
   * 											probably leave this with the default value of false.
   * @return	The new case operator.
   */
  def apply(loc: Loc, name: String, typ: BasicAtom, cases: AtomSeq,
    description: String, detail: String,
    evenMeta: Boolean = false): CaseOperator = {
    val nameS = Literal(Symbol(name))
    val binds = Bindings() + ("name" -> nameS) + ("cases" -> cases) +
      ("type" -> typ) + ("description" -> Literal(description)) +
      ("detail" -> Literal(detail))
    val sfh = new SpecialFormHolder(loc, Operator.tag, binds)

    return new CaseOperator(sfh, name, typ, cases, description, detail, evenMeta)
  }

  /**
   * Extract the parts of a case operator.
   *
   * @param co	The case operator.
   * @return	A triple of the name, type, and cases.
   */
  def unapply(co: CaseOperator) = Some((co.name, co.theType, co.cases,
    co.description, co.detail, co.evenMeta))
}

/**
 * Encapsulate a case operator.
 *
 * == Purpose ==
 * A case operator is actually a kind of macro.  Its definition consists of
 * a sequence of atoms.  When applied to some atom ''A'', it proceeds as
 * follows, considering each atom in its definition, in order.
 *  - If the atom is a rewriter, apply it and, if the success flag is true,
 *    the value is the result.  Otherwise continue.
 *  - If the atom is an applicable, apply it.  The value is the result of
 *    the application.
 *  - If the atom is neither a rewriter nor an applicable, just return that
 *    atom as the result.
 * If the end of the list is reached and no value is determined, then an
 * error is generated (an `ArgumentListException`).
 *
 * @param sfh						Special form data.
 * @param name					The operator name.
 * @param typ						The operator type.
 * @param cases					The definition.
 * @param description		An optional short description for the operator.
 * @param detail				Optional detailed help for the operator.
 * @param evenMeta			Apply this operator even when the arguments contain
 * 											meta-terms.  This is not advisable, and you should
 * 											probably leave this with the default value of false.
 */
class CaseOperator private (sfh: SpecialFormHolder,
  name: String, typ: BasicAtom, val cases: AtomSeq,
  description: String, detail: String, evenMeta: Boolean)
  extends Operator(sfh, name, typ, cases, description, detail, evenMeta) {
  /** The type of the operator is the provided type. */
  override val theType = typ

  /**
   * Apply this operator to the given arguments.
   *
   * @param atoms		The arguments.
   * @return	The result of applying this operator to the given argument list.
   */
  def apply(atoms: BasicAtom*) =
    doApply(AtomSeq(NoProps, atoms.toIndexedSeq))

  def doApply(args: BasicAtom, bypass: Boolean) = {
    // Traverse the list of cases and try to find a case that the arguments
    // match.  Every case should be a rewritable, an applicable, or an atom.
    // If a rewritable, apply it and if it succeeds, choose the result.
    // If an applicable, apply it.  If any other atom, choose that atom.
    var result: Option[BasicAtom] = None
    val done = cases.exists {
      _ match {
        case rew: Rewriter =>
          val pair = rew.doRewrite(args, Some(this))
          result = Some(pair._1)
          pair._2
        case app: Applicable =>
          result = Some(app.doApply(args, bypass))
          true
        case atom =>
          result = Some(atom)
          true
      }
    }
    // If nothing worked, then we need to generate an error since the operator
    // was incorrectly applied.
    if (!done)
      throw new ArgumentListException(args.loc, "Applied the operator " +
        toESymbol(name) + " to an incorrect argument list: " +
        args.toParseString)
    // If the result turned out to be ANY, then just construct a simple
    // apply for this operator.
    result.get match {
      case ANY => args match {
        case as: AtomSeq => OpApply(OperatorRef(this), as, Bindings())
        case _ => SimpleApply(OperatorRef(this), args)
      }
      case other =>
        // We have to do one more thing.  We need to bind $__ to this operator,
        // and $_ to the original argument list, and then rewrite the result.
        val binds = Bindings("_" -> args, "__" -> this)
        other.rewrite(binds)._1
    }
  }
}

/**
 * Package information about the application of an operator to an argument
 * list.  This is used to pass information to a native handler.
 *
 * @param op			The operator.
 * @param args		The argument list.
 * @param binds		Bindings of parameter to argument.
 */
class ApplyInfo(val op: SymbolicOperator, val args: AtomSeq,
  val binds: Bindings)

/**
 * Construction and matching of typed symbolic operators.
 */
object TypedSymbolicOperator {
  /**
   * Make a typed symbolic operator from the provided special form data.
   *
   * @param sfh		The parsed special form data.
   * @return	The typed symbolic operator.
   */
  def apply(sfh: SpecialFormHolder): TypedSymbolicOperator = {
    val bh = sfh.requireBindings
    bh.check(Map("name" -> true, "params" -> true, "type" -> false,
      "description" -> false, "detail" -> false, "evenmeta" -> false,
      "handler" -> false))
    val name = bh.fetchAs[SymbolLiteral]("name").value.name
    val params = bh.fetchAs[AtomSeq]("params")
    val typ = bh.fetchAs[BasicAtom]("type", Some(ANY))
    var description = bh.fetchAs[StringLiteral]("description", Some("No description."))
    if (description.length > 0 && description.value(0) == '|')
      description = description.value.stripMargin('|')
    val detail = bh.fetchAs[StringLiteral]("detail", Some("No detail."))
    val evenMeta = bh.fetchAs[BooleanLiteral]("evenmeta", Some(false)).value

    // Fetch the handler text, if any was provided.
    val handlertxt = bh.fetchAs[StringLiteral]("handler", Some(null)) match {
      case null => None
      case x: StringLiteral => Some(x.value)
    }

    // Now create the operator.
    new TypedSymbolicOperator(sfh, name, typ, params,
      description, detail, evenMeta, handlertxt)
  }

  /**
   * Make a typed symbolic operator from the provided parts.
   *
   * @param loc           Location of the definition of this operator.
   * @param name					The operator name.
   * @param typ						The type of the fully-applied operator.
   * @param params				The operator parameters.
   * @param description		An optional short description for the operator.
   * @param detail				Optional detailed help for the operator.
   * @param evenMeta			Apply this operator even when the arguments contain
   * 											meta-terms.  This is not advisable, and you should
   * 											probably leave this with the default value of false.
   * @param handler       Optional native handler code.  Default is `None`.
   * @return	The typed symbolic operator.
   */
  def apply(loc: Loc, name: String, typ: BasicAtom, params: AtomSeq,
    description: String, ddetail: String, evenMeta: Boolean = false,
    handler: Option[String] = None): TypedSymbolicOperator = {
    val detail = ddetail
    val nameS = Literal(Symbol(name))
    var binds = Bindings() + ("name" -> nameS) + ("params" -> params) +
      ("type" -> typ) + ("description" -> Literal(description)) +
      ("detail" -> Literal(detail)) + ("evenmeta" -> Literal(evenMeta))
    handler match {
      case None =>
      case Some(text) => binds += ("handler" -> Literal(text))
    }
    val sfh = new SpecialFormHolder(loc, Operator.tag, binds)
    return new TypedSymbolicOperator(sfh, name, typ, params,
      description, detail, evenMeta, handler)
  }

  /**
   * Extract the parts of a typed symbolic operator.
   *
   * @param so	The operator.
   * @return	The triple of name, computed type, and parameters.
   */
  def unapply(so: TypedSymbolicOperator) =
    Some((so.name, so.typ, so.params, so.description, so.detail,
      so.evenMeta, so.handlertxt))
}

/**
 * Encapsulate a typed symbolic operator.
 *
 * == Purpose ==
 * A ''typed'' symbolic operator computes its type based on the types of its
 * parameters and the provided "fully applied" type.  The result has the form
 * of a mapping from a domain to a co-domain.
 *
 * @param sfh         The parsed special form data.
 * @param name        The operator name.
 * @param typ         The type of the fully-applied operator.
 * @param params      The operator parameters.
 * @param evenMeta    Apply this operator even when the arguments contain
 *                    meta-terms.  This is not advisable, and you should
 *                    probably leave this with the default value of false.
 * @param handlertxt  The text for an optional native handler.
 */
class TypedSymbolicOperator private (sfh: SpecialFormHolder,
  name: String, typ: BasicAtom, params: AtomSeq,
  description: String, detail: String, evenMeta: Boolean,
  handlertxt: Option[String])
  extends SymbolicOperator(sfh, name, typ, params,
    description, detail, evenMeta, handlertxt) {
  /**
   * The type of an operator is a mapping from the operator domain to the
   * operator codomain.
   */
  override val theType = SymbolicOperator.makeOperatorType(params, typ)
}

/**
 * Construction and matching of symbolic operators.
 */
object SymbolicOperator {
  // Get the path separator.
  private val _prop = new scala.sys.SystemProperties
  private val _ps = _prop("path.separator")

  // Get the current class path and convert it into a proper path expression.
  private val _urls =
    java.lang.Thread.currentThread.getContextClassLoader match {
      case cl: java.net.URLClassLoader => cl.getURLs.toList
      case other => sys.error("classloader is not a URLClassLoader. " +
        "It is a " + other.getClass.getName)
    }
  private val _classpath = (_urls.map(_.getPath)).mkString(_ps)

  // Build a settings with the correct classpath.
  private val _settings = new scala.tools.nsc.Settings(knownExecutor.console.emitln _)
  _settings.classpath.value = _classpath

  /** Make an interpreter. */
  private val _main = new scala.tools.nsc.interpreter.IMain(_settings) {}

  // Make the core package available.  This is commented out and not simply
  // removed because it is part of a "to do" that must be revisited.
  // TODO See how Scala is now handling the classpath.
  //_main.beQuietDuring(_main.interpret("import ornl.elision.core._ //"))

  // Time the compilation of native handlers.
  import ornl.elision.util.Timeable
  private val _timer = new Timeable {
    timing = true
    def reportElapsed() = {}
  }
  private val _ncomp = new NativeCompiler

  /**
   * Compile Scala code to a native handler.
   *
   * @param op            The operator to get this native handler.
   * @param handlertxt    The optional text to compile.
   * @return  The optional handler result.
   */
  private def compileHandler(op: SymbolicOperator,
    code: Option[String]): Option[ApplyData => BasicAtom] = {

    // Fetch the handler text.
    if (code.isDefined) {
      var handlertxt = code.get.split("\n")
      def removeBlankLines(txt: Array[String]) =
        for (line <- txt if line.trim.length > 0) yield line
      //The compiler doesn't like blank lines, so we remove them.      
      handlertxt = removeBlankLines(handlertxt)
      // Concatenate all the lines into a single block of code. Each element
      // of the array represents a line of code, but the elements are not
      // terminated by a newline, so we insert them along with the concatenation.
      var handlerblock = ("" /: handlertxt)(_ + _ + "\n")
      if (handlerblock.length > 0 && handlerblock(0) == '|')
        handlerblock = handlerblock.stripMargin('|')

      // Compile the handler, if we were given one.
      if (handlerblock != "") {
        return Some(_timer.time {
          _ncomp.compile(op.loc, op.name, handlerblock)
        })
      }
    } // Handler has text.

    // If we get here then no handler code was provided, or the code was
    // empty.
    return None
  }

  /**
   * Print out the time spent compiling native handlers.
   */
  def reportTime() {
    knownExecutor.console.emit("Time Compiling Native Handlers: ")
    knownExecutor.console.emitln(
      Timeable.asTimeString(_timer.getCumulativeTimeMillis))
  }

  /**
   * Make a symbolic operator from the provided parts.
   *
   * @param loc           Location of the definition of this operator.
   * @param name					The operator name.
   * @param typ						The type of the fully-applied operator.
   * @param params				The operator parameters.
   * @param description		An optional short description for the operator.
   * @param detail				Optional detailed help for the operator.
   * @param evenMeta			Apply this operator even when the arguments contain
   * 											meta-terms.  This is not advisable, and you should
   * 											probably leave this with the default value of false.
   * @param handler       The text for an optional native handler.
   * @return	The typed symbolic operator.
   */
  def apply(loc: Loc, name: String, typ: BasicAtom, params: AtomSeq,
    description: String, ddetail: String, evenMeta: Boolean = false,
    handler: Option[String] = None): SymbolicOperator = {
    val detail = ddetail
    val nameS = Literal(Symbol(name))
    var binds = Bindings() + ("name" -> nameS) + ("params" -> params) +
      ("type" -> typ) + ("description" -> Literal(description)) +
      ("detail" -> Literal(detail)) + ("evenmeta" -> Literal(evenMeta))
    handler match {
      case None =>
      case Some(text) => binds += ("handler" -> Literal(text))
    }
    val sfh = new SpecialFormHolder(loc, Operator.tag, binds)
    return new SymbolicOperator(sfh, name, typ, params,
      description, detail, evenMeta, handler)
  }

  /**
   * Extract the parts of a symbolic operator.
   *
   * @param so	The operator.
   * @return	The triple of name, computed type, and parameters.
   */
  def unapply(so: SymbolicOperator) = Some((so.name, so.theType, so.params))

  /**
   * The well-known MAP operator.  This is needed to define the types of
   * operators, but is not used to define its own type.  The type of the MAP
   * operator is ^TYPE, indicating that it is a root type.  We could, with
   * great justice, use xx (the cross product) for this operator, but don't.
   * This makes the types of operators look more natural when viewed.
   */
  val MAP = OperatorRef(
    SymbolicOperator(Loc.internal, "MAP", TypeUniverse, AtomSeq(NoProps,
      'domain, 'codomain),
      "Mapping constructor.",
      "This operator is used to construct types for operators.  It " +
        "indicates a mapping from one type (the domain) to another type " +
        "(the codomain)."))

  /**
   * The well-known cross product operator.  This is needed to define the
   * types of operators, but is not used to define its own type.  The type
   * of the cross product is ANY.  Note that it must be ANY, since it is
   * associative.
   */
  val xx = OperatorRef(
    SymbolicOperator(Loc.internal, "xx", ANY, AtomSeq(Associative(true), 'x, 'y),
      "Cross product.",
      "This operator is used to construct types for operators.  It " +
        "indicates the cross product of two atoms (typically types).  " +
        "These originate from the types of the parameters of an operator."))

  /**
   * The well-known list operator.  This is used to define the type of lists
   * such as the atom sequence.  It has type ^TYPE, indicating that it is a
   * root type.
   */
  val LIST = OperatorRef(
    SymbolicOperator(Loc.internal, "LIST", TypeUniverse, AtomSeq(NoProps, 'type),
      "List type constructor.",
      "This operator is used to indicate the type of a list.  It takes a " +
        "single argument that is the type of the atoms in the list.  For " +
        "heterogeneous lists this will be ANY."))

  /**
   * Compute an operator type.
   *
   * @param params	The parameters.
   * @param typ			The type of the fully-applied operator.
   * @return	The type for the operator.
   */
  def makeOperatorType(params: AtomSeq, typ: BasicAtom) =
    params.length match {
      case 0 => MAP(NONE, typ)
      case 1 => MAP(params(0).theType, typ)
      case _ => MAP(xx(params.map(_.theType): _*), typ)
    }
}

/**
 * Encapsulate a symbolic operator.
 *
 * == Purpose ==
 * An (untyped) symbolic operator is a rudimentary form of operator used only
 * for special "primitive" operators that are themselves used to specify the
 * types of operators.
 *
 * @param sfh         The parsed special form data.
 * @param name        The operator name.
 * @param typ         The type of the fully-applied operator.
 * @param params		  The operator parameters.
 * @param evenMeta    Apply this operator even when the arguments contain
 *                    meta-terms.  This is not advisable, and you should
 *                    probably leave this with the default value of false.
 * @param handlertxt  The text for an optional native handler.
 */
protected class SymbolicOperator protected (
  sfh: SpecialFormHolder,
  name: String,
  typ: BasicAtom,
  val params: AtomSeq,
  description: String,
  detail: String,
  evenMeta: Boolean,
  val handlertxt: Option[String])
  extends Operator(sfh, name, typ, params, description, detail, evenMeta) {

  override val theType: BasicAtom = ANY

  // Check the properties.
  _check()

  /** The native handler, if any. */
  val handler = SymbolicOperator.compileHandler(this, handlertxt)

  /**
   * Apply this operator to the given arguments.
   *
   * @param atoms		The arguments.
   * @return	The result of applying this operator to the given argument list.
   */
  def apply(args: BasicAtom*): BasicAtom = {
    // Make an atom list from the arguments.
    val seq = AtomSeq(NoProps, args: _*)
    doApply(seq, false)
  }

  /**
   * Check the parameters against the properties.  If any problems are detected,
   * then an exception is thrown (`ArgumentListException`).
   */
  private def _check() {

    /**
     * Define a little method to require that all parameters have the same
     * type.
     *
     * @return	True if all parameters have the same type, and false if not.
     */
    def paramTypeCheck = {
      val aType = params(0).theType
      params.forall(_.theType == aType)
    }

    // Check the properties and make sure everything is in accordance with
    // them.
    if (params.props.isA(false)) {
      // There must be exactly two parameters.
      if (params.length != 2) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " is marked as associative, but does not have exactly two " +
          "parameters, as required: " + params.toParseString)
      }
      // All parameter types must be the same.
      if (!paramTypeCheck) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " is marked as associative, but all parameters do not have the " +
          "same type, as required: " + params.toParseString)
      }
      // The fully-applied type must be the same as the parameter type.
      if (params(0).theType != typ) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " is marked as associative, but the parameter type (" +
          params(0).theType.toParseString +
          ") is not the same as the fully-applied type (" +
          typ.toParseString + ").")
      }
    } else {
      // The operator is not associative, so it must not have an identity,
      // absorber, or be idempotent.
      if (params.props.isI(false)) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " is marked as idempotent, but it not marked as associative, as" +
          " required.")
      }
      if (params.props.identity.isDefined) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " is declared to have an identity, but it not marked as " +
          "associative, as required.")
      }
      if (params.props.absorber.isDefined) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " is declared to have an absorber, but it not marked as " +
          "associative, as required.")
      }
    }
    if (params.props.isC(false)) {
      // There must be at least two parameters.
      if (params.length < 2) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " is marked as commutative, but does not have at least two " +
          "parameters, as required: " + params.toParseString)
      }
      // All parameter types must be the same.
      if (!paramTypeCheck) {
        throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
          " is marked as commutative, but all parameters do not have the " +
          "same type, as required: " + params.toParseString)
      }
    }
    // Any identity must match the parameter type.  We just try to match
    // the first parameter's type.
    if (params.props.identity.isDefined) {
      params(0).theType.tryMatch(params.props.identity.get.theType) match {
        case Fail(reason, index) =>
          throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
            " has an identity whose type (" +
            params.props.identity.get.theType.toParseString +
            ") does not match the parameter type (" +
            params(0).theType.toParseString + ").")
        case _ =>
      }
    }
    // Any absorber must match the parameter type.  We just try to match
    // the first parameter's type.
    if (params.props.absorber.isDefined) {
      params(0).theType.tryMatch(params.props.absorber.get.theType) match {
        case Fail(reason, index) =>
          throw new ArgumentListException(loc, "The operator " + toESymbol(name) +
            " has an absorber whose type (" +
            params.props.absorber.get.theType.toParseString +
            ") does not match the parameter type (" +
            params(0).theType.toParseString + ").")
        case _ =>
      }
    }
  }

  /**
   * All symbolic operator applications arrive here to get resolved.
   *
   * @param rhs     The right-hand side (arguments).
   * @param bypass  If true, bypass any native handler.
   * @return  The constructed atom.
   */
  def doApply(rhs: BasicAtom, bypass: Boolean): BasicAtom = {
    // Temporarily disable rewrite timeouts if already timed out.
    val oldTimeout = BasicAtom.timeoutTime.value
    if (BasicAtom.rewriteTimedOut) {
      BasicAtom.timeoutTime.value = -1L
    } else {
      BasicAtom.timeoutTime.value = Platform.currentTime + 10 * 1000
    }

    rhs match {
      case args: AtomSeq =>
        // Things have to happen in the correct order here.  First increase
        // the argument list by flattening associative applications.  Second
        // we reduce by looking for identities, etc.  Third we check for an
        // empty argument list.

        // Save the properties for fast access.
        val props = params.props
        val assoc = props.isA(false)
        val ident = props.identity.getOrElse(null)

        // The args AtomSeq may have the same properties as the operator.
        // If it does, we use it as-is, otherwise, apply the operator's
        // properties
        val diffProps = (props != args.props)
        val newargs = if (diffProps) AtomSeq(props, args.atoms) else args
        val opcount = newargs.operatorCount(this)
        var newseq = newargs.atoms

        // Flatten associative lists if it contains this operator.
        if (assoc && opcount > 0) {
          // While loops are significantly faster than for comprehensions.
          // We are searching this Operator's arguments to find more instances
          // of this Operator to flatten out. We check how many we've found vs.
          // how many we know are in the sequence. Once we've found them all we
          // can cease looping.
          var index = 0
          var opsfound = 0
          while (opsfound < opcount && index < newseq.size) {
            val atom = newseq(index)
            atom match {
              case OpApply(opref, opargs, _) if (opref.operator.name == this.name) =>
                // Add the arguments directly to this list.  We can assume the
                // sub-list has already been processed, so no deeper checking
                // is needed.  This flattens associative lists, as required.
                newseq = newseq.omit(index)
                newseq = newseq.insert(index, opargs)
                opsfound += 1
              case _ =>
                // Nothing to do except increment the pointer.
                index += 1
            }
          }
          // We flattened the list, adding new arguments, so if this is
          // commutative we need to sort
          if (props.isC(false)) newseq = newseq.sorted(BasicAtomComparator)
        }

        // Handle actual operator application.
        def handleApply(binds: Bindings): BasicAtom = {
          // Re-package the arguments with the correct properties.
          val newargs = AtomSeq(props, newseq)
          // See if we are bypassing the native handler.
          var r: BasicAtom = null
          if (!bypass) {
            // Run any native handler.            
            if (handler.isDefined) {
              val ad = new ApplyData(this, newargs, binds)
              r = handler.get(ad)
              return r
            }
          }
          // No native handler.
          r = OpApply(OperatorRef(this), newargs, binds)
          return r
        }

        // Check the argument length versus the parameter length.
        if (!assoc) {
          // The number of arguments must exactly match the number of
          // parameters.
          if (newseq.length > params.length) {
            throw new ArgumentListException(rhs.loc,
              "Too many arguments for non-associative operator " +
                toESymbol(name) + ".  Expected " + params.length +
                " but got " + newseq.length + ".")
          } else if (newseq.length < params.length) {
            throw new ArgumentListException(rhs.loc,
              "Too few arguments for non-associative operator " +
                toESymbol(name) + ".  Expected " + params.length +
                " but got " + newseq.length + ".")
          }
        } else {
          // There are special cases to handle here.  First, if the argument
          // list is empty, but there is an identity, return it.  Second, if
          // the argument list is empty, but there is no identity, apply the
          // operator to the empty list.
          if (newseq.length == 0) {
            if (ident == null) {
              val r = handleApply(Bindings())

              // Resume timing out rewrites.
              BasicAtom.timeoutTime.value = oldTimeout
              return r
            } else {
              // Resume timing out rewrites.
              BasicAtom.timeoutTime.value = oldTimeout
              return ident
            }
          }
        }

        // If the argument list is associative, we have an identity, and we
        // have a single element, then that element must match the type of
        // the operator, and we return it.  Why is this the rule?  We want
        // to use associative operators to mimic "var args", but don't want
        // them to "collapse" when there is just one argument.  That is, we
        // don't want f(x)->x when we just want a var args f.  But if we give
        // f an identity, it is probably a mathematical operator of some kind,
        // and we probably do want f(x)->x.  So, for now, that's the rule.
        // For greater control, you have to use a case operator.
        if (newseq.length == 1) {
          if (assoc && ident != null) {
            // Get the atom.
            val atom = newseq(0)
            // Match the type of the atom against the type of the parameters.
            val param = params(0)
            param.tryMatch(atom) match {
              case Fail(reason, index) =>
                // The argument is invalid.  Reject!
                throw new ArgumentListException(atom.loc, "Incorrect argument " +
                  "for operator " + toESymbol(name) + " at position 0: " +
                  atom.toParseString + ".  " + reason())
              case mat: Match => {
                // Resume timing out rewrites.
                BasicAtom.timeoutTime.value = oldTimeout

                // The argument matches.
                return atom
              }
              case many: Many => {
                // Resume timing out rewrites.
                BasicAtom.timeoutTime.value = oldTimeout

                // The argument matches.
                return atom
              }
            }
          }
        }

        // Is the current operator associative?
        if (assoc) {
          // Handle type checking of an associative operator. All
          // formal parameters of an associative operator must have
          // the same type, so type checking of an associative
          // operator will be performed by checking:
          //
          // 1. That all arguments of the operator we are trying to
          //    create have the same type.
          // 2. That the type of 1 of the arguments matches the type
          //    of 1 of the formal parameters of the associative
          //    operator.

          // Check to see if all arguments have the same type.
          val anArg = newseq(0)
          /*
        if (!args.sameType) {
          throw new ArgumentListException(anArg.loc,
                                          "Not all arguments for associative operator have the same type.")
        }
        */

          // All arguments have the same type. Now try to match the
          // parameter type with the argument type. Note that the
          // bindings returned by the match are only used for
          // inferring the value of type variables. Since all
          // arguments/formal parameters have the same type, matching
          // 1 formal parameter with 1 argument gives us all the
          // binding information needed to do type inference.
          val aParam = params.atoms(0)
          aParam.tryMatch(anArg) match {
            case Fail(reason, index) =>
              throw new ArgumentListException(anArg.loc,
                "Incorrect argument for operator " + toESymbol(name) +
                  " at position " + index + ": " + newseq(index).toParseString +
                  ".  " + reason())
            case Match(binds1) => {
              // The argument matches.
              val r = handleApply(binds1)

              // Resume timing out rewrites.
              BasicAtom.timeoutTime.value = oldTimeout
              return r
            }
            case Many(iter) => {
              // The argument matches.
              val r = handleApply(iter.next)

              // Resume timing out rewrites.
              BasicAtom.timeoutTime.value = oldTimeout
              return r
            }
          }
        } else {
          // We've run out of special cases to handle.  Now just try to match
          // the arguments against the parameters.
          val newparams = params.atoms
          SequenceMatcher.tryMatch(newparams, newseq) match {
            case Fail(reason, index) =>
              throw new ArgumentListException(newseq(index).loc,
                "Incorrect argument for operator " + toESymbol(name) +
                  " at position " + index + ": " + newseq(index).toParseString +
                  ".  " + reason())
            case Match(binds1) => {
              // The argument list matches.
              val r = handleApply(binds1)

              // Resume timing out rewrites.
              BasicAtom.timeoutTime.value = oldTimeout
              return r
            }
            case Many(iter) => {
              // The argument list matches.
              val r = handleApply(iter.next)

              // Resume timing out rewrites.
              BasicAtom.timeoutTime.value = oldTimeout
              return r
            }
          }
        }

      case _ => {
        val r = SimpleApply(this, rhs)

        // Resume timing out rewrites.
        BasicAtom.timeoutTime.value = oldTimeout
        return r
      }
    }
  }
}
