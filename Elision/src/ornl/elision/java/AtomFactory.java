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
package ornl.elision.java;

import java.util.Set;

import ornl.elision.core.ANY$;
import ornl.elision.core.AlgProp;
import ornl.elision.core.Apply;
import ornl.elision.core.Associative;
import ornl.elision.core.AtomSeq;
import ornl.elision.core.BOOLEAN$;
import ornl.elision.core.BasicAtom;
import ornl.elision.core.BindingsAtom;
import ornl.elision.core.BooleanLiteral;
import ornl.elision.core.Commutative;
import ornl.elision.core.Context;
import ornl.elision.core.Executor;
import ornl.elision.core.FLOAT$;
import ornl.elision.core.FloatLiteral;
import ornl.elision.core.INTEGER$;
import ornl.elision.core.Idempotent;
import ornl.elision.core.IntegerLiteral;
import ornl.elision.core.Lambda;
import ornl.elision.core.Literal;
import ornl.elision.core.MapPair;
import ornl.elision.core.MetaVariable;
import ornl.elision.core.NONE$;
import ornl.elision.core.NoProps$;
import ornl.elision.core.OperatorRef;
import ornl.elision.core.RewriteRule;
import ornl.elision.core.STRING$;
import ornl.elision.core.SYMBOL$;
import ornl.elision.core.SpecialForm;
import ornl.elision.core.StringLiteral;
import ornl.elision.core.SymbolLiteral;
import ornl.elision.core.TypeUniverse$;
import ornl.elision.core.UndefinedOperatorException;
import ornl.elision.core.Variable;
import scala.Symbol;
import scala.collection.immutable.HashSet;

/**
 * The atom factory provides simple methods to invoke from Java to create atoms.
 * 
 * This class follows the singleton pattern, and provides static methods for
 * most operations. It can be instantiated, and in this case it provides its own
 * default implementation.
 * 
 * To use this class, invoke the methods. The atoms created are
 * [[ornl.elision.core.BasicAtom]] instances.
 * 
 * Methods that require access to the context or executor have instance
 * implementations.  The others have static-only implementations, as they
 * do not require access to the context or executor.
 * 
 * <b>Caution!</b>  The factory will use the default executor discovered
 * via the {@code knownExecutor} method unless you explicitly configure
 * it via the {@code configure} method before first use.  This should be
 * the correct thing to do in most cases, but if you cannot find operators
 * and rules, you may need to override this by explicitly invoking the
 * {@code configure} method.
 */
public class AtomFactory {

	// ======================================================================
	// Instance data.
	// ======================================================================
	
	/** The executor. */
	protected Executor executor = null;
	
	/** The context. */
	protected Context context = null;
	
	/**
	 * Make a new instance with the specified executor.
	 * 
	 * @param exec	The executor.
	 */
	public AtomFactory(Executor exec) {
		if (exec == null) {
			throw new NullPointerException("The executor is null.");
		}
		executor = exec;
		context = exec.context();
	}

	// ======================================================================
	// Singleton methods.
	// ======================================================================

	/** The factory instance. */
	private static AtomFactory _instance = null;
	
	/**
	 * Configure the default instance of the atom factory.  This must be
	 * done prior to using any methods that require access to a context or
	 * the executor, or seriously bad things may happen.
	 * 
	 * @param exec	The executor.
	 * @return	The configured default instance.
	 */
	public static AtomFactory configure(Executor exec) {
		_instance = new AtomFactory(exec);
		return _instance;
	}

	/**
	 * Set the atom factory instance to use.
	 * 
	 * @param factory
	 *            The instance to use.
	 * @return The factory instance.
	 */
	public static AtomFactory setInstance(AtomFactory factory) {
		if (factory == null) {
			throw new NullPointerException("The factory instance is null.");
		}
		_instance = factory;
		return factory;
	}

	/**
	 * Get the atom factory instance to use. If none has been set via the
	 * {@code setInstance} method, one is created now.
	 * 
	 * @return The factory instance.
	 */
	public static AtomFactory getInstance() {
		if (_instance == null) {
			// Go and get the default executor.
			_instance =
					new AtomFactory(ornl.elision.core.package$.MODULE$.knownExecutor());
		}
		return _instance;
	}

	// ======================================================================
	// The arbitrary build.
	// ======================================================================
	
	/**
	 * An error was detected during parse.
	 */
	public static class ParseException extends Exception {
		private static final long serialVersionUID = 1357425959156174265L;

		/**
		 * Make a new instance.
		 * 
		 * @param msg	Human-readable message.
		 */
		public ParseException(String msg) {
			super(msg);
		}
	}
	
	/**
	 * Parse the given Elision source and return the sequence of atoms that
	 * results.
	 * 
	 * @param source	The string to parse.
	 * @return	The resulting collection of atoms.
	 * @throws	ParseException
	 * 			An error was detected in parsing.
	 */
	public java.util.Collection<BasicAtom> buildImpl(String source)
			throws ParseException {
		Executor.ParseResult pr = executor.parse(source);
		if (pr instanceof Executor.ParseFailure) {
			throw new ParseException(((Executor.ParseFailure)pr).err());
		}
		Executor.ParseSuccess success = (Executor.ParseSuccess) pr;
		return scala.collection.JavaConversions.asJavaCollection(success.nodes());
	}
	
	/**
	 * Parse the given Elision source and return the sequence of atoms that
	 * results.
	 * 
	 * @param source	The string to parse.
	 * @return	The resulting collection of atoms.
	 * @throws	ParseException
	 * 			An error was detected in parsing.
	 */
	public static java.util.Collection<BasicAtom> build(String source)
			throws ParseException {
		return getInstance().buildImpl(source);
	}
	
	// ======================================================================
	// Known constants.
	// ======================================================================
	
	/** The symbol NONE. */
	public static SymbolLiteral NONE = NONE$.MODULE$;
	/** The special constant ANY. */
	public static SymbolLiteral ANY = ANY$.MODULE$;
	/** The constant TRUE. */
	public static BooleanLiteral TRUE = Literal.TRUE();
	/** The constant FALSE. */
	public static BooleanLiteral FALSE = Literal.FALSE();
	
	/** The type STRING. */
	public static SymbolLiteral STRING = STRING$.MODULE$;
	/** The type SYMBOL. */
	public static SymbolLiteral SYMBOL = SYMBOL$.MODULE$;
	/** The type INTEGER. */
	public static SymbolLiteral INTEGER = INTEGER$.MODULE$;
	/** The type FLOAT. */
	public static SymbolLiteral FLOAT = FLOAT$.MODULE$;
	/** The type BOOLEAN. */
	public static SymbolLiteral BOOLEAN = BOOLEAN$.MODULE$;

	// ======================================================================
	// Literal creation methods.
	// ======================================================================

	/**
	 * Make a literal.
	 * @param value	The value.
	 * @param type	The type.
	 * @return	The new literal.
	 */
	public static StringLiteral lit(String value, BasicAtom type) {
		return Literal.apply(type, value);
	}
	
	/**
	 * Make a literal with default type.
	 * @param value	The value.
	 * @return	The new literal.
	 */
	public static StringLiteral lit(String value) {
		return Literal.apply(STRING, value);
	}

	/**
	 * Make a literal.
	 * @param value	The value.
	 * @param type	The type.
	 * @return	The new literal.
	 */
	public static SymbolLiteral lit(Symbol value, BasicAtom type) {
		return Literal.apply(type, value);
	}
	
	/**
	 * Make a literal with default type.
	 * @param value	The value.
	 * @return	The new literal.
	 */
	public static SymbolLiteral lit(Symbol value) {
		return Literal.apply(SYMBOL, value);
	}

	/**
	 * Make a literal.
	 * @param value	The value.
	 * @param type	The type.
	 * @return	The new literal.
	 */
	public static IntegerLiteral lit(int value, BasicAtom type) {
		return Literal.apply(type, value);
	}
	
	/**
	 * Make a literal with default type.
	 * @param value	The value.
	 * @return	The new literal.
	 */
	public static IntegerLiteral lit(int value) {
		return Literal.apply(INTEGER, value);
	}

	/**
	 * Make a literal.
	 * @param value	The value.
	 * @param type	The type.
	 * @return	The new literal.
	 */
	public static BooleanLiteral lit(boolean value, BasicAtom type) {
		return Literal.apply(type, value);
	}
	
	/**
	 * Make a literal with default type.
	 * @param value	The value.
	 * @return	The new literal.
	 */
	public static BooleanLiteral lit(boolean value) {
		return Literal.apply(BOOLEAN, value);
	}
	
	/**
	 * Make a literal with default type.
	 * @param significand	The significand.
	 * @param exponent		The exponent.
	 * @param radix			The radix.
	 * @return	The new literal.
	 */
	public static FloatLiteral lit(int significand, int exponent, int radix) {
		return Literal.apply(scala.math.BigInt.apply(significand), exponent, radix);
	}

	// ======================================================================
	// Variables.
	// ======================================================================
	
	/** Fast access to an empty set of strings. */
	private static HashSet<String> _emptyStringSet = new HashSet<String>();
	
	/**
	 * Make a new variable with the given name.
	 * @param name	The variable name.
	 * @return	The variable.
	 */
	public static Variable var(String name) {
		return Variable.apply(ANY, name, TRUE, _emptyStringSet);
	}
	
	/**
	 * Make a new metavariable with the given name.
	 * @param name	The variable name.
	 * @return	The variable.
	 */
	public static Variable mvar(String name) {
		return MetaVariable.apply(ANY, name, TRUE, _emptyStringSet);
	}

	/**
	 * Make a new variable with the given name and type.
	 * @param name	The variable name.
	 * @param type 	The type.
	 * @return	The variable.
	 */
	public static Variable var(String name, BasicAtom type) {
		return Variable.apply(type, name, TRUE, _emptyStringSet);
	}
	
	/**
	 * Make a new metavariable with the given name and type.
	 * @param name	The variable name.
	 * @param type 	The type.
	 * @return	The variable.
	 */
	public static Variable mvar(String name, BasicAtom type) {
		return MetaVariable.apply(type, name, TRUE, _emptyStringSet);
	}

	/**
	 * Make a new variable with the given name, type, and guard.
	 * @param name		The variable name.
	 * @param type 		The type.
	 * @param guard		The guard.
	 * @return	The variable.
	 */
	public static Variable var(String name, BasicAtom type, BasicAtom guard) {
		return Variable.apply(type, name, guard, _emptyStringSet);
	}
	
	/**
	 * Make a new metavariable with the given name, type, and guard.
	 * @param name		The variable name.
	 * @param type 		The type.
	 * @param guard		The guard.
	 * @return	The variable.
	 */
	public static Variable mvar(String name, BasicAtom type, BasicAtom guard) {
		return MetaVariable.apply(type, name, guard, _emptyStringSet);
	}

	/**
	 * Make a new variable with the given name, type, guard, and labels.
	 * @param name		The variable name.
	 * @param type 		The type.
	 * @param guard		The guard.
	 * @param labels	The labels.
	 * @return	The variable.
	 */
	public static Variable var(String name, BasicAtom type, BasicAtom guard,
			Set<String> labels) {
		scala.collection.immutable.Set<String> set =
				scala.collection.JavaConversions.asScalaSet(labels).toSet();
		return Variable.apply(type, name, guard, set);
	}
	
	/**
	 * Make a new metavariable with the given name, type, guard, and labels.
	 * @param name		The variable name.
	 * @param type 		The type.
	 * @param guard		The guard.
	 * @param labels	The labels.
	 * @return	The variable.
	 */
	public static Variable mvar(String name, BasicAtom type, BasicAtom guard,
			Set<String> labels) {
		scala.collection.immutable.Set<String> set =
				scala.collection.JavaConversions.asScalaSet(labels).toSet();
		return MetaVariable.apply(type, name, guard, set);
	}

	// ======================================================================
	// Lambdas.
	// ======================================================================
	
	/**
	 * Make a new lambda.
	 * @param parameter	The lambda parameter.
	 * @param body		The lambda body.
	 * @return	The new lambda.
	 */
	public static Lambda lam(Variable parameter, BasicAtom body) {
		return Lambda.apply(parameter, body);
	}
	
	// ======================================================================
	// Algebraic Properties and Collections.
	// ======================================================================
	
	/** No properties specified. */
	public static AlgProp noprops = NoProps$.MODULE$;
	
	/** The associative property. */
	public static AlgProp associative = new Associative(TRUE);
	
	/** The commutative property. */
	public static AlgProp commutative = new Commutative(TRUE);
	
	/** The idempotent property. */
	public static AlgProp idempotent = new Idempotent(TRUE);
	
	/**
	 * Conditional associativity.
	 * @param val	The condition.
	 * @return	The property.
	 */
	public static AlgProp associative(BasicAtom val) {
		return new Associative(val);
	}
	
	/**
	 * Conditional commutativity.
	 * @param val	The condition.
	 * @return	The property.
	 */
	public static AlgProp commutative(BasicAtom val) {
		return new Commutative(val);
	}
	
	/**
	 * Conditional idempotency.
	 * @param val	The condition.
	 * @return	The property.
	 */
	public static AlgProp idempotent(BasicAtom val) {
		return new Idempotent(val);
	}
	
	/**
	 * Build a composite from specified properties.
	 * @param properties	The properties to combine.
	 * @return	The resulting property.
	 */
	public static AlgProp properties(AlgProp ... properties) {
		// Join all the properties into a single algebraic property.
		AlgProp props = noprops;
		for (AlgProp prop : properties) {
			props = props.and(prop);
		} // Combine all properties.
		return props;
	}
	
	/**
	 * Make a collection with the specified properties and the atoms.
	 * @param props	The properties.
	 * @param atoms	The atoms.
	 * @return	The new collection.
	 */
	public static AtomSeq seq(AlgProp props, BasicAtom... atoms) {
		// Convert the atoms to an indexed sequence.
		scala.collection.mutable.WrappedArray.ofRef<BasicAtom> arr =
				new scala.collection.mutable.WrappedArray.ofRef<BasicAtom>(atoms);
		return new AtomSeq(props, arr);
	}

	// ======================================================================
	// Bindings.
	// ======================================================================
	
	/**
	 * Make a bindings atom with a single bind.
	 * 
	 * @param name	The name of the bound variable.
	 * @param value	The bound value.
	 * @return	The new atom.
	 */
	public static BindingsAtom bind(String name, BasicAtom value) {
		// Make a hash map and add the entry.
		scala.collection.immutable.HashMap<String,BasicAtom> map =
				new scala.collection.immutable.HashMap<String,BasicAtom>();
		map = map.$plus(new scala.Tuple2<String,BasicAtom>(name, value));
		return BindingsAtom.apply(map);
	}
	
	/**
	 * Convert a hash map into a binding atom.
	 * 
	 * @param map	The map.
	 * @return	The atom.
	 */
	public static BindingsAtom bind(java.util.HashMap<String,BasicAtom> map) {
		// Create a Scala map and populate it.  This is costly, but it works.
		scala.collection.immutable.HashMap<String,BasicAtom> smap =
				new scala.collection.immutable.HashMap<String,BasicAtom>();
		for (java.util.Map.Entry<String,BasicAtom> entry : map.entrySet()) {
			smap = smap.$plus(new scala.Tuple2<String,BasicAtom>(
					entry.getKey(), entry.getValue()));
		} // Populate the Scala map.
		return BindingsAtom.apply(smap);
	}

	// ======================================================================
	// Operators.
	// ======================================================================
	
	/**
	 * Obtain an operator by its name.
	 * 
	 * @param name	The operator name.
	 * @return	The operator, or an exception if it is not known.
	 * @throws	UndefinedOperatorException
	 * 			The operator is not know.
	 */
	public OperatorRef opImpl(String name) throws UndefinedOperatorException {
		return context.operatorLibrary().apply(name);
	}
	
	/**
	 * Obtain an operator by its name.
	 * 
	 * @param name	The operator name.
	 * @return	The operator, or an exception if it is not known.
	 * @throws	UndefinedOperatorException
	 * 			The operator is not know.
	 */
	public static OperatorRef op(String name) throws UndefinedOperatorException {
		return getInstance().opImpl(name);
	}

	// ======================================================================
	// The Type Universe.
	// ======================================================================
	
	/**
	 * Get the type universe.
	 * 
	 * @return	Get the type universe.
	 */
	public static TypeUniverse$ TYPE = TypeUniverse$.MODULE$;

	// ======================================================================
	// Apply.
	// ======================================================================
	
	/**
	 * Apply one atom to another.  This constructs {@code lhs.rhs}.  The
	 * result can be any atom.
	 * 
	 * @param lhs	An atom.
	 * @param rhs	An atom.
	 * @return	The result of application.
	 */
	public static BasicAtom apply(BasicAtom lhs, BasicAtom rhs) {
		return Apply.apply(lhs, rhs, false);
	}

	// ======================================================================
	// Map pairs and rewrite rules.
	// ======================================================================
	
	/**
	 * Construct a map pair.  This builds {@code lhs -> rhs}.
	 * 
	 * @param lhs	The left hand side (pattern).
	 * @param rhs	The right hand side (rewrite).
	 * @return	The map pair.
	 */
	public static MapPair mapPair(BasicAtom lhs, BasicAtom rhs) {
		return new MapPair(lhs, rhs);
	}
	
	/**
	 * Make a rewrite rule.  The rule is not automatically added to the
	 * rule library; you can use the {@code addRule} method to do that.
	 * 
	 * @param pair		The map pair that forms the core of the rule.
	 * @param guards	Guards.
	 * @param rulesets	Ruleset names.
	 * @return	The new rule.
	 */
	public static RewriteRule rule(MapPair pair, 
			java.util.List<BasicAtom> guards, Set<String> rulesets) {
		// Convert Java collections into Scala collections.
		scala.collection.immutable.List<BasicAtom> list =
				scala.collection.JavaConversions.asScalaBuffer(guards).toList();
		scala.collection.immutable.Set<String> set =
				scala.collection.JavaConversions.asScalaSet(rulesets).toSet();
		return RewriteRule.apply(pair.left(), pair.right(), list, set);
	}
	
	/**
	 * Add a rule to the rule library for the current instance.  Because of
	 * the way rule libraries work, this may generate synthetic rules to
	 * account for associativity.
	 * 
	 * @param rule	The rule to add.
	 * @return	The rule added.
	 */
	public RewriteRule addRuleImpl(RewriteRule rule) {
		context.ruleLibrary().add(rule);
		return rule;
	}

	/**
	 * Add a rule to the rule library for the current instance.  Because of
	 * the way rule libraries work, this may generate synthetic rules to
	 * account for associativity.
	 * 
	 * @param rule	The rule to add.
	 * @return	The rule added.
	 */
	public static RewriteRule addRule(RewriteRule rule) {
		return getInstance().addRuleImpl(rule);
	}
	
	// ======================================================================
	// Special Forms.
	// ======================================================================

	/**
	 * Construct a special form from the provided tag and content.  The
	 * result can be any atom.
	 * 
	 * @param tag		The tag.
	 * @param content	The content.
	 * @return	The new atom.
	 */
	public static BasicAtom specialForm(BasicAtom tag, BasicAtom content) {
		return SpecialForm.apply(tag, content);
	}
}
