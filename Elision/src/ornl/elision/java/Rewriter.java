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

import ornl.elision.core.BasicAtom;
import ornl.elision.core.Executor;
import ornl.elision.core.RuleLibrary;

/**
 * Provide programmatic control and access to the automatic rewriting
 * functionality of the rule library.
 * 
 * This obtains the rule library via an {@link AtomFactory} instance, so
 * make sure that instance is correctly configured.
 */
public class Rewriter {

	// ======================================================================
	// Instance data.
	// ======================================================================
	
	/** The rule library. */
	protected RuleLibrary library = null;
	
	/**
	 * Make a new instance with the specified executor.
	 * 
	 * @param exec	The executor.
	 */
	public Rewriter(Executor exec) {
		if (exec == null) {
			throw new NullPointerException("The executor is null.");
		}
		library = exec.context().ruleLibrary();
	}

	// ======================================================================
	// Singleton methods.
	// ======================================================================

	/** The factory instance. */
	private static Rewriter _instance = null;
	
	/**
	 * Configure the default instance of the atom factory.  This must be
	 * done prior to using any methods that require access to a context or
	 * the executor, or seriously bad things may happen.
	 * 
	 * @param exec	The executor.
	 * @return	The configured default instance.
	 */
	public static Rewriter configure(Executor exec) {
		_instance = new Rewriter(exec);
		return _instance;
	}

	/**
	 * Set the atom factory instance to use.
	 * 
	 * @param factory
	 *            The instance to use.
	 * @return The factory instance.
	 */
	public static Rewriter setInstance(Rewriter factory) {
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
	public static Rewriter getInstance() {
		if (_instance == null) {
			// Go and get the default executor.
			_instance =
					new Rewriter(ornl.elision.core.package$.MODULE$.knownExecutor());
		}
		return _instance;
	}

	// ======================================================================
	// Rewriting controls.
	// ======================================================================
	
	/**
	 * Set the rewrite limit.  Use zero to disable rewrites, and a negative
	 * number for *no* limit.
	 * 
	 * @param limit	The limit on rewriting.
	 * @return	This rewriter.
	 */
	public Rewriter setLimit(int limit) {
		library.setLimit(scala.math.BigInt.apply(limit));
		return this;
	}
	
	/**
	 * Whether to descend into child nodes during rewriting.
	 * 
	 * @param flag	True to descend, false to prevent.
	 * @return	This rewriter.
	 */
	public Rewriter setDescend(boolean flag) {
		library.setDescend(flag);
		return this;
	}
	
	/**
	 * Enable a ruleset.
	 * 
	 * @param ruleset	The ruleset name.
	 * @return	This rewriter.
	 */
	public Rewriter enable(String ruleset) {
		library.enableRuleset(ruleset);
		return this;
	}
	
	/**
	 * Disable a ruleset.
	 * 
	 * @param ruleset	The ruleset name.
	 * @return	This rewriter.
	 */
	public Rewriter disable(String ruleset) {
		library.disableRuleset(ruleset);
		return this;
	}
	
	// ======================================================================
	// Perform rewriting.
	// ======================================================================
	
	/**
	 * Package a rewrite result.
	 */
	public static class Result {
		/**
		 * Make a new rewrite result.
		 * 
		 * @param atom	The atom.
		 * @param flag	The flag.
		 */
		public Result(BasicAtom atom, boolean flag) {
			this.atom = atom;
			this.flag = flag;
		}
		
		/** The atom. */
		public BasicAtom atom;
		
		/** Whether a rewrite was performed. */
		public boolean flag;
	}
	
	/**
	 * Perform rewriting and return a result that contains a flag that is
	 * true if rewriting was successful, and false if not.
	 * 
	 * All enabled rulesets are used.
	 * 
	 * @param atom	The atom to rewrite.
	 * @return	The result.
	 */
	public Result testRewrite(BasicAtom atom) {
		if (atom == null) {
			throw new NullPointerException("The atom is null.");
		}
		scala.Tuple2<BasicAtom, Object> res = library.rewrite(atom);
		boolean flag = scala.Boolean.unbox(res._2);
		return new Result(res._1, flag);
	}
	
	/**
	 * Rewrite the requested atom.  Do not track the flag; just return the
	 * result.
	 * 
	 * All enabled rulesets are used.
	 * 
	 * @param atom	The atom to rewrite.
	 * @return	The result of rewriting.
	 */
	public BasicAtom rewrite(BasicAtom atom) {
		if (atom == null) {
			throw new NullPointerException("The atom is null.");
		}
		return library.rewrite(atom)._1;
	}
	
	/**
	 * Perform rewriting and return a result that contains a flag that is
	 * true if rewriting was successful, and false if not.
	 * 
	 * @param atom			The atom to rewrite.
	 * @param rulesets		The rulesets to use.
	 * @return	The result.
	 */
	public Result testRewrite(BasicAtom atom, java.util.Set<String> rulesets) {
		if (atom == null) {
			throw new NullPointerException("The atom is null.");
		}

		// Convert the rulesets into a Scala set.
		scala.collection.immutable.Set<String> set =
				scala.collection.JavaConversions.asScalaSet(rulesets).toSet();
		scala.Tuple2<BasicAtom, Object> res = library.rewrite(atom, set);
		boolean flag = scala.Boolean.unbox(res._2);
		return new Result(res._1, flag);
	}
	
	/**
	 * Rewrite the requested atom.  Do not track the flag; just return the
	 * result.
	 * 
	 * All enabled rulesets are used.
	 * 
	 * @param atom	The atom to rewrite.
	 * @param rulesets		The rulesets to use.
	 * @return	The result of rewriting.
	 */
	public BasicAtom rewrite(BasicAtom atom, java.util.Set<String> rulesets) {
		if (atom == null) {
			throw new NullPointerException("The atom is null.");
		}
		// Convert the rulesets into a Scala set.
		scala.collection.immutable.Set<String> set =
				scala.collection.JavaConversions.asScalaSet(rulesets).toSet();
		return library.rewrite(atom, set)._1;
	}
}
