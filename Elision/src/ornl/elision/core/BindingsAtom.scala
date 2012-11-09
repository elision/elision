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
======================================================================
* */
package ornl.elision.core

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import ornl.elision.repl.ReplActor

/**
 * Encapsulate a set of bindings as an atom.
 * 
 * While bindings are a special form, they are also a key ingredient of most
 * special forms, and thus get some specialized handling here.  In particular,
 * they do not extend [[ornl.elision.core.SpecialForm]], though this might
 * change.  This helps avoid a loop in the use hierarchy.
 * 
 * == Purpose ==
 * A bindings atom wraps a set of bindings and allows them to be treated as if
 * they were an atom (matched and rewritten, for instance).  Since this is
 * costly, and since bindings are critical to the operation of the rewriter,
 * this class is typically used "just in time" by an implicit conversion.
 *
 * == Structure and Syntax ==
 * Bindings are a special form, and the general syntax is the tag ''bind''
 * and content equal to a list of map pairs, each of whose left-hand sides
 * must be a symbol.
 * 
 * == Type ==
 * All bindings atoms have the special type BINDING.
 * 
 * == Equality and Matching ==
 * Bindings are equal iff they bind the same symbols to equal values.  They
 * match only if they bind the same symbols, and their respective bindings
 * match.
 */
case class BindingsAtom(mybinds: Bindings) extends BasicAtom with Applicable {
  require(mybinds != null, "Bindings are null.")
  
  /** The type of a bindings atom is the special bindings type. */
  val theType = ANY
  val isConstant = mybinds.values.forall(_.isConstant)
  val isTerm = mybinds.values.forall(_.isTerm)
  val deBruijnIndex = mybinds.values.foldLeft(0)(_ max _.deBruijnIndex)
  val depth = mybinds.values.foldLeft(0)(_ max _.depth) + 1
    
  /**
   * Match this bindings atom against the provided atom.
   * 
   * Two binding atoms match iff they bind the same variables to terms that
   * can be matched.  The variables that are bound cannot be matched against
   * variables, but the bindings can be.
   */
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) = subject match {
    case BindingsAtom(obinds) =>
      // The bindings must bind the same variables.  Check that first.
      if (mybinds.keySet != obinds.keySet) {
        Fail("Bindings bind different variables.", this, subject)
      } else {
	      // Now iterate over the keys.  The ordering does not matter.  This
	      // creates two lists of atoms that we then match using the sequence
        // matcher.
	      var mine = OmitSeq[BasicAtom]()
	      var theirs = OmitSeq[BasicAtom]()
	      for ((key, value) <- mybinds) {
	        mine :+= value
	        theirs :+= obinds(key)
	      } // Build lists of atoms.
	      SequenceMatcher.tryMatch(mine, theirs, binds)
      }
    case _ => Fail("Bindings can only match other bindings.", this, subject)
  }
	
	//////////////////// GUI changes
	
  def doRewrite(binds: Bindings) = {
	ReplActor ! ("Eva", "pushTable", "BindingsAtom rewrite")
    // top node of this subtree
	ReplActor ! ("Eva", "addToSubroot", ("rwNode", "BindingsAtom rewrite: ")) // val rwNode = RWTree.addToCurrent("BindingsAtom")
	
    var changed = false
    var newmap = Bindings()
    for ((key, value) <- mybinds) {	
	  ReplActor ! ("Eva", "addTo", ("rwNode", "val", key + " -> ", value)) //val valNode = RWTree.addTo(rwNode, key + " -> ", value) 
	  ReplActor ! ("Eva", "setSubroot", "val") // RWTree.current = valNode
        
      val (newvalue, valuechanged) = value.rewrite(binds)
      
	  ReplActor ! ("Eva", "addTo", ("val", "", newvalue)) // RWTree.addTo(valNode, newvalue)
      
      changed |= valuechanged
      newmap += (key -> newvalue)
    } // Rewrite all bindings.
    if (changed) {
		ReplActor ! ("Eva", "setSubroot", "rwNode") // RWTree.current = rwNode
		val newBA = BindingsAtom(newmap)
		ReplActor ! ("Eva", "addTo", ("rwNode", "", newBA)) // RWTree.addTo(rwNode, newBA)
        ReplActor ! ("Eva", "popTable", "BindingsAtom rewrite")
		(newBA, true) 
	} else {
        ReplActor ! ("Eva", "popTable", "BindingsAtom rewrite")
        (this, false)
    }
  }
  
  //////////////////// end GUI changes
    
  override def equals(other: Any) = other match {
    case BindingsAtom(obinds) if (obinds == mybinds) => true
    case _ => false
  }
  
  //////////////////// GUI changes
  
  def doApply(atom: BasicAtom, bypass: Boolean) = {
		ReplActor ! ("Eva", "pushTable", "BindingsAtom doApply")
        // top node of this subtree
		ReplActor ! ("Eva", "addToSubroot", ("rwNode", "BindingsAtom doApply: ")) //val rwNode = RWTree.addToCurrent("BindingsAtom doApply: ") 
		ReplActor ! ("Eva", "addTo", ("rwNode", "atom", atom)) //val atomNode = RWTree.addTo(rwNode, atom) 
		
		ReplActor ! ("Eva", "setSubroot", "atom") // RWTree.current = atomNode
		// Check the argument to see if it is a single symbol.
		atom match {
		  case SymbolLiteral(SYMBOL, sym) =>
			// Try to extract the symbol from the binding.  If it is not there,
			// then the answer is NONE.
			mybinds.get(sym.name) match {
			  case Some(oatom) => 
				ReplActor ! ("Eva", "addTo", ("atom", "", oatom)) //RWTree.addTo(atomNode, oatom)
				ReplActor ! ("Eva", "addTo", ("rwNode", "", oatom)) //RWTree.addTo(rwNode, oatom)
                
                ReplActor ! ("Eva", "popTable", "BindingsAtom doApply")
				oatom
			  case _ => 
				ReplActor ! ("Eva", "addTo", ("atom", "", NONE)) // RWTree.addTo(atomNode, NONE)
				ReplActor ! ("Eva", "addTo", ("rwNode", "", NONE)) // RWTree.addTo(rwNode, NONE) 
				
                ReplActor ! ("Eva", "popTable", "BindingsAtom doApply")
                NONE
			}
		  case _ =>
			  // Try to rewrite the argument using the bindings and whatever we get
			  // back is the result.
			  val newatom = atom.rewrite(mybinds)._1
			  ReplActor ! ("Eva", "addTo", ("atom", "", newatom)) // RWTree.addTo(atomNode, newatom)
			  ReplActor ! ("Eva", "addTo", ("rwNode", "", newatom)) // RWTree.addTo(rwNode, newatom)
			  
              ReplActor ! ("Eva", "popTable", "BindingsAtom doApply")
              newatom
		}
	}
	
	//////////////////// end GUI changes
}

/**
 * Simplified construction of bindings atoms.
 */
object BindingsAtom {
  /** The special form tag. */
  val tag = Literal('binds)
  
  /**
   * Make a bindings atom from the specified special form data.
   * 
   * @param sfh	Parsed special form data.
   */
  def apply(sfh: SpecialFormHolder): BindingsAtom = sfh.content match {
    case AtomSeq(_, atoms) => _build(atoms)
    case _ =>
      val bh = sfh.requireBindings
      bh.check(Map("" -> false))
      val seq = bh.fetchAs[AtomSeq]("", Some(EmptySeq))
      _build(seq.atoms)
  }
  
  /**
   * Make a bindings atom directly froma map.
   * 
   * @param map The map.
   * @return  The new atom.
   */
  def apply(map: HashMap[String,BasicAtom]) =
    new BindingsAtom(new Bindings(map))
  
  /**
   * Make a bindings atom from the provided sequence of atoms.  Every atom
   * must be an instance of [[ornl.elision.core.MapPair]].
   * 
   * @param atoms	The atoms making up the binding.
   */
  private def _build(atoms: Seq[BasicAtom]) = atoms.foldLeft(Bindings()) {
    (binds, atom) => binds + (atom match {
      case MapPair(left, right) => left match {
        case SymbolLiteral(_, sym) => (sym.name -> right)
        case _ =>
          throw new SpecialFormException(
              "Invalid binding specification: " + atom.toParseString)
      }
      case _ =>
        throw new SpecialFormException(
            "Invalid binding specification (not a map pair): " +
            atom.toParseString)
    })
  }
}
