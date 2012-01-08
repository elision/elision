/* Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 */
package sjp.elision.core

import scala.collection.mutable.HashMap

/**
 * Represent a rule.
 * @param pattern		The pattern to match.
 * @param rewrite		The rewrite.
 * @param guards		The (zero or more) guards.
 */
case class Rule(pattern: BasicAtom, rewrite: BasicAtom, guards: BasicAtom*)
extends BasicAtom {
  /** All rules have the same type. */
	val theType = TypeUniverse
	
	private def matchGuards(otherGuards: BasicAtom*, binds: Bindings) = {
	  guards.zip(otherGuards) forall {
	    (pg: BasicAtom, og:BasicAtom) =>
	      pg.tryMatch(og, binds) match {
	        case fail:Fail => 
	        
	      }
	  }
	}
  
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    subject match {
	  case Rule(opattern, orewrite, oguards@_*) =>
	    pattern.tryMatch(opattern, binds) match {
	      case fail:Fail => Fail("The rule patterns do not match.", this, subject, fail)
	      case Match(patbind) =>
	        rewrite.tryMatch(orewrite, binds) match {
	          case fail:Fail => Fail("The rule rewrites do not match.", this, subject, fail)
	          case Match(rewbind) => {
	            // The guards must match.
	            
	          }
	        }
	    }
	}

  def rewrite(binds: Bindings) = (this, false)

}