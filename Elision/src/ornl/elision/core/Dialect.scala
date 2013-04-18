/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2013 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *  - Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package ornl.elision.core

import scala.collection.immutable.Map
import scala.io.Source
import ornl.elision.util.ElisionException
import ornl.elision.util.Loc
import ornl.elision.util.Version

/**
 * Hold the registry of known dialects.  Also provide the classes that
 * provide data transfer to and from a dialect.
 */
object Dialect extends Fickle with Mutable {
  
  /** The registry of known dialects indexed by dialect name. */
  private var _registry = Map[Symbol, Dialect]();
  
  /**
   * The named dialect was not found in the registry.
   */
  class NoSuchDialect(loc: Loc, msg: String) extends ElisionException(loc, msg);
  
  /**
   * Base class for all parse results.
   */
  sealed abstract class Result
  
  /**
   * Result when parsing fails.
   * 
   * @param loc   Location of failure, if known.
   * @param msg   The failure message.
   */
  case class Failure(loc: Loc, msg: String) extends Result
  
  /**
   * Result when parsing succeeds.  A (possibly empty) sequence of atoms is
   * provided.
   * 
   * @param atoms The atoms parsed, in order.
   */
  case class Success(atoms: Seq[BasicAtom]) extends Result
  
  /**
   * Declare a dialect.  Any prior dialect for the given symbol is discarded!
   * 
   * @param name    The name of the dialect.
   * @param dialect The dialect.
   * @return  The dialect.
   */
  def declare(name: Symbol, dialect: Dialect) = {
    _registry += (name -> dialect)
    dialect
  }
  
  /**
   * Find a dialect for the given name.  No error is generated if the dialect
   * does not exist.
   * 
   * @param dialect   The dialect name.
   * @return The optional dialect.
   */
  def apply(dialect: Symbol) = _registry.get(dialect)
  
  /**
   * Serialize the given atom to the provided appendable in the specified
   * dialect.  If the dialect is not known, an exception is generated.
   * 
   * @param dialect   Name of the dialect.
   * @param app       Appendable to get output.
   * @param atom      The basic atom to serialize.
   * @param limit     A descent limit for the generator, if relevant to the
   *                  dialect.  The value -1 (the default) disables this.
   * @return  The appendable.
   */
  def serialize(dialect: Symbol, app: Appendable, atom: BasicAtom,
      limit: Int = -1) = apply(dialect) match {
    case None =>
      throw new NoSuchDialect(Loc.internal,
          "The dialect "+toESymbol(dialect.name)+" is not known.")
      
    case Some(dia) =>
      dia.serialize(app, atom)
  }
  
  /**
   * Parse a sequence of atoms from the given source.    If the dialect is
   * not known, an exception is generated.
   * 
   * @param dialect The dialect to use for parsing.
   * @param name    Name of the data source.
   * @param source  The data source.
   */
  def parse(dialect: Symbol, name: String, source: Source) =
    apply(dialect) match {
    case None =>
      throw new NoSuchDialect(Loc.internal,
          "The dialect "+toESymbol(dialect.name)+" is not known.")
      
    case Some(dia) =>
      dia.parse(name, source)
  }
  
  // Get the dialects now and configure the registry.  We need to load the
  // dialects from the version class that parsed the configuration file.
  // This happens the first time a dialect is requested.
  Version.getDialects map {
    pair =>
      // Cast the class.
      try {
        declare(pair._1, pair._2.asInstanceOf[Dialect])
      } catch {
        case ex: Exception =>
          // Not a dialect; misconfigured!
          knownExecutor.console.error("ERROR: Elision is mis-configured.  " +
              "The class "+pair._2.getClass.toString+" for dialect " +
              pair._1.name + " is not a subclass of Dialect.")
          throw new ElisionException(Loc.internal,
              "ERROR: Elision is mis-configured.  The class for " +
              "dialect " + pair._1.name + " is not a subclass of Dialect.")
      }
  } // Add all declared dialects.
}

/**
 * A dialect specifies the conversion of an atom to and from another form.
 * 
 * To use this extend it and implement the desired methods.  Override the
 * fields to specify what sort of data transfer is possible.
 */
abstract class Dialect extends Fickle {

  /**
   * Can data in this dialect be parsed to create atom(s)?  If this is true,
   * override the `parse` method, and override this to be `true`.
   */
  val canParse = false
  
  /**
   * Can data in this dialect be generated from an atom?  If this is true,
   * override the `serialize` method, and override this to be `true`.
   */
  val canSerialize = false
  
  /**
   * Serialize the given atom in this dialect to the given appendable.  If
   * the dialect does not support serialization (see `canSerialize`) then
   * nothing is done.
   * 
   * @param app       The appendable.
   * @param atom      The atom.
   * @param limit     A descent limit for the generator, if relevant to the
   *                  dialect.  The value -1 (the default) disables this.
   * @return  The appendable.
   */
  def serialize(app: Appendable, atom: BasicAtom, limit: Int = -1) = app
  
  /**
   * Parse from the named, provided source, and generate a result.
   * 
   * @param name    Name of the source, for location information.
   * @param source  The data source.
   * @return  The result of parsing.
   */
  def parse(name: String, source: Source): Dialect.Result =
    Dialect.Failure(Loc.internal, "Parsing is not implemented for this dialect.")
}