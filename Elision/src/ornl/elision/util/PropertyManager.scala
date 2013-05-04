/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
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
package ornl.elision.util

import scala.collection.mutable.{OpenHashMap => HashMap}

/**
 * Manage a collection of properties.
 * 
 * To use this trait simply inherit it and implement the `clazzes` abstract
 * value to the list of classes that should be allowed.  If there are special
 * values that need a unique representation when written, override the
 * `writeProperty` value.
 * 
 * @author Stacy Prowell (prowellsj@ornl.gov)
 */
trait PropertyManager {
  val _prop2val = HashMap[String,Any]()
  val _prop2desc = HashMap[String,String]()
  val _prop2close = HashMap[String,(PropertyManager => Unit)]()
  
  /**
   * The classes that are supported by this property manager.  Give this as
   * a set of class objects.  This is easily done.  The following is an example
   * that allows storing (and retrieving) Ints, Strings, and Floats.
   * {{{
   * val clazzes = Set(classOf[Int], classOf[String], classOf[Float])
   * }}}
   * 
   * The property values `x` must be assignable to a variable of one of the
   * given types.  That is, there must be a class `C` in this set such that
   * {{{
   * C.isAssignableFrom(x)
   * }}}
   */
  val clazzes: Set[_ <: Class[_]]
  
  /**
   * Declare a property.
   * 
   * @param name        The property name.
   * @param description Human-readable description of the property.
   * @param default     The default value of the property.
   * @param onchange    A closure to execute when the property is changed
   *                    via the `setProperty` methods.  Can be omitted.
   *                    The property manager instance is passed, not the new
   *                    property value, as this provides access to all
   *                    properties.
   * @return  The default value.
   */
  def declareProperty[TYPE](name: String, description: String, default: TYPE,
      onchange: (PropertyManager => Unit) = null) = {
    val defclass = default.getClass
    if (!clazzes.exists(defclass.isAssignableFrom(_))) {
      throw new CacheException("Unsupported data type for property " +
          toQuotedString(name) + ".  " + "Got " +
          defclass + ", but require " +
          clazzes.mkString(", "))
    }
    _prop2val(name) = default
    _prop2desc(name) = description
    _prop2close(name) = onchange
    default
  }
  
  /**
   * Get the value of a property.  Properties are intended to be used to
   * control various functions of the executor and modular extensions to
   * it, and not just for storing arbitrary data during execution.
   * 
   * Properties must be declared before they can be set or read.  See
   * `declareProperty`.  The type of the property may not be changed once
   * it has been declared.
   * 
   * When getting a property value, specify the `TYPE` so the returned value
   * is correctly typed.
   * 
   * @param key     The property name.
   * @param default The default value of the property.
   * @return  The requested property value.
   */
  def getProperty[TYPE](name: String)
  (implicit mTYPE: scala.reflect.Manifest[TYPE]): TYPE = {
    _prop2val.get(name) match {
      case None =>
        throw new CacheException("No such property: " + toQuotedString(name) + ".")
      case Some(item) =>
        item.asInstanceOf[TYPE]
    }
  }
  
  /**
   * Set the value of a property.  See the documentation of `getProperty` for
   * information about the use of properties.
   * 
   * @param key     The property name.
   * @param value   The property value.
   * @return  The new property value.
   */
  def setProperty[TYPE](name: String, value: TYPE)
  (implicit mTYPE: scala.reflect.Manifest[TYPE]) = {
    _prop2val.get(name) match {
      case None =>
        throw new CacheException("No such property: " + toQuotedString(name) + ".")
      case Some(item) =>
        _prop2val(name) = value
        _prop2close(name) match {
          case null =>
          case x => x(this)
        }
        value
    }
  }
  
  /**
   * Get all declared properties.
   * 
   * @return  The returned value is a set of entries of the form (name,
   *          description, value).
   */
  def getProperties = {
    var list = List[(String, String, Any)]()
    for (name <- _prop2val.
        keySet.
        toList.
        sortWith(_.toLowerCase < _.toLowerCase).
        filter(!_.startsWith("_"))) {
      val description = _prop2desc(name)
      val value = _prop2val(name)
      list :+= ((name, description, value))
    } // Collect all properties.
    list
  }
  
  /**
   * Get all public property names, sorted in alphabetical order.
   * 
   * @return  The property names.
   */
  def getPropertyNames =
    _prop2val.
    keys.
    toList.
    sortWith(_.toLowerCase < _.toLowerCase).
    filter(!_.startsWith("_"))
    
  /**
   * Write properties to a console.
   * 
   * @param console The console to get the output.
   * @param width   The width of the console, or 80 if not specified.
   */
  def writeProperties(console: Console, width: Int = 80) {
    // Get the property names and compute the longest.
    val pwidth = getPropertyNames.foldLeft(0)(_ max _.length)
    
    // There must be dots between the property name and the description,
    // and there must be spaces around these.
    val remain = width - 5 - pwidth
    
    // Compute the width of the description.
    val dwidth = (if (remain >= 20) remain else (20 max (width-5)))
    
    // Now write the properties.
    val text = new ornl.elision.util.Text
    for ((name, description, value) <- getProperties) {
      // Format the description.
      text.add(description + "  ("+writeProperty(value)+")")
      val lines = text.wrap(dwidth-2)
      text.clear
      
      // Write out the property name.
      console.send(name)
      
      // Write the remainder.
      if (remain >= 20) {
        var lead = " "+("."*(width-dwidth-name.length))+" "
        for (line <- lines) {
          console.sendln(lead + line)
          lead = " "+(" "*(width-dwidth))+" "
        } // Print all lines.
      } else {
        console.sendln("")
        var lead = " "+("."*(width-dwidth))+" "
        for (line <- lines) {
          console.sendln(lead + line)
          lead = " "+(" "*(width-dwidth))+" "
        } // Print all lines.
      }
    } // Print all properties.
  }
  
  /**
   * Write a single property.  This is used by `writeProperties` to write the
   * value of each property.  If you have a property that requires special
   * handling or representation, override this.  Use `super.writeProperty` for
   * values you do not wish to handle.
   * 
   * Alternately you can just override this method in the derived class for
   * the values you want to catch.
   * 
   * For example:
   * {{{
   * def writeProperty(value: Int) = "[+value+"]"
   * }}}
   * This will write `Int` values with square brackets, and will defer to the
   * base class for all other values.
   * 
   * By default this method uses the `toString` method for the value.
   * 
   * @param value The value to write.
   * @return  The string value to write.
   */
  def writeProperty(value: Any) = {
    value.toString
  }
}
